#lang racket/base
;; The Dagger session: GraphQL over HTTP on localhost, opened by `dagger api with-session`.

(require racket/port net/http-client net/base64 json)
(provide session? query (struct-out exn:engine))

;; An engine error, with the extensions it sends (an EXEC_ERROR has cmd, exitCode, stdout, stderr).
(struct exn:engine exn:fail (extensions))

(define (session?) (and (getenv "DAGGER_SESSION_PORT") (getenv "DAGGER_SESSION_TOKEN") #t))

;; A GraphQL document → the value of its leaf; engine errors are raised.
(define (query text)
  (unless (session?) (error 'disegno "no Dagger session: run under `dagger api with-session`"))
  (define token (bytes->string/utf-8 (base64-encode (string->bytes/utf-8 (string-append (getenv "DAGGER_SESSION_TOKEN") ":")) #"")))
  (define-values (status headers body)
    (http-sendrecv "127.0.0.1" "/query"
                   #:port (string->number (getenv "DAGGER_SESSION_PORT"))
                   #:method "POST"
                   #:headers (list (string-append "Authorization: Basic " token) "Content-Type: application/json")
                   #:data (jsexpr->string (hasheq 'query text))))
  (define r (read-json body))
  (define errors (hash-ref r 'errors '()))
  (unless (null? errors)
    (define e (car errors))
    (raise (exn:engine (string-append "engine: " (hash-ref e 'message "error")) (current-continuation-marks)
                       (hash-ref e 'extensions (hasheq)))))
  (let leaf ([v (hash-ref r 'data)])               ; { container { from { stdout } } } → the stdout
    (if (and (hash? v) (= (hash-count v) 1)) (leaf (car (hash-values v))) v)))
