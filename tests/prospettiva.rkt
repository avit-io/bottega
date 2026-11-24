#lang racket/base
;; #lang prospettiva: the example's projection, as YAML (tests/golden), and the models whose
;; invariants do not hold.

(require rackunit racket/file racket/string racket/runtime-path
         bottega/check prospettiva/render (only-in prospettiva/dialect languages))

(define-runtime-path root "..")
(define model (build-path root "cookbook" "prospettiva" "service.rkt"))

(check-equal? (render-text model "qa") (file->string (build-path root "tests" "golden" "prospettiva-service-qa.yaml")))

;; The example with one change: the model must be rejected, with this message.
(define (rejects-change from to expected)
  (define src (string-replace (file->string model) from to #:all? #f))
  (check-true (string-contains? src to))
  (define msg (with-handlers ([exn:fail? exn-message])
                (parameterize ([current-languages languages])
                  (check-string (cadr (regexp-match #px"^#lang prospettiva\n(.*)$" src))))
                #f))
  (check-true (and msg (string-contains? msg expected) #t) (format "~a → ~a: expected «~a», got ~s" from to expected msg)))

;; a legacy cluster with the nonroot port: legacy-80 no longer holds
(rejects-change "(legacy 80)" "(legacy 8080)" "auto: the proposition reduces to ⊥")
;; qa moved to the new cluster: it no longer has prod's port
(rejects-change "(qa   (cluster \"eks-legacy\" legacy))" "(qa   (cluster \"eks-blue\" chart))" "expected (≡ 8080 80)")
;; a region that does not exist is a type error while typing
(rejects-change "\"eu-west-1\" (port-of" "\"eu-west-9\" (port-of" "is not a valid Region")
