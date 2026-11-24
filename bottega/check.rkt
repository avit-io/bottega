#lang racket/base
;; Checking a program: read it, desugar it, check its declarations in order, after the prelude.
;; ponytail: the first error stops the file; phase 3 (#lang bottega) reports through the editor.

(require racket/runtime-path
         "core/state.rkt" "core/surface.rkt" "core/define.rkt")
(provide check-file check-string call-with-prelude)

(define-runtime-path prelude-path "lib/prelude.bottega")

(define (read-all name in)
  (port-count-lines! in)
  (let loop ([acc '()])
    (define stx (read-syntax name in))
    (if (eof-object? stx) (reverse acc) (loop (cons stx acc)))))

(define (check-port name in)
  (for-each check-declaration (parse-program (read-all name in))))

(define (check-path path)
  (call-with-input-file path (λ (in) (check-port path in))))

(define (load-prelude)
  (parameterize ([privileged #t]) (check-path prelude-path)))

;; Each check starts from a clean state with the prelude loaded.
(define (call-with-prelude thunk) (with-fresh-state (λ () (load-prelude) (thunk))))

(define (check-file path) (call-with-prelude (λ () (check-path path))))
(define (check-string s) (call-with-prelude (λ () (check-port "<string>" (open-input-string s)))))
