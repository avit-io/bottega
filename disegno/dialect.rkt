#lang racket/base
;; What #lang disegno adds to bottega: the plan's builtins (Obj, and main's inputs), the schema's
;; root fields (container is Query.container), and its library, disegno/lib.

(require racket/runtime-path bottega/core/state bottega/core/surface bottega/check)
(provide languages)

(define-runtime-path lib "lib")

(register-builtin-keys! '((obj . data) (host . def) (host-secret . def) (host-env . def) (host-choice . def)))
(register-root-namespace! "Query")
(register-library! lib)

;; A disegno module may require bottega modules and disegno modules.
(define languages '("bottega" "disegno"))

;; The plan's sugar, as in Scheme: begin realizes in sequence, when and unless decide on an
;; observation while the plan runs.
;;   (begin a b c)     (then a (then b c))
;;   (when c a)        (branch c a skip)
;;   (unless c a)      (branch c skip a)
(define (rebuild like datum) (datum->syntax like datum like))

(register-sugar! 'begin
  (λ (stx)
    (define xs (cdr (syntax->list stx)))
    (when (null? xs) (raise-syntax-error 'begin "empty begin" stx))
    (let loop ([xs xs]) (if (null? (cdr xs)) (car xs) (rebuild stx `(then ,(car xs) ,(loop (cdr xs))))))))

(register-sugar! 'when
  (λ (stx) (syntax-case stx () [(_ c a) (rebuild stx `(branch ,#'c ,#'a skip))])))

(register-sugar! 'unless
  (λ (stx) (syntax-case stx () [(_ c a) (rebuild stx `(branch ,#'c skip ,#'a))])))
