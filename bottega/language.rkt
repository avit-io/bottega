#lang racket/base
;; #lang bottega: a module of bottega declarations. It is checked when it is compiled, so an
;; error stops compilation at its position (raco make, DrRacket, racket-langserver).
;; Racket only sees the requires: bottega modules compile in dependency order and recompile
;; when a dependency changes.

(require (for-syntax racket/base "check.rkt"))
(provide (rename-out [module-begin #%module-begin]) require)

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     (let* ([forms (syntax->list #'(form ...))]
            [source (and (pair? forms) (syntax-source (car forms)))])
       (define tooltips (check-module (and (path? source) source) forms))
       ;; the prelude is implicit; requiring it too makes a change to it recompile every module
       (let-values ([(specs _) (split-requires forms)])
         ;; the hover tooltips ride on an expression of the body, where check-syntax finds them
         (with-syntax ([(spec ...) (if (prelude? source) specs (cons #'bottega/lib/prelude specs))]
                       [hover (syntax-property #'(void) 'mouse-over-tooltips tooltips)])
           #'(#%module-begin (require spec ...) hover))))]))
