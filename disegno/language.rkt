#lang racket/base
;; #lang disegno: bottega, plus the plan. The module is checked when it is compiled, like a
;; bottega module; if it has a main, its main submodule realizes it:
;;   racket prog.rkt          the plan as GraphQL, or the run under `dagger api with-session`
;;   racket prog.rkt plan     the outer design: one GraphQL per outcome of the branches
;;   racket prog.rkt run      the work, on Dagger

(require (for-syntax racket/base bottega/check "dialect.rkt"))
(provide (rename-out [module-begin #%module-begin]) require)

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     (let* ([forms (syntax->list #'(form ...))]
            [source (and (pair? forms) (path? (syntax-source (car forms))) (syntax-source (car forms)))])
       (define tooltips (parameterize ([current-languages languages]) (check-module source forms)))
       (define-values (specs _) (split-requires forms))
       (define has-main? (for/or ([f forms]) (let ([d (syntax->datum f)]) (and (pair? d) (eq? (car d) 'claim) (pair? (cdr d)) (eq? (cadr d) 'main)))))
       (with-syntax ([(spec ...) (cons #'bottega/lib/prelude specs)]
                     [hover (syntax-property #'(void) 'mouse-over-tooltips tooltips)]
                     [file (and source (path->string source))])
         (if (and has-main? source)
             #'(#%module-begin
                (require spec ...)
                hover
                (module+ main
                  (require disegno/run)
                  (main-program file)))
             #'(#%module-begin (require spec ...) hover))))]))
