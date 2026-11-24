#lang racket/base
;; #lang prospettiva: bottega, plus the projections. The module is checked when it is compiled;
;; if it has a main, its main submodule renders it:
;;   racket model.rkt render DIR   DIR/<env>/values.yaml, for every environment
;;   racket model.rkt show ENV     one environment, on stdout
;;   racket model.rkt check        types and theorems

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
                  (require prospettiva/render)
                  (main-program file)))
             #'(#%module-begin (require spec ...) hover))))]))
