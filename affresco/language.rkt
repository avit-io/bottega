#lang racket/base
;; #lang affresco: bottega, plus Html and Program (affresco/lib/html, always required). The
;; module is checked when it is compiled, proofs included; if it has a main, its main submodule
;; runs it:
;;   racket app.rkt show         the initial view, as HTML
;;   racket app.rkt serve PORT   the program, on http://localhost:PORT
;;   racket app.rkt check        types and proofs

(require (for-syntax racket/base bottega/check "dialect.rkt"))
(provide (rename-out [module-begin #%module-begin]) require)

;; The library itself is affresco too, but does not require itself.
(define-for-syntax (in-library? source)
  (and source (regexp-match? #rx"/affresco/lib/[^/]*$" (path->string source))))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ form ...)
     (let* ([written (syntax->list #'(form ...))]
            [source (and (pair? written) (path? (syntax-source (car written))) (syntax-source (car written)))]
            [forms written])
       (define tooltips (parameterize ([current-languages languages] [current-implicit implicit]) (check-module source forms)))
       (define-values (written-specs _) (split-requires forms))
       ;; Racket sees the implicit library too: a change to it recompiles the program
       (define specs (if (in-library? source) written-specs (append written-specs (map (λ (m) (datum->syntax stx m)) implicit))))
       (define has-main? (for/or ([f forms]) (let ([d (syntax->datum f)]) (and (pair? d) (eq? (car d) 'claim) (pair? (cdr d)) (eq? (cadr d) 'main)))))
       (with-syntax ([(spec ...) (cons #'bottega/lib/prelude specs)]
                     [hover (syntax-property #'(void) 'mouse-over-tooltips tooltips)]
                     [file (and source (path->string source))])
         (if (and has-main? source)
             #'(#%module-begin
                (require spec ...)
                hover
                (module+ main
                  (require affresco/run)
                  (main-program file)))
             #'(#%module-begin (require spec ...) hover))))]))
