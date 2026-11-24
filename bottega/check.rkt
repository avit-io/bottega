#lang racket/base
;; Checking a bottega module: the prelude, then its requires (each module once, from its
;; source), then its declarations in order. The modules in bottega/lib are the standard
;; library: only there are extern and builtin allowed.
;; ponytail: dependencies are rechecked from source on every check; cache them if it gets slow.
;; ponytail: the first error stops the module.

(require racket/runtime-path racket/string racket/list racket/match syntax/modresolve
         "core/state.rkt" "core/surface.rkt" "core/define.rkt" "core/ast.rkt" "core/eval.rkt" "core/print.rkt")
(provide check-module check-file check-string call-with-prelude split-requires prelude?)

(define-runtime-path lib-dir "lib")
(define-runtime-path prelude-path "lib/prelude.rkt")

(define (normal p) (simplify-path (path->complete-path p)))

(define (prelude? path) (and (path? path) (equal? (normal path) (normal prelude-path))))

(define (in-library? path)
  (and path (string-prefix? (path->string (normal path)) (path->string (normal lib-dir)))))

;; --- reading ---

;; The forms of a bottega module file, after its #lang line.
(define (read-module-forms path)
  (call-with-input-file path
    (λ (in)
      (port-count-lines! in)
      (unless (regexp-match? #rx"^#lang bottega[ \t]*$" (read-line in))
        (error 'bottega "~a: only bottega modules can be required (#lang bottega)" path))
      (read-forms path in))))

(define (read-forms name in)
  (let loop ([acc '()])
    (define stx (read-syntax name in))
    (if (eof-object? stx) (reverse acc) (loop (cons stx acc)))))

;; (require m …) forms apart from the declarations. → (values module-paths declarations)
(define (split-requires forms)
  (define-values (requires declarations)
    (partition (λ (f) (let ([d (syntax->datum f)]) (and (pair? d) (eq? (car d) 'require)))) forms))
  (values (append-map (λ (r) (cdr (syntax->list r))) requires) declarations))

;; --- checking ---

;; path: the module's file, or #f (a string: relative requires resolve from the current directory).
;; → the module's hover tooltips.
(define (check-module path forms)
  (with-fresh-state
    (λ ()
      (define loaded (make-hash))                  ; normalized path → #t
      (unless (prelude? path) (load-dependency! prelude-path loaded))
      (filter values (check-forms path forms loaded)))))

(define (check-forms path forms loaded)
  (define-values (specs declarations) (split-requires forms))
  (for ([s specs])
    (at (syntax-loc s)
      (define file (with-handlers ([exn:fail? (λ (_) #f)])
                     (resolve-module-path (syntax->datum s) (and path (normal path)))))
      (unless (and file (file-exists? file)) (oops "require: module ~s not found" (syntax->datum s)))
      (load-dependency! file loaded)))
  (parameterize ([privileged (in-library? path)])
    (for/list ([d (parse-program declarations)])
      (check-declaration d)
      (tooltip d))))

;; What the editor shows on hover over a declaration: the type of the name it declares.
;; → (vector syntax start end text), positions from 0, as DrRacket's check-syntax wants; or #f.
;; ponytail: on the whole declaration; per-name positions when the surface keeps them.
(define (tooltip d)
  (define-values (loc name)
    (match d
      [(d-define loc n _ _ _) (values loc n)]
      [(d-data loc n _ _) (values loc n)]
      [(d-extern loc n _ _) (values loc n)]
      [_ (values #f #f)]))
  (and loc (srcloc-position loc) (srcloc-span loc) (lookup name)
       (vector (datum->syntax #f name loc) (sub1 (srcloc-position loc)) (+ (sub1 (srcloc-position loc)) (srcloc-span loc))
               (format "~a : ~s" name (term->sexp '() (reify 0 (global-type (lookup name))))))))

(define (load-dependency! path loaded)
  (define key (normal path))
  (unless (hash-ref loaded key #f)
    (hash-set! loaded key #t)
    (check-forms path (read-module-forms path) loaded)))

(define (check-file path) (check-module path (read-module-forms path)))

(define (check-string s) (check-module #f (read-forms "<string>" (let ([in (open-input-string s)]) (port-count-lines! in) in))))

;; For tests that look inside: a fresh state with the prelude loaded.
(define (call-with-prelude thunk)
  (with-fresh-state (λ () (load-dependency! prelude-path (make-hash)) (thunk))))
