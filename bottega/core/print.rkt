#lang racket/base
;; Terms back to s-expressions, for error messages, TODO goals and hover.
;; Refined literals print as they are written: /out/bin, "bin/hello", "10.0.0.5", "alpine".

(require racket/match racket/list racket/string "state.rkt" "term.rkt" "primitives.rkt")
(provide term->sexp)

;; names: the names of the bound variables, innermost first.
(define (term->sexp names t)
  (or (literal-of t)
      (match t
        [(tvar i) (if (< i (length names)) (list-ref names i) `(var ,i))]
        [(tglob n) n]
        [(tstr s) s]
        [(tnum n) n]
        [(ttype 0) 'Type]
        [(ttype n) `(Type ,n)]
        [(tann e ty) `(the ,(term->sexp names ty) ,(term->sexp names e))]
        [(or (tmeta m) (timeta m)) (string->symbol (format "?~a" m))]
        [(tlam x _ body) `(λ (,x) ,(term->sexp (cons x names) body))]
        [(tpi '_ _ a b) `(→ ,(term->sexp names a) ,(term->sexp (cons '_ names) b))]
        [(tpi x 'i a b) `(∀ ((,x ,(term->sexp names a))) ,(term->sexp (cons x names) b))]
        [(tpi x _ a b) `(Π ((,x ,(term->sexp names a))) ,(term->sexp (cons x names) b))]
        [(tapp _ _ _)                            ; only the explicit arguments are shown
         (define-values (head args) (explicit-spine t))
         (if (null? args) (term->sexp names head) (map (λ (x) (term->sexp names x)) (cons head args)))])))

(define (explicit-spine t)
  (let loop ([t t] [args '()])
    (match t
      [(tapp f u 'e) (loop f (cons u args))]
      [(tapp f _ 'i) (loop f args)]
      [_ (values t args)])))

(define (spine t)                                 ; all arguments
  (let loop ([t t] [args '()])
    (match t
      [(tapp f u _) (loop f (cons u args))]
      [_ (values t args)])))

(define (constructor-of-key? t key)
  (match t
    [(tglob (? constructor? k)) (builtin? (global-info (lookup k)) key)]
    [_ #f]))

;; --- literals ---

(define (literal-of t)
  (or (let ([ss (path-segments t)]) (and ss (string->symbol (string-append "/" (string-join ss "/")))))
      (segment-of t)
      (refined t)
      (let ([ss (rel-segments t)]) (and ss (string-join ss "/")))))

(define (segment-of t)                            ; (segment "x" proof) → "x"
  (define-values (head args) (spine t))
  (and (constructor-of-key? head 'segment)
       (pair? args)
       (match (car args) [(tstr s) s] [_ #f])))

(define (path-segments t)                         ; (/ (/ root "go") "pkg") → ("go" "pkg")
  (define-values (head args) (spine t))
  (cond [(and (builtin 'root) (equal? t (tglob (builtin 'root)))) '()]
        [(and (builtin 'slash) (equal? head (tglob (builtin 'slash))) (= (length args) 2))
         (let ([p (path-segments (car args))] [s (segment-of (cadr args))]) (and p s (append p (list s))))]
        [else #f]))

(define (rel-segments t)                          ; (step (step here "bin") "hello") → ("bin" "hello")
  (define-values (head args) (spine t))
  (cond [(and (builtin 'here) (equal? t (tglob (builtin 'here)))) '()]
        [(and (builtin 'step) (equal? head (tglob (builtin 'step))) (= (length args) 2))
         (let ([p (rel-segments (car args))] [s (segment-of (cadr args))]) (and p s (append p (list s))))]
        [else #f]))

;; A constructor with a literal and then only proofs: (image "alpine" tt) → "alpine".
;; Addresses have their own form: (ip n tt) → "10.0.0.5", (cidr 2561 24 tt) → "10.0.1.0/24".
(define (refined t)
  (define-values (head args) (spine t))
  (define (num i) (and (> (length args) i) (match (list-ref args i) [(tnum n) n] [_ #f])))
  (define (only-proofs? as) (andmap (λ (a) (equal? a (tglob (builtin 'tt)))) as))
  (cond [(and (constructor-of-key? head 'cidr) (num 0) (num 1))
         (format "~a/~a" (nat->ip (* (num 0) (expt 2 (- 32 (num 1))))) (num 1))]
        [(and (constructor-of-key? head 'ip) (num 0)) (nat->ip (num 0))]
        [(and (tglob? head) (constructor? (tglob-name head)) (>= (length args) 2)
              (only-proofs? (cdr args)))
         (match (car args) [(tstr s) s] [(tnum n) n] [_ #f])]
        [else #f]))
