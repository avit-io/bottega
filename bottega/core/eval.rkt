#lang racket/base
;; Evaluation (normalization by evaluation): terms → values, and back with reify.
;; A define reduces when it has all its arguments and one of its clauses matches; otherwise it
;; stays stuck as a vglob. Numerals are native integers, as in Lean.

(require racket/match racket/list "state.rkt" "term.rkt" "primitives.rkt")
(provide evaluate apply-value apply-spine force reify
         pi-codomain meta-value
         accelerate fast-operations
         (struct-out unify-failure) fail-unify)

;; Unification failures are raised from here too: applying a non-function is a mismatch.
;; kind: 'conflict (two different constructors), 'mismatch, 'stuck, 'occurs, 'scope.
(struct unify-failure (kind))
(define (fail-unify kind) (raise (unify-failure kind)))

;; --- evaluation ---

(define (evaluate env t)
  (match t
    [(tvar i) (list-ref env i)]
    [(tglob n) (reduce n '())]
    [(tapp f u i) (apply-value (evaluate env f) (evaluate env u) i)]
    [(tlam x i body) (vlam x i env body)]
    [(tpi x i a b) (vpi x i (evaluate env a) env b)]
    [(ttype n) (vtype n)]
    [(tstr s) (vstr s)]
    [(tnum n) (vnum n)]
    [(tann t _) (evaluate env t)]
    [(tmeta m) (meta-value m)]
    [(timeta m) (for/fold ([f (meta-value m)]) ([v (reverse env)]) (apply-value f v 'e))]))

(define (meta-value m) (or (meta-solution m) (vflex m '())))

(define (pi-codomain v u)                    ; the codomain of a vpi, at the argument u
  (match-define (vpi _ _ _ env cod) v)
  (evaluate (cons u env) cod))

(define (apply-value f u i)
  (define (extend sp) (append sp (list (cons u i))))
  (match f
    [(vlam _ _ env body) (evaluate (cons u env) body)]
    [(vflex m sp) (vflex m (extend sp))]
    [(vrigid l sp) (vrigid l (extend sp))]
    [(vglob n sp) (reduce n (extend sp))]
    [_ (fail-unify 'mismatch)]))

(define (apply-spine f sp)
  (for/fold ([f f]) ([a sp]) (apply-value f (car a) (cdr a))))

;; Unfolds solved metavariables and reducible globals at the head.
(define (force v)
  (match v
    [(vflex m sp) (let ([s (meta-solution m)]) (if s (force (apply-spine s sp)) v))]
    [(vglob n sp) (let ([r (try-reduce n sp)]) (if r (force r) (numeral v)))]
    [_ v]))

;; zero and add1 of a number become numbers.
(define (numeral v)
  (match v
    [(vglob (? (λ (n) (builtin? n 'zero))) '()) (vnum 0)]
    [(vglob (? (λ (n) (builtin? n 'suc))) (list (cons x _)))
     (match (force x) [(vnum k) (vnum (+ k 1))] [_ v])]
    [_ v]))

;; --- reduction of globals ---

(define (reduce n sp) (or (try-reduce n sp) (vglob n sp)))

(define (try-reduce n sp)
  (define g (lookup n))
  (case (global-kind g)
    [(def) (and (global-info g)
                (or (accelerated n sp)
                    (let* ([info (global-info g)] [arity (car info)])
                      (and (>= (length sp) arity)
                           (let ([r (match-clauses (cdr info) (take sp arity))])
                             (and r (apply-spine r (drop sp arity))))))))]
    [(extern) (primitive n sp)]
    [else #f]))

;; Acceleration (trusted, as in Lean): these library defs, on numeric arguments, compute in
;; Racket instead of by their clauses. The unary definitions stay the logical truth; the tests
;; check that the two computations agree, with accelerate switched off.
(define accelerate (make-parameter #t))
(define fast-operations                      ; builtin key → (arity procedure)
  (hasheq 'plus (list 2 +) 'monus (list 2 (λ (a b) (max 0 (- a b)))) 'times (list 2 *)
          'leq? (list 2 <=) 'lt? (list 2 <) 'nat-eq? (list 2 =) 'pow2 (list 1 (λ (k) (expt 2 k)))))

(define (accelerated n sp)
  (define op (and (accelerate) (hash-ref fast-operations (builtin-key n) #f)))
  (and op (compute op sp vnum?)))

;; A pure primitive, on literal arguments.
(define (primitive n sp)
  (define spec (car (global-info (lookup n))))
  (define op (hash-ref primitives (car spec) #f))
  (and op (compute op sp (λ (v) (or (vstr? v) (vnum? v))))))

(define (compute op sp literal?)
  (match-define (list arity proc) op)
  (define args (map (λ (a) (force (car a))) sp))
  (and (= (length args) arity)
       (andmap literal? args)
       (literal-value (apply proc (map (match-lambda [(vstr s) s] [(vnum n) n]) args)))))

(define (literal-value r)
  (cond [(string? r) (vstr r)]
        [(number? r) (vnum r)]
        [r (vglob (builtin 'true) '())]
        [else (vglob (builtin 'false) '())]))

;; --- matching run-time clauses ---

;; → the value of the first clause that matches, or #f if a pattern is stuck or none matches.
(define (match-clauses clauses args)
  (and (pair? clauses)
       (let* ([c (car clauses)] [env (make-vector (rt-clause-size c) #f)])
         (case (match-all (rt-clause-patterns c) (map car args) env)
           [(yes) (evaluate (reverse (vector->list env)) (rt-clause-body c))]
           [(no) (match-clauses (cdr clauses) args)]
           [else #f]))))

(define (match-all patterns values env)
  (if (null? patterns)
      'yes
      (let ([r (match-pattern (car patterns) (car values) env)])
        (if (eq? r 'yes) (match-all (cdr patterns) (cdr values) env) r))))

;; → 'yes, 'no or 'stuck
(define (match-pattern p v env)
  (match p
    [(pvar slot) (vector-set! env slot v) 'yes]
    [(pdot) 'yes]
    [(pcon k args)
     (match (force v)
       [(vnum n) (cond [(and (builtin? k 'zero) (= n 0)) 'yes]
                       [(and (builtin? k 'suc) (> n 0)) (match-pattern (car args) (vnum (- n 1)) env)]
                       [else 'no])]
       [(vglob (? constructor? c) sp) (if (eq? c k) (match-all args (map car sp) env) 'no)]
       [_ 'stuck])]))

;; --- reification: values → terms in normal form, at level l ---

(define (reify l v)
  (define (spine head sp) (for/fold ([t head]) ([a sp]) (tapp t (reify l (car a)) (cdr a))))
  (match (force v)
    [(vflex m sp) (spine (tmeta m) sp)]
    [(vrigid x sp) (spine (tvar (- l x 1)) sp)]
    [(vglob n sp) (spine (tglob n) sp)]
    [(and f (vlam x i _ _)) (tlam x i (reify (+ l 1) (apply-value f (vvar l) i)))]
    [(and p (vpi x i a _ _)) (tpi x i (reify l a) (reify (+ l 1) (pi-codomain p (vvar l))))]
    [(vtype n) (ttype n)]
    [(vstr s) (tstr s)]
    [(vnum n) (tnum n)]))
