#lang racket/base
;; Unification of values at level l. Without metavariables it is the kernel's conversion check.
;; Metavariables are solved by Miller pattern unification: ?m x1 … xn = rhs, xi distinct variables.

(require racket/match "state.rkt" "term.rkt" "eval.rkt")
(provide unify)

(define (unify l a b)
  (match* ((force a) (force b))
    ;; η for functions: compare the bodies on a fresh variable
    [((and f (vlam _ i _ _)) g) (unify (+ l 1) (apply-value f (vvar l) i) (apply-value g (vvar l) i))]
    [(f (and g (vlam _ i _ _))) (unify (+ l 1) (apply-value f (vvar l) i) (apply-value g (vvar l) i))]
    [((and p (vpi _ i a1 _ _)) (and q (vpi _ i a2 _ _)))
     (unify l a1 a2)
     (unify (+ l 1) (pi-codomain p (vvar l)) (pi-codomain q (vvar l)))]
    [((vtype n) (vtype n)) (void)]
    [((vstr s) (vstr t)) (unless (string=? s t) (fail-unify 'conflict))]
    [((vrigid x s1) (vrigid x s2)) (unify-spines l s1 s2)]
    [((vglob n s1) (vglob m s2))
     (cond [(eq? n m) (unify-spines l s1 s2)]
           [(and (constructor? n) (constructor? m)) (fail-unify 'conflict)]
           [else (fail-unify 'mismatch)])]
    [((vflex m s1) (vflex m s2)) (unify-spines l s1 s2)]
    ;; flex-flex: solve the one with the longer spine, so the other stays in its scope
    [((and x (vflex _ s1)) (vflex m s2)) #:when (< (length s1) (length s2)) (solve l m s2 x)]
    [((vflex m sp) v) (solve l m sp v)]
    [(v (vflex m sp)) (solve l m sp v)]
    [((vnum n) (vnum m)) (unless (= n m) (fail-unify 'conflict))]
    [((vnum n) v) (unify-numeral l n v)]
    [(v (vnum n)) (unify-numeral l n v)]
    [(_ _) (fail-unify 'mismatch)]))

;; A number against (add1 x), zero, another constructor or anything else.
(define (unify-numeral l n v)
  (match v
    [(vglob (? (λ (k) (builtin? k 'suc))) (list (cons x _)))
     (if (= n 0) (fail-unify 'conflict) (unify l (vnum (- n 1)) x))]
    [(vglob (? constructor?) _) (fail-unify 'conflict)]
    [_ (fail-unify 'mismatch)]))

(define (unify-spines l s1 s2)
  (unless (= (length s1) (length s2)) (fail-unify 'mismatch))
  (for ([x s1] [y s2]) (unify l (car x) (car y))))

;; ?m applied to sp equals rhs: ?m := λ x1 … xn. rhs, with rhs renamed into the λs' scope.
(define (solve l m sp rhs)
  (define body (rename m (length sp) l (invert sp) rhs))
  (solve-meta! m (evaluate '() (for/foldr ([t body]) ([a sp]) (tlam 'x (cdr a) t)))))

;; The spine must be distinct bound variables: → renaming, level in the context → level in the solution.
(define (invert sp)
  (for/fold ([ren '()]) ([a sp] [k (in-naturals)])
    (match (force (car a))
      [(vrigid x '()) #:when (not (assv x ren)) (cons (cons x k) ren)]
      [_ (fail-unify 'stuck)])))

;; Reifies v into the solution's scope: dom variables bound by the λs, cod the current level.
(define (rename m dom cod ren v)
  (define (spine head sp) (for/fold ([t head]) ([a sp]) (tapp t (rename m dom cod ren (car a)) (cdr a))))
  (define (under f) (rename m (+ dom 1) (+ cod 1) (cons (cons cod dom) ren) (f (vvar cod))))
  (match (force v)
    [(vflex m2 sp) (if (= m2 m) (fail-unify 'occurs) (spine (tmeta m2) sp))]
    [(vrigid x sp) (let ([p (assv x ren)])
                     (if p (spine (tvar (- dom (cdr p) 1)) sp) (fail-unify 'scope)))]
    [(vglob n sp) (spine (tglob n) sp)]
    [(and f (vlam x i _ _)) (tlam x i (under (λ (y) (apply-value f y i))))]
    [(and p (vpi x i a _ _)) (tpi x i (rename m dom cod ren a) (under (λ (y) (pi-codomain p y))))]
    [(vtype n) (ttype n)]
    [(vstr s) (tstr s)]
    [(vnum n) (tnum n)]))
