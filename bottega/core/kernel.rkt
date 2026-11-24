#lang racket/base
;; The kernel: the only trusted part of checking. It rechecks every elaborated term, with
;; stratified universes: Type n : Type n+1, never Type : Type. It knows only Π and Type; data,
;; constructors and defines are globals with a type.

(require racket/match "state.rkt" "term.rkt" "eval.rkt" "unify.rkt")
(provide kernel-infer kernel-check kernel-level kernel-type-level)

;; env: values of the variables; types: their types; l: the level.
(define (kernel-infer env types l t)
  (match t
    [(tvar i) (list-ref types i)]
    [(tglob n) (let ([g (lookup n)]) (if g (global-type g) (oops "kernel: ~a does not exist" n)))]
    [(ttype n) (vtype (+ n 1))]
    [(tstr _) (if (builtin 'string) (vglob (builtin 'string) '()) (oops "kernel: no String"))]
    [(tnum _) (vglob (builtin 'nat) '())]
    [(tann e ty) (kernel-level (kernel-infer env types l ty))
                 (let ([v (evaluate env ty)]) (kernel-check env types l e v) v)]
    [(tpi _ _ a b)
     (define la (kernel-level (kernel-infer env types l a)))
     (define lb (kernel-level (kernel-infer (cons (vvar l) env) (cons (evaluate env a) types) (+ l 1) b)))
     (vtype (max la lb))]
    [(tapp f u i)
     (match (force (kernel-infer env types l f))
       [(and p (vpi _ (== i) a _ _))
        (kernel-check env types l u a)
        (pi-codomain p (evaluate env u))]
       [_ (oops "kernel: ill-typed application")])]
    [_ (oops "kernel: term not inferable ~s" t)]))

(define (kernel-check env types l t ty)
  (match* (t (force ty))
    [((tlam _ i body) (and p (vpi _ i a _ _)))
     (kernel-check (cons (vvar l) env) (cons a types) (+ l 1) body (pi-codomain p (vvar l)))]
    [(_ want)
     (with-handlers ([unify-failure? (λ (_) (oops "kernel: wrong type"))])
       (unify l (kernel-infer env types l t) want))]))

(define (kernel-level ty)
  (match (force ty)
    [(vtype n) n]
    [_ (oops "kernel: expected a type")]))

;; The universe level of a closed type.
(define (kernel-type-level t) (kernel-level (kernel-infer '() '() 0 t)))
