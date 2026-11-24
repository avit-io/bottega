#lang racket/base
;; The elaborator (untrusted): from surface nodes to terms. It resolves names, inserts implicit
;; arguments as metavariables, reads literals by their expected type and proves auto by
;; computation. Every term it produces is rechecked by the kernel (define.rkt does that).

(require racket/match racket/list racket/string
         "ast.rkt" "state.rkt" "term.rkt" "eval.rkt" "unify.rkt" "kernel.rkt" "print.rkt" "primitives.rkt")
(provide (struct-out ctx) empty-ctx bind-var define-local show
         infer check check-type elaborate-signature zonked has-meta?
         explicit-args spine-of
         todos (struct-out todo-pending) flush-todos!
         defining install-definer!)

;; --- the context ---

;; level: how many variables are bound; env: their values; types: their types; names: their
;; names (all innermost first). defs: names bound to a value, (name value type): let and forced patterns.
(struct ctx (level env types names defs))
(define empty-ctx (ctx 0 '() '() '() '()))

(define (bind-var c x ty)
  (ctx (+ 1 (ctx-level c)) (cons (vvar (ctx-level c)) (ctx-env c)) (cons ty (ctx-types c))
       (cons x (ctx-names c)) (ctx-defs c)))

(define (define-local c x v ty)
  (struct-copy ctx c [defs (cons (list x v ty) (ctx-defs c))]))

(define (local? c x) (or (memq x (ctx-names c)) (assq x (ctx-defs c))))

(define (show c v) (term->sexp (ctx-names c) (reify (ctx-level c) v)))

(define (unifies? c a b)
  (with-handlers ([unify-failure? (λ (_) #f)]) (unify (ctx-level c) a b) #t))

;; Elaborating a match on a computed value, or a refutation for auto, defines an auxiliary
;; global from clauses. define.rkt installs the procedure that checks a definition.
(define definer #f)
(define (install-definer! f) (set! definer f))

;; --- names ---

(define (core-reserved? x) (string-prefix? (symbol->string x) "%"))

(define (path-literal? x)                     ; /out, /go/pkg/mod
  (let ([s (symbol->string x)]) (and (> (string-length s) 1) (char=? (string-ref s 0) #\/))))

(define (unknown-name x) (oops "unknown name: ~a" x))

;; Short names: Type.field is written field, and the type of the first argument picks the
;; global: (⊕ /out "bin") is Abs.⊕. → the globals named Type.field, sorted.
(define (qualified f)
  (define suffix (string-append "." (symbol->string f)))
  (sort (filter (λ (n) (let ([s (symbol->string n)])
                         (and (> (string-length s) (string-length suffix)) (string-suffix? s suffix))))
                (all-global-names))
        symbol<?))

(define (short-name? c f) (and (not (local? c f)) (not (lookup f)) (pair? (qualified f))))

(define (self-type n)                          ; the type of the first explicit argument, reified
  (let loop ([t (force (global-type (lookup n)))] [l 0])
    (match t
      [(vpi _ 'i _ _ _) (loop (force (pi-codomain t (vvar l))) (+ l 1))]
      [(vpi _ 'e a _ _) (reify l a)]
      [_ #f])))

(define (string-type)
  (if (builtin 'string) (vglob (builtin 'string) '()) (oops "string literals need String")))
(define (nat-type)
  (if (builtin 'nat) (vglob (builtin 'nat) '()) (oops "numeric literals need the nat builtin")))

;; --- inference: → (values term type) ---

(define (infer c e) (at (node-loc e) (infer* c e)))

(define (infer* c e)
  (match e
    [(s-type _ n) (values (ttype n) (vtype (+ n 1)))]
    [(s-lit _ (? string? s)) (values (tstr s) (string-type))]
    [(s-lit _ n) (values (tnum n) (nat-type))]
    [(s-name _ x) (infer-name c x)]
    [(s-hole _) (oops "TODO: the type is not known here; annotate it with (the T TODO)")]
    [(s-auto _) (oops "auto needs an expected type")]
    [(s-match _ _ _) (oops "match needs an expected type: write (the T (match …))")]
    [(s-lam _ _ _) (oops "λ needs an expected type: write (the T (λ …))")]
    [(s-let _ bs body) (infer (bind-lets c bs) body)]
    [(s-the _ type x)
     (define-values (ta _) (check-type c type))
     (define ty (evaluate (ctx-env c) ta))
     (values (tann (check c x ty) ta) ty)]
    [(s-arrow _ bs result)
     (define-values (t n) (elaborate-arrow c bs result))
     (values t (vtype n))]
    [(s-list loc items) (infer c (list->conses loc items))]
    [(s-app _ (s-name _ f) args) #:when (short-name? c f) (infer-short-name c e f args)]
    [(s-app _ head args)
     (define-values (t ty) (infer c head))
     (apply-args c e t ty args #f)]))

(define (infer-name c x)
  (cond [(eq? x '_) (oops "_ is not a term")]
        [(index-of (ctx-names c) x) => (λ (i) (values (tvar i) (list-ref (ctx-types c) i)))]
        [(assq x (ctx-defs c)) => (λ (d) (values (reify (ctx-level c) (cadr d)) (caddr d)))]
        [(and (not (privileged)) (core-reserved? x)) (oops "~a is reserved to the core" x)]
        [(lookup x) => (λ (g) (values (tglob x) (global-type g)))]
        [(path-literal? x) (values (path-term c x) (vglob (builtin 'abs) '()))]
        [else (unknown-name x)]))

;; The first argument is elaborated once: its type picks the global.
(define (infer-short-name c e f args)
  (define-values (t1 ty1) (let-values ([(t ty) (infer c (car args))]) (insert c t ty)))
  (define self (reify (ctx-level c) ty1))
  (define n (or (findf (λ (n) (equal? (self-type n) self)) (qualified f))
                (oops "~a does not exist for ~s" f (show c ty1))))
  (apply-args c e (tglob n) (global-type (lookup n)) args (cons t1 ty1)))

;; Applies t : ty to the arguments. pre: the first argument already elaborated, (term . type), or #f.
(define (apply-args c e t ty args pre)
  (if (null? args)
      (values t ty)
      (let-values ([(t ty) (insert c t ty)])
        (match ty
          [(vpi _ 'e dom _ _)
           (define u (if pre
                         (begin (unless (unifies? c (cdr pre) dom)
                                  (oops "~s: expected ~s, found ~s" (unparse (car args)) (show c dom) (show c (cdr pre))))
                                (car pre))
                         (check c (car args) dom)))
           (apply-args c e (tapp t u 'e) (pi-codomain ty (evaluate (ctx-env c) u)) (cdr args) #f)]
          [_ (oops "too many arguments in ~s" (unparse e))]))))

;; Applies the leading implicit arguments, as metavariables. → (values term forced-type)
(define (insert c t ty)
  (match (force ty)
    [(and p (vpi _ 'i _ _ _))
     (define m (timeta (new-meta! #f #f)))
     (insert c (tapp t m 'i) (pi-codomain p (evaluate (ctx-env c) m)))]
    [ty (values t ty)]))

;; (let ((x e) …) body): in sequence; x is a defined name, its value is substituted.
(define (bind-lets c bindings)
  (for/fold ([c c]) ([b bindings])
    (define-values (t ty) (infer c (cdr b)))
    (define-local c (car b) (evaluate (ctx-env c) t) ty)))

;; (list a b) is (:: a (:: b nil)).
(define (list->conses loc items)
  (unless (builtin 'cons) (oops "lists need the list builtins"))
  (for/foldr ([acc (s-name loc (builtin 'nil))]) ([x items])
    (s-app loc (s-name loc (builtin 'cons)) (list x acc))))

;; /out is (/ root "out"); built directly, so a let binding root does not capture it.
(define (path-term c x)
  (unless (builtin 'abs) (oops "the path ~a needs paths" x))
  (for/fold ([acc (tglob (builtin 'root))]) ([seg (string-split (symbol->string x) "/")])
    (tapp (tapp (tglob (builtin 'slash)) acc 'e) (check c (s-lit #f seg) (vglob (builtin 'segment) '())) 'e)))

;; --- types ---

(define (check-type c e)                       ; → (values term level)
  (define-values (t ty) (infer c e))
  (at (node-loc e)
    (match (force ty)
      [(vtype n) (values t n)]
      [(and m (vflex _ _)) (unify (ctx-level c) m (vtype 0)) (values t 0)]   ; ponytail: unknown ⇒ Type₀
      [_ (oops "~s is not a type" (unparse e))])))

(define (elaborate-arrow c bs result)          ; → (values term level)
  (if (null? bs)
      (check-type c result)
      (match-let ([(binder x vis a) (car bs)])
        (define-values (ta na) (check-type c a))
        (define-values (tb nb) (elaborate-arrow (bind-var c x (evaluate (ctx-env c) ta)) (cdr bs) result))
        (values (tpi x vis ta tb) (max na nb)))))

;; --- checking: → term ---

(define (check c e ty) (at (node-loc e) (check* c e ty)))

(define (check* c e ty)
  (define want (force ty))
  (match e
    [(s-hole _) (hole! c ty)]
    [(s-auto _) (auto c ty)]
    [(s-match _ _ _) (check-match c e want)]
    [(s-let _ bs body) (check (bind-lets c bs) body ty)]
    [(s-lam _ xs body) (check-lambda c xs body ty)]
    [(s-lit _ (? string? s)) #:when (literal-reader want) (check-literal s want)]
    [(s-lit _ (? string? s)) #:when (builtin-type? want 'rel) (rel-term c s)]
    [(s-list _ items) #:when (builtin-type? want 'list) (list-term c items want)]
    [(s-app _ (s-name _ f) _) #:when (and (lookup f) (not (local? c f)))
     (or (check-introduction c e want) (check-inferred c e ty))]
    [_ (check-inferred c e ty)]))

(define (builtin-type? v key) (match v [(vglob n _) (builtin? n key)] [_ #f]))

(define (check-inferred c e ty)
  (define-values (t ty2) (let-values ([(t ty) (infer c e)]) (insert c t ty)))
  (or (and (unifies? c ty2 ty) t)
      (coerce c e t ty2 ty)
      (oops "~s: expected ~s, found ~s" (unparse e) (show c ty) (show c ty2))))

;; An application of a global is checked against the expected type: first the result type is
;; unified with it, then the arguments are checked in order, each against its domain. So the
;; expected type goes down into the arguments: (pair 2 refl) against (Σ Nat (λ (n) (≡ (+ n n) 4))).
;; → term, or #f if the result does not match (then inference gives the error).
(struct pending (vis meta value arg dom))     ; an argument waiting for the result to be unified

(define (check-introduction c e want)
  (match-define (s-app _ (s-name _ f) args) e)
  (when (and (not (privileged)) (core-reserved? f)) (oops "~a is reserved to the core" f))
  (let loop ([kt (global-type (lookup f))] [args args] [spine '()])
    (match (force kt)
      [(and p (vpi _ vis dom _ _)) #:when (or (eq? vis 'i) (pair? args))
       (define m (timeta (new-meta! #f #f)))
       (define mv (evaluate (ctx-env c) m))
       (if (eq? vis 'i)
           (loop (pi-codomain p mv) args (cons (pending 'i m #f #f #f) spine))
           (loop (pi-codomain p mv) (cdr args) (cons (pending 'e m mv (car args) dom) spine)))]
      [_ #:when (pair? args) #f]
      [result
       (and (unifies? c result want)
            (for/fold ([t (tglob f)]) ([s (reverse spine)])
              (if (eq? (pending-vis s) 'i)
                  (tapp t (pending-meta s) 'i)
                  (let ([u (check c (pending-arg s) (pending-dom s))])
                    (unless (unifies? c (pending-value s) (evaluate (ctx-env c) u))
                      (oops "~s: inconsistent argument" (unparse e)))
                    (tapp t u 'e)))))])))

;; (λ (x y) body) against a Π. An implicit Π gets an implicit λ by itself, as in Agda.
(define (check-lambda c xs body ty)
  (if (null? xs)
      (check c body ty)
      (match (force ty)
        [(and p (vpi x vis dom _ _))
         (define implicit? (eq? vis 'i))
         (define name (if implicit? '_ (car xs)))
         (define inner (check-lambda (bind-var c name dom) (if implicit? xs (cdr xs)) body
                                     (pi-codomain p (vvar (ctx-level c)))))
         (tlam (if implicit? x name) vis inner)]
        [_ (oops "λ: expected ~s, which is not a function" (show c ty))])))

;; --- literals read by their expected type ---

(define (literal-reader want)
  (match want [(vglob n _) (hash-ref literal-readers (builtin-key n) #f)] [_ #f]))

;; "1h30m" expected as a Duration: the reader gives the fields of its only constructor, auto the proofs.
(define (check-literal s want)
  (define fields ((literal-reader want) s))
  (unless fields (oops "«~a» is not a valid ~a" s (vglob-name want)))
  (define k (car (global-info (lookup (vglob-name want)))))
  (define holes (make-list (- (length (explicit-args k)) (length fields)) (s-auto #f)))
  (check empty-ctx (s-app #f (s-name #f k) (append (map (λ (v) (s-lit #f v)) fields) holes)) want))

;; "bin/hello" expected as a Rel is a vector: (step (step here "bin") "hello").
(define (rel-term c s)
  (for/fold ([acc (tglob (builtin 'here))]) ([seg (string-split s "/")])
    (tapp (tapp (tglob (builtin 'step)) acc 'e) (check c (s-lit #f seg) (vglob (builtin 'segment) '())) 'e)))

;; (list a b) expected as a (List A): every element is checked against A.
(define (list-term c items want)
  (define a (car (car (vglob-spine want))))
  (define l (ctx-level c))
  (for/foldr ([acc (tapp (tglob (builtin 'nil)) (reify l a) 'i)]) ([x items])
    (tapp (tapp (tapp (tglob (builtin 'cons)) (reify l a) 'i) (check c x a) 'e) acc 'e)))

;; The literal stays, the type changes: if T has a constructor whose first field takes the value
;; and whose other fields are propositions, the value becomes (k value auto …). So "alpine"
;; expected as an Image is the pair (image "alpine" proof).
(define (coerce c e t ty2 ty)
  (match (force ty)
    [(vglob n '()) #:when (global-kind? n 'data)
     (for/or ([k (global-info (lookup n))]) (coerce-with c e t ty2 k n))]
    [_ #f]))

(define (coerce-with c e t ty2 k tname)
  (match (force (global-type (lookup k)))
    [(and kt (vpi _ 'e dom _ _))
     (and (unifies? c ty2 dom)
          (let loop ([tm (tapp (tglob k) t 'e)] [kt (force (pi-codomain kt (evaluate (ctx-env c) t)))])
            (match kt
              [(vpi _ _ dom _ _)
               (define p (force dom))
               (cond [(not (memq (match p [(vglob n _) (builtin-key n)] [_ #f]) '(unit eq empty))) #f]
                     [(builtin-type? p 'empty)
                      (oops "«~a» is not a valid ~a" (if (s-lit? e) (s-lit-value e) (show c (evaluate (ctx-env c) t))) tname)]
                     [else (define proof (auto c p))
                           (loop (tapp tm proof 'e) (force (pi-codomain kt (evaluate (ctx-env c) proof))))])]
              [_ tm])))]
    [_ #f]))

;; The explicit argument positions (levels) of a constructor, last first.
(define (explicit-args k)
  (let loop ([t (force (global-type (lookup k)))] [l 0] [acc '()])
    (match t
      [(vpi _ vis _ _ _) (loop (force (pi-codomain t (vvar l))) (+ l 1) (if (eq? vis 'e) (cons l acc) acc))]
      [_ acc])))

;; --- auto: proofs by computation ---

;; Decidable propositions are written (So (test? …)), which reduces to ⊤ or ⊥: auto is tt when
;; the expected type computes to ⊤, and refl when it is an equality. For inductive relations,
;; like (⊑ p q), it searches a proof by composing constructors. For a negation it refutes.
(define (auto c ty)
  (define v (force ty))
  (cond [(builtin-type? v 'unit) (tglob (builtin 'tt))]
        [(builtin-type? v 'eq) (check c (s-name #f (builtin 'refl)) ty)]
        [(builtin-type? v 'empty) (oops "auto: the proposition reduces to ⊥, not to true")]
        [(negated v) => (λ (a) (refutation c v a))]
        [else (define b (box #f))
              (or (parameterize ([blame b]) (search c v 32))
                  (oops "auto: no proof of ~s" (show c (or (unbox b) ty))))]))

;; (¬ A) is (→ A ⊥): → A, or #f.
(define (negated v)
  (match v
    [(vpi _ 'e dom _ _) (and (builtin-type? (force (pi-codomain v (vvar 0))) 'empty) dom)]
    [_ #f]))

;; A refutation is an auxiliary define by absurd patterns covering A; the definition's checks
;; (coverage, absurdity) verify it.
(define (refutation c v a)
  (unless (= (ctx-level c) 0) (oops "auto: refutation only on closed propositions"))
  (define pats (or (refute a 8) (oops "auto: no refutation of ~s" (show c a))))
  (define name (string->symbol (format "%auto~a" (fresh-number!))))
  (definer name (reify 0 v) (for/list ([p pats]) (clause #f (list p) #f)))
  (tglob name))

;; → the patterns covering ty with absurdities, or #f. A constructor ruled out by the indices is
;; not needed; a possible one must be ruled out by one of its refutable fields.
(define (refute ty depth)
  (match (force ty)
    [(and v (vglob n _)) #:when (and (> depth 0) (global-kind? n 'data) (not (has-meta? (reify 0 v))))
     (let loop ([ks (global-info (lookup n))] [acc '()])
       (cond [(null? ks) (if (null? acc) (list '()) acc)]
             [else (define r (refute-constructor (car ks) v depth))
                   (and r (loop (cdr ks) (if (eq? r 'impossible) acc (append acc r))))]))]
    [_ #f]))

(define (refute-constructor k goal depth)
  (let loop ([kt (force (global-type (lookup k)))] [fields '()])
    (match kt
      [(vpi _ vis dom _ _)
       (define m (meta-value (new-meta! #f #f)))
       (loop (force (pi-codomain kt m)) (if (eq? vis 'e) (append fields (list dom)) fields))]
      [_
       (define r (with-handlers ([unify-failure? unify-failure-kind]) (unify 0 kt goal) 'ok))
       (cond [(eq? r 'conflict) 'impossible]
             [(not (eq? r 'ok)) #f]
             [else (for/or ([f fields] [i (in-naturals)])
                     (define ps (refute f (- depth 1)))
                     (and ps (for/list ([p ps])
                               (cons k (for/list ([j (length fields)]) (if (= i j) p '_))))))])])))

;; The first failed goal under a conjunction (×), to say which part does not hold.
(define blame (make-parameter #f))

;; ponytail: bounded depth-first search, the first constructor that succeeds; enough for
;; structural relations like ⊑. Only on goals without metavariables, so failed attempts touch
;; only new metavariables.
(define (search c goal depth [conj #t])
  (match (force goal)
    [(and g (vglob n _)) #:when (and (> depth 0) (not (has-meta? (reify (ctx-level c) g))))
     (or (case (builtin-key n)
           [(unit) (tglob (builtin 'tt))]
           [(eq) (with-handlers ([exn:fail? (λ (_) #f)] [unify-failure? (λ (_) #f)])
                   (check c (s-name #f (builtin 'refl)) g))]
           [else (and (global-kind? n 'data)
                      (for/or ([k (global-info (lookup n))]) (try-constructor c k g depth conj)))])
         (begin (when (and conj (blame) (not (unbox (blame))) (not (memq (builtin-key n) '(sigma unit empty))))
                  (set-box! (blame) g))
                #f))]
    [_ #f]))

(define (try-constructor c k goal depth conj)
  (define sigma-pair? (builtin? (global-info (lookup k)) 'sigma))
  (let loop ([t (tglob k)] [kt (global-type (lookup k))] [holes '()])
    (match (force kt)
      [(and p (vpi _ vis dom _ _))
       (define m (timeta (new-meta! #f #f)))
       (define mv (evaluate (ctx-env c) m))
       (loop (tapp t m vis) (pi-codomain p mv) (if (eq? vis 'e) (cons (cons mv dom) holes) holes))]
      [result
       (and (unifies? c result goal)
            (for/and ([h (reverse holes)])
              (or (not (vflex? (force (car h))))        ; already determined by the indices
                  (let ([u (search c (cdr h) (- depth 1) (and conj sigma-pair?))])
                    (and u (unifies? c (car h) (evaluate (ctx-env c) u))))))
            t)])))

;; --- match on a computed value ---

;; (match e clause …) on a computed value is an auxiliary define, f/matchN, that takes the
;; context and the scrutinees and does the cases; it goes through every define's checks.
;; The motive does not depend on the scrutinee: the cases refine neither the type nor the context.
;; The clauses do not call the define in progress: termination is checked define by define,
;; and a call inside the auxiliary would escape it.
(define defining (make-parameter '()))         ; the defines in progress, innermost first

(define (check-match c e want)
  (match-define (s-match _ scrutinees clauses) e)
  (for ([cl clauses] #:when (clause-body cl))
    (define called (findf (λ (d) (memq d (mentioned-names (clause-body cl)))) (defining)))
    (when called
      (oops "match on a computed value: it cannot call ~a, which is being defined; use match on the define's arguments" called)))
  (define l (ctx-level c))
  (define scrs (for/list ([x scrutinees]) (let-values ([(t ty) (infer c x)]) (cons t ty))))
  (define names (reverse (ctx-names c)))                        ; outermost first
  (define defs (filter (λ (d) (not (memq (car d) names))) (reverse (ctx-defs c))))
  (define binders (append (for/list ([x names] [a (reverse (ctx-types c))] [j (in-naturals)]) (list x (reify j a)))
                          (for/list ([d defs]) (list (car d) (reify l (caddr d))))))
  (define k (length binders))
  (define result (reify (+ k (length scrs)) want))
  (define with-scrutinees
    (for/foldr ([acc result]) ([s scrs] [j (range (length scrs))]) (tpi '_ 'e (reify (+ k j) (cdr s)) acc)))
  (define ty (for/foldr ([acc with-scrutinees]) ([b binders]) (tpi (car b) 'e (cadr b) acc)))
  (define pattern-names                         ; a name shadowed further on becomes _
    (let loop ([ns (map car binders)])
      (cond [(null? ns) '()]
            [(memq (car ns) (cdr ns)) (cons '_ (loop (cdr ns)))]
            [else (cons (car ns) (loop (cdr ns)))])))
  (define aux (string->symbol (format "~a/match~a" (if (pair? (defining)) (car (defining)) 'match) (fresh-number!))))
  (when (has-meta? ty) (oops "match: the type of the scrutinee or of the result is not known yet; annotate it with the"))
  (definer aux ty (for/list ([cl clauses])
                    (clause (clause-loc cl) (append pattern-names (clause-patterns cl)) (clause-body cl))))
  (for/fold ([t (tglob aux)])
            ([a (append (for/list ([j l]) (tvar (- l 1 j)))
                        (for/list ([d defs]) (reify l (cadr d)))
                        (map car scrs))])
    (tapp t a 'e)))

;; --- signatures ---

;; In signatures only variable-shaped names quantify themselves: a letter, then digits (A, x, n2).
;; Any other free name is an unknown name: a Port without its module does not become any type.
(define (variable-shaped? x)
  (define cs (string->list (symbol->string x)))
  (and (pair? cs) (char-alphabetic? (car cs)) (andmap char-numeric? (cdr cs))))

(define (pattern-symbols pats)
  (append* (for/list ([p pats]) (cond [(symbol? p) (list p)] [(pair? p) (pattern-symbols (cdr p))] [else '()]))))

;; The free variables of a signature, in order of first occurrence.
(define (free-variables e)
  (define found '())
  (define (walk e bound)
    (match e
      [(s-name loc x)
       (unless (or (eq? x '_) (memq x bound) (memq x found) (lookup x) (path-literal? x))
         (if (variable-shaped? x) (set! found (cons x found)) (at loc (unknown-name x))))]
      [(s-lam _ xs body) (walk body (append xs bound))]
      [(s-let _ bs body) (walk body (for/fold ([bound bound]) ([b bs]) (walk (cdr b) bound) (cons (car b) bound)))]
      [(s-arrow _ bs result)
       (walk result (for/fold ([bound bound]) ([b bs]) (walk (binder-type b) bound) (cons (binder-name b) bound)))]
      [(s-app _ (s-name _ f) args) #:when (and (not (memq f bound)) (not (lookup f)) (pair? (qualified f)))
       (for ([a args]) (walk a bound))]           ; a short name: resolved from the type
      [(s-app _ head args) (walk head bound) (for ([a args]) (walk a bound))]
      [(s-the _ type x) (walk type bound) (walk x bound)]
      [(s-list _ items) (for ([x items]) (walk x bound))]
      [(s-match _ scrs cls)
       (for ([s scrs]) (walk s bound))
       (for ([cl cls] #:when (clause-body cl))
         (walk (clause-body cl) (append (pattern-symbols (clause-patterns cl)) bound)))]
      [_ (void)]))
  (walk e '())
  (reverse found))

;; A signature becomes a closed term: its free variables are implicit arguments. If the type of
;; one stays unknown it is generalized: (≡ x x) becomes (∀ ((x:type Type) (x x:type)) (≡ x x)).
(define (elaborate-signature e)
  (let retry ([generalized '()])                 ; (x . x:type), for the variables to generalize
    (define opened '())                          ; (x . meta) for the types still unknown
    (define t
      (let go ([c empty-ctx] [vs (append (map cdr generalized) (free-variables e))])
        (if (null? vs)
            (let-values ([(t _) (check-type c e)]) t)
            (let* ([x (car vs)]
                   [a (cond [(memq x (map cdr generalized)) (ttype 0)]
                            [(assq x generalized) => (λ (p) (let-values ([(t _) (check-type c (s-name #f (cdr p)))]) t))]
                            [else (define m (new-meta! x #f))
                                  (set! opened (cons (cons x m) opened))
                                  (timeta m)])])
              (tpi x 'i a (go (bind-var c x (evaluate (ctx-env c) a)) (cdr vs)))))))
    (define unsolved (filter (λ (p) (not (meta-solution (cdr p)))) opened))
    (if (null? unsolved)
        (zonked 0 '() t)
        (retry (append generalized
                       (for/list ([p unsolved]) (cons (car p) (string->symbol (format "~a:type" (car p))))))))))

;; --- zonking: substitutes the solved metavariables ---

(define (zonk l env t)
  (match t
    [(or (tmeta _) (timeta _)) (reify l (evaluate env t))]
    [(tapp f u i) (tapp (zonk l env f) (zonk l env u) i)]
    [(tlam x i body) (tlam x i (zonk (+ l 1) (cons (vvar l) env) body))]
    [(tpi x i a b) (tpi x i (zonk l env a) (zonk (+ l 1) (cons (vvar l) env) b))]
    [(tann e ty) (tann (zonk l env e) (zonk l env ty))]
    [_ t]))

(define (has-meta? t)
  (match t
    [(or (tmeta _) (timeta _)) #t]
    [(tapp f u _) (or (has-meta? f) (has-meta? u))]
    [(tlam _ _ body) (has-meta? body)]
    [(tpi _ _ a b) (or (has-meta? a) (has-meta? b))]
    [(tann e ty) (or (has-meta? e) (has-meta? ty))]
    [_ #f]))

;; A hole leaves metavariables: the clause is skipped, and its TODO reported with the others.
(struct todo-pending ())

(define (zonked l env t)
  (define z (zonk l env t))
  (when (and (todos) (pair? (unbox (todos)))) (raise (todo-pending)))
  (when (has-meta? z) (oops "cannot infer the implicit arguments in ~s" (term->sexp '() z)))
  z)

(define (spine-of t)                             ; → (values head arguments)
  (let loop ([t t] [args '()])
    (match t
      [(tapp f u _) (loop f (cons u args))]
      [_ (values t args)])))

;; --- TODO, as in Pie: a hole in a define's body ---

;; The checker collects the goal and context of every hole of the define and rejects it with
;; them: a hole is never a postulate. The message is written at the end, when unification has
;; solved what it could.
(define todos (make-parameter #f))              ; box of message thunks, for the define being checked

(define (hole! c ty)
  (unless (todos) (oops "TODO goes in the body of a define"))
  (define p (here))
  (set-box! (todos) (cons (λ () (todo-message p c ty)) (unbox (todos))))
  (timeta (new-meta! 'TODO #f)))

(define (todo-message p c ty)
  (define l (ctx-level c))
  (define names (ctx-names c))
  (define hypotheses
    (for/list ([i (reverse (range l))]
               #:unless (string-prefix? (symbol->string (list-ref names i)) "%"))
      (format "  ~a : ~s" (list-ref names i)
              (term->sexp (list-tail names (+ i 1)) (reify (- l i 1) (list-ref (ctx-types c) i))))))
  (define definitions
    (for/list ([d (reverse (ctx-defs c))])
      (format "  ~a = ~s : ~s" (car d) (show c (cadr d)) (show c (caddr d)))))
  (string-append (if p (string-append (srcloc->string p) ": ") "")
                 "TODO"
                 (apply string-append (map (λ (h) (string-append "\n" h)) (append hypotheses definitions)))
                 "\n  ──────────\n  "
                 (format "~s" (show c ty))))

(define (flush-todos!)
  (define messages (map (λ (k) (k)) (reverse (unbox (todos)))))
  (unless (null? messages)
    (raise (exn:fail:bottega (string-join messages "\n") (current-continuation-marks) #f))))
