#lang racket/base
;; Declarations: data, define, extern, builtin.
;; A define is checked in three steps: its clauses (patterns, then the body, rechecked by the
;; kernel), their coverage, and termination (a structurally decreasing argument).
;; Data are checked for universe levels and strict positivity.

(require racket/match racket/list
         "ast.rkt" "state.rkt" "term.rkt" "eval.rkt" "unify.rkt" "kernel.rkt" "print.rkt" "primitives.rkt"
         "elab.rkt")
(provide check-declaration)

(define (check-declaration d)
  (match d
    [(d-data loc _ _ _) (at loc (check-data d))]
    [(d-define _ _ _ _ _) (check-define d)]
    [(d-extern loc _ _ _) (at loc (check-extern d))]
    [(d-builtin loc key name) (at loc (check-builtin key name))]))

(define reserved-words '(→ Type _ TODO pipe auto let λ the))

;; A name this module defines: → its internal name (state.rkt, names).
(define (fresh-name! n)
  (when (memq n reserved-words) (oops "~a is a reserved word" n))
  (declare! n))

;; ============================================================================================
;; data
;; ============================================================================================

;; A constructor's telescope: its fields, then the indices of the type it returns.
(struct field (vis level type))                  ; type: the field's domain, normalized
(struct telescope (fields indices))              ; indices: the spine of the result

(define (check-data d)
  (match-define (d-data _ name* sig constructors*) d)
  (define name (fresh-name! name*))
  (define t (elaborate-signature sig))
  (kernel-type-level t)
  (define level (data-level (evaluate '() t)))
  (define-global! name (global 'data (evaluate '() t) '()))
  (define constructors (for/list ([k constructors*]) (cons (fresh-name! (car k)) (cdr k))))
  (define types (for/list ([k constructors]) (elaborate-signature (cdr k))))
  (define teles (for/list ([k constructors] [ct types]) (constructor-telescope name (car k) ct)))
  (define params (parameter-positions teles))
  (for ([k constructors] [tl teles]) (check-constructor name level params (car k) tl))
  (for ([k constructors] [ct types]) (define-global! (car k) (global 'con (evaluate '() ct) name)))
  (set-global-info! (lookup name) (map car constructors)))

(define (data-level v)
  (let loop ([v (force v)] [l 0])
    (match v
      [(vpi _ _ _ _ _) (loop (force (pi-codomain v (vvar l))) (+ l 1))]
      [(vtype n) n]
      [_ (oops "the type of a data must end in Type")])))

(define (constructor-telescope d k t)
  (kernel-type-level t)
  (let loop ([env '()] [types '()] [l 0] [t t] [fields '()])
    (match t
      [(tpi _ vis a b)
       (define va (evaluate env a))
       (loop (cons (vvar l) env) (cons va types) (+ l 1) b
             (cons (field vis (kernel-level (kernel-infer env types l a)) (reify l va)) fields))]
      [_ (match (force (evaluate env t))
           [(vglob (== d) sp) (telescope (reverse fields) sp)]
           [_ (oops "constructor ~a must return ~a" k d)])])))

;; A parameter: an index position held in every constructor by a bare implicit variable (A in Vec A n).
(define (parameter-positions teles)
  (if (null? teles)
      '()
      (for/list ([p (length (telescope-indices (car teles)))]
                 #:when (for/and ([tl teles])
                          (match (force (car (list-ref (telescope-indices tl) p)))
                            [(vrigid x '()) (eq? 'i (field-vis (list-ref (telescope-fields tl) x)))]
                            [_ #f])))
        p)))

(define (check-constructor d level params k tl)
  (define exempt                                 ; parameters may live in a higher universe
    (for/list ([p params]) (vrigid-level (force (car (list-ref (telescope-indices tl) p))))))
  (for ([f (telescope-fields tl)] [l (in-naturals)])
    (unless (or (memv l exempt) (<= (field-level f) level))
      (oops "~a: a field of level ~a does not fit in ~a, of level ~a" k (field-level f) d level))
    (unless (strictly-positive? d (field-type f))
      (oops "~a: ~a does not occur strictly positively" k d)))
  (for ([a (telescope-indices tl)])
    (when (mentions-global? d (reify (length (telescope-fields tl)) (car a)))
      (oops "~a: ~a in its own indices" k d))))

(define (mentions-global? d t)
  (match t
    [(tglob n) (eq? n d)]
    [(tapp f u _) (or (mentions-global? d f) (mentions-global? d u))]
    [(tlam _ _ body) (mentions-global? d body)]
    [(tpi _ _ a b) (or (mentions-global? d a) (mentions-global? d b))]
    [(tann e ty) (or (mentions-global? d e) (mentions-global? d ty))]
    [_ #f]))

(define (strictly-positive? d t)
  (define-values (head args) (spine-of t))
  (or (not (mentions-global? d t))
      (match t
        [(tpi _ _ a b) (and (not (mentions-global? d a)) (strictly-positive? d b))]
        [_ #f])
      (and (equal? head (tglob d)) (not (ormap (λ (a) (mentions-global? d a)) args)))
      ;; nested in a list: (List D) is fine, List is strictly positive in its parameter
      (and (builtin 'list) (equal? head (tglob (builtin 'list))) (= (length args) 1)
           (strictly-positive? d (car args)))))

;; ============================================================================================
;; define
;; ============================================================================================

(define (check-define d)
  (match-define (d-define loc name* type clauses _) d)
  (define name (at loc (fresh-name! name*)))
  (define t (at loc (elaborate-signature type)))   ; at the claim
  (record-signature! name t)
  (naming name* (λ () (check-definition name t clauses))))

;; An error inside a define says which: a broken proof is a failing test, and its name tells which.
(define (naming name thunk)
  (with-handlers ([exn:fail:bottega?
                   (λ (e)
                     (define where (exn:fail:bottega-where e))
                     (define prefix (if where (string-append (srcloc->string where) ": ") ""))
                     (define msg (substring (exn-message e) (string-length prefix)))
                     (if (regexp-match? (regexp (string-append "^" (regexp-quote (symbol->string name)) "[: ]")) msg)
                         (raise e)
                         (raise (exn:fail:bottega (string-append prefix (symbol->string name) ": " msg)
                                                  (exn-continuation-marks e) where))))])
    (thunk)))

;; Also used by the elaborator, for the auxiliary defines of match and auto.
(define (check-definition name t clauses)
  (kernel-type-level t)
  (define ty (evaluate '() t))
  (define-global! name (global 'def ty #f))
  (define patterns (for/list ([cl clauses]) (at (clause-loc cl) (map normalize-pattern (clause-patterns cl)))))
  (unless (apply = (map length patterns))
    (oops "~a: the clauses have different numbers of patterns" name))
  (define checked                                ; (run-time clause or #f . the patterns, resolved)
    (parameterize ([todos (box '())] [defining (cons name (defining))])
      (begin0
        (for/list ([cl clauses] [ps patterns])
          (with-handlers ([todo-pending? (λ (_) (cons #f '()))])
            (at (clause-loc cl) (check-clause name ty ps (clause-body cl)))))
        (flush-todos!))))
  (define runtime (filter values (map car checked)))
  (cover name ty (map cdr checked))
  (check-termination name runtime)
  (set-global-info! (lookup name)
                    (if (null? runtime) '(0) (cons (length (rt-clause-patterns (car runtime))) runtime))))

(install-definer! check-definition)

(define (normalize-pattern p)
  (cond [(null? p) '()]                                    ; the absurd pattern
        [(exact-nonnegative-integer? p) (if (zero? p) 'zero `(add1 ,(normalize-pattern (- p 1))))]
        [(symbol? p) (if (constructor? (scope-ref* p)) (scope-ref* p) p)]  ; a variable or a constructor: the type decides
        [(and (pair? p) (constructor? (scope-ref* (car p)))) (cons (scope-ref* (car p)) (map normalize-pattern (cdr p)))]
        [else (oops "invalid pattern: ~s" p)]))

;; --- the state of the clause being checked ---

;; pmetas: the pattern variables, as metavariables, newest first; absurds: the types of the ()
;; patterns, checked once all patterns are unified; implicits: the signature's implicits.
(struct clause-state ([pmetas #:mutable] [absurds #:mutable] [implicits #:mutable]))
(define current-clause (make-parameter #f))
(define (fresh-clause) (clause-state '() '() '()))

(define (pmetas) (clause-state-pmetas (current-clause)))
(define (new-pattern-var! name type)
  (define m (new-meta! name type))
  (set-clause-state-pmetas! (current-clause) (cons m (pmetas)))
  m)

;; --- patterns, step A: variables are metavariables, so unifying the indices solves them ---

(struct tree-var (meta))                         ; a pattern variable
(struct tree-con (name args))                    ; a constructor, with all its arguments

;; → (values trees remaining-type explicit-trees); the explicit ones are for coverage.
;; The signature's implicits take the name of their binder ({X Type} gives X in the body),
;; unless a pattern of the clause uses that name.
(define (check-patterns ty pats)
  (define used (flatten pats))
  (let loop ([ty ty] [pats pats] [trees '()] [explicit '()])
    (if (null? pats)
        (values (reverse trees) ty (reverse explicit))
        (match (force ty)
          [(and p (vpi x 'i dom _ _))
           (define m (new-pattern-var! (if (memq x used) '_ x) dom))
           (set-clause-state-implicits! (current-clause) (cons m (clause-state-implicits (current-clause))))
           (loop (pi-codomain p (meta-value m)) pats (cons (tree-var m) trees) explicit)]
          [(and p (vpi _ 'e dom _ _))
           (define-values (tree v) (check-pattern (car pats) dom))
           (loop (pi-codomain p v) (cdr pats) (cons tree trees) (cons tree explicit))]
          [_ (oops "too many patterns")]))))

(define (check-pattern p ty)                     ; → (values tree value)
  (cond
    [(null? p)                                   ; (): verified later by check-absurd
     (set-clause-state-absurds! (current-clause) (cons ty (clause-state-absurds (current-clause))))
     (define m (new-pattern-var! '_ ty))
     (values (tree-var m) (meta-value m))]
    [(and (symbol? p) (constructor-here? p ty)) (check-pattern (list p) ty)]
    [(symbol? p)
     (define m (new-pattern-var! p ty))
     (values (tree-var m) (meta-value m))]
    [else (check-constructor-pattern p ty)]))

;; A symbol is a constructor only if it is a constructor of the expected type: so (port) or
;; (step) stay variables where a Nat or a function is expected.
(define (constructor-here? p ty)
  (and (constructor? p)
       (match (force ty)
         [(vglob n _) (eq? (global-info (lookup p)) n)]
         [(vflex _ _) #t]                          ; the type is still unknown: the name wins
         [_ #f])))

(define (check-constructor-pattern p ty)
  (define k (car p))
  (let loop ([kt (global-type (lookup k))] [ps (cdr p)] [trees '()] [spine '()])
    (match (force kt)
      [(and q (vpi _ 'i dom _ _))
       (define m (new-pattern-var! '_ dom))
       (loop (pi-codomain q (meta-value m)) ps (cons (tree-var m) trees) (cons (cons (meta-value m) 'i) spine))]
      [(and q (vpi _ 'e dom _ _)) #:when (pair? ps)
       (define-values (tree v) (check-pattern (car ps) dom))
       (loop (pi-codomain q v) (cdr ps) (cons tree trees) (cons (cons v 'e) spine))]
      [(vpi _ _ _ _ _) (oops "wrong number of arguments in ~s" p)]
      [_ #:when (pair? ps) (oops "wrong number of arguments in ~s" p)]
      [result
       (unify 0 result ty)
       (values (tree-con k (reverse trees)) (vglob k (reverse spine)))])))

;; An absurd pattern: every constructor of its type must conflict with the indices.
(define (check-absurd ty)
  (match (force ty)
    [(and v (vglob n _)) #:when (global-kind? n 'data)
     (for ([k (global-info (lookup n))])
       (define saved-pmetas (pmetas))
       (define saved (for/list ([m saved-pmetas]) (cons m (meta-solution m))))
       (define r (with-handlers ([unify-failure? unify-failure-kind])
                   (check-pattern (cons k (map (λ (_) '_) (explicit-args k))) v)
                   'ok))
       (for ([s saved]) (solve-meta! (car s) (cdr s)))   ; the attempt leaves no trace
       (set-clause-state-pmetas! (current-clause) saved-pmetas)
       (unless (eq? r 'conflict)
         (oops "() is not absurd: ~a is possible in ~s" k (term->sexp '() (reify 0 v)))))]
    [v (oops "() needs an inductive type, not ~s" (term->sexp '() (reify 0 v)))]))

;; The resolved syntax of a tree, for coverage: (constructor explicit-arguments …).
(define (tree->syntax tr)
  (match tr
    [(tree-var m) (or (meta-name m) '_)]
    [(tree-con k args)
     (cons k (let loop ([kt (force (global-type (lookup k)))] [ts args] [l 0])
               (match kt
                 [(vpi _ vis _ _ _) #:when (pair? ts)
                  (define rest (loop (force (pi-codomain kt (vvar l))) (cdr ts) (+ l 1)))
                  (if (eq? vis 'e) (cons (tree->syntax (car ts)) rest) rest)]
                 [_ '()])))]))

;; --- patterns, step B: the body ---

;; → (run-time clause or #f . resolved explicit patterns)
(define (check-clause f ty pats body)
  (parameterize ([current-clause (fresh-clause)])
    (define-values (trees rest explicit)
      (with-handlers ([unify-failure? (λ (_) (oops "~a: the case ~s is impossible or ill-typed" f (cons f pats)))])
        (check-patterns ty pats)))
    (for-each check-absurd (clause-state-absurds (current-clause)))
    (define resolved (map tree->syntax explicit))
    (cond [body (cons (check-clause-body f pats body trees rest) resolved)]
          [(null? (clause-state-absurds (current-clause)))
           (oops "~a: clause ~s without a body and without ()" f pats)]
          [else (cons #f resolved)])))

;; The unsolved pattern variables become the context, in dependency order; the solved ones
;; become defined names. Then the body is elaborated, zonked and rechecked by the kernel.
(define (check-clause-body f pats body trees rest)
  (define implicits (clause-state-implicits (current-clause)))
  (define named (filter (λ (x) (not (eq? x '_)))
                        (map meta-name (filter (λ (m) (not (memv m implicits))) (pmetas)))))
  (when (check-duplicates named) (oops "~a: repeated variable in ~s" f (cons f pats)))
  (define free (dependency-order (filter (λ (m) (not (meta-solution m))) (reverse (pmetas)))))
  (define c0 (for/fold ([c empty-ctx]) ([m free])
               (define c2 (bind-var c (meta-name m) (meta-type m)))
               (solve-meta! m (vvar (ctx-level c)))
               c2))
  (define c (for/fold ([c c0]) ([m (reverse (pmetas))]
                                #:unless (or (memv m free) (eq? (meta-name m) '_)))
              (define-local c (meta-name m) (meta-value m) (meta-type m))))
  (define t (zonked (ctx-level c) (ctx-env c) (check c body rest)))
  (kernel-check (ctx-env c) (ctx-types c) (ctx-level c) t rest)
  (rt-clause (map (λ (tr) (runtime-pattern tr free)) trees) (ctx-level c) t))

(define (dependency-order ms)
  (define (deps m)
    (let walk ([t (reify 0 (meta-type m))])
      (match t
        [(tmeta id) (list id)]
        [(tapp f u _) (append (walk f) (walk u))]
        [(tlam _ _ b) (walk b)]
        [(tpi _ _ a b) (append (walk a) (walk b))]
        [_ '()])))
  (let loop ([todo ms] [done '()])
    (if (null? todo)
        (reverse done)
        (let ([m (findf (λ (m) (for/and ([d (deps m)]) (or (= d m) (memv d done) (not (memv d ms))))) todo)])
          (unless m (oops "circular dependencies between patterns"))
          (loop (remv m todo) (cons m done))))))

(define (runtime-pattern tr free)
  (match tr
    [(tree-var m) (let ([i (index-of free m)]) (if i (pvar i) (pdot)))]
    [(tree-con k args) (pcon k (map (λ (a) (runtime-pattern a free)) args))]))

;; ============================================================================================
;; coverage: a candidate is split until every possible case is taken by a clause
;; ============================================================================================

(define (fresh-variable) (string->symbol (format "?~a" (fresh-number!))))
(define (fresh-variable? x) (and (symbol? x) (char=? (string-ref (symbol->string x) 0) #\?)))

;; The missing case as a call, without metavariables: (pick (right _) _ _).
(define (show-case f cand)
  (define (pat p)
    (cond [(fresh-variable? p) '_]
          [(and (pair? p) (null? (cdr p))) (pat (car p))]     ; (winter) is winter
          [(pair? p) (map pat p)]
          [else p]))
  (format "~s" (cons f (map pat cand))))

;; Does the candidate typecheck as a case? #f if the indices conflict (an impossible case).
(define (possible-case? f ty cand)
  (with-handlers ([(λ (x) (and (unify-failure? x) (eq? (unify-failure-kind x) 'conflict))) (λ (_) #f)]
                  [unify-failure? (λ (_) (oops "~a: cannot decide whether the case ~a is possible" f (show-case f cand)))])
    (check-patterns ty cand)
    #t))

(define (cover f ty clause-patterns)
  (let loop ([todo (list (map (λ (_) (fresh-variable)) (car clause-patterns)))])
    (unless (null? todo)
      (define cand (car todo))
      (parameterize ([current-clause (fresh-clause)])
        (cond
          [(not (possible-case? f ty cand)) (loop (cdr todo))]
          [else
           (define r (for/or ([q clause-patterns]) (let ([r (match-cases q cand)]) (and (not (eq? r 'no)) r))))
           (cond [(eq? r 'yes) (loop (cdr todo))]
                 [r (loop (append (split f r cand) (cdr todo)))]
                 [(empty-split? f ty cand) (loop (cdr todo))]
                 [else (oops "~a: missing case ~a" f (show-case f cand))])])))))

;; A case no clause takes is covered anyway if one of its variables, split by constructors,
;; gives only impossible cases: that variable's type is empty (the implicit ()).
(define (empty-split? f ty cand)
  (define (impossible? c)
    (parameterize ([current-clause (fresh-clause)])
      (with-handlers ([(λ (x) (and (unify-failure? x) (eq? (unify-failure-kind x) 'conflict))) (λ (_) #t)]
                      [unify-failure? (λ (_) #f)])
        (check-patterns ty c)
        #f)))
  (for/or ([x (filter fresh-variable? (flatten cand))])
    (parameterize ([current-clause (fresh-clause)])
      (check-patterns ty cand)
      (define m (findf (λ (m) (eq? (meta-name m) x)) (pmetas)))
      (define t (and m (force (meta-type m))))
      (and (vglob? t) (global-kind? (vglob-name t) 'data)
           (andmap impossible? (split f x cand))))))

;; A clause's patterns against a candidate: 'yes, 'no, or the candidate's variable to split.
(define (match-case q c)
  (cond [(or (symbol? q) (null? q)) 'yes]       ; () covers: the position is empty (verified)
        [(symbol? c) c]
        [(not (eq? (car q) (car c))) 'no]
        [else (match-cases (cdr q) (cdr c))]))

(define (match-cases qs cs)
  (define rs (map match-case qs cs))
  (cond [(memq 'no rs) 'no]
        [(findf (λ (r) (not (eq? r 'yes))) rs) => values]
        [else 'yes]))

;; Replaces x, in the candidate, by each constructor of its type. Needs the pattern variables
;; of the candidate's check in the current clause state.
(define (split f x cand)
  (define m (findf (λ (m) (eq? (meta-name m) x)) (pmetas)))
  (define ty (force (meta-type m)))
  (unless (and (vglob? ty) (global-kind? (vglob-name ty) 'data)) (oops "~a: cannot split ~a" f x))
  (for/list ([k (global-info (lookup (vglob-name ty)))])
    (define new (cons k (map (λ (_) (fresh-variable)) (explicit-args k))))
    (let subst ([t cand]) (cond [(eq? t x) new] [(pair? t) (map subst t)] [else t]))))

;; ============================================================================================
;; termination: in every recursive call a fixed argument is a strict subterm
;; ponytail: a single decreasing argument, no lexicographic order; add it when needed.
;; ============================================================================================

(define (check-termination f clauses)
  ;; the recursive calls in t, at depth d: → list of (arguments . depth)
  (define (calls t d)
    (define-values (head args) (spine-of t))
    (append (if (equal? head (tglob f)) (list (cons args d)) '())
            (match head
              [(tlam _ _ body) (calls body (+ d 1))]
              [(tpi _ _ a b) (append (calls a d) (calls b (+ d 1)))]
              [(tann e ty) (append (calls e d) (calls ty d))]
              [_ '()])
            (append-map (λ (a) (calls a d)) args)))
  (define (inside? l p)
    (match p
      [(pvar (== l)) #t]
      [(pcon _ ps) (ormap (λ (q) (inside? l q)) ps)]
      [_ #f]))
  (define (decreases? call i c)
    (define args (car call))
    (and (> (length args) i)
         (match* ((list-ref args i) (list-ref (rt-clause-patterns c) i))
           [((tvar j) (pcon _ ps)) (ormap (λ (q) (inside? (- (cdr call) j 1) q)) ps)]
           [(_ _) #f])))
  (define all-calls (for/list ([c clauses]) (calls (rt-clause-body c) (rt-clause-size c))))
  (unless (or (andmap null? all-calls)
              (for/or ([i (length (rt-clause-patterns (car clauses)))])
                (for/and ([c clauses] [cs all-calls])
                  (for/and ([call cs]) (decreases? call i c)))))
    (oops "~a: recursion is not structural" f)))

;; ============================================================================================
;; extern and builtin: reserved to the standard library
;; ============================================================================================

;; (extern concat : (→ String String String) %concat)
;; An extern does not lie: it is a type itself, a pure primitive that computes on literals, or
;; it returns an opaque type of another extern. Never a proposition: no data from the world
;; comes back into the language as a fact.
(define (check-extern d)
  (match-define (d-extern _ name* type spec*) d)
  (unless (privileged) (oops "extern is reserved to the standard library"))
  (define spec (if (symbol? spec*) (list spec*) spec*))
  (define name (fresh-name! name*))
  (define t (elaborate-signature type))
  (kernel-type-level t)
  (let loop ([r t] [names '()])
    (match r
      [(tpi x vis _ b) (loop b (if (eq? vis 'e) (cons x names) names))]
      [_
       (define-values (head _) (spine-of r))
       (unless (or (ttype? r)
                   (hash-ref primitives (car spec) #f)
                   (match head
                     [(tglob n) (and (not (builtin? n 'string)) (global-kind? n 'extern))]
                     [_ #f]))
         (oops "~a: an extern must return an opaque type" name))
       (define-global! name (global 'extern (evaluate '() t) (cons spec (reverse names))))])))

;; (builtin nat Nat): trusted like extern, only the kind of the name is checked.
(define (check-builtin key name*)
  (define name (resolve name*))
  (unless (privileged) (oops "builtin is reserved to the standard library"))
  (define kind (builtin-key-kind key))
  (unless kind (oops "builtin: unknown key ~a" key))
  (when (builtin key) (oops "builtin: ~a is already bound to ~a" key (builtin key)))
  (unless (global-kind? name kind) (oops "builtin ~a: ~a is not a ~a" key name kind))
  (bind-builtin! key name))

;; A name in a pattern: its internal name, or #f if it is not a global (then it is a variable).
(define (scope-ref* x) (let ([n (scope-ref x)]) (and (symbol? n) n)))
