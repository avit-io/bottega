#lang racket/base
;; The surface: Scheme with types, in the style of Pie (The Little Typer).
;;   (claim f T) then (define (f x …) body) or (define f body), with an optional docstring
;;   (data D (k A …) …) or (data D : T (k : T) …), (record R (field T) …)
;;   (Π ((x A) …) B), (∀ ((A Type) …) B), (→ A … B), (λ (x …) body), (let ((x e) …) body)
;;   (the T e), (list a …), (pipe x (f a) g), (cond (c e) … (else e)), TODO, auto
;;   (match x clause …) and (match* (x y) clause …), on the arguments or on any value
;; Syntax objects come in, ast.rkt nodes go out; positions are kept on every node.

(require racket/list racket/string syntax/parse "ast.rkt" "state.rkt")
(provide parse-program syntax-loc)

(define (syntax-loc s)
  (srcloc (syntax-source s) (syntax-line s) (syntax-column s) (syntax-position s) (syntax-span s)))

(define (rebuild like datum) (datum->syntax like datum like))   ; new syntax, at like's position

(define declaration-keywords '(claim define data record extern builtin))

;; --- expressions ---

(define (parse-expr stx)
  (define loc (syntax-loc stx))
  (syntax-parse stx
    #:datum-literals (TODO auto Type Π ∀ → λ lambda let let* list the pipe match match* cond else)
    [TODO (s-hole loc)]
    [auto (s-auto loc)]
    [Type (s-type loc 0)]
    [x:id (s-name loc (syntax-e #'x))]
    [s:str (s-lit loc (syntax-e #'s))]
    [n:exact-nonnegative-integer (s-lit loc (syntax-e #'n))]
    [(Type n:exact-nonnegative-integer) (s-type loc (syntax-e #'n))]
    [(Π ((x:id A) ...) B) (s-arrow loc (binders 'e #'(x ...) #'(A ...)) (parse-expr #'B))]
    [(∀ ((x:id A) ...) B) (s-arrow loc (binders 'i #'(x ...) #'(A ...)) (parse-expr #'B))]
    [(→ A ...+ B) (s-arrow loc (for/list ([a (syntax->list #'(A ...))]) (binder '_ 'e (parse-expr a)))
                           (parse-expr #'B))]
    [((~or λ lambda) (x:id ...) body) (s-lam loc (syntax->datum #'(x ...)) (parse-expr #'body))]
    [((~or let let*) ((x:id e) ...) body)
     (s-let loc (map (λ (x e) (cons x (parse-expr e))) (syntax->datum #'(x ...)) (syntax->list #'(e ...)))
            (parse-expr #'body))]
    [(the T e) (s-the loc (parse-expr #'T) (parse-expr #'e))]
    [(list item ...) (s-list loc (map parse-expr (syntax->list #'(item ...))))]
    [(pipe x step ...) (parse-pipe (parse-expr #'x) (syntax->list #'(step ...)))]
    [(match x cl ...+) (s-match loc (list (parse-expr #'x)) (parse-clauses (syntax->list #'(cl ...)) 1))]
    [(match* (x ...+) cl ...+)
     (s-match loc (map parse-expr (syntax->list #'(x ...)))
              (parse-clauses (syntax->list #'(cl ...)) (length (syntax->list #'(x ...)))))]
    [(cond c ...+) (parse-cond (syntax->list #'(c ...)))]
    [(kw:id . _) #:when (memq (syntax-e #'kw) declaration-keywords)
     (at loc (oops "~a: it is a declaration, not an expression" (syntax-e #'kw)))]
    [(f arg ...+) (s-app loc (parse-expr #'f) (map parse-expr (syntax->list #'(arg ...))))]
    [_ (at loc (oops "invalid expression: ~s" (syntax->datum stx)))]))

(define (binders vis xs as)
  (map (λ (x a) (binder x vis (parse-expr a))) (syntax->datum xs) (syntax->list as)))

;; (pipe x (f a) g) is (g (f x a)): the value goes in as the first argument.
(define (parse-pipe first steps)
  (for/fold ([acc first]) ([s steps])
    (syntax-parse s
      [(f arg ...) (s-app (syntax-loc s) (parse-expr #'f) (cons acc (map parse-expr (syntax->list #'(arg ...)))))]
      [_ (s-app (syntax-loc s) (parse-expr s) (list acc))])))

;; (cond (c e) … (else e)) is a chain of if, the prelude's.
(define (parse-cond clauses)
  (syntax-parse (car clauses)
    #:datum-literals (else)
    [(else e) (if (null? (cdr clauses))
                  (parse-expr #'e)
                  (at (syntax-loc (car clauses)) (oops "cond: else goes last")))]
    [(c e) (if (null? (cdr clauses))
               (at (syntax-loc (car clauses)) (oops "cond: else is missing"))
               (s-app (syntax-loc (car clauses)) (s-name #f 'if)
                      (list (parse-expr #'c) (parse-expr #'e) (parse-cond (cdr clauses)))))]
    [_ (at (syntax-loc (car clauses)) (oops "cond: malformed clause ~s" (syntax->datum (car clauses))))]))

;; --- clauses and patterns ---

;; A clause is (pattern body), or (pattern) for an absurd one; with match*, (patterns body).
;; → (values patterns body-syntax-or-#f)
(define (split-clause cl n)
  (define parts (syntax->list cl))
  (unless (and parts (<= 1 (length parts) 2))
    (at (syntax-loc cl) (oops "match: malformed clause ~s" (syntax->datum cl))))
  (define pats (if (= n 1) (list (syntax->datum (car parts))) (syntax->datum (car parts))))
  (unless (and (list? pats) (= (length pats) n))
    (at (syntax-loc cl) (oops "match*: ~a patterns expected in ~s" n (syntax->datum cl))))
  (values pats (and (pair? (cdr parts)) (cadr parts))))

(define (parse-clauses cls n)
  (for/list ([cl cls])
    (define-values (pats body) (split-clause cl n))
    (clause (syntax-loc cl) pats (and body (parse-expr body)))))

;; A pattern variable: a symbol of the patterns that is not a constructor's head.
(define (pattern-var? x pats)
  (for/or ([p pats]) (if (pair? p) (pattern-var? x (cdr p)) (eq? p x))))

;; Substitutes xs by qs in the patterns, all together: (match* (m n) ((n m) …)) swaps.
(define (subst-patterns pats xs qs)
  (for/list ([p pats])
    (cond [(and (symbol? p) (index-of xs p)) => (λ (i) (list-ref qs i))]
          [(pair? p) (cons (car p) (subst-patterns (cdr p) xs qs))]
          [else p])))

;; (match x clause …) at the head of a define's body, on its arguments: x becomes each clause's
;; pattern, and so on inside. → list of clauses.
(define (define-clauses pats body)
  (define (expand xs cls)
    (append* (for/list ([cl cls])
               (define-values (ps b) (split-clause cl (length xs)))
               (define new (subst-patterns pats xs ps))
               (if b
                   (define-clauses new b)
                   (list (clause (syntax-loc cl) new #f))))))
  (define (on-args? xs) (andmap (λ (x) (pattern-var? x pats)) xs))
  (syntax-parse body
    #:datum-literals (match match*)
    [(match x:id cl ...+) #:when (on-args? (list (syntax-e #'x)))
     (expand (list (syntax-e #'x)) (syntax->list #'(cl ...)))]
    [(match* (x:id ...+) cl ...+) #:when (on-args? (syntax->datum #'(x ...)))
     (expand (syntax->datum #'(x ...)) (syntax->list #'(cl ...)))]
    [_ (list (clause (syntax-loc body) pats (parse-expr body)))]))

;; --- declarations ---

;; → list of declarations, in order. A claim waits for its define.
(define (parse-program stxs)
  (define claims (make-hasheq))                 ; name → (loc . type)
  (define decls
    (append* (for/list ([stx stxs]) (parse-declaration stx claims))))
  (for ([(name c) claims])
    (at (car c) (oops "~a: claim without define" name)))
  decls)

(define (parse-declaration stx claims)
  (define loc (syntax-loc stx))
  (syntax-parse stx
    #:datum-literals (claim define data record extern builtin :)
    [(claim name:id type)
     (define n (syntax-e #'name))
     (when (hash-ref claims n #f) (at loc (oops "~a: already claimed, the define is missing" n)))
     (hash-set! claims n (cons loc (parse-expr #'type)))
     '()]
    [(define (name:id param:id ...) (~optional doc:str) body)
     (list (make-define loc claims (syntax-e #'name) (attribute doc)
                        (define-clauses (syntax->datum #'(param ...)) #'body)))]
    [(define name:id (~optional doc:str) body)
     (list (make-define loc claims (syntax-e #'name) (attribute doc)
                        (list (clause (syntax-loc #'body) '() (parse-expr #'body)))))]
    [(data name:id : type (k:id : kt) ...)
     (list (d-data loc (syntax-e #'name) (parse-expr #'type)
                   (map (λ (k t) (cons k (parse-expr t))) (syntax->datum #'(k ...)) (syntax->list #'(kt ...)))))]
    [(data name:id k ...)
     (list (d-data loc (syntax-e #'name) (s-type #f 0)
                   (map (λ (k) (short-constructor #'name k)) (syntax->list #'(k ...)))))]
    [(record head field ...) (append* (map (λ (d) (parse-declaration d claims)) (record->declarations stx)))]
    [(extern name:id : type spec)
     (list (d-extern loc (syntax-e #'name) (parse-expr #'type) (syntax->datum #'spec)))]
    [(builtin key:id name:id) (list (d-builtin loc (syntax-e #'key) (syntax-e #'name)))]
    [_ (at loc (oops "unknown declaration: ~s" (syntax->datum stx)))]))

(define (make-define loc claims name doc clauses)
  (define c (or (hash-ref claims name #f)
                (at loc (oops "~a: the claim is missing, (claim ~a type)" name name))))
  (hash-remove! claims name)
  (d-define (car c) name (cdr c) clauses (and doc (syntax-e doc))))

;; In the short form (k A B) is a constructor of type (→ A B D), and k or (k) of type D.
(define (short-constructor d ctor)
  (syntax-parse ctor
    [k:id (cons (syntax-e #'k) (parse-expr d))]
    [(k:id) (cons (syntax-e #'k) (parse-expr d))]
    [(k:id arg ...+) (cons (syntax-e #'k) (parse-expr (rebuild ctor `(→ ,@(syntax->list #'(arg ...)) ,d))))]
    [_ (at (syntax-loc ctor) (oops "malformed constructor: ~s" (syntax->datum ctor)))]))

;; (record Service (name String) (replicas Nat) (ok (≥ replicas 1))) is sugar for a data with the
;; single constructor service, and the projections Service.name, Service.replicas, …
;; In the types of later fields an earlier field f becomes (Service.f r).
;; With parameters, (record (Affine P V (≤ (→ P P Type))) …): implicit in constructor and projections;
;; a parameter without a type is a Type.
(define (record->declarations stx)
  (syntax-parse stx
    [(_ head (f:id T) ...)
     (define-values (name params)
       (syntax-parse #'head
         [n:id (values (syntax-e #'n) '())]
         [(n:id p ...) (values (syntax-e #'n)
                               (for/list ([p (syntax->list #'(p ...))])
                                 (syntax-parse p
                                   [x:id (list (syntax-e #'x) 'Type)]
                                   [(x:id t) (list (syntax-e #'x) #'t)])))]))
     (define fields (map list (syntax->datum #'(f ...)) (syntax->list #'(T ...))))
     (define self (if (null? params) name (cons name (map car params))))
     (define k (let ([s (symbol->string name)]) (string->symbol (string-append (string-downcase (substring s 0 1)) (substring s 1)))))
     (define (proj f) (string->symbol (format "~a.~a" name f)))
     (define (forall body) (if (null? params) body `(∀ ,params ,body)))
     (define (pi bs body) (if (null? bs) body `(Π ,bs ,body)))
     (define data-type (if (null? params) 'Type `(Π ,params Type)))
     (cons (rebuild stx `(data ,name : ,data-type (,k : ,(forall (pi fields self)))))
           (append-map
            (λ (field i)
             (define earlier (map car (take fields i)))
             (define type (replace-fields (cadr field) earlier proj))
             (define pattern `(,k ,@(for/list ([j (length fields)]) (if (= i j) '%v '_))))
             (list (rebuild stx `(claim ,(proj (car field)) ,(forall `(Π ((%r ,self)) ,type))))
                   (rebuild stx `(define (,(proj (car field)) %r) (match %r (,pattern %v))))))
            fields (range (length fields))))]))

(define (replace-fields t earlier proj)          ; t: syntax; an earlier field f → (proj-f %r)
  (syntax-parse t
    [x:id #:when (memq (syntax-e #'x) earlier) (rebuild t `(,(proj (syntax-e #'x)) %r))]
    [(part ...) (rebuild t (map (λ (p) (replace-fields p earlier proj)) (syntax->list #'(part ...))))]
    [_ t]))
