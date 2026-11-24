#lang racket/base
;; The surface syntax after reading: what the programmer wrote, with its positions.
;; Every node has loc, a srcloc (or #f for nodes the desugaring makes up).

(provide (all-defined-out))

;; --- expressions ---

(struct node (loc) #:transparent)
(struct s-name node (sym) #:transparent)             ; x, Nat, /out (a path literal)
(struct s-lit node (value) #:transparent)            ; "text" or a natural number
(struct s-type node (level) #:transparent)           ; Type, (Type 1)
(struct s-app node (head args) #:transparent)        ; (f a b)
(struct s-arrow node (binders result) #:transparent) ; (→ A B), (Π ((x A)) B), (∀ ((A Type)) B)
(struct s-lam node (names body) #:transparent)       ; (λ (x y) body)
(struct s-let node (bindings body) #:transparent)    ; bindings: ((name . expr) …), in sequence
(struct s-the node (type expr) #:transparent)        ; (the T e)
(struct s-list node (items) #:transparent)           ; (list a b)
(struct s-match node (scrutinees clauses) #:transparent)  ; match on a computed value
(struct s-hole node () #:transparent)                ; TODO
(struct s-auto node () #:transparent)                ; auto: a proof by computation

(struct binder (name vis type) #:transparent)        ; name '_ when it binds nothing; vis 'e or 'i

;; A clause: patterns and body; body #f for an absurd clause.
;; A pattern is plain data: a symbol (a variable or a constructor, the type decides), a natural,
;; () for the absurd pattern, or (constructor pattern …).
(struct clause (loc patterns body) #:transparent)

;; --- back to s-expressions, for error messages ---

(define (unparse e)
  (define (binders->sexp bs) (map (λ (b) (list (binder-name b) (unparse (binder-type b)))) bs))
  (cond
    [(s-name? e) (s-name-sym e)]
    [(s-lit? e) (s-lit-value e)]
    [(s-type? e) (if (zero? (s-type-level e)) 'Type `(Type ,(s-type-level e)))]
    [(s-app? e) (map unparse (cons (s-app-head e) (s-app-args e)))]
    [(s-arrow? e) `(→ ,@(map (λ (b) (unparse (binder-type b))) (s-arrow-binders e)) ,(unparse (s-arrow-result e)))]
    [(s-lam? e) `(λ ,(s-lam-names e) ,(unparse (s-lam-body e)))]
    [(s-let? e) `(let ,(map (λ (b) (list (car b) (unparse (cdr b)))) (s-let-bindings e)) ,(unparse (s-let-body e)))]
    [(s-the? e) `(the ,(unparse (s-the-type e)) ,(unparse (s-the-expr e)))]
    [(s-list? e) `(list ,@(map unparse (s-list-items e)))]
    [(s-match? e) `(match ,@(map unparse (s-match-scrutinees e)) …)]
    [(s-hole? e) 'TODO]
    [(s-auto? e) 'auto]
    [else e]))

;; The names an expression mentions.
(define (mentioned-names e)
  (cond
    [(s-name? e) (list (s-name-sym e))]
    [(s-app? e) (apply append (map mentioned-names (cons (s-app-head e) (s-app-args e))))]
    [(s-arrow? e) (apply append (mentioned-names (s-arrow-result e)) (map (λ (b) (mentioned-names (binder-type b))) (s-arrow-binders e)))]
    [(s-lam? e) (mentioned-names (s-lam-body e))]
    [(s-let? e) (apply append (mentioned-names (s-let-body e)) (map (λ (b) (mentioned-names (cdr b))) (s-let-bindings e)))]
    [(s-the? e) (append (mentioned-names (s-the-type e)) (mentioned-names (s-the-expr e)))]
    [(s-list? e) (apply append (map mentioned-names (s-list-items e)))]
    [(s-match? e) (apply append (append (map mentioned-names (s-match-scrutinees e))
                                        (for/list ([c (s-match-clauses e)] #:when (clause-body c))
                                          (mentioned-names (clause-body c)))))]
    [else '()]))

;; --- declarations ---

(struct d-data node (name type constructors) #:transparent)    ; constructors: ((name . type) …)
(struct d-define node (name type clauses doc) #:transparent)   ; type: the claim's; doc: string or #f
(struct d-extern node (name type spec) #:transparent)          ; spec: the primitive or engine field
(struct d-builtin node (key name) #:transparent)
