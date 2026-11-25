#lang racket/base
;; The checker's state: errors with positions, globals, builtins and metavariables.
;; Everything lives in parameters, so every check starts from a clean slate (with-fresh-state).

(provide (struct-out exn:fail:bottega) here at oops
         (struct-out global) lookup define-global! global-kind? constructor? all-global-names
         register-builtin-keys! builtin-key-kind builtin builtin? builtin-key bind-builtin!
         register-root-namespace! root-field
         privileged
         new-meta! meta-solution meta-name meta-type solve-meta!
         record-signature! signature-of
         fresh-number!
         with-fresh-state)

;; --- errors ---------------------------------------------------------------------------------

;; A language error. It carries the source location, so DrRacket and raco show where it is.
(struct exn:fail:bottega exn:fail (where)
  #:property prop:exn:srclocs
  (λ (e) (if (exn:fail:bottega-where e) (list (exn:fail:bottega-where e)) '())))

(define here (make-parameter #f))          ; srcloc of the form being elaborated, or #f

(define-syntax-rule (at loc body ...)      ; elaborate body with loc as the current position
  (let ([l loc]) (if l (parameterize ([here l]) body ...) (let () body ...))))

(define (oops fmt . args)
  (define msg (apply format fmt args))
  (define loc (here))
  (raise (exn:fail:bottega (if loc (string-append (srcloc->string loc) ": " msg) msg)
                           (current-continuation-marks)
                           loc)))

;; --- globals --------------------------------------------------------------------------------

;; kind  'data    info: the names of its constructors
;;       'con     info: the name of its data
;;       'def     info: (arity . run-time clauses), #f while the define is being checked
;;       'extern  info: (spec . explicit argument names)
(struct global (kind type [info #:mutable]))

(define current-globals (make-parameter (make-hasheq)))

(define (lookup name) (and (symbol? name) (hash-ref (current-globals) name #f)))
(define (define-global! name g) (hash-set! (current-globals) name g))
(define (global-kind? name kind) (let ([g (lookup name)]) (and g (eq? (global-kind g) kind))))
(define (constructor? name) (global-kind? name 'con))
(define (all-global-names) (hash-keys (current-globals)))

;; --- builtins -------------------------------------------------------------------------------

;; The library names the core knows about, like Agda's BUILTIN. The keys are a closed list; the
;; library binds each key to one of its names with (builtin key name). So the checker never
;; names the library: numerals, strings, paths, lists, auto and acceleration go through here.
;; key → the kind of global it must name. A dialect adds its own keys (disegno: the plan's Obj
;; and main's inputs) with register-builtin-keys!.
(define (register-builtin-keys! keys) (for ([k keys]) (hash-set! builtin-keys (car k) (cdr k))))
(define (builtin-key-kind key) (hash-ref builtin-keys key #f))
(define builtin-keys
  (make-hasheq
   '((nat . data) (zero . con) (suc . con)
    (bool . data) (true . con) (false . con)
    (unit . data) (tt . con) (empty . data) (eq . data) (refl . con) (sigma . data)
    (list . data) (nil . con) (cons . con) (nothing . con) (just . con)
    (string . extern)
    (abs . data) (root . con) (slash . con) (rel . data) (here . con) (step . con) (segment . data)
    (duration . data) (instant . data) (ip . data) (cidr . data)
    (plus . def) (monus . def) (times . def) (leq? . def) (lt? . def) (nat-eq? . def) (pow2 . def))))

;; Root namespaces: a name x that is not defined is looked up as Prefix.x. Disegno registers
;; "Query", so (container) is the schema's root field Query.container.
(define root-namespaces '())
(define (register-root-namespace! prefix) (set! root-namespaces (append root-namespaces (list prefix))))
(define (root-field x)
  (for/or ([p root-namespaces])
    (define n (string->symbol (format "~a.~a" p x)))
    (and (lookup n) n)))

(define current-builtins (make-parameter (make-hasheq)))     ; key → name
(define current-builtin-of (make-parameter (make-hasheq)))   ; name → key

(define (builtin key) (hash-ref (current-builtins) key #f))
(define (builtin? name key) (and name (eq? name (builtin key))))
(define (builtin-key name) (hash-ref (current-builtin-of) name #f))
(define (bind-builtin! key name)
  (hash-set! (current-builtins) key name)
  (hash-set! (current-builtin-of) name key))

;; extern and builtin are reserved to the standard library.
(define privileged (make-parameter #f))

;; --- metavariables --------------------------------------------------------------------------

;; A hole the elaborator fills by unification. name and type are known for pattern variables.
(struct hole ([solution #:mutable] name type))

(define current-metas (make-parameter (make-hasheqv)))   ; id → hole

(define (new-meta! name type)
  (define id (+ 1 (hash-count (current-metas))))
  (hash-set! (current-metas) id (hole #f name type))
  id)

(define (meta-solution id) (hole-solution (hash-ref (current-metas) id)))
(define (meta-name id) (hole-name (hash-ref (current-metas) id)))
(define (meta-type id) (hole-type (hash-ref (current-metas) id)))
(define (solve-meta! id v) (set-hole-solution! (hash-ref (current-metas) id) v))

;; --- signatures, as written ------------------------------------------------------------------

;; A define's elaborated signature, not reduced: (Host ".") stays Host, not Directory. The
;; runner reads main's inputs from it.
(define current-signatures (make-parameter (make-hasheq)))
(define (record-signature! name t) (hash-set! (current-signatures) name t))
(define (signature-of name) (hash-ref (current-signatures) name #f))

;; --- fresh names ----------------------------------------------------------------------------

(define current-counter (make-parameter (box 0)))
(define (fresh-number!)
  (define b (current-counter))
  (set-box! b (+ 1 (unbox b)))
  (unbox b))

(define (with-fresh-state thunk)
  (parameterize ([current-globals (make-hasheq)]
                 [current-builtins (make-hasheq)]
                 [current-builtin-of (make-hasheq)]
                 [current-metas (make-hasheqv)]
                 [current-signatures (make-hasheq)]
                 [current-counter (box 0)])
    (thunk)))
