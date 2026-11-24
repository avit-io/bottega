#lang racket/base
;; Generates disegno/lib/engine.rkt from the engine's schema, by introspection on a session:
;;   dagger api with-session racket disegno/gen.rkt
;;
;; The plan is data: every engine field becomes a constructor of the family
;; (data Obj : (→ Kind Type) …), indexed by the GraphQL type (Kind.container, …).
;; The constructor's name gives the field (Container.with-exec → withExec), its binders the
;; arguments. Only the types in `allowed`, the required arguments and the promoted ones; fields
;; that cannot be expressed are skipped. A Boolean result becomes an observation, (Obj Kind.bool).
;; The generated file is committed: checking a program needs no session.

(require racket/list racket/string racket/runtime-path json "session.rkt")

(define-runtime-path engine-file "lib/engine.rkt")

(define allowed '("Container" "Directory" "File" "CacheVolume" "GitRepository" "GitRef" "Secret" "Service"))

;; --- names ---

(define (kebab s)                                         ; withNewFile → with-new-file
  (list->string
   (let loop ([cs (string->list s)] [prev #f])
     (cond [(null? cs) '()]
           [(and (char-upper-case? (car cs)) prev (not (char-upper-case? prev)))
            (list* #\- (char-downcase (car cs)) (loop (cdr cs) (car cs)))]
           [else (cons (char-downcase (car cs)) (loop (cdr cs) (car cs)))]))))

(define (camel s)                                         ; with-new-file → withNewFile (as the runner does)
  (define parts (string-split s "-" #:trim? #f))
  (string-append* (car parts) (map string-titlecase (cdr parts))))

(define (obj k) `(Obj ,(string->symbol (string-append "Kind." (kebab k)))))
(define (obj? t) (and (pair? t) (eq? (car t) 'Obj)))

;; --- the schema ---

(define typeref "type { kind name ofType { kind name ofType { kind name ofType { kind name } } } }")

(define (schema-types)
  (query (format "{ __schema { types { name kind fields { name isDeprecated args { name defaultValue directives { name args { name value } } ~a } ~a } } } }"
                 typeref typeref)))           ; the leaf: the list of types

;; IDs are generic: the object's type is in the @expectedType directive.
(define (expected a)
  (define d (findf (λ (d) (equal? (hash-ref d 'name) "expectedType")) (hash-ref a 'directives '())))
  (and d (string->jsexpr (hash-ref (car (hash-ref d 'args)) 'value))))

;; Refined types by argument name: the schema says String, the design knows more.
;; ponytail: a rule by name, enough for Dagger; a per-field table if a name changes meaning.
(define refined '(("path" String Abs) ("address" String Image) ("args" (List String) (List Arg)) ("port" Nat Port)))

(define (arg-type a)
  (define t (base-type a (hash-ref a 'type)))
  (define r (assoc (hash-ref a 'name) refined))
  (if (and r (equal? t (cadr r))) (caddr r) t))

(define (base-type a t)
  (define k (hash-ref t 'kind)) (define n (hash-ref t 'name))
  (cond [(equal? k "NON_NULL") (base-type a (hash-ref t 'ofType))]
        [(equal? k "LIST") (let ([e (base-type a (hash-ref t 'ofType))])   ; lists of texts or of objects (by ID)
                             (and (or (memq e '(String Build-arg)) (obj? e)) `(List ,e)))]
        [(equal? n "String") 'String]
        [(equal? n "Int") 'Nat]                           ; ponytail: Int → Nat, ports and counts are enough
        [(equal? n "Boolean") 'Bool]
        [(and (equal? k "INPUT_OBJECT") (equal? n "BuildArg")) 'Build-arg]
        [(equal? n "ID") (let ([e (expected a)]) (and e (member e allowed) (obj e)))]
        [else #f]))

(define (result-type t)
  (define k (hash-ref t 'kind)) (define n (hash-ref t 'name))
  (cond [(equal? k "NON_NULL") (result-type (hash-ref t 'ofType))]
        [(and (equal? k "OBJECT") (member n allowed)) (obj n)]
        [(and (equal? k "SCALAR") (equal? n "Boolean")) (obj "bool")]
        [else #f]))

(define (required? a)
  (and (equal? (hash-ref (hash-ref a 'type) 'kind) "NON_NULL") (eq? (hash-ref a 'defaultValue) (json-null))))

;; Promoted optional arguments: for these fields the constructor asks for them, in the schema's
;; order. Whoever does not need them uses a curated library name that passes the default.
;; ponytail: a hand-written list, field by field; it grows when a library needs it.
(define promoted
  '(("Directory" "filter" "include" "exclude")            ; the globs of files to keep and to drop
    ("Directory" "withNewFile" "permissions")             ; an executable file
    ("Container" "asService" "useEntrypoint")             ; a service with the image's entrypoint
    ("Directory" "dockerBuild" "dockerfile" "buildArgs" "secrets")))

(define (wanted? type field a)
  (or (required? a)
      (let ([p (findf (λ (p) (and (equal? (car p) type) (equal? (cadr p) field))) promoted)])
        (and p (member (hash-ref a 'name) (cddr p)) #t))))

;; Root fields made into core junctions, and those a program must not reach.
(define excluded '("cacheVolume"                          ; the core junction cache-volume
                   "secret" "setSecret"))                 ; a secret comes in only from main's signature

;; A constructor: (name ((argument type) …) result), or #f if the field cannot be expressed.
(define (constructor type f)
  (define n (hash-ref f 'name))
  (define args (filter (λ (a) (wanted? type n a)) (hash-ref f 'args)))
  (define types (map arg-type args))
  (define result (result-type (hash-ref f 'type)))
  (and result
       (not (hash-ref f 'isDeprecated))
       (andmap values types)
       (not (and (equal? type "Query") (member n excluded)))
       (string=? (camel (kebab n)) n)                     ; the runner must get back to the field
       (list (string->symbol (format "~a.~a" type (kebab n)))
             (append (if (equal? type "Query") '() (list (list 'self (obj type))))
                     (map (λ (a t) (list (string->symbol (hash-ref a 'name)) t)) args types))
             result)))

(define (constructors schema type)
  (define t (findf (λ (t) (equal? (hash-ref t 'name) type)) schema))
  (filter-map (λ (f) (constructor type f))
              (sort (hash-ref t 'fields) string<? #:key (λ (f) (hash-ref f 'name)))))

;; The junctions the runner realizes itself: constructors too, not engine fields.
(define core-nodes
  `((%host-dir ((path String)) ,(obj "directory"))
    (%host-secret ((name String)) ,(obj "secret"))       ; the secret of a host environment variable
    (try-exec ((self ,(obj "container")) (args (List Arg))) ,(obj "container"))
    (passes? ((self ,(obj "container"))) ,(obj "bool"))
    (branch ((cond ,(obj "bool")) (then (Obj k)) (else (Obj k))) (Obj k))
    (overlay ((path String) (dir ,(obj "directory"))) ,(obj "export"))
    (replace ((path String) (dir ,(obj "directory"))) ,(obj "export"))
    (publish ((self ,(obj "container")) (address Image)) ,(obj "export"))
    ;; effects in sequence: apply runs a command as an effect (outside the cache); then realizes
    ;; two outputs in order; skip is the empty output; probe observes the world outside the cache
    (apply ((self ,(obj "container")) (args (List Arg))) ,(obj "export"))
    (then ((first ,(obj "export")) (second ,(obj "export"))) ,(obj "export"))
    (skip () ,(obj "export"))
    (otherwise ((first ,(obj "export")) (second ,(obj "export"))) ,(obj "export"))   ; if a fails, b; then it fails
    (fail ((message String)) ,(obj "export"))
    (start ((self ,(obj "service"))) ,(obj "export"))   ; a service alive until the end of the session
    (probe ((self ,(obj "container")) (args (List Arg))) ,(obj "bool"))
    (with-fresh-exec ((self ,(obj "container")) (args (List Arg))) ,(obj "container"))
    ;; a secret read at run time: the runner hands it to Dagger, it never comes back to the language
    (secret-from ((self ,(obj "container")) (args (List Arg)) (name String)) ,(obj "secret"))
    ;; the only state that persists between runs: a declared junction, not a generated field
    (cache-volume ((key String)) ,(obj "cache-volume"))))

;; --- writing engine.rkt ---

(define (signature c)
  (if (null? (cadr c))
      (format "~s" (caddr c))
      (format "(Π (~a) ~s)" (string-join (for/list ([b (cadr c)]) (format "(~a ~s)" (car b) (cadr b))) " ") (caddr c))))

(define (variables c) (for/list ([i (length (cadr c))]) (string->symbol (format "x~a" i))))
(define (head c) (if (null? (cadr c)) (car c) (cons (car c) (variables c))))

;; nodes: all the sub-plans, by structural recursion on every constructor.
(define (nodes-clause c)
  (define subs (for/list ([b (cadr c)] [x (variables c)] #:when (obj? (cadr b))) x))
  (format "    (~s (:: (any ~s) ~a))" (head c) (head c)
          (for/foldr ([acc "nil"]) ([x subs]) (format "(++ (nodes ~a) ~a)" x acc))))

;; chain: the nodes along self's chain, without going into the arguments; branch, both branches.
(define (chain-clause c)
  (define rest (cond [(eq? (car c) 'branch) "(++ (chain x1) (chain x2))"]
                     [(and (pair? (cadr c)) (eq? (car (car (cadr c))) 'self)) "(chain x0)"]
                     [else "nil"]))
  (format "    (~s (:: (any ~s) ~a))" (head c) (head c) rest))

(define (write-engine all)
  (with-output-to-file engine-file #:exists 'replace
    (λ ()
      (printf "#lang disegno\n; Generated by disegno/gen.rkt from Dagger's schema: do not edit by hand.\n\n")
      (printf "(require \"values.rkt\")\n\n")
      (printf "(data Kind\n~a)\n\n"
              (string-join (for/list ([k (append allowed '("bool" "export"))]) (format "  (~a)" (cadr (obj k)))) "\n"))
      (printf "; An argument of a Dockerfile build: name and value (the schema's BuildArg).\n(data Build-arg\n  (build-arg String String))\n\n")
      (printf "(data Obj : (→ Kind Type)\n~a)\n(builtin obj Obj)\n\n"
              (string-join (for/list ([c all]) (format "  (~a : ~a)" (car c) (signature c))) "\n"))
      (for ([k allowed]) (printf "(claim ~a Type)\n(define ~a ~s)\n\n" k k (obj k)))
      (printf "; Any sub-plan, of any type.\n(data Any\n  (any (Obj k)))\n\n")
      (printf "(claim nodes (→ (Obj k) (List Any)))\n(define (nodes n)\n  (match n\n~a))\n\n"
              (string-join (map nodes-clause all) "\n"))
      (printf "(claim chain (→ (Obj k) (List Any)))\n(define (chain n)\n  (match n\n~a))\n"
              (string-join (map chain-clause all) "\n")))))

(module+ main
  (define schema (schema-types))
  (define all (append core-nodes (append-map (λ (t) (constructors schema t)) (cons "Query" allowed))))
  (write-engine all)
  (printf "~a written: ~a constructors\n" engine-file (length all)))
