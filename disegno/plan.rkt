#lang racket/base
;; The plan: main is evaluated (purely) into data, a tree of Obj constructors, and here it
;; becomes GraphQL chains. The constructor Type.kebab-field is the field typeField; the names of
;; its explicit binders are the arguments; for a method the first argument (self) is the object.
;; Object arguments go by ID: resolve gets their chain and returns the ID (live) or a placeholder (dry).
;; branch is the only point where the engine decides: observe gets the condition's chain and
;; returns the outcome. Both branches are in the plan; at run time only one is taken.

(require racket/match racket/list racket/string
         bottega/core/state bottega/core/term bottega/core/eval bottega/core/print
         "graphql.rkt")
(provide entry-value realize all-plans resolve-dry
         observe program-dir run-id realize-dry)

(define observe (make-parameter (λ (ch) (oops "branch needs a Dagger session"))))
(define program-dir (make-parameter (current-directory)))   ; relative Host and Export resolve here
(define run-id (make-parameter "<run>"))                    ; changes on every run: effects skip the cache
(define realize-dry (make-parameter #t))

(define (resolve-dry ch) (format "<~a>" (chain->graphql ch)))

;; --- reading plan values ---

(define (explicit sp) (for/list ([a sp] #:when (eq? (cdr a) 'e)) (car a)))
(define (args-of v) (explicit (vglob-spine v)))

(define (node v)                                  ; → the constructor's name, if v is a plan node
  (match v
    [(vglob n _) (and (constructor? n) (builtin? (global-info (lookup n)) 'obj) n)]
    [_ #f]))

(define (arg-names c)                             ; the constructor's explicit binders
  (let loop ([t (force (global-type (lookup c)))] [l 0])
    (match t
      [(vpi x vis _ _ _) (let ([rest (loop (force (pi-codomain t (vvar l))) (+ l 1))])
                           (if (eq? vis 'e) (cons x rest) rest))]
      [_ '()])))

(define (camel s)                                 ; with-new-file → withNewFile
  (define parts (string-split s "-" #:trim? #f))
  (string->symbol (string-append* (car parts) (map string-titlecase (cdr parts)))))

(define (not-static what v) (oops "~a not static: ~s" what (term->sexp '() (reify 0 v))))

;; Resolves the branches at the head of v.
(define (pick v resolve)
  (define f (force v))
  (if (eq? (node f) 'branch)
      (match-let ([(list c a b) (args-of f)])
        (pick (if ((observe) (chain c resolve)) a b) resolve))
      f))

;; --- chains ---

(define (chain v* resolve)
  (define v (pick v* resolve))
  (define c (or (node v) (not-static "plan" v)))
  (define args (args-of v))
  (define names (arg-names c))
  (define (field f names args . fixed)
    (cons f (append (for/list ([n names] [a args]) (list n (gql-arg a resolve))) fixed)))
  (define (method f . fixed)                      ; the receiver first: requests go out left to right
    (define receiver (chain (car args) resolve))
    (append receiver (list (apply field f (cdr names) (cdr args) fixed))))
  (case c
    [(%host-dir) (list '(host) (field 'directory names args))]
    [(%host-secret) (list `(secret (uri ,(string-append "env://" (gql-arg (car args) resolve)))))]   ; the name, never the value
    [(try-exec) (method 'withExec '(expect ANY))]
    [(cache-volume) (list (field 'cacheVolume names args))]
    [(passes?) (method 'exitCode)]
    [(probe) (append (fresh-exec v resolve '(expect ANY)) '((exitCode)))]   ; the world, outside the cache
    [(with-fresh-exec) (fresh-exec v resolve)]
    [(secret-from) (match-let ([(list _ cmd name) args])                    ; dry: the command, never a value
                     (list `(secretFrom (name ,(gql-arg name resolve)) (args ,(gql-arg cmd resolve)))))]
    ;; A secret's value is not in BuildKit's cache key: with secrets the build also gets
    ;; DISEGNO_RUN, so an ARG DISEGNO_RUN before the RUN that reads the secret reruns it.
    [(Directory.docker-build)
     (define ch (method 'dockerBuild))
     (define f (last ch))
     (if (null? (cadr (assq 'secrets (cdr f))))
         ch
         (let ([run `(input (name . "DISEGNO_RUN") (value . ,(run-id)))])
           (append (drop-right ch 1)
                   (list (cons (car f) (for/list ([a (cdr f)])
                                         (if (eq? (car a) 'buildArgs) (list 'buildArgs (append (cadr a) (list run))) a)))))))]
    [(overlay replace publish apply then skip start otherwise fail)
     (oops "an output (Export) goes at the top of the plan, or in then and branch")]
    [else (match (string-split (symbol->string c) "." #:trim? #f)
            [(list "Query" f) (list (field (camel f) names args))]
            [(list _ f) (method (camel f))])]))

;; An argument: a string, a natural, a boolean, a list or an object (by ID).
(define (gql-arg v* resolve)
  (define v (pick v* resolve))
  (define (key? k) (match v [(vglob n _) (builtin? n k)] [_ #f]))
  (cond [(node v) (resolve (chain v resolve))]
        [(vnum? v) (vnum-n v)]
        [(vstr? v) (vstr-s v)]
        [(key? 'true) #t]
        [(key? 'false) #f]
        [(key? 'nil) '()]
        [(key? 'cons) (match-let ([(list x xs) (args-of v)]) (cons (gql-arg x resolve) (gql-arg xs resolve)))]
        [(or (key? 'root) (key? 'slash)) (path-string v resolve)]
        [(and (vglob? v) (eq? (vglob-name v) 'build-arg))             ; an input object: {name, value}
         (match-let ([(list n x) (args-of v)]) `(input (name . ,(gql-arg n resolve)) (value . ,(gql-arg x resolve))))]
        ;; a refined value (Image, Segment, Arg…) goes to the engine as its first field
        [(and (vglob? v) (constructor? (vglob-name v)) (pair? (args-of v))) (gql-arg (car (args-of v)) resolve)]
        [else (not-static "argument" v)]))

(define (path-string v resolve)                   ; (/ (/ root "go") "pkg") → "/go/pkg"
  (let loop ([v (force v)] [segments '()])
    (match v
      [(vglob n _) #:when (builtin? n 'root) (if (null? segments) "/" (string-append* (map (λ (s) (string-append "/" s)) segments)))]
      [(vglob n _) #:when (builtin? n 'slash)
       (match-let ([(list p s) (args-of v)]) (loop (force p) (cons (gql-arg s resolve) segments)))]
      [_ (not-static "path" v)])))

;; An effect or an observation of the world is not cached: a new environment variable on each
;; run, right before the command, so the preparation of the environment stays cached. Unique
;; per occurrence, so two identical fresh commands stay two.
(define fresh-count 0)
(define (fresh-nonce) (set! fresh-count (+ fresh-count 1)) (format "~a-~a" (run-id) fresh-count))

(define (fresh-exec v resolve . fixed)            ; (apply c cmd), (probe c cmd): the chain up to the withExec
  (match-let ([(list c cmd) (args-of v)])
    (append (chain c resolve)
            (list `(withEnvVariable (name "DISEGNO_RUN") (value ,(fresh-nonce)))
                  `(withExec (args ,(gql-arg cmd resolve)) ,@fixed)))))

;; --- main's inputs, from its signature ---

;; (Host "path") a directory, (Host-secret "NAME") a secret, (Host-env "NAME" "default") a
;; parameter, (Host-choice "NAME" Type default) a constructor: read when the plan is built.
(define (main-input dom)
  (define-values (head args) (let loop ([t dom] [as '()]) (match t [(tapp f u _) (loop f (cons u as))] [_ (values t as)])))
  (define (is? key) (equal? head (tglob (builtin key))))
  (match args
    [(list (tstr p)) #:when (is? 'host)
     (vglob '%host-dir (list (cons (vstr (path->string (path->complete-path p (program-dir)))) 'e)))]
    [(list (tstr s)) #:when (is? 'host-secret)
     (check-env-name 'Host-secret s)
     (vglob '%host-secret (list (cons (vstr s) 'e)))]
    [(list (tstr s) (tstr default)) #:when (is? 'host-env)
     (check-env-name 'Host-env s)
     (vstr (let ([v (getenv s)]) (if (and v (not (string=? v ""))) v default)))]
    [(list (tstr s) type default) #:when (is? 'host-choice) (choice s type default)]
    [_ (oops "main's arguments must be (Host \"path\"), (Host-secret \"NAME\"), (Host-env \"NAME\" \"default\") or (Host-choice \"NAME\" Type default)")]))

(define (check-env-name who s)                    ; AWS_ACCESS_KEY_ID: letters, digits, _, not a digit first
  (unless (regexp-match-exact? #px"[A-Za-z_][A-Za-z0-9_]*" s)
    (oops "~a: «~a» is not the name of an environment variable" who s)))

;; The constructor of Type named like the variable; a name that is not there stops the plan.
(define (choice s type default)
  (check-env-name 'Host-choice s)
  (define ks (match type [(tglob n) #:when (global-kind? n 'data) (global-info (lookup n))] [_ #f]))
  (unless (and ks (andmap (λ (k) (vglob? (force (global-type (lookup k))))) ks))
    (oops "Host-choice ~a: the type must be a data of constructors without arguments only" s))
  (define v (getenv s))
  (if (or (not v) (string=? v ""))
      (evaluate '() default)
      (let ([k (findf (λ (k) (string=? (symbol->string k) v)) ks)])
        (unless k (oops "~a: «~a» is not one of ~a" s v (string-join (map symbol->string ks) ", ")))
        (vglob k '()))))

(define (entry-value entry)
  (define sig (or (signature-of entry) (oops "~a is missing" entry)))
  (define inputs
    (let loop ([t sig])
      (match t
        [(tpi _ 'e dom cod) (cons (main-input dom) (loop cod))]
        [(tpi _ _ _ _) (oops "main's arguments are explicit")]
        [_ '()])))
  (apply-spine (evaluate '() (tglob entry)) (map (λ (v) (cons v 'e)) inputs)))

;; --- realizing the outputs, in order ---

;; branch is resolved when reached, then realizes a and then b, skip does nothing; each leaf
;; output (export, publish, effect) is a chain that emit gets. Live, emit runs it at once, so an
;; observation after an effect sees the world after the effect.
(define (realize v* resolve emit)
  (define v (pick v* resolve))
  (define args (if (node v) (args-of v) '()))
  (case (node v)
    [(then) (realize (car args) resolve emit) (realize (cadr args) resolve emit)]
    [(otherwise)                                  ; if a fails, b; then a's error
     (if (realize-dry)
         (begin (realize (car args) resolve emit) (emit 'otherwise) (realize (cadr args) resolve emit))
         (with-handlers ([exn:fail? (λ (e) (realize (cadr args) resolve emit) (raise e))])
           (realize (car args) resolve emit)))]
    [(skip) (void)]
    [(fail) (let ([msg (gql-arg (car args) resolve)]) (if (realize-dry) (emit msg) (oops "~a" msg)))]
    [(overlay replace)                            ; export to the host; replace wipes first
     (define path (path->string (path->complete-path (gql-arg (car args) resolve) (program-dir))))
     (emit (append (chain (cadr args) resolve) (list `(export (path ,path) (wipe ,(eq? (node v) 'replace))))))]
    [(publish) (emit (append (chain (car args) resolve) (list `(publish (address ,(gql-arg (cadr args) resolve))))))]
    [(apply) (emit (append (fresh-exec v resolve) '((exitCode))))]
    [(start) (emit (append (chain (car args) resolve) '((start))))]
    [else (oops "the plan must return an Export")]))

;; Every outcome of the branches, dry: → ((taken . events) …), events (if chain bool),
;; (output chain), (fails message), (otherwise).
(define (all-plans entry)
  (let explore ([prefix '()])
    (define taken '())
    (define events '())
    (parameterize ([observe (λ (ch)
                              (define b (if (< (length taken) (length prefix)) (list-ref prefix (length taken)) #t))
                              (set! taken (append taken (list (cons ch b))))
                              (set! events (cons (list 'if ch b) events))
                              b)])
      (realize (entry-value entry) resolve-dry
               (λ (ch) (set! events (cons (cond [(pair? ch) (list 'output ch)]
                                                [(string? ch) (list 'fails ch)]
                                                [else (list 'otherwise)])
                                          events)))))
    (cons (cons taken (reverse events))
          (append* (for/list ([i (reverse (range (length taken)))] #:when (>= i (length prefix)))
                     (explore (append (map cdr (take taken i)) '(#f))))))))
