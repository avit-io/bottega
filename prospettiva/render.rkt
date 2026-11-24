#lang racket/base
;; The projections of a model. main takes an environment, a constructor of an enumeration, and
;; gives that environment's configuration: every environment is a view of the same model, and the
;; theorems are the invariants of the projection. Here they are computed and written as YAML, for
;; Nix to build. A model's main submodule calls main-program.
;;   racket model.rkt render DIR    DIR/<env>/values.yaml, for every environment
;;   racket model.rkt show ENV      one environment, on stdout
;;   racket model.rkt check         types and theorems only
;;
;; A value becomes data in one way only, from its normal form with the proofs erased:
;;   a record          a mapping, by field names, in order
;;   an enumeration    its constructor's name
;;   a list            a sequence
;;   Nat, String, Bool themselves
;;   a refined literal (Port, Region, Arn, a path) its text
;; ponytail: proofs are erased when they are tt or refl (what auto gives); other proofs, and data
;; with constructors taking arguments, are refused. Add an optional field (omitted key) when a
;; model needs one.

(require racket/match racket/list racket/string racket/file json
         bottega/check bottega/core/state bottega/core/term bottega/core/eval bottega/core/print
         "dialect.rkt")
(provide main-program render-text)

(define (main-program file)
  (define args (vector->list (current-command-line-arguments)))
  (with-handlers ([exn:fail:bottega? (λ (e) (eprintf "~a\n" (exn-message e)) (exit 1))])
    (parameterize ([current-languages languages])
      (call-with-checked-file file
        (λ ()
          (match args
            [(list "check") (displayln "checked: types and theorems")]
            [(list "render" dir)
             (for ([e (environments)])
               (define file (build-path dir (car e) "values.yaml"))
               (make-parent-directory* file)
               (call-with-output-file file #:exists 'truncate (λ (out) (write-string (yaml (cdr e)) out)))
               (printf "~a\n" file))]
            [(list "show" name)
             (define e (or (assoc name (environments))
                           (oops "~a is not an environment: ~a" name (string-join (map car (environments)) ", "))))
             (display (yaml (cdr e)))]
            [_ (eprintf "usage: racket model.rkt render DIR | show ENV | check\n") (exit 2)]))))))

;; One environment's YAML, for the tests.
(define (render-text file name)
  (parameterize ([current-languages languages])
    (call-with-checked-file file (λ () (yaml (cdr (assoc name (environments))))))))

;; --- the environments: main applied to every constructor of its domain ---

(define (environments)                            ; → ((name . data) …), in the order of the constructors
  (define env
    (match (signature-of 'main)
      [(tpi _ 'e (tglob n) _) #:when (global-kind? n 'data) n]
      [#f (oops "main is missing: (claim main (Π ((e Env)) Config))")]
      [_ (oops "main takes an environment: (claim main (Π ((e Env)) Config)), Env a data")]))
  (for/list ([k (global-info (lookup env))])
    (unless (vglob? (force (global-type (lookup k))))
      (oops "~a: an environment is a constructor without arguments" k))
    (define v (apply-spine (evaluate '() (tglob 'main)) (list (cons (vglob k '()) 'e))))
    (cons (symbol->string k) (term->data (reify 0 v)))))

;; --- normal forms → data ---

(struct mapping (pairs))                          ; a record: ((field . data) …), in order
(define erased (string->uninterned-symbol "proof"))

(define (spine t)                                 ; → (values head explicit-arguments)
  (let loop ([t t] [args '()])
    (match t
      [(tapp f a 'e) (loop f (cons a args))]
      [(tapp f _ _) (loop f args)]
      [_ (values t args)])))

(define (field-names k)                           ; a constructor's explicit binders
  (let loop ([t (force (global-type (lookup k)))] [l 0])
    (match t
      [(vpi x vis _ _ _) (let ([rest (loop (force (pi-codomain t (vvar l))) (+ l 1))])
                           (if (eq? vis 'e) (cons x rest) rest))]
      [_ '()])))

(define (term->data t)
  (define-values (head args) (spine t))
  (define (is? key) (match head [(tglob n) (builtin? n key)] [_ #f]))
  (define lit (literal-of t))
  (cond [(tnum? t) (tnum-n t)]
        [(tstr? t) (tstr-s t)]
        [lit (if (symbol? lit) (symbol->string lit) lit)]
        [(is? 'true) #t]
        [(is? 'false) #f]
        [(or (is? 'tt) (is? 'refl)) erased]
        [(is? 'nil) '()]
        [(is? 'cons) (cons (term->data (car args)) (term->data (cadr args)))]
        [(and (tglob? head) (constructor? (tglob-name head)))
         (define k (tglob-name head))
         (define siblings (global-info (lookup (data-of k))))
         (cond [(null? args) (symbol->string k)]  ; an enumeration
               [(= (length siblings) 1)           ; a record
                (mapping (for/list ([f (field-names k)] [a args]
                                    #:unless (eq? (term->data a) erased))
                           (cons f (term->data a))))]
               [else (oops "cannot become data, a constructor with arguments: ~s" (term->sexp '() t))])]
        [else (oops "cannot become data: ~s" (term->sexp '() t))]))

(define (data-of k)                               ; the data a constructor belongs to
  (let loop ([t (force (global-type (lookup k)))] [l 0])
    (match t
      [(vpi _ _ _ _ _) (loop (force (pi-codomain t (vvar l))) (+ l 1))]
      [(vglob n _) n])))

;; --- data → YAML ---
;; Strings are always quoted, as JSON strings (valid YAML), so "2012-10-17" or "true" stay text.

(define (yaml d [indent 0])
  (define pad (make-string indent #\space))
  (match d
    [(mapping ps)
     (string-append* (for/list ([p ps])
                       (if (scalar? (cdr p))
                           (format "~a~a: ~a\n" pad (key (car p)) (scalar (cdr p)))
                           (format "~a~a:\n~a" pad (key (car p)) (yaml (cdr p) (+ indent 2))))))]
    [(? list?)
     (string-append* (for/list ([x d])
                       (if (scalar? x)
                           (format "~a- ~a\n" pad (scalar x))
                           ;; a nested block: its first line's indentation becomes the dash
                           (string-append pad "- " (substring (yaml x (+ indent 2)) (+ indent 2))))))]))

(define (scalar? d) (not (or (and (mapping? d) (pair? (mapping-pairs d))) (pair? d))))

(define (scalar d)
  (match d
    [(? string?) (jsexpr->string d)]
    [(? number?) (number->string d)]
    [#t "true"]
    [#f "false"]
    ['() "[]"]
    [(mapping '()) "{}"]))

(define (key k)
  (define s (symbol->string k))
  (if (regexp-match-exact? #px"[A-Za-z_][A-Za-z0-9_.-]*" s) s (jsexpr->string s)))
