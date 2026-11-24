#lang racket/base
;; The three moments of a disegno program, Zuccari's: the inner design (check: types and
;; theorems), the outer design (plan: the GraphQL, one per outcome of the branches), the work
;; (run: the execution on Dagger). A program's main submodule calls main-program.
;;   racket prog.rkt [check|plan|run]    default: run under a Dagger session, plan otherwise

(require racket/match racket/list racket/path racket/port racket/string
         bottega/check bottega/core/state "dialect.rkt" "plan.rkt" "graphql.rkt" "session.rkt")
(provide main-program plan-text)

(define (main-program file)
  (define args (vector->list (current-command-line-arguments)))
  (define mode (match args
                 ['() (if (session?) 'run 'plan)]
                 [(list (and m (or "check" "plan" "run"))) (string->symbol m)]
                 [_ (eprintf "usage: racket program.rkt [check|plan|run]\n") (exit 2)]))
  (with-handlers ([exn:fail:bottega? (λ (e) (eprintf "~a\n" (exn-message e)) (exit 1))]
                  [exn:engine? (λ (e) (eprintf "~a\n" (exn-message e)) (exit 1))])
    (parameterize ([current-languages languages]
                   [program-dir (path-only (simplify-path (path->complete-path file)))])
      (call-with-checked-file file
        (λ ()
          (case mode
            [(check) (displayln "checked: types and theorems")]
            [(plan) (print-plans (all-plans 'main))]
            [(run) (run-plan 'main)]))))))

;; For each outcome of the branches: the observations taken, and the outputs in order.
(define (print-plans plans)
  (for ([p plans] [i (in-naturals)])
    (when (> i 0) (newline))
    (for ([ev (cdr p)])
      (match ev
        [(list 'if ch b) (printf "# if ~a → ~a\n" (chain->graphql ch) (if b "true" "false"))]
        [(list 'otherwise) (displayln "# if something before fails:")]
        [(list 'fails msg) (printf "# fails: ~a\n" msg)]
        [(list 'output ch) (displayln (chain->graphql ch))]))))

;; Live: objects resolve to IDs, observations to booleans (an exit code is true if 0), and every
;; output runs when it is reached.
(define (resolve-live ch) (query (format "{ ~a }" (chain->graphql (append ch '((id)))))))

(define (observe-live ch)
  (define b (match (timed (command ch) (λ () (query (format "{ ~a }" (chain->graphql ch)))))
              [(? boolean? b) b]
              [(? number? n) (= n 0)]
              [r (oops "observation not boolean: ~s" r)]))
  (printf "↳ ~a → ~a\n" (command ch) (if b "yes" "no"))
  b)

;; The report: one line per output, with its time; a failing step shows the end of its stderr.
;; ponytail: lines say the command, not the definition's docstring; Chez tracked which definition
;; each step came from (and streamed the OTel logs). Add both when CI needs grouped output.
(define (run-plan entry)
  (parameterize ([observe observe-live]
                 [realize-dry #f]
                 [read-secret (λ (ch) (timed "reading a secret" (λ () (query (format "{ ~a }" (chain->graphql ch))))))]
                 [run-id (number->string (current-milliseconds))])
    (realize (entry-value entry) resolve-live
             (λ (ch)
               (define t (timed (outcome ch) (λ () (query (format "{ ~a }" (chain->graphql ch))))
                                #:seconds #t))
               (printf "✓ ~a  ~as\n" (outcome ch) t)))))

;; Runs thunk; on an engine error prints ✗ what, the exit code and the last lines of stderr.
;; → the result, or with #:seconds the time it took.
(define (timed what thunk #:seconds [seconds? #f])
  (define t0 (current-inexact-milliseconds))
  (define r (with-handlers ([exn:engine? (λ (e) (report-failure what e) (raise e))]) (thunk)))
  (if seconds? (/ (round (/ (- (current-inexact-milliseconds) t0) 100)) 10.) r))

(define (report-failure what e)
  (define x (exn:engine-extensions e))
  (printf "✗ ~a~a\n" what (let ([c (hash-ref x 'exitCode #f)]) (if c (format "  exit ~a" c) "")))
  (define lines (string-split (hash-ref x 'stderr "") "\n"))
  (for ([l (take-right lines (min 20 (length lines)))]) (printf "  │ ~a\n" l)))

(define (command ch)                              ; the last command of a chain
  (match (filter (λ (f) (eq? (car f) 'withExec)) ch)
    ['() (chain->graphql (list (last ch)))]
    [es (string-join (map (λ (a) (if (string? a) a (format "~a" a))) (cadr (assq 'args (cdr (last es))))) " ")]))

(define (outcome ch)                              ; what an output did, in words
  (match (last ch)
    [(list 'export (list 'path p) _) (format "exported ~a" p)]
    [(list 'publish (list 'address a)) (format "published ~a" a)]
    [_ (command ch)]))

;; The dry plan of a program, as text: what `racket prog.rkt plan` prints.
(define (plan-text file)
  (parameterize ([current-languages languages]
                 [program-dir (path-only (simplify-path (path->complete-path file)))])
    (call-with-checked-file file (λ () (with-output-to-string (λ () (print-plans (all-plans 'main))))))))
