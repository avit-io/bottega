#lang racket/base
;; #lang affresco: the example's proofs reject a wrong program; the runtime turns events into
;; patches; and patches turn the old page into the new one.

(require rackunit racket/file racket/string racket/runtime-path
         bottega/check affresco/run (only-in affresco/dialect languages implicit))

(define-runtime-path root "..")
(define (example f) (build-path root "cookbook" "affresco" f))
(define counter (example "counter.rkt"))
(define timer (example "timer.rkt"))

;; --- the proofs are the tests: each change breaks one ---

(define (rejects-change from to expected #:in [file counter])
  (define src (string-replace (file->string file) from to #:all? #f))
  (check-true (string-contains? src to))
  (define msg (with-handlers ([exn:fail? exn-message])
                (parameterize ([current-languages languages] [current-implicit implicit])
                  (check-string (cadr (regexp-match #px"^#lang affresco\n(.*)$" src))))
                #f))
  (check-true (and msg (string-contains? msg expected) #t) (format "~a → ~a: expected «~a», got ~s" from to expected msg)))

;; inc counts two: dec no longer undoes it
(rejects-change "(inc (model (add1 (Model.count m))" "(inc (model (add1 (add1 (Model.count m)))" "inc-then-dec: refl: expected")
;; reset forgets the name
(rejects-change "(reset (model 0 (Model.name m)))" "(reset (model 0 \"\"))" "reset-keeps-name: refl: expected")
;; the buttons in another order
(rejects-change "(list (button (list (class \"btn\") (on-click dec))" "(list (button (list (class \"btn\") (on-click inc))" "three-buttons: refl: expected")
;; a button that is not round (Ocra)
(rejects-change "(button (list (class \"btn\") (on-click reset))" "(button (list (on-click reset))" "rounded-iff-applies: refl: expected")
;; something round that does nothing (Ocra)
(rejects-change "(span (list (class \"digits\"))" "(span (list (class \"digits btn\"))" "rounded-iff-applies: refl: expected")
;; the field ignores what is typed
(rejects-change "(on-input rename)" "(on-input (λ (s) (rename \"x\")))" "typing-renames: refl: expected")

;; --- the runtime: a click, a keystroke ---

(parameterize ([current-languages languages] [current-implicit implicit])
  (call-with-checked-file counter
    (λ ()
      (define s (new-session))
      (check-equal? (session-page s)
                    '((div ((class "card"))
                           (h1 () "contatore")
                           (input ((value "contatore") (data-on "input")))
                           (div ((class "row"))
                                (button ((class "btn") (data-on "click")) "−")
                                (button ((class "btn") (data-on "click")) "0")
                                (span ((class "digits")) "0")
                                (button ((class "btn") (data-on "click")) "+")))))
      (check-equal? (step! s '(0 2 3) "click" "") '((text (0 2 2 0) "1")))          ; +
      (check-equal? (step! s '(0 2 3) "click" "") '((text (0 2 2 0) "2")))
      (check-equal? (step! s '(0 1) "input" "ciao")                                 ; the field
                    '((text (0 0 0) "ciao") (set (0 1) "value" "ciao")))
      (check-equal? (step! s '(0 2 1) "click" "") '((text (0 2 2 0) "0")))          ; reset
      (check-equal? (step! s '(9 9) "click" "") '()))))                             ; nothing there

;; --- subscriptions: the timer's proofs, and its ticks ---

;; it ticks even when stopped
(rejects-change "(list (every 1000 tick)) (list)" "(list (every 1000 tick)) (list (every 1000 tick))"
                "stopped-is-silent: refl: expected" #:in timer)
;; it ticks twice as often
(rejects-change "(list (every 1000 tick)) (list)" "(list (every 500 tick)) (list)" "running-ticks: refl: expected" #:in timer)

(parameterize ([current-languages languages] [current-implicit implicit])
  (call-with-checked-file timer
    (λ ()
      (define s (new-session))
      (check-equal? (session-every s) '())                                          ; stopped: no timer
      (check-equal? (step! s '(0 1 0) "click" "") '((text (0 1 0 0) "ferma")))     ; parti
      (check-equal? (session-every s) '(1000))
      (check-equal? (fire! s 0) '((text (0 1 1 0) "1")))                           ; a second goes by
      (check-equal? (fire! s 0) '((text (0 1 1 0) "2")))
      (check-equal? (step! s '(0 1 0) "click" "") '((text (0 1 0 0) "parti")))     ; ferma
      (check-equal? (session-every s) '())
      (check-equal? (fire! s 0) '()))))                                             ; a late tick: nothing

;; --- diff, then patch, gives the new page ---

(define pages
  '(()
    ("a")
    ((p () "a") "b")
    ((p ((class "x")) "a" (b () "c")))
    ((p ((class "y") (id "i")) "a" (i () "c")))
    ((div () (p () "a") (p () "b")))
    ((div () (p () "a")))
    ((span () "a") "b")))

(for* ([a pages] [b pages])
  (check-equal? (patch (diff a b) a) b (format "~s → ~s" a b)))

;; --- Ocra: one source for the page and for Tailwind ---

(require affresco/ocra)
(let ([css (ocra-css)] [tw (ocra-tailwind)])
  (define (block from) (car (regexp-match (pregexp (string-append (regexp-quote from) "[^}]*")) css)))
  (for ([c colors])
    (define name (symbol->string (car c)))
    (check-true (string-contains? (block ":root{") (format "--~a:~a" name (cadr c))) name)        ; light
    (check-true (string-contains? (block ":root[data-theme=\"dark\"]{") (format "--~a:~a" name (caddr c))) name)
    (check-true (string-contains? tw (format "--color-~a: var(--~a);" name name)) name))
  (check-true (regexp-match? #px"\\.btn\\{[^}]*border-radius:var\\(--radius-paren\\)" css))       ; apply is round
  (check-true (regexp-match? #px"\\.card\\{[^}]*border-radius:var\\(--radius-0\\)" css)))         ; eval is square
