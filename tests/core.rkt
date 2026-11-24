#lang racket/base
;; The core: what it accepts, what it rejects and why. Each program starts from the prelude.

(require rackunit racket/string racket/list racket/file racket/runtime-path
         "../bottega/check.rkt" "../bottega/core/state.rkt" "../bottega/core/term.rkt" "../bottega/core/eval.rkt")

(define-runtime-path book "../book")

(define (rejects src expected)
  (define msg (with-handlers ([exn:fail? exn-message]) (check-string src) #f))
  (check-true (and msg (string-contains? msg expected) #t)
              (format "~a\n  expected an error with «~a», got ~s" src expected msg)))

(define (accepts src) (check-not-exn (λ () (check-string src)) src))

;; --- the book: chapters and solutions that need only the prelude ---
;; ponytail: 05, 07 and 09 import text and dagger; they come back with #lang bottega (phase 3).

(for ([f (find-files (λ (p) (regexp-match? #rx"[.]bottega$" p)) book)]
      #:unless (regexp-match? #rx"/0[579]-" (path->string f)))
  (check-not-exn (λ () (check-file f)) (path->string f)))

;; --- acceleration is trusted: on small numbers it agrees with the unary clauses ---

(call-with-prelude
 (λ ()
   (for* ([(key op) fast-operations] [a 7] [b 7])
     (define t (for/fold ([t (tglob (builtin key))]) ([x (take (list a b) (car op))]) (tapp t (tnum x) 'e)))
     (check-equal? (reify 0 (evaluate '() t))
                   (parameterize ([accelerate #f]) (reify 0 (evaluate '() t)))
                   (format "~a on ~a ~a" key a b)))))

;; --- totality ---

(rejects "(claim absurd ⊥)\n(define absurd absurd)" "recursion is not structural")
(rejects "(claim spin (→ Nat Nat))\n(define (spin n) (spin n))" "recursion is not structural")
(rejects "(claim postulate ⊥)" "claim without define")
(rejects "(claim pred (→ Nat Nat))\n(define (pred n)\n  (match n\n    ((add1 k) k)))" "missing case")
(rejects "(claim pick (→ (Either A B) (→ A C) (→ B C) C))\n(define (pick n f _)\n  (match n\n    ((left a) (f a))))"
         "missing case (pick (right _) _ _)")

;; --- data: positivity and universes ---

(rejects "(data Bad (mk (→ Bad ⊥)))" "strictly positively")
(rejects "(data Bad2 (mk2 (List (→ Bad2 ⊥))))" "strictly positively")
(accepts "(data Rose (node (List Rose)))")               ; List is strictly positive in its parameter
(rejects "(data U : Type (u : (→ Type U)))" "level")
(rejects "(claim type-in-type Type)\n(define type-in-type Type)" "expected")

;; --- types ---

(rejects "(claim wrong (≡ 1 2))\n(define wrong refl)" "expected")
(rejects "(claim wrong-auto (≡ (+ 1 1) 3))\n(define wrong-auto auto)" "expected")
;; ponytail: the String tests ("a" ≢ "b", a mixed list) come back with (require bottega/text), phase 3.
(rejects "(claim mixed (List Nat))\n(define mixed (list 1 Nat))" "expected")
(rejects (string-append "(data Vec : (→ Type Nat Type) (vnil : (Vec A 0)) (vcons : (→ A (Vec A n) (Vec A (add1 n)))))\n"
                        "(claim head-of-empty (→ (Vec A 0) A))\n(define (head-of-empty vnil) vnil)")
         "expected")
(accepts (string-append "(data Vec : (→ Type Nat Type) (vnil : (Vec A 0)) (vcons : (→ A (Vec A n) (Vec A (add1 n)))))\n"
                        "(claim head (→ (Vec A (add1 n)) A))\n(define (head v) (match v ((vcons x _) x)))"))
(rejects "(claim two Nat)\n(define two 1 2)" "define")
(rejects "(claim pipe ⊥)\n(define pipe pipe)" "reserved word")

;; --- the standard library's privileges ---

(rejects "(extern lie : ⊥ nope)" "reserved to the standard library")
(rejects "(claim theft Nat)\n(define theft %secret)" "reserved to the core")

;; --- TODO, as in Pie: every hole with its context and goal; the define is rejected ---

(let ([m (with-handlers ([exn:fail? exn-message])
           (check-string "(claim plus-zero (Π ((n Nat)) (≡ (+ n 0) n)))
(define (plus-zero n)
  (match n
    (zero TODO)
    ((add1 k) (cong add1 TODO))))")
           #f)])
  (check-true (and m (string-contains? m "TODO\n  ──────────\n  (≡ 0 0)")) m)
  (check-true (and m (string-contains? m "<string>:5:25: TODO\n  k : Nat\n  ──────────\n  (≡ (+ k 0) k)")) m))
(rejects "(claim t Type)\n(define t (the TODO Nat))" "is not known here")

;; --- match on a computed value: an auxiliary define, closed over the context ---

(define mx-neg "(claim mx-neg (→ Bool Bool))\n(define (mx-neg b) (match b (true false) (false true)))\n")
(accepts (string-append mx-neg "(claim mx-zero? (→ Nat Bool Bool))
(define (mx-zero? n b)
  (let ((m (mx-neg b)))
    (match* (n m)
      ((zero true) true)
      ((_ _) false))))
(claim mx-proof (≡ (mx-zero? 0 false) true))
(define mx-proof refl)"))
(rejects (string-append mx-neg "(claim mx-missing (→ Bool Bool))\n(define (mx-missing b) (match (mx-neg b) (true true)))")
         "missing case")
(rejects (string-append mx-neg "(claim mx-spin (→ Nat Nat))\n(define (mx-spin n) (match (mx-neg true) (true (mx-spin n)) (false 0)))")
         "cannot call mx-spin")
(rejects "(claim mx-free Type)\n(define mx-free (≡ (match true (true false) (false true)) false))" "annotate it with the")

;; --- absurd patterns: only where no constructor is possible ---

(rejects "(claim fake-absurd (¬ (≡ 0 0)))\n(define (fake-absurd n)\n  (match n\n    (())))" "() is not absurd: refl is possible")
(rejects "(claim direct-false ⊥)\n(define (direct-false n)\n  (match n\n    (())))" "too many patterns")
(accepts "(claim one-is-not-zero (¬ (≡ 1 0)))\n(define one-is-not-zero auto)")

;; --- records: projections, and invariants checked by auto ---

(define service "(record Service (port Nat) (replicas Nat) (ok (≥ replicas 2)))\n")
(accepts (string-append service "(claim api Service)\n(define api (service 8080 3 auto))
(claim api-replicas (≡ (Service.replicas api) 3))\n(define api-replicas refl)"))
(rejects (string-append service "(claim api Service)\n(define api (service 8080 1 auto))")
         "auto: the proposition reduces to ⊥")
;; a parameterized record, with fields depending on earlier ones
(accepts "(record (Box A) (value A) (same (≡ value value)))
(claim b (Box Nat))\n(define b (box 3 refl))
(claim b-value (≡ (Box.value b) 3))\n(define b-value refl)")
