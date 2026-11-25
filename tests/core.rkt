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

;; --- the book: every chapter and solution is a #lang bottega module ---
;; ponytail: chapter 9 requires the Dagger library; it comes back with #lang disegno (phase 4).

(define chapters
  (for/list ([f (find-files (λ (p) (regexp-match? #rx"/[0-9][^/]*[.]rkt$" (path->string p))) book)]
             #:unless (regexp-match? #rx"/09-" (path->string f)))
    f))
(check-equal? (length chapters) 16)
(for ([f chapters]) (check-not-exn (λ () (check-file f)) (path->string f)))

;; hover: every declaration's type, for the editor
(check-not-false (member "successore : (→ Nat Nat)"
                         (map (λ (t) (vector-ref t 3)) (check-file (build-path book "03-functions.rkt")))))

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
(rejects "(require bottega/lib/text)\n(claim different (≡ \"a\" \"b\"))\n(define different refl)" "expected")
(rejects "(require bottega/lib/text)\n(claim mixed (List String))\n(define mixed (list \"a\" 1))" "expected")
(rejects "(claim needs-text Nat)\n(define needs-text (show 1))" "unknown name: show")
(rejects (string-append "(data Vec : (→ Type Nat Type) (vnil : (Vec A 0)) (vcons : (→ A (Vec A n) (Vec A (add1 n)))))\n"
                        "(claim head-of-empty (→ (Vec A 0) A))\n(define (head-of-empty vnil) vnil)")
         "expected")
(accepts (string-append "(data Vec : (→ Type Nat Type) (vnil : (Vec A 0)) (vcons : (→ A (Vec A n) (Vec A (add1 n)))))\n"
                        "(claim head (→ (Vec A (add1 n)) A))\n(define (head v) (match v ((vcons x _) x)))"))
(rejects "(claim two Nat)\n(define two 1 2)" "define")
(rejects "(claim pipe ⊥)\n(define pipe pipe)" "reserved word")

;; --- modules and the standard library's privileges ---

(rejects "(require bottega/lib/nowhere)" "module bottega/lib/nowhere not found")
(rejects "(require racket/list)" "only modules in #lang bottega")
(rejects "(import text)" "import is gone: (require bottega/lib/text)")

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

;; an enumeration gets D.name, the constructor's name, where String exists
(accepts "(require bottega/lib/text)\n(data Env (dev) qa)
(claim qa-name (≡ (Env.name qa) \"qa\"))\n(define qa-name refl)")

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

;; --- namespaces: a module's names are its own ---
;; It sees what it requires (and what those require); its own definitions hide the imported
;; ones; a name two imports define differently is ambiguous where it is used.

(define (modules files main)                     ; files: ((name . source) …) → main's error, or #f
  (define dir (make-temporary-directory))
  (for ([f files]) (call-with-output-file (build-path dir (car f)) (λ (o) (write-string (cdr f) o))))
  (call-with-output-file (build-path dir "main.rkt") (λ (o) (write-string main o)))
  (begin0 (with-handlers ([exn:fail? exn-message]) (check-file (build-path dir "main.rkt")) #f)
          (delete-directory/files dir)))

(define account-lib
  (cons "account.rkt" "#lang bottega\n(record Account (id Nat))\n(claim one Account)\n(define one (account 1))\n(data Color (red))\n"))

;; account is the library's constructor; here it is a number, and the library still works
(check-false (modules (list account-lib) "#lang bottega\n(require \"account.rkt\")
(claim account Nat)\n(define account 7)
(claim seven (≡ account 7))\n(define seven refl)
(claim still-one (≡ (Account.id one) 1))\n(define still-one refl)\n"))
;; a hidden type is printed by its name
(let ([m (modules (list account-lib) "#lang bottega\n(require \"account.rkt\")
(data Color (blue))\n(claim c Color)\n(define c 5)\n")])
  (check-true (and m (string-contains? m "expected Color, found Nat") #t) m))
;; still one module, one name
(check-true (string-contains? (modules (list account-lib) "#lang bottega\n(claim x Nat)\n(define x 1)\n(claim x Nat)\n(define x 2)\n")
                              "x is already defined"))
;; two libraries, the same name: fine until it is used
(define other-lib (cons "other.rkt" "#lang bottega\n(claim one Nat)\n(define one 1)\n"))
(check-false (modules (list account-lib other-lib) "#lang bottega\n(require \"account.rkt\" \"other.rkt\")\n(claim two Nat)\n(define two 2)\n"))
(let ([m (modules (list account-lib other-lib) "#lang bottega\n(require \"account.rkt\" \"other.rkt\")\n(claim two Nat)\n(define two (+ one one))\n")])
  (check-true (and m (string-contains? m "one is ambiguous: account.rkt, other.rkt") #t) m))
;; what is not required is not seen
(let ([m (modules (list account-lib) "#lang bottega\n(claim o Account)\n(define o (account 1))\n")])
  (check-true (and m (string-contains? m "unknown name: Account") #t) m))
