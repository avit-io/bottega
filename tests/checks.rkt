#lang bottega
; Checks: the laws and guarantees of the pure library, as theorems. The module compiles only
; if they all hold: raco test compiles it. Ported from disegno's tests/checks.disegno, without
; the plan theorems (they come back with #lang disegno, phase 4).

(require bottega/lib/paths bottega/lib/time bottega/lib/net)
; --- arithmetic and lists ---


(claim +-assoc (Π ((a Nat) (b Nat) (c Nat)) (≡ (+ (+ a b) c) (+ a (+ b c)))))
(define (+-assoc a b c)
  (match a
    (zero refl)
    ((add1 a) (cong add1 (+-assoc a b c)))))

(claim ++-nil (Π ((xs (List A))) (≡ (++ xs nil) xs)))
(define (++-nil xs)
  (match xs
    (nil       refl)
    ((:: x xs) (cong (:: x) (++-nil xs)))))

(claim ++-assoc (Π ((xs (List A)) (ys (List A)) (zs (List A))) (≡ (++ (++ xs ys) zs) (++ xs (++ ys zs)))))
(define (++-assoc xs ys zs)
  (match xs
    (nil refl)
    ((:: x xs) (cong (:: x) (++-assoc xs ys zs)))))

; --- paths: the affine space ---

(claim ⊑-trans (→ (⊑ p q) (⊑ q r) (⊑ p r)))
(define (⊑-trans n h)
  (match n
    (⊑-refl h)
    ((⊑-step g) (⊑-step (⊑-trans g h)))))

(claim ⊕-here (Π ((p Abs)) (≡ (⊕ p here) p)))
(define (⊕-here p) refl)

(claim path-law (Π ((p Abs) (q Abs) (h (⊑ p q))) (≡ (⊕ q (⊖ p q h)) p)))
(define (path-law p q h) (Affine.law path-affine p q h))

(claim ⊖-back (≡ (⊖ /out/bin/hello /out auto) "bin/hello"))
(define ⊖-back auto)

(claim apart-paths (⋔ /src /go/pkg/mod))
(define apart-paths auto)

; --- literals: the literal stays, the type changes ---

(claim point-literal (≡ /go/pkg/mod (/ (/ (/ root "go") "pkg") "mod")))
(define point-literal auto)

(claim vector-literal (≡ (⊕ /out "bin/hello") /out/bin/hello))
(define vector-literal auto)

; --- negations: what must not hold, as a theorem ---

(claim outside-by-hand (¬ (⊑ /out /src)))
(define (outside-by-hand n)
  (match n
    ((⊑-step ()))))

(claim outside (¬ (⊑ /out /src)))
(define outside auto)


(claim nested-not-disjoint (¬ (⋔ /src /src/cache)))
(define nested-not-disjoint auto)


(claim zero-is-not-one (¬ (≡ 0 1)))
(define (zero-is-not-one n)
  (match n
    (())))

; --- naturals and acceleration: the law holds on big numbers too, computed natively ---

(claim nat-law (≡ (+ 1000000 (∸ 4294967296 1000000)) 4294967296))
(define nat-law auto)

; --- time: instants and durations ---

(claim day-start Instant)
(define day-start "2026-10-15T00:00:00Z")
(claim day-end Instant)
(define day-end "2026-10-16T00:00:00Z")

(claim a-day-later (≡ (⊕ day-start "1d") day-end))
(define a-day-later auto)

(claim day-length (≡ (⊖ day-end day-start auto) "24h"))
(define day-length auto)

(claim apart-windows (Apart (window day-start "1h") (window day-end "1h")))
(define apart-windows auto)

(claim overlapping-windows (¬ (Apart (window day-start "2h") (window (⊕ day-start "1h") "1h"))))
(define overlapping-windows auto)

; --- addresses: refined points and CIDR blocks as regions ---

(claim gateway Ip)
(define gateway "10.0.0.1")

(claim next-address (≡ (⊕ gateway 1 auto) "10.0.0.2"))
(define next-address auto)

(claim in-the-block (So (in? "10.0.0.5" "10.0.0.0/24")))
(define in-the-block auto)

(claim disjoint-subnets (Disjoint-subnets (list "10.0.0.0/24" "10.0.1.0/24" "10.1.0.0/16")))
(define disjoint-subnets auto)

(claim nested-subnets (¬ (Disjoint-subnets (list "10.0.0.0/8" "10.1.0.0/16"))))
(define nested-subnets auto)




; --- strings: the literal is composed, the computation happens on literals ---

(claim rendered (≡ (string-append "port = " (show 8080)) "port = 8080"))
(define rendered auto)

(claim rendered-cidr (≡ (render-cidr "10.0.1.0/24") "10.0.1.0/24"))
(define rendered-cidr auto)

; --- the pure language: λ, the, implicits, Σ, Either, transport, ex falso ---

(claim id (∀ ((A Type)) (→ A A)))
(define id (λ (x) x))

(claim applied (≡ ((the (→ Nat Nat) (λ (n) (add1 n))) 1) 2))
(define applied auto)

(claim a-witness (Σ Nat (λ (n) (≡ (+ n n) 4))))
(define a-witness (pair 2 refl))

(claim the-witness (≡ (fst a-witness) 2))
(define the-witness auto)

(claim ex-falso (→ ⊥ A))
(define (ex-falso n)
  (match n
    (())))

(claim move (→ (≡ x y) (≡ y y) (≡ x y)))
(define (move e p) (transport (sym e) (λ (m) (≡ m y)) p))

(claim names-as-variables (→ Nat (→ Nat Nat) Nat))   ; step and port are constructors elsewhere
(define (names-as-variables port n)
  (match n
    (step (step port))))

(claim expected-in-arguments (→ Bool (Σ Nat (λ (n) (≡ n n)))))   ; the expected type goes down into if
(define (expected-in-arguments b) (if b (pair 1 refl) (pair 2 refl)))
