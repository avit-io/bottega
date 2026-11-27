#lang racket/base
;; Pure primitives and literal readers. They compute on literals only (strings and numbers):
;; nothing here observes the world. Trusted, like the kernel.

(require racket/string racket/list "state.rkt")
(provide primitives literal-readers segment? nat->ip)

;; --- primitives: name → (arity procedure); a procedure returns a string, a number or a boolean ---

(define primitives
  (hasheq '%concat    (list 2 string-append)
          '%show      (list 1 number->string)
          '%show-ip   (list 1 (λ (n) (nat->ip n)))
          '%contains? (list 2 string-contains?)
          '%distinct? (list 2 (λ (a b) (not (string=? a b))))
          '%re?       (list 2 (λ (pattern s) (re? pattern s)))
          '%image-ref? (list 1 (λ (s) (image-ref? s)))
          '%segment?  (list 1 (λ (s) (segment? s)))))

;; A regular expression matched against the whole string. A malformed pattern is a type error.
(define (re? pattern s)
  (define rx (with-handlers ([exn:fail? (λ (_) (oops "re?: malformed regular expression: ~s" pattern))])
               (pregexp (string-append "^(?:" pattern ")$"))))  ; anchored: the whole of s, with backtracking
  (regexp-match? rx s))

;; A path step: not empty, no /, neither . nor ..
(define (segment? s)
  (and (positive? (string-length s))
       (not (string-contains? s "/"))
       (not (member s '("." "..")))))

;; An image reference: [registry/]name[/name…][:tag][@sha256:<64 hex digits>]
;; ponytail: a simplified distribution/reference grammar; no digests other than sha256.
(define (image-ref? s)
  (define (component? x) (regexp-match-exact? #px"[a-z0-9]([a-z0-9._-]*[a-z0-9])?" x))
  (define (registry? x) (or (equal? x "localhost")
                            (and (regexp-match? #rx"[.:]" x) (regexp-match-exact? #px"[A-Za-z0-9.:-]+" x))))
  (define (tag? x) (regexp-match-exact? #px"[A-Za-z0-9_][A-Za-z0-9_.-]{0,127}" x))
  (define (digest? x) (regexp-match-exact? #px"sha256:[0-9a-f]{64}" x))
  (define at (string-split s "@" #:trim? #f))
  (and (<= 1 (length at) 2)
       (or (null? (cdr at)) (digest? (cadr at)))
       (let* ([parts (string-split (car at) "/" #:trim? #f)]
              [name+tag (string-split (last parts) ":" #:trim? #f)]
              [path (drop-right parts 1)]
              [path (if (and (pair? path) (registry? (car path))) (cdr path) path)])
         (and (<= 1 (length name+tag) 2)
              (or (null? (cdr name+tag)) (tag? (cadr name+tag)))
              (component? (car name+tag))
              (andmap component? path)))))

;; --- literal readers: string → the fields of the literal's constructor, or #f if malformed ---
;; The constructor is the only one of the type bound to the key; the proofs are computed by auto.

(define (natural s) (and (regexp-match-exact? #px"[0-9]+" s) (string->number s)))

;; "90s", "5m", "1h30m", "2d" → (seconds)
(define (read-duration s)
  (define unit-seconds (hash "d" 86400 "h" 3600 "m" 60 "s" 1))
  (and (regexp-match-exact? #px"([0-9]+[dhms])+" s)
       (list (for/sum ([m (regexp-match* #px"([0-9]+)([dhms])" s #:match-select cdr)])
               (* (string->number (car m)) (hash-ref unit-seconds (cadr m)))))))

;; Days since 1970-01-01 (H. Hinnant's algorithm).
(define (days-from-civil y m d)
  (let* ([y (if (<= m 2) (- y 1) y)]
         [era (floor (/ y 400))]
         [yoe (- y (* era 400))]
         [doy (+ (quotient (+ (* 153 (+ m (if (> m 2) -3 9))) 2) 5) (- d 1))]
         [doe (+ (* yoe 365) (quotient yoe 4) (- (quotient yoe 100)) doy)])
    (+ (* era 146097) doe -719468)))

;; "2026-10-15" or "2026-10-15T10:00:00Z" → (seconds since the epoch)
(define (read-instant s)
  (define m (or (regexp-match #px"^([0-9]{4})-([0-9]{2})-([0-9]{2})$" s)
                (regexp-match #px"^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})Z$" s)))
  (and m
       (let-values ([(y mo d h mi se) (apply values (map string->number
                                                         (append (cdr m) (if (= (length m) 4) '("0" "0" "0") '()))))])
         (and (<= 1 mo 12) (<= 1 d 31) (< h 24) (< mi 60) (< se 60) (>= y 1970)
              (list (+ (* 86400 (days-from-civil y mo d)) (* 3600 h) (* 60 mi) se))))))

;; "10.0.0.5" → 167772165
(define (ip->nat s)
  (define octets (map natural (string-split s "." #:trim? #f)))
  (and (= (length octets) 4)
       (andmap (λ (o) (and o (<= o 255))) octets)
       (for/fold ([acc 0]) ([o octets]) (+ (* acc 256) o))))

(define (nat->ip n)
  (format "~a.~a.~a.~a" (quotient n 16777216) (remainder (quotient n 65536) 256)
          (remainder (quotient n 256) 256) (remainder n 256)))

(define (read-ip s) (let ([n (ip->nat s)]) (and n (list n))))

;; "10.0.0.0/8": the block is (prefix length), the prefix counted in blocks of its size.
(define (read-cidr s)
  (define parts (string-split s "/" #:trim? #f))
  (and (= (length parts) 2)
       (let ([base (ip->nat (car parts))] [len (natural (cadr parts))])
         (and base len (<= len 32)
              (let ([size (expt 2 (- 32 len))])
                (and (zero? (remainder base size)) (list (quotient base size) len)))))))

;; builtin key → reader
(define literal-readers
  (hasheq 'duration read-duration 'instant read-instant 'ip read-ip 'cidr read-cidr))

(module+ test
  (require rackunit)
  (check-true (image-ref? "golang:1.25-alpine"))
  (check-true (image-ref? "localhost:5000/team/app:1.0"))
  (check-false (image-ref? "Golang:1.25"))
  (check-equal? (read-duration "1h30m") '(5400))
  (check-false (read-duration "5x"))
  (check-equal? (read-instant "1970-01-02") '(86400))
  (check-false (read-instant "2026-13-01"))
  (check-equal? (read-ip "10.0.0.5") '(167772165))
  (check-false (read-ip "10.0.0.256"))
  (check-equal? (read-cidr "10.0.1.0/24") '(655361 24))   ; the prefix counts blocks of 256
  (check-false (read-cidr "10.0.1.1/24"))
  (check-equal? (nat->ip 167772165) "10.0.0.5")
  (check-true (re? "[a-z]+(-[a-z]+)*" "eu-west-x"))
  (check-false (re? "[a-z]+" "abc1")))                 ; the whole string must match
