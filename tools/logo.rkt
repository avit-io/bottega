#lang racket/base
;; The logo: the first two lessons of a Renaissance workshop. A sphere in chiaroscuro with its cast
;; shadow (disegno), on a tiled floor in one-point perspective (prospettiva); drawn by stippling,
;; black dots on white, denser where it is darker.
;; racket tools/logo.rkt > logo.svg   (deterministic: the same seed, the same dots)

(require racket/math racket/list)

(define size 400)
(define cx 200) (define cy 175) (define r 120)              ; the sphere
(define light (let ([v '(-0.55 -0.65 0.52)]) (let ([n (sqrt (apply + (map sqr v)))]) (map (λ (x) (/ x n)) v))))

;; darkness ∈ [0, 1] at a point of the canvas
(define (darkness x y)
  (define dx (/ (- x cx) r)) (define dy (/ (- y cy) r))
  (define d2 (+ (sqr dx) (sqr dy)))
  (cond
    [(< d2 1)                                                ; on the sphere: Lambert, plus reflected light at the rim
     (define nz (sqrt (- 1 d2)))
     (define lit (max 0 (+ (* dx (car light)) (* dy (cadr light)) (* nz (caddr light)))))
     (define rim (* 0.25 (expt (sqrt d2) 6) (max 0 (+ dx dy))))
     (max 0 (min 1 (- (* 0.95 (- 1 lit)) rim)))]
    [else                                                    ; the cast shadow, an ellipse on the ground, soft, over the floor
     (define sx (/ (- x (+ cx 45)) (* 1.15 r))) (define sy (/ (- y (+ cy r -8)) (* 0.28 r)))
     (define s (+ (sqr sx) (sqr sy)))
     (max (if (< s 1) (* 0.95 (expt (- 1 s) 0.6)) 0) (floor-darkness x y))]))

;; The floor: a pavement of square tiles in one-point perspective, Alberti's costruzione
;; legittima. Every line of tiles goes to the vanishing point on the horizon; far away the tiles
;; fade, before they become smaller than the dots.
(define horizon 225) (define eye 60) (define tile 20) (define half-width 3)
(define (floor-darkness x y)
  (cond
    [(<= y horizon) 0]
    [else
     (define depth (/ (* eye 400) (- y horizon)))        ; the distance of the floor at this row
     (define across (/ (* (- x cx) depth) 400))           ; and the position across it
     (define dark? (odd? (+ (exact-floor (/ across tile)) (exact-floor (/ depth tile)))))
     (define fade (min 1 (/ (- y horizon) 45)))
     (if (< (abs across) (* half-width tile))
         (* fade (if dark? 0.8 0.04))
         0)]))

;; Stippling: candidates in random order, kept with probability darkness, never too close.
(define (stipple n)
  (random-seed 1568)                                         ; Vasari's Vite, second edition
  (define cell 11)
  (define grid (make-hash))
  (define (near? x y spacing)          ; darker, closer: from 1.8 to 10.8 units apart
    (for*/or ([i (in-range -1 2)] [j (in-range -1 2)])
      (for/or ([p (hash-ref grid (cons (+ (quotient (exact-floor x) cell) i) (+ (quotient (exact-floor y) cell) j)) '())])
        (< (+ (sqr (- x (car p))) (sqr (- y (cdr p)))) (sqr spacing)))))
  (for/fold ([dots '()]) ([_ n])
    (define x (* size (random))) (define y (* size (random)))
    (define d (darkness x y))
    (cond [(and (< (random) (expt d 0.8)) (not (near? x y (+ 1.8 (* 9 (expt (- 1 d) 1.5))))))
           (hash-update! grid (cons (quotient (exact-floor x) cell) (quotient (exact-floor y) cell)) (λ (l) (cons (cons x y) l)) '())
           (cons (cons x y) dots)]
          [else dots])))

(define dots (stipple 400000))
(printf "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 ~a ~a\" width=\"~a\" height=\"~a\">\n" size size size size)
(printf "<rect width=\"100%\" height=\"100%\" fill=\"#fff\"/>\n<g fill=\"#111\">\n")
(for ([p (reverse dots)])
  (printf "<circle cx=\"~a\" cy=\"~a\" r=\"1.3\"/>\n" (/ (round (* 10 (car p))) 10) (/ (round (* 10 (cdr p))) 10)))
(printf "</g>\n</svg>\n")
