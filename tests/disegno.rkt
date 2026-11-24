#lang racket/base
;; #lang disegno: the cookbook's dry plans, the same GraphQL as the Chez disegno's (tests/golden),
;; and the programs that must be rejected.

(require rackunit racket/file racket/string racket/runtime-path
         bottega/check disegno/run (only-in disegno/dialect languages))

(define-runtime-path root "..")
(define-runtime-path golden "golden")

;; Absolute paths become relative to the repository; the nonce of fresh commands is normalized.
(define (normalize s)
  (regexp-replace* #rx"<run>-[0-9]+"
                   (string-replace s (path->string (simplify-path (path->complete-path root))) "")
                   "<run>-N"))

(for ([g (directory-list golden)])
  (define recipe (build-path root "cookbook" (path-replace-extension g #".rkt")))
  (check-equal? (normalize (plan-text recipe)) (file->string (build-path golden g)) (path->string g)))

;; every recipe and chapter 9 check as #lang disegno
(for ([f (append (find-files (λ (p) (regexp-match? #rx"[0-9][^/]*[.]rkt$" (path->string p))) (build-path root "cookbook"))
                 (list (build-path root "book" "09-proofs-as-programs.rkt")
                       (build-path root "book" "solutions" "09-proofs-as-programs.rkt")))])
  (check-not-exn (λ () (parameterize ([current-languages languages]) (check-file f))) (path->string f)))

(define (rejects src expected)
  (define msg (with-handlers ([exn:fail? exn-message])
                (parameterize ([current-languages languages]) (check-string (string-append "(require disegno/lib/dagger)\n" src)))
                #f))
  (check-true (and msg (string-contains? msg expected) #t) (format "~a\n  expected «~a», got ~s" src expected msg)))

(rejects "(claim bad Image)\n(define bad \"Golang:1.25\")" "«Golang:1.25» is not a valid Image")
(rejects "(claim points Abs)\n(define points (⊕ /out /bin))" "expected")
(rejects "(claim wrong-exec Container)\n(define wrong-exec (with-exec empty (list \"x\")))" "with-exec does not exist for")
(rejects "(claim must-pin (All pinned? (images (from (container) \"alpine\"))))\n(define must-pin auto)"
         "auto: no proof of (Holds pinned? \"alpine\")")
(rejects "(claim theft Dir)\n(define theft (%host-dir \"/etc\"))" "reserved to the core")
