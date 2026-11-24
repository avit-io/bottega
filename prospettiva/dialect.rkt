#lang racket/base
;; What #lang prospettiva adds to bottega: for now, only the right to require prospettiva
;; modules. A model is pure data, and nothing from the world comes back into it.
;; ponytail: no prospettiva/lib yet; register it here (register-library!) when one is needed.

(provide languages)

(define languages '("bottega" "prospettiva"))
