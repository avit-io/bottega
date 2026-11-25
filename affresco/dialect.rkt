#lang racket/base
;; What #lang affresco adds to bottega: its library, affresco/lib (Html, Program), required by
;; every affresco module. The world comes back only as messages, through update.

(require racket/runtime-path bottega/check bottega/core/state)
(provide languages implicit)

(define-runtime-path lib "lib")
(register-library! lib)

;; The runtime finds the view's nodes and the program by these keys, whatever their names.
(register-builtin-keys! '((html-end . con) (html-text . con) (html-node . con)
                         (attr . con) (on-click . con) (on-input . con) (program . con)))

(define languages '("bottega" "affresco"))
(define implicit '(affresco/lib/html))   ; every program sees Html and Program
