#lang racket/base
;; Terms and values, in the style of Kovács' elaboration-zoo.
;; Terms are what elaboration produces: variables are de Bruijn indices (0 = the innermost).
;; Values are what evaluation produces: variables are de Bruijn levels (0 = the outermost).
;; A visibility is 'e (explicit) or 'i (implicit).

(provide (all-defined-out))

;; --- terms ---

(struct tvar (index) #:transparent)
(struct tglob (name) #:transparent)
(struct tapp (fn arg vis) #:transparent)
(struct tlam (name vis body) #:transparent)
(struct tpi (name vis dom cod) #:transparent)
(struct ttype (level) #:transparent)
(struct tstr (s) #:transparent)
(struct tnum (n) #:transparent)                  ; a numeral: zero/add1 in the logic, an integer here
(struct tann (term type) #:transparent)          ; (the type term): the annotation does not compute
(struct tmeta (id) #:transparent)                ; a metavariable, as it is
(struct timeta (id) #:transparent)               ; a metavariable applied to the whole context

;; --- values ---

;; A spine is the list of arguments of a stuck head, in application order: (value . visibility).
(struct vlam (name vis env body) #:transparent)
(struct vpi (name vis dom env cod) #:transparent)
(struct vtype (level) #:transparent)
(struct vrigid (level spine) #:transparent)      ; a bound variable, applied
(struct vflex (meta spine) #:transparent)        ; an unsolved metavariable, applied
(struct vglob (name spine) #:transparent)        ; a global that does not reduce (yet)
(struct vstr (s) #:transparent)
(struct vnum (n) #:transparent)

(define (vvar level) (vrigid level '()))

;; --- run-time clauses of a define ---

;; patterns: pvar binds the next slot of the environment, pdot is a forced (inaccessible) pattern.
(struct pvar (slot) #:transparent)
(struct pdot () #:transparent)
(struct pcon (name args) #:transparent)
(struct rt-clause (patterns size body) #:transparent)   ; size: how many variables it binds
