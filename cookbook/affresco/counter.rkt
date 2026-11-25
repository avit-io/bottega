#lang affresco
; Un contatore con un nome: l'architettura di Elm, totale, sul server.
;
; Il programma è tre cose pure: lo stato iniziale, come un messaggio cambia lo stato, e come lo
; stato si vede. Il runtime tiene lo stato, ricalcola la vista a ogni evento e manda al browser
; solo le differenze. Le prove in fondo sono i test: si scrivono prima, e il programma è
; giusto quando compila.
;
;   racket cookbook/affresco/counter.rkt show        l'HTML della vista iniziale
;   racket cookbook/affresco/counter.rkt serve 8080  il programma, su http://localhost:8080

(require bottega/lib/text)

(record Model
  (count Nat)
  (name String))

(data Msg : Type
  (inc : Msg)
  (dec : Msg)
  (reset : Msg)
  (rename : (→ String Msg)))

; --- il programma ---

(claim init Model)
(define init (model 0 "contatore"))

(claim update (→ Msg Model Model))
(define (update msg m)
  (match msg
    (inc (model (add1 (Model.count m)) (Model.name m)))
    (dec (model (match (Model.count m) (zero 0) ((add1 k) k)) (Model.name m)))
    (reset (model 0 (Model.name m)))
    ((rename s) (model (Model.count m) s))))

(claim view (→ Model (Html Msg)))
(define (view m)
  (div (list (class "card"))
       (list (h1 (list) (list (text (Model.name m))))
             (input (list (value (Model.name m)) (on-input rename)))
             (div (list (class "row"))
                  (list (button (list (on-click dec)) (list (text "−")))
                        (button (list (on-click reset)) (list (text "0")))
                        (span (list (class "digits")) (list (text (show (Model.count m)))))
                        (button (list (on-click inc)) (list (text "+"))))))))

(claim main (Program Model Msg))
(define main (program init update view silent))

; --- le prove: che cosa il programma deve fare ---

; dec annulla inc
(claim inc-then-dec (Π ((m Model)) (≡ (Model.count (update dec (update inc m))) (Model.count m))))
(define (inc-then-dec m) refl)

; sotto zero non si va
(claim dec-at-zero (Π ((s String)) (≡ (Model.count (update dec (model 0 s))) 0)))
(define (dec-at-zero s) refl)

; reset riporta a zero, e non tocca il nome
(claim reset-zero (Π ((m Model)) (≡ (Model.count (update reset m)) 0)))
(define (reset-zero m) refl)

(claim reset-keeps-name (Π ((m Model)) (≡ (Model.name (update reset m)) (Model.name m))))
(define (reset-keeps-name m) refl)

; rinominare non tocca il conto
(claim rename-keeps-count (Π ((s String) (m Model)) (≡ (Model.count (update (rename s) m)) (Model.count m))))
(define (rename-keeps-count s m) refl)

; la vista offre tre bottoni, in quest'ordine, in ogni stato
(claim three-buttons (Π ((m Model)) (≡ (clicks (view m)) (list dec reset inc))))
(define (three-buttons m) refl)

; scrivere nel campo rinomina
(claim typing-renames (Π ((m Model) (s String)) (≡ (typed (view m) s) (list (rename s)))))
(define (typing-renames m s) refl)
