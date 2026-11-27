#lang affresco
; Un cronometro: il mondo che manda messaggi da sé.
;
; Oltre a init, update e view, un programma dice a che cosa è iscritto, in ogni stato: qui al
; tempo, un tick al secondo, ma solo mentre corre. Il runtime accende e spegne i timer quando
; le iscrizioni cambiano, e manda al browser le differenze anche senza un clic.
;
;   racket cookbook/affresco/timer.rkt serve 8080   il cronometro, su http://localhost:8080

(require bottega/lib/text)

(record Model
  (seconds Nat)
  (running Bool))

(data Msg : Type
  (tick : Msg)
  (toggle : Msg)
  (reset : Msg))

; --- il programma ---

(claim init Model)
(define init (model 0 false))

(claim update (→ Msg Model Model))
(define (update msg m)
  (match msg
    (tick (model (add1 (Model.seconds m)) (Model.running m)))
    (toggle (model (Model.seconds m) (not (Model.running m))))
    (reset (model 0 (Model.running m)))))

(claim view (→ Model (Html Msg)))
(define (view m)
  (div (list (class "card"))
       (list (h1 (list) (list (text "cronometro")))
             (div (list (class "row"))
                  (list (button (list (class "btn") (on-click toggle)) (list (text (if (Model.running m) "ferma" "parti"))))
                        (span (list (class "digits")) (list (text (show (Model.seconds m)))))
                        (button (list (class "btn") (on-click reset)) (list (text "0"))))))))

(claim subs (→ Model (List (Sub Msg))))
(define (subs m) (if (Model.running m) (list (every 1000 tick)) (list)))

(claim main (Program Model Msg))
(define main (program init update view subs))

; --- le prove ---

; fermo, il tempo non lo raggiunge
(claim stopped-is-silent (Π ((s Nat)) (≡ (subs (model s false)) (list))))
(define (stopped-is-silent s) refl)

; mentre corre, un tick al secondo
(claim running-ticks (Π ((s Nat)) (≡ (subs (model s true)) (list (every 1000 tick)))))
(define (running-ticks s) refl)

; un tick è un secondo in più
(claim tick-counts (Π ((m Model)) (≡ (Model.seconds (update tick m)) (add1 (Model.seconds m)))))
(define (tick-counts m) refl)

; parte fermo
(claim starts-stopped (≡ (subs init) (list)))
(define starts-stopped refl)

; reset azzera, e non ferma né fa partire
(claim reset-keeps-running (Π ((m Model)) (≡ (Model.running (update reset m)) (Model.running m))))
(define (reset-keeps-running m) refl)

; due bottoni, in ogni stato
(claim two-buttons (Π ((m Model)) (≡ (clicks (view m)) (list toggle reset))))
(define (two-buttons m) refl)

; ciò che si applica è tondo, e solo quello (Ocra)
(claim rounded-iff-applies (Π ((m Model)) (≡ (ocra-shapes (view m)) true)))
(define (rounded-iff-applies m) refl)
