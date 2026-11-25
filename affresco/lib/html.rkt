#lang affresco
; L'HTML di affresco, e il programma: l'architettura di Elm.
;
; Una vista è una sequenza di nodi, come il testo di una pagina: ogni nodo ha i suoi figli e i
; suoi fratelli. Così ogni funzione sulla vista è ricorsiva in modo strutturale, senza passare
; per una lista di alberi. Si scrive con le funzioni dei tag: (div attributi (list figlio …)).

(require bottega/lib/text)

; Un attributo: un valore, o un evento che diventa un messaggio. Il messaggio di un input si
; calcola dal testo del campo.
(data Attr : (→ Type Type)
  (attr : (→ String String (Attr M)))             ; nome, valore
  (on-click : (→ M (Attr M)))
  (on-input : (→ (→ String M) (Attr M))))

(data Html : (→ Type Type)
  (end : (Html M))
  (text-then : (→ String (Html M) (Html M)))                        ; un testo, poi i fratelli
  (node : (→ String (List (Attr M)) (Html M) (Html M) (Html M))))   ; tag, attributi, figli, fratelli

; Un'iscrizione: un messaggio che arriva da sé, senza un evento della pagina.
; ponytail: solo il tempo; altre fonti (un socket, un file) quando un programma le chiede.
(data Sub : (→ Type Type)
  (every : (→ Nat M (Sub M))))                    ; ogni tanti millisecondi, il messaggio

; Il programma: lo stato iniziale, come un messaggio lo cambia, come si vede, e a che cosa è
; iscritto in ogni stato.
(record (Program Model Msg)
  (init Model)
  (update (→ Msg Model Model))
  (view (→ Model (Html Msg)))
  (subs (→ Model (List (Sub Msg)))))

(builtin html-end end)
(builtin html-text text-then)
(builtin html-node node)
(builtin attr attr)
(builtin on-click on-click)
(builtin on-input on-input)
(builtin every every)
(builtin program program)

; --- scrivere una vista ---

(claim then (→ (Html M) (Html M) (Html M)))      ; una sequenza, poi un'altra
(define (then h rest)
  (match h
    (end rest)
    ((text-then s next) (text-then s (then next rest)))
    ((node t as cs next) (node t as cs (then next rest)))))

(claim all-of (→ (List (Html M)) (Html M)))
(define (all-of hs)
  (match hs
    (nil end)
    ((:: h more) (then h (all-of more)))))

(claim text (→ String (Html M)))
(define (text s) (text-then s end))

(claim tag (→ String (List (Attr M)) (List (Html M)) (Html M)))
(define (tag t as cs) (node t as (all-of cs) end))

(claim div (→ (List (Attr M)) (List (Html M)) (Html M)))
(define (div as cs) (tag "div" as cs))
(claim span (→ (List (Attr M)) (List (Html M)) (Html M)))
(define (span as cs) (tag "span" as cs))
(claim h1 (→ (List (Attr M)) (List (Html M)) (Html M)))
(define (h1 as cs) (tag "h1" as cs))
(claim button (→ (List (Attr M)) (List (Html M)) (Html M)))
(define (button as cs) (tag "button" as cs))
(claim input (→ (List (Attr M)) (Html M)))
(define (input as) (tag "input" as (list)))

(claim class (→ String (Attr M)))
(define (class c) (attr "class" c))
(claim value (→ String (Attr M)))
(define (value v) (attr "value" v))

(claim silent (→ A (List (Sub M))))                ; nessuna iscrizione, in ogni stato
(define (silent _) (list))

; --- leggere una vista: per le prove ---

; I messaggi dei click, in ordine di pagina.
(claim click-messages (→ (List (Attr M)) (List M)))
(define (click-messages as)
  (match as
    (nil (list))
    ((:: (on-click m) more) (:: m (click-messages more)))
    ((:: _ more) (click-messages more))))

(claim clicks (→ (Html M) (List M)))
(define (clicks h)
  (match h
    (end (list))
    ((text-then _ next) (clicks next))
    ((node _ as cs next) (++ (click-messages as) (++ (clicks cs) (clicks next))))))

; I messaggi dei campi, se si scrive s in ciascuno.
(claim input-messages (→ (List (Attr M)) String (List M)))
(define (input-messages as s)
  (match as
    (nil (list))
    ((:: (on-input f) more) (:: (f s) (input-messages more s)))
    ((:: _ more) (input-messages more s))))

(claim typed (→ (Html M) String (List M)))
(define (typed h s)
  (match h
    (end (list))
    ((text-then _ next) (typed next s))
    ((node _ as cs next) (++ (input-messages as s) (++ (typed cs s) (typed next s))))))

; --- le prove della libreria ---

(claim tag-is-one-node (≡ (the (Html ⊤) (div (list) (list (text "a") (text "b"))))
                          (node "div" (list) (text-then "a" (text-then "b" end)) end)))
(define tag-is-one-node refl)

(claim clicks-in-order (≡ (clicks (div (list (on-click 1)) (list (button (list (class "x") (on-click 2)) (list)) (text "t"))))
                          (list 1 2)))
(define clicks-in-order refl)
