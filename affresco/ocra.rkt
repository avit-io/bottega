#lang racket/base
;; Ocra: the visual system of affresco and of avit-io, as data. Ochre and nero di vite; eval is the
;; field (squared surfaces, what you read), apply is the action (round ends, what you press).
;; The tokens below are the one source: the page of every affresco program and the Tailwind theme
;; of a webapp come from them.
;; ponytail: the design system's tokens.json and the blog's main.css are still copied by hand;
;; generate them from here when they drift.
;;   racket affresco/ocra.rkt css        the stylesheet: tokens in both themes, then the classes
;;   racket affresco/ocra.rkt tailwind   a Tailwind v4 @theme block over the same variables
;; The rule that round means "applies" is a proof, not a guideline: ocra-shapes in lib/html.rkt.

(require racket/string racket/list)
(provide colors ocra-css ocra-tailwind)

;; (name light dark usage): the roles first, then the fixed pigments
(define colors
  '((surface        "#efe0b4" "#12110e" "The page: the plaster.")
    (surface-raised "#f7eed6" "#1c1a15" "One level up: cards, dialogs. Elevation is tone, not shadow.")
    (surface-sunken "#e4d094" "#0a0907" "One level down: code, the REPL, input fields.")
    (ink            "#15130f" "#ebddb2" "Text on the three surfaces (12:1 and up).")
    (ink-muted      "#594c33" "#a99b78" "Metadata and captions on the three surfaces (5.4:1 and up).")
    (line           "#15130f2e" "#ebddb22e" "Decorative rules; control borders use ink.")
    (primary        "#15130f" "#d6b866" "Apply: buttons, active links, selection. Highest contrast on the ground.")
    (on-primary     "#efe0b4" "#15130f" "Text on primary.")
    (focus          "#6e4f12" "#f2d37a" "Focus ring, 2px solid with 2px offset.")
    (cinabro        "#9a3220" "#e8806a" "Error, always with a word.")
    (terraverde     "#43603a" "#9dbb86" "Success, a closed proof, always with a word.")
    (syn-keyword    "#15130f" "#f2e4b8" "Code: special forms, bold.")
    (syn-string     "#6e4f12" "#d6b866" "Code: strings and literals.")
    (syn-comment    "#62553a" "#9a8c69" "Code: comments, italic.")
    (syn-paren      "#7d6c48" "#7d7259" "Code: parentheses, dimmed as Lisp likes them.")
    (ocra           "#d6b866" "#d6b866" "The pigment: fields of colour, never text on the light theme.")
    (ocra-pastello  "#efe0b4" "#efe0b4" "Ochre let down: the light ground.")
    (ocra-terra     "#8c6b27" "#8c6b27" "Burnt ochre: marks of 24px and up.")
    (nero           "#15130f" "#15130f" "Nero di vite: the light ink, the dark ground.")))

(define fonts
  '((display "\"Alegreya\", \"Iowan Old Style\", Palatino, Georgia, serif")
    (serif   "\"Alegreya\", Charter, \"Iowan Old Style\", Georgia, serif")
    (sans    "\"Alegreya Sans\", \"Gill Sans\", system-ui, sans-serif")
    (mono    "\"IBM Plex Mono\", ui-monospace, Menlo, Consolas, monospace")))
(define font-url "https://fonts.googleapis.com/css2?family=Alegreya:ital,wght@0,400;0,700;0,800;1,400&family=Alegreya+Sans:wght@500;700&family=IBM+Plex+Mono:ital,wght@0,400;0,600;1,400&display=swap")

(define spacing '((space-1 "4px") (space-2 "8px") (space-4 "16px") (space-6 "24px") (space-10 "40px") (space-16 "64px")))
(define radii '((radius-0 "0") (radius-1 "3px") (radius-paren "9999px")))

;; --- css ---

(define (decls pick) (string-join (for/list ([c colors]) (format "--~a:~a" (first c) (pick c))) ";"))

;; The classes are the shapes: .btn is round (apply), .card and .repl are squared (eval).
(define classes #<<CSS
body{margin:0;background:var(--surface);color:var(--ink);font:400 19px/31px var(--font-serif)}
#affresco{padding:var(--space-10) max(var(--space-4),env(safe-area-inset-left,0px))}
h1,h2{font-family:var(--font-display);font-weight:700;line-height:1.2;margin:0}
.card{display:flex;flex-direction:column;gap:var(--space-4);max-width:28em;background:var(--surface-raised);padding:var(--space-6);border-radius:var(--radius-0)}
.row{display:flex;gap:var(--space-2);align-items:center;flex-wrap:wrap}
.digits{min-width:2em;text-align:center;font:600 20px/1 var(--font-mono)}
.btn{font:500 16px/24px var(--font-sans);padding:var(--space-2) var(--space-4);border-radius:var(--radius-paren);border:2px solid var(--primary);background:var(--primary);color:var(--on-primary);cursor:pointer}
.btn--quiet{background:transparent;color:var(--ink);border-color:var(--ink)}
.btn:focus-visible,input:focus-visible{outline:2px solid var(--focus);outline-offset:2px}
input{font:500 16px/24px var(--font-sans);color:var(--ink);background:var(--surface-sunken);border:1px solid var(--ink);border-radius:var(--radius-1);padding:var(--space-1) var(--space-2)}
.repl{background:var(--surface-sunken);font:400 15px/24px var(--font-mono);padding:var(--space-6);overflow-x:auto;border-radius:var(--radius-0)}
.repl .kw{color:var(--syn-keyword);font-weight:600}.repl .str{color:var(--syn-string)}.repl .com{color:var(--syn-comment);font-style:italic}.repl .par{color:var(--syn-paren)}
.repl .val::before{content:"⇒ ";color:var(--ink-muted)}.err{color:var(--cinabro)}.ok{color:var(--terraverde)}
CSS
  )

(define (ocra-css)
  (string-append
   (format "@import url(\"~a\");\n" font-url)
   ":root{color-scheme:light;" (decls second) ";"
   (string-join (for/list ([f fonts]) (format "--font-~a:~a" (first f) (second f))) ";") ";"
   (string-join (for/list ([s (append spacing radii)]) (format "--~a:~a" (first s) (second s))) ";") "}\n"
   "@media (prefers-color-scheme: dark){:root:not([data-theme=\"light\"]){color-scheme:dark;" (decls third) "}}\n"
   ":root[data-theme=\"dark\"]{color-scheme:dark;" (decls third) "}\n"
   classes "\n"))

;; --- tailwind: the utilities read the variables, so the themes switch by themselves ---

(define (ocra-tailwind)
  (string-append
   "@theme inline {\n"
   (string-append* (for/list ([c colors]) (format "  --color-~a: var(--~a);\n" (first c) (first c))))
   (string-append* (for/list ([f fonts]) (format "  --font-~a: var(--font-~a);\n" (first f) (first f))))
   "  --radius-paren: 9999px;\n}\n"
   "@custom-variant dark (&:where([data-theme=\"dark\"], [data-theme=\"dark\"] *));\n"))

(module+ main
  (define what (vector-ref (current-command-line-arguments) 0))
  (display (case what [("css") (ocra-css)] [("tailwind") (ocra-tailwind)]
             [else (error 'ocra "css or tailwind, not ~a" what)])))
