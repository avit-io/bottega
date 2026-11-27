#lang racket/base
;; Ocra: the visual system of affresco and of avit-io, as data. Ochre and nero di vite; eval is the
;; field (squared surfaces, what you read), apply is the action (round ends, what you press).
;; The tokens below are the one source: the page of every affresco program and the Tailwind theme
;; of a webapp come from them.
;; ponytail: the design system's tokens.json and the blog's main.css are still copied by hand;
;; generate them from here when they drift.
;;   racket affresco/ocra.rkt css        the stylesheet: tokens in both themes, then the classes
;;   racket affresco/ocra.rkt tailwind   a Tailwind v4 @theme block over the same variables
;;   racket affresco/ocra.rkt nvim       colors/ocra.lua for Neovim, both themes
;;   racket affresco/ocra.rkt wezterm    a WezTerm scheme, Ocra Nero (wezterm-light: Ocra)
;; The rule that round means "applies" is a proof, not a guideline: ocra-shapes in lib/html.rkt.

(require racket/string racket/list)
(provide colors ocra-css ocra-tailwind ocra-nvim ocra-wezterm)

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

;; --- the editor and the terminal: the same roles, as a colorscheme ---

;; A terminal needs eight hues and Ocra has four; the other three are muted earths, terminal only.
;; (name light dark)
(define ansi-extra
  '((blue    "#3d5566" "#8fa3b0")
    (magenta "#7a3f55" "#c48b9f")
    (cyan    "#3f665b" "#8fb3a6")))

(define (color name theme)                                   ; theme: second (light) or third (dark)
  (theme (or (assq name colors) (assq name ansi-extra))))

;; WezTerm: a TOML scheme for the colors/ directory beside wezterm.lua
(define (ocra-wezterm theme)
  (define (c n) (format "\"~a\"" (color n theme)))
  (define ansi (list (c 'nero) (c 'cinabro) (c 'terraverde) (c 'syn-string) (c 'blue) (c 'magenta) (c 'cyan) (c 'ink-muted)))
  (define bright (list (c 'ink-muted) (c 'cinabro) (c 'terraverde) (c 'ocra) (c 'blue) (c 'magenta) (c 'cyan) (c 'ink)))
  (string-append
   (format "# Ocra ~a, generated by bottega/affresco/ocra.rkt\n[metadata]\nname = \"~a\"\n\n[colors]\n"
           (if (eq? theme second) "(light)" "Nero (dark)") (if (eq? theme second) "Ocra" "Ocra Nero"))
   (format "background = ~a\nforeground = ~a\ncursor_bg = ~a\ncursor_fg = ~a\ncursor_border = ~a\n"
           (c 'surface) (c 'ink) (c 'primary) (c 'on-primary) (c 'primary))
   (format "selection_bg = ~a\nselection_fg = ~a\n" (c 'primary) (c 'on-primary))
   (format "ansi = [~a]\nbrights = [~a]\n" (string-join ansi ", ") (string-join bright ", "))))

;; Neovim: one colors/ocra.lua, Nero when background=dark, Ocra when light. Few colours, on purpose:
;; keywords stand out by weight, parentheses step back, as on the page.
(define nvim-groups
  ;; (group fg bg style), colour names from the palette, #f for none
  '((Normal ink surface #f) (NormalFloat ink surface-raised #f) (FloatBorder ink-muted surface-raised #f)
    (Comment syn-comment #f italic) (String syn-string #f #f) (Character syn-string #f #f)
    (Number syn-string #f #f) (Boolean syn-string #f #f) (Constant syn-string #f #f)
    (Identifier ink #f #f) (Function ink #f bold) (Statement syn-keyword #f bold) (Keyword syn-keyword #f bold)
    (Conditional syn-keyword #f bold) (Repeat syn-keyword #f bold) (Operator ink #f #f)
    (PreProc syn-keyword #f #f) (Type ink #f italic) (Special syn-string #f #f) (Delimiter syn-paren #f #f)
    (Title syn-keyword #f bold) (Directory ink #f bold) (Todo on-primary primary bold)
    (Error cinabro #f bold) (ErrorMsg cinabro #f bold) (WarningMsg syn-string #f bold)
    (LineNr syn-paren #f #f) (CursorLineNr ink #f bold) (CursorLine #f surface-raised #f) (ColorColumn #f surface-raised #f)
    (SignColumn ink-muted surface #f) (NonText syn-paren #f #f) (Whitespace syn-paren #f #f)
    (Visual on-primary primary #f) (Search nero ocra #f) (IncSearch on-primary primary bold) (CurSearch on-primary primary bold)
    (MatchParen ink surface-sunken bold)
    (Pmenu ink surface-raised #f) (PmenuSel on-primary primary bold) (PmenuSbar #f surface-raised #f) (PmenuThumb #f ink-muted #f)
    (StatusLine ink surface-raised #f) (StatusLineNC ink-muted surface #f) (WinSeparator syn-paren #f #f)
    (TabLine ink-muted surface-raised #f) (TabLineSel on-primary primary bold) (TabLineFill #f surface #f)
    (DiagnosticError cinabro #f #f) (DiagnosticWarn syn-string #f #f) (DiagnosticInfo ink-muted #f #f) (DiagnosticHint ink-muted #f #f)
    (DiffAdd terraverde #f #f) (DiffChange syn-string #f #f) (DiffDelete cinabro #f #f) (DiffText syn-string surface-sunken bold)
    (Added terraverde #f #f) (Changed syn-string #f #f) (Removed cinabro #f #f)
    (TelescopeBorder ink-muted #f #f) (TelescopeSelection on-primary primary #f)))

;; treesitter and lsp captures, by link
(define nvim-links
  '((@keyword Keyword) (@keyword.function Keyword) (@keyword.return Keyword) (@conditional Conditional)
    (@string String) (@number Number) (@boolean Boolean) (@constant Constant) (@constant.builtin Constant)
    (@comment Comment) (@function Function) (@function.call Identifier) (@function.builtin Function)
    (@variable Identifier) (@variable.builtin Identifier) (@type Type) (@type.builtin Type) (@operator Operator)
    (@punctuation.bracket Delimiter) (@punctuation.delimiter Delimiter) (@punctuation.special Delimiter)
    (@tag Keyword) (@markup.heading Title) (@markup.raw String)))

(define (ocra-nvim)
  (define (palette theme)
    (string-join (for/list ([c (append colors ansi-extra)])
                   (format "  [~s] = ~s," (symbol->string (car c)) (theme c)))
                 "\n"))
  (define (spec g)
    (define (part k v) (and v (format "~a = p[~s]" k (symbol->string v))))
    (string-join (filter values (list (part "fg" (second g)) (part "bg" (third g))
                                      (and (fourth g) (format "~a = true" (fourth g)))))
                 ", "))
  (string-append
   "-- Ocra for Neovim, generated by bottega/affresco/ocra.rkt: Nero when background=dark, Ocra when light.\n"
   "local palettes = {\n light = {\n" (palette second) "\n },\n dark = {\n" (palette third) "\n },\n}\n"
   "vim.cmd('hi clear')\nif vim.fn.exists('syntax_on') == 1 then vim.cmd('syntax reset') end\n"
   "vim.g.colors_name = 'ocra'\nlocal p = palettes[vim.o.background] or palettes.dark\n"
   "local set = function(g, s) vim.api.nvim_set_hl(0, g, s) end\n"
   (string-append* (for/list ([g nvim-groups]) (format "set(~s, { ~a })\n" (symbol->string (car g)) (spec g))))
   (string-append* (for/list ([l nvim-links]) (format "set(~s, { link = ~s })\n" (symbol->string (car l)) (symbol->string (cadr l)))))
   "for i, c in ipairs({ 'nero', 'cinabro', 'terraverde', 'syn-string', 'blue', 'magenta', 'cyan', 'ink-muted',\n"
   "                     'ink-muted', 'cinabro', 'terraverde', 'ocra', 'blue', 'magenta', 'cyan', 'ink' }) do\n"
   "  vim.g['terminal_color_' .. (i - 1)] = p[c]\nend\n"))

(module+ main
  (define what (vector-ref (current-command-line-arguments) 0))
  (display (case what
             [("css") (ocra-css)] [("tailwind") (ocra-tailwind)] [("nvim") (ocra-nvim)]
             [("wezterm") (ocra-wezterm third)] [("wezterm-light") (ocra-wezterm second)]
             [else (error 'ocra "css, tailwind, nvim, wezterm or wezterm-light, not ~a" what)])))
