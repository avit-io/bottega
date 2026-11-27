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
;;   racket affresco/ocra.rkt claude     a Claude Code theme, for ~/.claude/themes/ocra.json
;; The rule that round means "applies" is a proof, not a guideline: ocra-shapes in lib/html.rkt.

(require racket/string racket/list)
(provide colors ocra-css ocra-tailwind ocra-nvim ocra-wezterm ocra-claude)

;; (name light dark usage): the roles first, then the fixed pigments
(define colors
  '((surface        "#efe0b4" "#12110e" "The page: the plaster.")
    (surface-raised "#f7eed6" "#1c1a15" "One level up: cards, dialogs. Elevation is tone, not shadow.")
    (surface-sunken "#e4d094" "#0a0907" "One level down: code, the REPL, input fields.")
    (ink            "#15130f" "#ebddb2" "Text on the three surfaces (12:1 and up).")
    (ink-muted      "#594c33" "#a99b78" "Metadata and captions on the three surfaces (5.4:1 and up).")
    (line           "#15130f2e" "#ebddb22e" "Decorative rules; control borders use ink.")
    (primary        "#15130f" "#d6b866" "Apply: buttons, active links. Highest contrast on the ground.")
    (on-primary     "#efe0b4" "#15130f" "Text on primary.")
    (selection      "#d6b866" "#4a412c" "Selected text, under ink: ochre, not black.")
    (focus          "#6e4f12" "#f2d37a" "Focus ring, 2px solid with 2px offset.")
    (cinabro        "#9a3220" "#e8806a" "Error, always with a word.")
    (terraverde     "#43603a" "#9dbb86" "Success, a closed proof, always with a word.")
    (syn-keyword    "#15130f" "#f2e4b8" "Code: special forms, bold.")
    (syn-string     "#3f7027" "#9dbb86" "Code: strings, the values that evaluate to themselves (terra verde).")
    (syn-constant   "#9a2f63" "#c48b9f" "Code: numbers, booleans, constants (morellone).")
    (syn-apply      "#2b5582" "#8fa3b0" "Code: what is applied, functions and methods (blu di Prussia). Bold where defined.")
    (syn-type       "#1f6b63" "#8fb3a6" "Code: types, italic (verdacqua).")
    (syn-special    "#94520e" "#d6b866" "Code: escapes, macros, labels, warnings (ocra bruciata).")
    (syn-member     "#6a4a2a" "#a99b78" "Code: fields and properties, a warm umber.")
    (paper          "#f7eed6" "#12110e" "The editor ground: paper, paler than the plaster, so code pigments do not sit on ochre.")
    (search         "#cbd2bd" "#2f3a33" "Search matches: a pale green field, so search and selection never look alike.")
    (syn-comment    "#62553a" "#9a8c69" "Code: comments, italic.")
    (syn-paren      "#655535" "#968b6f" "Code: parentheses, dimmed as Lisp likes them, still 4.5:1 on every surface.")
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

;; A terminal needs sixteen colours and Ocra has four hues; blue, magenta and cyan are muted earths.
;; Every entry reads at 4.5:1 or more on its ground, except black on the dark ground, which is the
;; ground by convention. On the light ground the brights are darker, not lighter: bright is emphasis.
;; (light dark), in ANSI order: black red green yellow blue magenta cyan white, then the brights.
(define ansi
  '(("#15130f" "#15130f") ("#9a3220" "#e8806a") ("#43603a" "#9dbb86") ("#6e4f12" "#d6b866")
    ("#3d5566" "#8fa3b0") ("#7a3f55" "#c48b9f") ("#3f665b" "#8fb3a6") ("#594c33" "#a99b78")
    ("#594c33" "#a99b78") ("#7d2716" "#f09a86") ("#33502b" "#b3cf9d") ("#57400e" "#f2d37a")
    ("#2c4252" "#a9bccb") ("#622f44" "#d6a3b6") ("#2f5048" "#a6c9bc") ("#15130f" "#ebddb2")))

(define (color name theme) (theme (assq name colors)))      ; theme: second (light) or third (dark)
(define (ansi-of theme) (map (if (eq? theme second) car cadr) ansi))
(define (lua-list l) (string-join (map (λ (h) (format "~s" h)) l) ", "))

;; WezTerm: a TOML scheme for the colors/ directory beside wezterm.lua
(define (ocra-wezterm theme)
  (define (c n) (format "\"~a\"" (color n theme)))
  (define-values (ansi8 bright8) (split-at (map (λ (h) (format "\"~a\"" h)) (ansi-of theme)) 8))
  (string-append
   (format "# Ocra ~a, generated by bottega/affresco/ocra.rkt\n[metadata]\nname = \"~a\"\n\n[colors]\n"
           (if (eq? theme second) "(light)" "Nero (dark)") (if (eq? theme second) "Ocra" "Ocra Nero"))
   (format "background = ~a\nforeground = ~a\ncursor_bg = ~a\ncursor_fg = ~a\ncursor_border = ~a\n"
           (c 'surface) (c 'ink) (c 'primary) (c 'on-primary) (c 'primary))
   (format "selection_bg = ~a\nselection_fg = ~a\n" (c 'selection) (c 'ink))
   (format "ansi = [~a]\nbrights = [~a]\n" (string-join ansi8 ", ") (string-join bright8 ", "))))

;; Neovim: one colors/ocra.lua, Nero when background=dark, Ocra when light. Few colours, on purpose:
;; keywords stand out by weight, parentheses step back, as on the page.
(define nvim-groups
  ;; (group fg bg style), colour names from the palette, #f for none
  '((Normal ink paper #f) (NormalFloat ink surface #f) (FloatBorder ink-muted surface #f)
    (Comment syn-comment #f italic) (String syn-string #f #f) (Character syn-string #f #f)
    (Number syn-constant #f #f) (Boolean syn-constant #f #f) (Constant syn-constant #f #f)
    (Identifier ink #f #f) (Call syn-apply #f #f) (Member syn-member #f #f) (Parameter ink #f italic) (Function syn-apply #f bold) (Statement syn-keyword #f bold) (Keyword syn-keyword #f bold)
    (Conditional syn-keyword #f bold) (Repeat syn-keyword #f bold) (Operator ink #f #f)
    (PreProc syn-special #f #f) (Type syn-type #f italic) (Special syn-special #f #f) (Delimiter syn-paren #f #f)
    (Title syn-keyword #f bold) (Directory ink #f bold) (Todo on-primary primary bold)
    (Error cinabro #f bold) (ErrorMsg cinabro #f bold) (WarningMsg syn-special #f bold)
    (LineNr syn-paren #f #f) (CursorLineNr ink #f bold) (CursorLine #f surface #f) (ColorColumn #f surface #f)
    (SignColumn ink-muted paper #f) (NonText syn-paren #f #f) (Whitespace syn-paren #f #f)
    (Visual ink selection #f) (Search ink search #f) (IncSearch on-primary primary bold) (CurSearch on-primary primary bold)
    (MatchParen ink surface-sunken bold)
    (Pmenu ink surface #f) (PmenuSel on-primary primary bold) (PmenuSbar #f surface #f) (PmenuThumb #f ink-muted #f)
    (StatusLine ink surface #f) (StatusLineNC ink-muted paper #f) (WinSeparator syn-paren #f #f)
    (TabLine ink-muted surface #f) (TabLineSel on-primary primary bold) (TabLineFill #f paper #f)
    (DiagnosticError cinabro #f #f) (DiagnosticWarn syn-special #f #f) (DiagnosticInfo ink-muted #f #f) (DiagnosticHint ink-muted #f #f)
    (DiffAdd terraverde #f #f) (DiffChange syn-special #f #f) (DiffDelete cinabro #f #f) (DiffText syn-special surface-sunken bold)
    (Added terraverde #f #f) (Changed syn-special #f #f) (Removed cinabro #f #f)
    (TelescopeBorder ink-muted #f #f) (TelescopeSelection on-primary primary #f)
    ;; Neovim's own greys, and the messages
    (Conceal ink-muted #f #f) (SpecialKey ink-muted #f #f) (MsgArea ink #f #f) (ModeMsg ink #f bold)
    (MoreMsg terraverde #f bold) (Question terraverde #f bold) (QuickFixLine #f surface-sunken bold)
    (Folded ink-muted surface #f) (FoldColumn ink-muted #f #f) (EndOfBuffer syn-paren #f #f)
    (LspInlayHint ink-muted surface italic) (LspReferenceText #f surface-sunken #f)
    (LspReferenceRead #f surface-sunken #f) (LspReferenceWrite #f surface-sunken bold)
    (DiagnosticOk terraverde #f #f) (DiagnosticUnnecessary ink-muted #f italic)
    ;; plugins that bring colours made for dark grounds
    (GitSignsAdd terraverde #f #f) (GitSignsChange syn-special #f #f) (GitSignsDelete cinabro #f #f)
    (GitSignsStagedAdd terraverde #f #f) (GitSignsStagedChange syn-special #f #f) (GitSignsStagedChangedelete syn-special #f #f)
    (GitSignsStagedDelete cinabro #f #f) (GitSignsStagedTopdelete cinabro #f #f) (GitSignsStagedUntracked terraverde #f #f)
    (NeotestPassed terraverde #f #f) (NeotestFailed cinabro #f bold) (NeotestRunning syn-special #f #f)
    (NeotestSkipped ink-muted #f #f) (NeotestWatching syn-special #f #f) (NeotestMarked syn-keyword #f bold)
    (NeotestTarget cinabro #f #f) (NeotestAdapterName syn-keyword #f bold) (NeotestNamespace ink #f #f)
    (NeotestDir ink #f #f) (NeotestFile ink #f #f) (NeotestIndent syn-paren #f #f) (NeotestExpandMarker syn-paren #f #f)
    (NeotestWinSelect on-primary primary bold)
    (AvanteTitle on-primary primary bold) (AvanteSubtitle on-primary primary #f) (AvanteThirdTitle ink surface-sunken #f)
    (AvanteReversedTitle ink surface bold) (AvanteReversedSubtitle ink-muted surface #f)
    (AvanteReversedThirdTitle ink-muted #f #f) (AvanteSidebarWinSeparator syn-paren surface #f)
    (AvanteSidebarWinHorizontalSeparator syn-paren surface #f)
    (AvanteTaskCompleted terraverde #f #f) (AvanteTaskFailed cinabro #f #f) (AvanteTaskRunning syn-special #f #f)
    (AvanteThinking syn-special #f italic)))

;; treesitter and lsp captures, by link
(define nvim-links
  '((@keyword Keyword) (@keyword.function Keyword) (@keyword.return Keyword) (@conditional Conditional)
    (@string String) (@number Number) (@boolean Boolean) (@constant Constant) (@constant.builtin Constant)
    (@comment Comment) (@function Function) (@function.call Call) (@function.builtin Function)
    (@variable Identifier) (@variable.builtin Identifier) (@type Type) (@type.builtin Type) (@operator Operator)
    (@punctuation.bracket Delimiter) (@punctuation.delimiter Delimiter) (@punctuation.special Delimiter)
    (@tag Keyword) (@function.macro PreProc) (@keyword.import PreProc) (@string.special String) (@variable.parameter Parameter) (@markup.heading Title) (@markup.raw String)
    (@variable.member Member) (@property Member)
    (@function.method Function) (@function.method.call Call) (@constructor Type)
    (@keyword.operator Keyword) (@string.escape Special) (@module Type) (@label Special)))

;; gitsigns derives these from colours made for dark grounds: they follow the plain ones instead
(define gitsigns-links
  (for*/list ([k '("Add" "Change" "Changedelete" "Delete" "Topdelete" "Untracked")] [v '("Cul" "Nr" "Ln")])
    (list (string->symbol (string-append "GitSignsStaged" k v)) (string->symbol (string-append "GitSignsStaged" k)))))

(define (ocra-nvim)
  (define (palette theme)
    (string-join (for/list ([c colors])
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
   "vim.g.ocra = p  -- for the plugins that take colours, not groups (lualine)\n"
   "local set = function(g, s) vim.api.nvim_set_hl(0, g, s) end\n"
   (string-append* (for/list ([g nvim-groups]) (format "set(~s, { ~a })\n" (symbol->string (car g)) (spec g))))
   (string-append* (for/list ([l (append nvim-links gitsigns-links)]) (format "set(~s, { link = ~s })\n" (symbol->string (car l)) (symbol->string (cadr l)))))
   (format "local ansi = { light = { ~a },\n               dark = { ~a } }\n" (lua-list (ansi-of second)) (lua-list (ansi-of third)))
   "for i, c in ipairs(ansi[vim.o.background] or ansi.dark) do vim.g['terminal_color_' .. (i - 1)] = c end\n"))

;; --- Claude Code: a custom theme, ~/.claude/themes/ocra.json, chosen with "theme": "custom:ocra" ---
;; On the light base: muted ochre, nero for text, diff lines as tints of terraverde and cinabro over
;; the plaster (ink reads 7:1 or more on each). Keys the theme leaves out keep the base's colour.
(define claude-overrides
  '((text "#15130f") (inverseText "#efe0b4") (inactive "#594c33") (inactiveShimmer "#7d6c48") (subtle "#655535")
    (claude "#6e4f12") (claudeShimmer "#8c6b27") (clawd_body "#8c6b27") (clawd_background "#efe0b4")
    (claudeBlue_FOR_SYSTEM_SPINNER "#3d5566") (claudeBlueShimmer_FOR_SYSTEM_SPINNER "#5a7080")
    (permission "#3d5566") (permissionShimmer "#5a7080") (suggestion "#3d5566") (remember "#3d5566")
    (planMode "#3f665b") (ide "#3d5566") (background "#3f665b")
    (promptBorder "#7d6c48") (promptBorderShimmer "#a08e66")
    (autoAccept "#7a3f55") (autoAcceptShimmer "#9c6178") (skill "#7a3f55") (bashBorder "#7a3f55") (merged "#7a3f55")
    (success "#43603a") (error "#9a3220") (warning "#6e4f12") (warningShimmer "#8c6b27")
    (diffAdded "#bfbc92") (diffRemoved "#dcba93") (diffAddedDimmed "#dad1a5") (diffRemovedDimmed "#e7cfa5")
    (diffAddedWord "#a2a67d") (diffRemovedWord "#cf9e7c")
    (red_FOR_SUBAGENTS_ONLY "#9a3220") (blue_FOR_SUBAGENTS_ONLY "#3d5566") (green_FOR_SUBAGENTS_ONLY "#43603a")
    (yellow_FOR_SUBAGENTS_ONLY "#6e4f12") (purple_FOR_SUBAGENTS_ONLY "#7a3f55") (orange_FOR_SUBAGENTS_ONLY "#8c5a2b")
    (pink_FOR_SUBAGENTS_ONLY "#8a4a5e") (cyan_FOR_SUBAGENTS_ONLY "#3f665b")
    (professionalBlue "#3d5566") (chromeYellow "#8c6b27")
    (userMessageBackground "#e2d4aa") (userMessageBackgroundHover "#dccc9c") (composerSidebarBackground "#e8d9ad")
    (selectionBg "#d6b866") (bashMessageBackgroundColor "#e2d4aa") (memoryBackgroundColor "#e2d4aa")
    (rate_limit_fill "#6e4f12") (rate_limit_empty "#d4c391") (fastMode "#8c5a2b") (fastModeShimmer "#a87a46")
    (effortUltra "#7a3f55") (briefLabelYou "#3d5566") (briefLabelClaude "#6e4f12")))

(define (ocra-claude)
  (string-append
   "{\n  \"name\": \"Ocra\",\n  \"base\": \"light\",\n  \"overrides\": {\n"
   (string-join (for/list ([o claude-overrides]) (format "    ~s: ~s" (symbol->string (car o)) (cadr o))) ",\n")
   "\n  }\n}\n"))

(module+ main
  (define what (vector-ref (current-command-line-arguments) 0))
  (display (case what
             [("css") (ocra-css)] [("tailwind") (ocra-tailwind)] [("nvim") (ocra-nvim)]
             [("wezterm") (ocra-wezterm third)] [("wezterm-light") (ocra-wezterm second)] [("claude") (ocra-claude)]
             [else (error 'ocra "css, tailwind, nvim, wezterm, wezterm-light or claude, not ~a" what)])))
