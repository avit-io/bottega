#lang racket/base
;; The runtime of an affresco program: Elm's loop, on the server. The model never leaves it; the
;; browser sends events and receives patches. A program's main submodule calls main-program.
;;   racket app.rkt show         the initial view, as HTML
;;   racket app.rkt serve PORT   the program, on http://localhost:PORT
;;   racket app.rkt check        types and proofs
;;
;; A view is a normal form: its nodes become x-expressions, its events stay terms, found by the
;; path of their element (child indices from the root, text nodes included). An event evaluates
;; its message, update gives the new model, view the new page; diff says what changed.
;; ponytail: one lock for all the sessions, and they live in memory until the process ends.
;; ponytail: no Cmd and no Sub: nothing reaches update but the browser's events.

(require racket/match racket/list racket/string json xml net/url
         web-server/servlet-env web-server/http
         bottega/check bottega/core/state bottega/core/term bottega/core/eval
         "dialect.rkt")
(provide main-program new-session step! session-page diff patch)

(define (main-program file)
  (define args (vector->list (current-command-line-arguments)))
  (with-handlers ([exn:fail:bottega? (λ (e) (eprintf "~a\n" (exn-message e)) (exit 1))])
    (parameterize ([current-languages languages] [current-implicit implicit])
      (call-with-checked-file file
        (λ ()
          (match args
            [(list "check") (displayln "checked: types and proofs")]
            [(list "show") (let-values ([(xs _) (render (view (init)))]) (displayln (xexprs->string xs)))]
            [(list "serve" port) (serve (string->number port))]
            [_ (eprintf "usage: racket app.rkt show | serve PORT | check\n") (exit 2)]))))))

;; --- the program: main's three fields, as values ---

(define (program-field i)
  (match (force (evaluate '() (tglob (or (resolve 'main) (oops "main is missing: (claim main (Program Model Msg))")))))
    [(vglob k sp) #:when (builtin? k 'program) (car (list-ref (filter (λ (a) (eq? (cdr a) 'e)) sp) i))]
    [_ (oops "main must be (program init update view)")]))

(define (init) (program-field 0))
(define (update msg model) (apply-spine (program-field 1) (list (cons msg 'e) (cons model 'e))))
(define (view model) (reify 0 (apply-value (program-field 2) model 'e)))

;; --- a view: x-expressions, and the events by (path . event) ---

(define (spine t)                                 ; → (values head explicit-arguments)
  (let loop ([t t] [args '()])
    (match t
      [(tapp f a 'e) (loop f (cons a args))]
      [(tapp f _ _) (loop f args)]
      [_ (values t args)])))

(define (is? t key) (let-values ([(h _) (spine t)]) (match h [(tglob n) (builtin? n key)] [_ #f])))
(define (args-of t) (let-values ([(_ as) (spine t)]) as))

(define (term->list t)                            ; a List, as a Racket list
  (match (args-of t)
    [(list x more) (cons x (term->list more))]
    ['() '()]))

;; → (values x-expressions events), events: ((path event . term) …); path: the indices, root first.
(define (render t [path '()])
  (let loop ([t t] [i 0] [xs '()] [events '()])
    (cond
      [(is? t 'html-end) (values (reverse xs) events)]
      [(is? t 'html-text)
       (match-define (list s next) (args-of t))
       (loop next (+ i 1) (cons (tstr-s s) xs) events)]
      [(is? t 'html-node)
       (match-define (list tag as kids next) (args-of t))
       (define here (append path (list i)))
       (define-values (attrs ons) (render-attrs (term->list as) here))
       (define-values (children inner) (render kids here))
       (loop next (+ i 1) (cons `(,(string->symbol (tstr-s tag)) ,attrs ,@children) xs) (append events ons inner))]
      [else (oops "a view must be in normal form: ~s" t)])))

(define (render-attrs as path)
  (for/fold ([attrs '()] [events '()] #:result (values (reverse attrs) events)) ([a as])
    (match (args-of a)
      [(list n v) (values (cons (list (string->symbol (tstr-s n)) (tstr-s v)) attrs) events)]   ; attr
      [(list x) (define on (if (is? a 'on-click) "click" "input"))
                (values (cons `(data-on ,on) attrs) (cons (list* path on x) events))])))

(define (xexprs->string xs) (string-append* (map xexpr->string xs)))

;; --- what changed: the patches the browser applies ---
;; (replace path html): the node at path becomes html; (text path s); (set path name value);
;; (remove path name); (children path html): all the children of path.

(define (diff old new [path '()])
  (if (not (= (length old) (length new)))
      (list `(children ,path ,(xexprs->string new)))
      (append*
       (for/list ([a old] [b new] [i (in-naturals)])
         (define here (append path (list i)))
         (match* (a b)
           [((? string?) (? string?)) (if (equal? a b) '() (list `(text ,here ,b)))]
           [((list t as ka ...) (list t bs kb ...))
            (append (for/list ([x bs] #:unless (member x as)) `(set ,here ,(symbol->string (car x)) ,(cadr x)))
                    (for/list ([x as] #:unless (assq (car x) bs)) `(remove ,here ,(symbol->string (car x))))
                    (diff ka kb here))]
           [(_ _) (list `(replace ,here ,(xexpr->string b)))])))))

;; The browser's side, for the check: patches applied to x-expressions.
(define (patch ps xs)
  (define (attrs-without e n) (filter (λ (a) (not (eq? (car a) n))) (cadr e)))
  (cddr
   (for/fold ([root (list* 'root '() xs)]) ([p ps])
     (match p
       [(list 'children path html) (update-at root path (λ (e) (list* (car e) (cadr e) (parse html))))]
       [(list 'replace path html) (update-at root path (λ (_) (car (parse html))))]
       [(list 'text path s) (update-at root path (λ (_) s))]
       [(list 'set path n v) (let ([n (string->symbol n)])
                                 (update-at root path (λ (e) (list* (car e) (append (attrs-without e n) (list (list n v))) (cddr e)))))]
       [(list 'remove path n) (update-at root path (λ (e) (list* (car e) (attrs-without e (string->symbol n)) (cddr e))))]))))

(define (update-at e path f)                      ; f on the node at path, from the element e
  (if (null? path)
      (f e)
      (list* (car e) (cadr e) (list-update (cddr e) (car path) (λ (c) (update-at c (cdr path) f))))))

(define (parse html)                              ; html of zero or more nodes → x-expressions
  (cddr (xml->xexpr (document-element (read-xml (open-input-string (string-append "<r>" html "</r>")))))))

;; --- the server ---

(struct session (model page events) #:mutable)
(define sessions (make-hash))
(define lock (make-semaphore 1))

(define (serve port)
  (printf "affresco: http://localhost:~a\n" port)
  (serve/servlet (λ (req) (call-with-semaphore lock (λ () (respond req))))
                 #:port port #:servlet-regexp #rx"" #:command-line? #t #:listen-ip #f))

(define (respond req)
  (match (url->string* req)
    ["/event" (event (bytes->jsexpr (request-post-data/raw req)))]
    [_ (page)]))

(define (url->string* req) (string-append "/" (string-join (map path/param-path (url-path (request-uri req))) "/")))

(define (page)
  (define id (number->string (random 1000000000)))
  (define s (new-session))
  (hash-set! sessions id s)
  (response/full 200 #"OK" (current-seconds) TEXT/HTML-MIME-TYPE '()
                 (list (string->bytes/utf-8 (shell id (xexprs->string (session-page s)))))))

;; {"session", "path", "event", "value"} → the patches, as JSON
(define (event e)
  (define s (hash-ref sessions (hash-ref e 'session) #f))
  (define patches (if s (step! s (hash-ref e 'path) (hash-ref e 'event) (hash-ref e 'value "")) '()))
  (response/jsexpr (for/list ([p patches]) (cons (symbol->string (car p)) (cdr p)))))

;; A session starts at init; an event finds its message, update the model, view the page.
(define (new-session)
  (define model (init))
  (define-values (xs events) (render (view model)))
  (session model xs events))

(define (step! s path on value)                   ; → the patches
  (define handler (findf (λ (x) (and (equal? (car x) path) (equal? (cadr x) on))) (session-events s)))
  (cond
    [(not handler) '()]                          ; the page changed meanwhile: nothing to do
    [else
     (define msg (if (equal? on "input")
                     (apply-value (evaluate '() (cddr handler)) (vstr value) 'e)
                     (evaluate '() (cddr handler))))
     (define model (update msg (session-model s)))
     (define-values (xs events) (render (view model)))
     (begin0 (diff (session-page s) xs)
             (set-session-model! s model) (set-session-page! s xs) (set-session-events! s events))]))

(define (shell id body)
  (string-append
   "<!doctype html><html><head><meta charset=\"utf-8\"><title>affresco</title>"
   "<style>body{font-family:Georgia,serif;margin:3em} .card{max-width:24em} .row{display:flex;gap:.5em;align-items:center}"
   " button{font:inherit;padding:.2em .8em} .digits{min-width:2em;text-align:center}</style></head><body>"
   "<div id=\"affresco\" data-session=\"" id "\">" body "</div><script>" client "</script></body></html>"))

;; The browser: events up, patches down. A node's path is its child indices from the root.
(define client #<<JS
const root = document.getElementById("affresco");
const pathOf = (n) => { const p = []; while (n !== root) { p.unshift([...n.parentNode.childNodes].indexOf(n)); n = n.parentNode; } return p; };
const at = (p) => p.reduce((n, i) => n.childNodes[i], root);
const html = (s) => { const t = document.createElement("template"); t.innerHTML = s; return t.content; };
for (const type of ["click", "input"]) root.addEventListener(type, async (e) => {
  const n = e.target.closest("[data-on]");
  if (!n || n.dataset.on !== type) return;
  const r = await fetch("/event", { method: "POST", body: JSON.stringify(
    { session: root.dataset.session, path: pathOf(n), event: type, value: n.value || "" }) });
  for (const [op, p, a, b] of await r.json()) {
    const n = at(p);
    if (op === "text") n.textContent = a;
    else if (op === "set") { n.setAttribute(a, b); if (a === "value" && n.value !== b) n.value = b; }
    else if (op === "remove") n.removeAttribute(a);
    else if (op === "replace") n.replaceWith(html(a));
    else if (op === "children") n.replaceChildren(html(a));
  }
});
JS
  )
