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
;; Subscriptions are timers, one thread each, started when the page opens its stream and restarted
;; when subs gives another list. Every patch, a click's or a tick's, goes down that one stream, in
;; the order the server made it.
;; ponytail: no Cmd: update stays pure, nothing runs because of a message. Add it with a first effect.

(require racket/match racket/list racket/string racket/async-channel json xml net/url
         web-server/servlet-env web-server/http
         bottega/check bottega/core/state bottega/core/term bottega/core/eval
         "dialect.rkt" "ocra.rkt")
(provide main-program new-session step! fire! session-page session-every diff patch)

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

;; --- the program: main's four fields, as values ---

(define (program-field i)
  (match (force (evaluate '() (tglob (or (resolve 'main) (oops "main is missing: (claim main (Program Model Msg))")))))
    [(vglob k sp) #:when (builtin? k 'program) (car (list-ref (filter (λ (a) (eq? (cdr a) 'e)) sp) i))]
    [_ (oops "main must be (program init update view)")]))

(define (init) (program-field 0))
(define (update msg model) (apply-spine (program-field 1) (list (cons msg 'e) (cons model 'e))))
(define (view model) (reify 0 (apply-value (program-field 2) model 'e)))
(define (subs model)                              ; → ((milliseconds . message) …)
  (for/list ([s (term->list (reify 0 (apply-value (program-field 3) model 'e)))])
    (match-define (list (tnum ms) msg) (args-of s))
    (cons ms msg)))

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

;; subs: ((milliseconds . message) …); out: the page's stream, once open; gen: which timers are
;; current, so an old one stops by itself, even the one whose tick changed the subscriptions
(struct session (model page events subs out gen) #:mutable)
(define sessions (make-hash))
(define lock (make-semaphore 1))                  ; one model change at a time, whoever asks
(define (locked f) (call-with-semaphore lock f))

(define (serve port)
  (printf "affresco: http://localhost:~a\n" port)
  (serve/servlet respond #:port port #:servlet-regexp #rx"" #:command-line? #t #:listen-ip #f))

(define (respond req)
  (match (url->string* req)
    ["/event" (locked (λ () (event (bytes->jsexpr (request-post-data/raw req)))))]
    [(regexp #rx"^/events/(.*)$" (list _ id)) (stream id)]
    [_ (locked page)]))

(define (url->string* req) (string-append "/" (string-join (map path/param-path (url-path (request-uri req))) "/")))

(define (page)
  (define id (number->string (random 1000000000)))
  (define s (new-session))
  (hash-set! sessions id s)
  (response/full 200 #"OK" (current-seconds) TEXT/HTML-MIME-TYPE '()
                 (list (string->bytes/utf-8 (shell id (xexprs->string (session-page s)))))))

;; {"session", "path", "event", "value"}: the patches go down the stream
(define (event e)
  (define s (hash-ref sessions (hash-ref e 'session) #f))
  (when s (step! s (hash-ref e 'path) (hash-ref e 'event) (hash-ref e 'value "")))
  (response/jsexpr '()))

;; The page's stream: server-sent events, one per batch of patches. Its timers start here.
;; ponytail: a closed tab is noticed at the next write; a session without timers stays in memory.
(define (stream id)
  (define s (hash-ref sessions id #f))
  (define out (make-async-channel))
  (if (not s)
      (response/full 204 #"No Content" (current-seconds) #f '() '())   ; gone: the browser stops asking
      (begin (locked (λ () (set-session-out! s out) (restart-timers! s)))
             (response 200 #"OK" (current-seconds) #"text/event-stream" (list (header #"Cache-Control" #"no-cache"))
                       (λ (port)
                         (with-handlers ([exn:fail? (λ (_) (locked (λ () (set-session-out! s #f) (hash-remove! sessions id))))])
                           (let loop ()
                             (define ps (for/list ([p (async-channel-get out)]) (cons (symbol->string (car p)) (cdr p))))
                             (write-string (format "data: ~a\n\n" (jsexpr->string ps)) port)
                             (flush-output port)
                             (loop))))))))

;; A session starts at init; a message updates the model, the view gives the page, diff the patches.
(define (new-session)
  (define model (init))
  (define-values (xs events) (render (view model)))
  (session model xs events (subs model) #f 0))

(define (deliver! s msg)                          ; → the patches, sent down the stream if open
  (define model (update msg (session-model s)))
  (define-values (xs events) (render (view model)))
  (define ps (diff (session-page s) xs))
  (set-session-model! s model) (set-session-page! s xs) (set-session-events! s events)
  (define new (subs model))
  (unless (equal? new (session-subs s)) (set-session-subs! s new) (restart-timers! s))
  (when (and (session-out s) (pair? ps)) (async-channel-put (session-out s) ps))
  ps)

(define (step! s path on value)                   ; an event of the page → the patches
  (define handler (findf (λ (x) (and (equal? (car x) path) (equal? (cadr x) on))) (session-events s)))
  (cond
    [(not handler) '()]                          ; the page changed meanwhile: nothing to do
    [(equal? on "input") (deliver! s (apply-value (evaluate '() (cddr handler)) (vstr value) 'e))]
    [else (deliver! s (evaluate '() (cddr handler)))]))

(define (fire! s i)                               ; the i-th subscription fires → the patches
  (if (< i (length (session-subs s))) (deliver! s (evaluate '() (cdr (list-ref (session-subs s) i)))) '()))

(define (session-every s) (map car (session-subs s)))

(define (restart-timers! s)                       ; only for an open page: tests fire by hand
  (define gen (add1 (session-gen s)))
  (set-session-gen! s gen)
  (when (session-out s)
    (for ([sub (session-subs s)] [i (in-naturals)])
      (thread (λ () (let loop ()
                      (sleep (/ (car sub) 1000))
                      (when (locked (λ () (and (= gen (session-gen s)) (session-out s) (fire! s i) #t)))
                        (loop))))))))

(define (shell id body)
  (string-append
   "<!doctype html><html><head><meta charset=\"utf-8\"><title>affresco</title>"
   "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"><style>" (ocra-css) "</style></head><body>"
   "<div id=\"affresco\" data-session=\"" id "\">" body "</div><script>" client "</script></body></html>"))

;; The browser: events up by POST, patches down by one stream, in the server's order.
;; A node's path is its child indices from the root.
(define client #<<JS
const root = document.getElementById("affresco");
const pathOf = (n) => { const p = []; while (n !== root) { p.unshift([...n.parentNode.childNodes].indexOf(n)); n = n.parentNode; } return p; };
const at = (p) => p.reduce((n, i) => n.childNodes[i], root);
const html = (s) => { const t = document.createElement("template"); t.innerHTML = s; return t.content; };
for (const type of ["click", "input"]) root.addEventListener(type, (e) => {
  const n = e.target.closest("[data-on]");
  if (!n || n.dataset.on !== type) return;
  fetch("/event", { method: "POST", body: JSON.stringify(
    { session: root.dataset.session, path: pathOf(n), event: type, value: n.value || "" }) });
});
new EventSource("/events/" + root.dataset.session).onmessage = (m) => {
  for (const [op, p, a, b] of JSON.parse(m.data)) {
    const n = at(p);
    if (op === "text") n.textContent = a;
    else if (op === "set") { n.setAttribute(a, b); if (a === "value" && n.value !== b) n.value = b; }
    else if (op === "remove") n.removeAttribute(a);
    else if (op === "replace") n.replaceWith(html(a));
    else if (op === "children") n.replaceChildren(html(a));
  }
};
JS
  )
