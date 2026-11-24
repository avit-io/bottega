#lang racket/base
;; A plan's chain as GraphQL. A chain is a list of fields, from the root to the leaf:
;;   ((container) (from (address "alpine")) (stdout))
;; A field is (name (argument value) …); a value is a string, a number, a boolean, a list, or
;; an input object (input (name . value) …).

(require racket/string json)
(provide chain->graphql)

(define (value->graphql v)
  (cond [(string? v) (jsexpr->string v)]
        [(boolean? v) (if v "true" "false")]
        [(and (pair? v) (eq? (car v) 'input))
         (format "{~a}" (string-join (for/list ([f (cdr v)]) (format "~a: ~a" (car f) (value->graphql (cdr f)))) ", "))]
        [(list? v) (format "[~a]" (string-join (map value->graphql v) ", "))]
        [else (format "~a" v)]))

(define (field->graphql f)
  (if (null? (cdr f))
      (symbol->string (car f))
      (format "~a(~a)" (car f)
              (string-join (for/list ([a (cdr f)]) (format "~a: ~a" (car a) (value->graphql (cadr a)))) ", "))))

(define (chain->graphql chain)
  (if (null? (cdr chain))
      (field->graphql (car chain))
      (format "~a { ~a }" (field->graphql (car chain)) (chain->graphql (cdr chain)))))

(module+ test
  (require rackunit)
  (check-equal? (chain->graphql '((container) (from (address "alpine")) (withExec (args ("echo" "hello"))) (stdout)))
                "container { from(address: \"alpine\") { withExec(args: [\"echo\", \"hello\"]) { stdout } } }")
  (check-equal? (value->graphql '(input (name . "A") (value . "b"))) "{name: \"A\", value: \"b\"}"))
