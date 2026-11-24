#lang disegno
; Refined values for the plan: the literal stays, the type changes. engine.rkt uses them.

(require bottega/lib/text bottega/lib/paths bottega/lib/net)

(extern image-ref? : (→ String Bool) %image-ref?)

; --- refined values: the literal stays, the type changes ---
; A record (value, proof) takes a literal: "golang:1.25-alpine" expected as an Image becomes
; (image "golang:1.25-alpine" auto). If it is malformed it is a type error, not a container one.

(record Image
  (raw String)
  (ok (So (image-ref? raw))))

; The command line stays the chisel: the values interpolated into it are typed, not its syntax.

(data Arg
  (arg-text String)
  (arg-path Abs))
