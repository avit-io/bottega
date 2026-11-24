#lang bottega
; The AWS vocabulary, pure: regions, accounts, ARNs and bucket names, refined literals checked
; while typing ("eu-west-1" expected as a Region). Shared by disegno (the pipeline) and
; prospettiva (the configuration).

(require "text.rkt")

; --- regions: a literal "eu-west-1" expected as a Region is checked while typing ---

(claim regions (List String))
(define regions
  (list "us-east-1" "us-east-2" "us-west-1" "us-west-2" "ca-central-1" "sa-east-1"
        "eu-west-1" "eu-west-2" "eu-west-3" "eu-central-1" "eu-central-2" "eu-north-1" "eu-south-1" "eu-south-2"
        "ap-south-1" "ap-south-2" "ap-northeast-1" "ap-northeast-2" "ap-northeast-3"
        "ap-southeast-1" "ap-southeast-2" "ap-southeast-3" "ap-east-1" "me-south-1" "me-central-1" "af-south-1"))

(claim region? (→ String Bool))
(define (region? s) (any? (same? s) regions))

(record Region
  (name String)
  (ok (So (region? name))))

; --- domain names, checked with a regular expression while typing ---

(record Bucket                             ; an S3 bucket: 3–63 characters, lowercase, digits, . and -
  (name String)
  (ok (So (re? "[a-z0-9][a-z0-9.-]{1,61}[a-z0-9]" name))))

(record Arn                                ; arn:partition:service:region:account:resource
  (raw String)
  (ok (So (re? "arn:aws(-cn|-us-gov)?:[a-z0-9-]+:([a-z]{2}(-[a-z]+)+-[0-9])?:([0-9]{12})?:.+" raw))))

(record Account                            ; an AWS account: twelve digits
  (id String)
  (ok (So (re? "[0-9]{12}" id))))
