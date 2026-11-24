#lang prospettiva
; Un servizio, quattro ambienti: un modello solo, e ogni ambiente ne è una proiezione.
;
; Il servizio dice com'è fatto una volta (la porta, le repliche); l'ambiente aggiunge dove gira.
; I teoremi sono le invarianti della proiezione: che cosa resta uguale passando da un ambiente
; all'altro, e che cosa una piattaforma non ammette. Si provano compilando, per ogni ambiente.
;
;   racket cookbook/prospettiva/service.rkt show qa         il YAML di qa
;   racket cookbook/prospettiva/service.rkt render out      out/<ambiente>/values.yaml
;   nix build .#example                                     lo stesso, come derivazione

(require bottega/lib/text bottega/lib/net bottega/lib/aws)

(data Env (dev) (test) (qa) (prod))

; Dove gira: un cluster con il chart nuovo, che rispetta containerPort, o uno legacy, dove la
; porta 80 è scritta nei manifest.
(data Platform (chart) (legacy))

(record Cluster
  (name String)
  (platform Platform))

(record Replicas
  (min Nat)
  (max Nat)
  (ok (So (≤? min max))))

(record Var
  (name String)
  (value String))

(record Values
  (stage Env)
  (region Region)
  (containerPort Port)
  (cluster Cluster)
  (replicas Replicas)
  (hostname String)
  (env (List Var)))

; --- il modello ---

(claim cluster-of (→ Env Cluster))
(define (cluster-of e)
  (match e
    (dev  (cluster "eks-blue" chart))
    (test (cluster "eks-blue" chart))
    (qa   (cluster "eks-legacy" legacy))
    (prod (cluster "eks-legacy" legacy))))

(claim port-of (→ Platform Port))                  ; il runtime nonroot vuole una porta alta
(define (port-of p)
  (match p
    (chart  8080)
    (legacy 80)))

(claim replicas-of (→ Env Replicas))
(define (replicas-of e)
  (match e
    (prod (replicas 3 10 auto))
    (_    (replicas 1 3 auto))))

(claim main (Π ((e Env)) Values))
(define (main e)
  (values e "eu-west-1" (port-of (Cluster.platform (cluster-of e))) (cluster-of e) (replicas-of e)
          (string-append (Env.name e) "-service.example.com")
          (list (var "ENVIRONMENT" (Env.name e)) (var "AWS_REGION" "eu-west-1"))))

; --- le invarianti della proiezione ---

; Su un cluster legacy la porta è 80, in ogni ambiente.
(claim legacy-80 (Π ((e Env))
                   (So (or (same? (Platform.name (Cluster.platform (Values.cluster (main e)))) "chart")
                           (nat=? (Port.n (Values.containerPort (main e))) 80)))))
(define (legacy-80 e)
  (match e (dev auto) (test auto) (qa auto) (prod auto)))

; qa è una prova di prod salvo la taglia: stesso cluster, quindi stessa porta.
(claim qa≈prod (≡ (Values.containerPort (main qa)) (Values.containerPort (main prod))))
(define qa≈prod auto)
