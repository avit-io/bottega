<p align="center"><img src="logo.svg" width="200" alt="Bottega: una sfera in chiaroscuro su un pavimento in prospettiva, a puntini"></p>

# Bottega

Un nucleo, tre dialetti. Bottega è un linguaggio totale con tipi dipendenti, scritto in Racket,
sopra cui si costruiscono linguaggi per casi d'uso diversi. Ognuno dichiara un piano come dato,
il type checker ne prova le proprietà, e un realizzatore che esiste già lo esegue.

| `#lang` | Che cosa dichiara | Chi lo realizza | Stato |
|---|---|---|---|
| `bottega` | la logica: Π, Σ, ≡, `data`, `record`, match dipendente, `auto` | — | fatto |
| `disegno` | la pipeline: il progetto, non l'esecuzione | Dagger (1.0 beta.28) | piano e run, AWS, git, ArgoCD; parità con Chez anche su be-tsid |
| `prospettiva` | un modello, molti ambienti come proiezioni; i teoremi sono le invarianti | Nix (flake, derivazioni) | values.yaml per ambiente |
| `affresco` | l'architettura di Elm, totale, lato server | un runtime Racket | da fare |

Il nome viene dalla bottega rinascimentale: il luogo dove il disegno diventa opera. L'apprendista
cominciava da due lezioni, una sfera con la luce, l'ombra e l'ombra portata, e un pavimento a
scacchi in prospettiva, le cui righe vanno tutte a un punto solo, come in Alberti. Sono il
disegno e la prospettiva, e insieme fanno il logo, a puntini (`racket tools/logo.rkt > logo.svg`).

```racket
#lang bottega
(require bottega/lib/paths)

(claim +-zero (Π ((n Nat)) (≡ (+ n 0) n)))
(define (+-zero n)
  (match n
    (zero     refl)
    ((add1 k) (cong add1 (+-zero k)))))

; i punti non si sommano: /out è un punto, "bin/hello" uno spostamento
(claim dentro (≡ (⊕ /out "bin/hello") /out/bin/hello))
(define dentro auto)
```

- **Scheme con i tipi**, nella forma di Pie (*The Little Typer*): `(claim nome tipo)`, poi
  `(define (nome x …) corpo)`. Solo tonde.
- **Totale**: ricorsione strutturale, copertura dei casi, positività stretta, universi stratificati.
  Un kernel ricontrolla ogni termine.
- **Si controlla compilando**: un errore ferma `raco make` alla sua riga, e DrRacket o
  racket-langserver lo mostrano nell'editor, con il tipo di ogni dichiarazione al passaggio del mouse.
- **`TODO`**, come in Pie: un buco nel corpo; il checker stampa il contesto e l'obiettivo.
- **Librerie curate**: `extern` e `builtin` stanno solo in `bottega/lib`; un programma le richiede
  con `require`.

## Uso

Serve [Nix](https://nixos.org) con i flake.

```
nix develop                      # Racket, con il repo come collection bottega
raco make book/03-functions.rkt  # controlla un modulo
raco test tests/                 # i test e i teoremi delle librerie

racket cookbook/02-go-build.rkt plan                                  # il piano, in GraphQL
dagger api with-session racket cookbook/02-go-build.rkt run           # l'opera, su Dagger

racket cookbook/prospettiva/service.rkt show qa                       # una proiezione, in YAML
nix build .#example                                                   # tutte, come derivazione
nix flake check                                                       # i teoremi e i test
```

Per imparare il linguaggio c'è il libro in `book/`, nove capitoli sui tipi dipendenti, con le
soluzioni degli esercizi.

```
bottega/core/   il nucleo: superficie, elaborazione, kernel, definizioni
bottega/lib/    la libreria standard: prelude, text, affine, paths, time, net, aws
disegno/        il dialetto: piano, runner, sessione, generatore; disegno/lib le librerie: dagger, files, delivery, git, argocd, aws
prospettiva/    il dialetto dei modelli: proiezioni e YAML
book/           il libro          cookbook/  le ricette; prospettiva/ il modello d’esempio e il template
tests/          i test e i golden  tools/     il logo
```

Bottega nasce da [Disegno](https://github.com/avit-io/disegno), un linguaggio per la CI scritto in
Chez Scheme: il nucleo è lo stesso, ora comune a tutti i dialetti.

### Dietro un proxy TLS

Dietro un proxy che ispeziona il TLS (Netskope, per esempio) il motore Dagger avviato dalla CLI
non riesce a scaricare le immagini. `nix develop` avvia allora un motore suo,
`bottega-engine-v<versione>`, con il bundle CA dell'host montato: il motore installa le CA e le
passa ai container che esegue. Il bundle si sceglie con `BOTTEGA_HOST_CA` (un percorso, o `none`);
di default è quello di sistema.

### Un modello in un flake

Un repository di modelli parte dal template, e Nix costruisce le proiezioni senza IFD:

```
nix flake init -t github:avit-io/bottega#prospettiva
nix build          # result/<ambiente>/values.yaml
nix flake check    # i tipi e i teoremi del modello
```

Nel `flake.nix` è una riga: `bottega.lib.prospettiva { src = ./.; model = "service.rkt"; }`.

Il YAML viene dalla forma normale, in un modo solo: un record è una mappa con i nomi dei campi,
un'enumerazione il nome del costruttore, un altro costruttore la mappa dei suoi argomenti, un
letterale raffinato (`Port`, `Region`, `Arn`) il suo testo, le prove spariscono. Un campo
`(Maybe T)` che vale `nothing` è una chiave che non c'è.
