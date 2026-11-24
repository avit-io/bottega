<p align="center"><img src="logo.svg" width="200" alt="Bottega: una sfera in chiaroscuro con la sua ombra, a puntini"></p>

# Bottega

Un nucleo, tre dialetti. Bottega è un linguaggio totale con tipi dipendenti, scritto in Racket,
sopra cui si costruiscono linguaggi per casi d'uso diversi. Ognuno dichiara un piano come dato,
il type checker ne prova le proprietà, e un realizzatore che esiste già lo esegue.

| `#lang` | Che cosa dichiara | Chi lo realizza | Stato |
|---|---|---|---|
| `bottega` | la logica: Π, Σ, ≡, `data`, `record`, match dipendente, `auto` | — | fatto |
| `disegno` | la pipeline: il progetto, non l'esecuzione | Dagger (1.0 beta.28) | piano e run; resoconto e AWS in corso |
| `prospettiva` | un modello, molti ambienti come proiezioni; i teoremi sono le invarianti | Nix | da fare |
| `affresco` | l'architettura di Elm, totale, lato server | un runtime Racket | da fare |

Il nome viene dalla bottega rinascimentale: il luogo dove il disegno diventa opera, e dove
l'apprendista cominciava disegnando una sfera, con la luce, l'ombra e l'ombra portata. È il logo,
a puntini (`racket tools/logo.rkt > logo.svg`).

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
```

Per imparare il linguaggio c'è il libro in `book/`, nove capitoli sui tipi dipendenti, con le
soluzioni degli esercizi.

```
bottega/core/   il nucleo: superficie, elaborazione, kernel, definizioni
bottega/lib/    la libreria standard: prelude, text, affine, paths, time, net
disegno/        il dialetto: piano, runner, sessione, generatore; disegno/lib la libreria Dagger
book/           il libro          cookbook/  le ricette di disegno
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
