# iterated-prisoners-dilemma

deterministic machines

- stateless machine

- fitness evolution

- code with struct (not vector-set! style)

# how to run

open Racket

```
(load "ipd.rkt")
(define A (random-population 10 100)) ; 10 auto x 100 types (there are only 32 types -> these types will duplicate)
(define A1 (evolve 300 50 50)) ; 300 cycles 50 speed 50 rounds per match
(send dynamic-frame show #t)
(plot-mean population-mean)
(scan-identify A1)
```
