(require racket/gui/base) ; to have TV
(require plot/no-gui) ; to have plot/dc
;(require racket/draw) ; to draw
;(require racket/math) ; to draw arc
;(require math) ; to have mean
(require math/base) ; to have sum
(plot-new-window? #t)

(define-struct automaton (init-claim cc cd dc dd))

;; p=1: cooperate
;; good guys
(define all-Cs (make-automaton 1 1 1 1 1))
(define tit-for-tat (make-automaton 1 1 0 1 0))
(define grim-trigger (make-automaton 1 1 0 0 0))
(define alternater (make-automaton 1 1 0 1 1))
(define good-pavlov (make-automaton 1 1 0 0 1))

;; bad guys
(define pavlov (make-automaton 0 1 0 0 1))
(define cautious-tft (make-automaton 0 1 0 1 0))
(define all-Ds (make-automaton 0 0 0 0 0))
;; bad guys play bad among themselves
;; bully all-Cs --
; alternate with tit for tat and pavlov
; but just like cautious-tft, defect among themselves =.=
(define bully (make-automaton 0 0 0 1 0))
(define mild-bully (make-automaton 1 0 0 1 0)) ; tri dc cautious-tft

;; those accomodate with all-highs but do better among themselves
;; than bully above
(define coward-bully (make-automaton 1 0 0 1 1))
(define coward-tough-bully (make-automaton 0 0 0 1 1))

(define contestants
  (list
   all-Ds pavlov bully mild-bully coward-bully coward-tough-bully
   all-Cs tit-for-tat cautious-tft alternater good-pavlov))
(define (make-population type-list type-number)
  (shuffle
   (flatten
    (for/list ([i (length type-list)])
      (make-list (list-ref type-number i) (list-ref type-list i))))))


(define (contest automaton contestant-list)
  (map (lambda (x) (take-sums (match-pair (list automaton x) 200)))
       contestant-list))

(define (identify automaton)
  (map (lambda (f) (f automaton))
       (list
        automaton-init-claim
        automaton-cc
        automaton-cd
        automaton-dc
        automaton-dd)))

(define (next-claim automaton previous-claims)
  (let ([look-up
         (cond
          [(equal? previous-claims '(1 1)) automaton-cc]
          [(equal? previous-claims '(1 0)) automaton-cd]
          [(equal? previous-claims '(0 1)) automaton-dc]
          [(equal? previous-claims '(0 0)) automaton-dd])])
    (look-up automaton)))

;; R = 3
;; S = 0
;; T = 5
;; P = 1

;; C 3 0
;; D 6 1

(define (match-claims claims)
  (cond [(equal? claims '(1 1)) (list 3 3)]
        [(equal? claims '(1 0)) (list 0 6)]
        [(equal? claims '(0 1)) (list 6 0)]
        [(equal? claims '(0 0)) (list 1 1)]))

(define (match-pair* au1 au2 results previous-claims countdown)
  (if (zero? countdown)
      results
      (match-pair* au1 au2
                   (append results (list (match-claims previous-claims)))
                   (list (next-claim au1 previous-claims)
                         (next-claim au2 (reverse previous-claims)))
                   (sub1 countdown))))

(define (match-pair automaton-pair rounds-per-match)
  (match-pair* (first automaton-pair)
               (second automaton-pair)
               '()
               (map automaton-init-claim automaton-pair)
               rounds-per-match))

(define (base10->base2 n)
  (~r n #:base 2 #:min-width 5 #:pad-string "0"))

(define (char->digit c)
  (case c
    [(#\0) 0]
    [(#\1) 1]))

(define (base2->digits a-string)
  (map char->digit (string->list a-string)))

(define (number->automaton n)
  (apply make-automaton (base2->digits (base10->base2 n))))

(define (take-sums round-results)
  (map (lambda (f) (sum (map f round-results)))
       (list first second)))

(define (mass-produce p1 p2)
  (for/list ([n (in-range p1 (add1 p2))])
    (number->automaton n)))

(define (match-population population rounds-per-match)
  (for/list ([i (/ (length population)
                   2)])
    (take-sums
     (match-pair (list
                  (list-ref population (* 2 i))
                  (list-ref population (add1 (* 2 i))))
                 rounds-per-match))))

(define (reductions-h f accumulated init a-list)
  (if (null? a-list)
      accumulated
      (let ((next-init (f init (first a-list))))
        (reductions-h f
                      (append accumulated (list next-init))
                      next-init
                      (rest a-list)))))
(define (reductions f init a-list)
  (if (null? a-list)
      accumulated
      (reductions-h f '() init a-list)))
(define (reductions* f a-list)
  (let ([init (first a-list)])
    (reductions-h f (list init) init (rest a-list))))

(define (accumulate a-list)
  (reductions* + (cons 0 a-list)))

(define (payoff-percentages payoff-list)
  (let ([s (sum payoff-list)]
        [l (length payoff-list)])
    (for/list ([i l])
      (/ (list-ref payoff-list i)
         s))))

(define (accumulated-fitness population rounds-per-match)
  (accumulate
   (payoff-percentages
    (flatten
     (match-population population rounds-per-match)))))

(define (randomise-over-fitness accumulated-payoff-percentage population speed)
  (let ([l (length population)])
    (for/list
        ([n speed])
      (let ([r (random)])
        (for/and ([i l])
          #:break (< r (list-ref accumulated-payoff-percentage i))
          (list-ref population i))))))

(define (randomisation-test an-accumulated-list)
  (let ([l (length an-accumulated-list)])
    (for/list
        ([n 20])
      (let ([r (random)])
        (for/and ([i (sub1 l)])
          #:break (< r (list-ref an-accumulated-list i))
          i)))))

;; create population
(define (random-population* n-automata-per-type types)
  (shuffle
   (flatten
    (for/list ([i types])
      (make-list n-automata-per-type i)))))

(define (random-population n-automata-per-type n-types)
  (random-population*
   n-automata-per-type
   (for/list ([i n-types])
     (number->automaton (random 32)))
   ))

;; COUNT TYPES

(define (automaton->number automaton)
  (string->number
   (apply string-append (map number->string automaton))
   2))

(define (scan population)
  (foldl
   (lambda (au h)
     (hash-update h au add1 0))
   (hash)
   population))

(define (scan-identify population)
  (foldl
   (lambda (au h)
     (hash-update h (identify au) add1 0))
   (hash)
   population))
;; scan : keys are unique, same clothes -> different guy
;; scan-identify: same clothes -> same guy

(define (rank a-hash)
  (sort (hash->list a-hash) #:key cdr >))

(define (n->xn n)
  (string->symbol
   (string-append "x" (number->string n))))

(define (top t a-hash)
  (let* ([top-list (map car (take (rank a-hash) t))]
         [l (length top-list)])
    (for/list ([i l])
      (eval
       (list 'define (n->xn i)
             (list-ref top-list i))))))

(define population-mean (list 0))
(define payoff-space (list 0))

(define (rank-payoff criterion population rounds-per-match)
  (let ([payoff-list (flatten (match-population population rounds-per-match))])
    (sort (hash->list (scan payoff-list)) #:key criterion >)))

(define (evolve population cycles speed rounds-per-match)
  (let* ([l (length population)]
         [round-results (match-population population rounds-per-match)]
         [payoff-list (flatten round-results)]
         [accum-fitness (accumulate (payoff-percentages payoff-list))]
         [survivors (drop population speed)]
         [successors
          (randomise-over-fitness accum-fitness population speed)]
         [new-population (shuffle (append survivors successors))]
         )
    ;(set! series (append series (list (identify-2-types new-population))))
    (set! population-mean
          (append population-mean (list
                                   (exact->inexact
                                    (/ (sum payoff-list)
                                       (* l rounds-per-match))))))
    (if (zero? cycles)
        (begin
          (set! payoff-space round-results)
          population)
        (evolve new-population (sub1 cycles) speed rounds-per-match)
        )))

;; TV
(define dynamic-frame (new frame%
                           [label "replicator dynamic"]
                           [width 400]
                           [height 400]))
(define dynamic-canvas (new canvas%
                            [parent dynamic-frame]))
(define dynamic-dc (send dynamic-canvas get-dc))
(define (plot-dynamic data)
  (plot/dc (lines data
                  #:x-min 0 #:x-max 1000
                  #:y-min 0 #:y-max 1000)
           dynamic-dc
           0 0 400 400))

(define (plot-mean data)
  (let* ([l (length data)]
         [coors (map list
                     (build-list l values)
                     data)])
    (plot/dc (lines coors
                    #:x-min 0 #:x-max l
                    #:y-min 0 #:y-max 7)
             dynamic-dc
             0 0 400 400)))

(define (plot-payoff-space pay-list)
  (plot/dc (points pay-list
                   #:x-min 0 #:x-max 320
                   #:y-min 0 #:y-max 320)
           dynamic-dc
           0 0
           400 400))

;; data:
;; '((1 2..)
;;   (2 3..))
;; if needed, map list data..
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'replace))
  (write-table data out)
  (close-output-port out))
