#lang racket
(require automata/dfa)
(provide (all-defined-out))

;; These are placeholders for now.
(define (waited-long-enough? crtr)
    #true)

(define (heard-something? crtr)
    #true)

(define (found-something? crtr)
    #true)

(define (investigated-long-enough? crtr)
    #true)

(define (enemy-lost? crtr)
    #true)

(define (enemy-in-range? crtr)
    #true)

(define (hurt-badly? crtr)
    #true)

(define (fleed-long-enough? crtr)
    #true)

(define creature
    (dfa 'wait
    ['wait ([(waited-long-enough?) 'wander]
            [(heard-something?) 'investigate]
            [(not (waited-long-enough?)) 'wait])]
    ['investigate ([(investigated-long-enough?) 'wait]
                   [(found-something?) 'chase]
                   [(investigated-long-enough?) 'wander])]
    ['chase ([(enemy-lost?) 'investigate]
             [(enemy-in-range?) 'fight])]
    ['fight ([(enemy-ran?) 'chase]
             [(hurt-badly?) 'flee])]
    ['flee ([(fleed-long-enough?) 'wait])]))
