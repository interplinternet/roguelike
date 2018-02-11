#lang racket
(require "../../fsm.rkt")
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

(define (enemy-ran? crtr)
  #true)

(define creature
  (list (transition 'wait (list (event waited-long-enough? 'wander)
                                (event found-something? 'chase)
                                (event (negate waited-long-enough?) 'wait)))
        (transition 'wander (list (event waited-long-enough? 'wander)
                                  (event found-something? 'chase)
                                  (event (negate waited-long-enough?) 'wait)))
        (transition 'chase (list (event enemy-lost? 'wait)
                                 (event enemy-in-range? 'fight)))
        (transition 'fight (list (event enemy-ran? 'chase)
                                 (event hurt-badly? 'flee)))
        (transition 'flee (list (event fleed-long-enough? 'wait)))))