#lang racket
(provide transition event next-state find-event)
;-----------------------------------------------------------------------------------------------------
#| DATA |#
;-----------------------------------------------------------------------------------------------------
; A FSM should take a starting state and a series of state transitions and give you the next state
; and the same series of transitions.

; A State Transition Table is
; [Listof Transition]
; where
; Transition := [struct State Events]
; State := Symbol
; Events := [struct Condition State]
; Condition := [Any -> Boolean]

(struct transition [state events])
(struct event [condition state])

; [Symbol -> [Symbol -> Boolean]]
(define (state-is a-state)
  (λ (new-state) (symbol=? a-state new-state)))

(define extransition1
  (transition 'state1 (list (event (const #t) 'state2)
                            (event (const #f) 'state1))))
(define extransition2
  (transition 'state2 (list (event (const #t) 'state1))))

(define extable
  (list extransition1 extransition2))

;-----------------------------------------------------------------------------------------------------
#| FUNCTIONS |#
;-----------------------------------------------------------------------------------------------------
; State Any State-Table -> [Maybe State]
(define (next-state start input table)
  (cond
    [(empty? table) #f]
    [(symbol=? start (transition-state (first table)))
     (define MaybeTransition (find-event input (transition-events (first table))))
     (if MaybeTransition
         MaybeTransition
         (next-state start input (rest table)))]
    [else (next-state start input (rest table))]))

; Condition Events -> [Maybe State]
(define (find-event input evts)
  (cond
    [(empty? evts) #f]
    [((event-condition (first evts)) input) (event-state (first evts))]
    [else (find-event input (rest evts))]))
;-----------------------------------------------------------------------------------------------------
#| TEST |#
;-----------------------------------------------------------------------------------------------------
;(equal? (next-state 'state1 'true extable) 'state2)
;(equal? (next-state 'state2 'true extable) 'state1)