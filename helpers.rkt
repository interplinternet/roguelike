#lang racket
(provide (all-defined-out))
;;---------------------------------------------------------------------------------------------------
#| Helpers |#
; Any -> Symbol
;(define dummy (const 'dummy))

; [X -> Y] X Number -> Y
(define (self-apply function initial-input times-to-apply)
  (for/fold ([base initial-input])
            ([n (in-range times-to-apply)])
    (function base)))
#|
; [X Y -> Z] [N -> M] -> Z
(define ((hook1 f1 f2) arg)
  (f1 arg (f2 arg)))

(define ((hook f1 f2) . args)
  (apply f1 (append args ; previously cons (apply f2 args) args, but I like right->left
                    (list (apply f2 args)))))

(define ((hook/dyadic f1 f2) arg1 arg2) ; how could I make this variadic?
  (f1 arg1 (f2 arg2)))

(define ((fork head . fns) arg)
  (apply head (map (λ (fn) (fn arg)) fns)))

(define ((fork1 f1 f2 f3) arg)
  (f1 (f2 arg) (f3 arg)))|#

; List -> X
(define/contract (select-random l)
  ((listof any/c) . -> . any/c)
  (list-ref l (random (length l))))

;[Listof [Listof Any]] [Any -> Any] -> [Listof [Listof Any]]
(define (deep-map lol f)
  (cond
    [(empty? lol) '()]
    [(cons? (first lol))
     (cons (deep-map (first lol) f)
           (deep-map (rest lol) f))]
    [else (cons (f (first lol))
                (deep-map (rest lol) f))]))

; hey here's an idea, what about "implicit cut" macro? similar to scala fancy app for racket, but
; using an already established srfi
(define (star-it fn args) (apply fn args))