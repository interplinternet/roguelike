#lang racket
(require rackunit rackunit/text-ui "level.rkt")

; Any -> [Posn -> Boolean]
; Curries the composition function with check-true/false for easy application. Allows you to define
; tests for shapes by composing check-true/false with the shape, instead of typing it out every time
; you can map it to a list of values.
(define check-true-shape* (curry compose check-true))
(define check-false-shape* (curry compose check-false))

(define-test-suite shapes
  ; A shape is a series of inequalities which consume a point & determine if a point exists within it
  (test-case
      "Circles"
    ; a circle centered at (10, 10) with a radius of 25 units
    (define ex-circle (circle (posn 10 10) 5))
    (define circle-true-test (check-true-shape* ex-circle))
    (define circle-false-test (check-false-shape* ex-circle))
    (andmap circle-true-test (list (posn 10 10) ;center
                                   (posn 10 15) ;top
                                   (posn 15 10) ;right
                                   (posn 5 10)));left
    (andmap circle-false-test (list (posn 20 20)
                                    (posn 15 15)))
    )
  (test-case
      "Rectangles"
    ; a rectangle with left side at x=0; top, y=5; left, x=5; bottom, y=0
    (define ex-rectangle (rectangle 0 5 5 0))
    (define rectangle-test (check-true-shape* ex-rectangle)#;(compose check-true ex-rectangle))
    (andmap rectangle-test (list (posn 0 0) (posn 5 5) (posn 0 5) (posn 5 0) (posn 2.5 2.5))))
  (test-case
      "Triangles"
    (define ex-triangle (triangle (λ (x) (+ x 2))
                                  (λ (x) (+ (- x) 2))
                                  (λ (x) 0)))
    (define triangle-true-test (check-true-shape* ex-triangle))
    (andmap triangle-true-test (list (posn 0 0) (posn -2 0) (posn 2 0) (posn 1/2 1/2))))
  #;(test-case
        "Crescents"
      #t)
  )

(define-test-suite grids
  (test-case
      "Cells"
    #t)
  )

(define-test-suite rooms
  (test-case
      "Rooms"
    #t))

(run-tests shapes)
(run-tests grids)
(run-tests rooms)
