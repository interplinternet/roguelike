#lang racket
(require "../data.rkt")
(provide (all-defined-out))
;;---------------------------------------------------------------------------------------------------
#| Room shapes |#
; A room's shape can be represented as a series of inequality functions. Each shape consumes a point
; and determines whether that point is within the shape.
(define max-radius 10)
(define max-width  10)
(define max-height 10)
(define random1 (curry random 1)) ; 1 <= random-number <= given-number
(define half-of (curryr / 2))
(define MIN-WIDTH 4)
(define MIN-HEIGHT 4)

; A crescent is all points within a function with a center at (h, k) and a radius of 5, except for
; those points which are to the right of...?
(define/match ((crescent center rad) pos)
  [((posn h k) radius (posn x y))
   (and
    (<=
     (+ (sqr (- x h)) (sqr (- y k)))
     (sqr radius))
    (not (<= (+ (sqr y) x) center)))])

; Posn [Posn Posn -> Boolean] -> [Posn -> Boolean]
; "A shape is a central point and a series of inequalities describing its boundaries."
; how do we describe a dimension? How do we combine a number of dimensions with the appropriate
; inequalities? 
; each inequality is a function taking an x- or y- value to calculate
; slot the x- and y- values into each inequality and determine if it's true or not.
; A line is an inequality which determines whether a point is on some side of it.

; [Number Number -> Boolean] [Number -> Boolean] -> Posn -> Boolean
(define ((line oper function) point)
  (oper (posn-y point) (function (posn-x point))))
; ((hook/dyadic oper function) posn-y posn-x)

; [Listof [Number Number -> Boolean]] -> [Posn -> Boolean] A shape consumes a number of inequalities
; representing lines of a geometric figure. It conjoins all functions given, then applies them to the
; x- and y-points of a posn.
(define ((shape . lines) point)
  [(apply conjoin lines) (posn-x point) (posn-y point)])

; Number Number Number Number -> [Posn -> Boolean]
(define (rectangle/g left top right bottom)
  (shape (λ (x y) (> right x left))
         (λ (x y) (> bottom y top)))); and the "bottom" is the maximum

; -> [Posn -> Boolean]
(define (random-rectangle)
  ; sometimes creates rectangles that don't display.
  (define left  (random (- WIDTH ROOM-WIDTH))); 0 < l < 18 ; move these outside the function for debugging
  (define right (random (+ left MIN-WIDTH) (+ left ROOM-WIDTH))); 0 < r < l+2
  (define top   (random (- HEIGHT ROOM-HEIGHT))); 0 < h < 18
  (define bottom(random (+ top MIN-HEIGHT) (+ top ROOM-HEIGHT))); 0 < b < h+2
  ; - IN
  (rectangle/g left top right bottom))

; Posn Number -> [Posn -> Boolean]
(define/match (circle/g center rad)
  [((posn h k) rad)
   (shape (λ (x y) (<= (+ (sqr (- x h)) (sqr (- y k))) (sqr rad))))])

; -> [Posn -> Boolean]
(define (random-circle)
  (define radians (random1 (round (/ ROOM-WIDTH 2))))
  (define center (posn (random radians (- WIDTH radians)) (random radians (- WIDTH radians))))
  (circle/g center radians))
