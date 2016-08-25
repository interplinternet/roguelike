#lang racket
(require racket/shared pict racket/trace)
(provide (all-defined-out))
;;---------------------------------------------------------------------------------------------------
#| Data |#
; Grid := [Listof Cell]
; Cell := (cell Posn)
; Where Posn is the anchor of the cell, in the upper left corner
(struct cell (anchor) #:transparent) 

; A room is:
; (struct [Number -> Any] Number)
(struct posn [x y] #:transparent)

; A Room is:
(struct room [name function center neighbors] #:transparent)
; (room Symbol [Posn -> Any] Posn [Listof Symbol])
; The function determines the shape of the room, and the center is the location of its central point
; in the level. Neighbors field is a list of symbols representing the names of all of its neighbors.
; -- A room is really a node in a graph.

; Symbol -> Number
; takes a symbol representing a cardinal direction and returns the appropriate index of a vector.
(define/match (dir->pos direction)
  [('north) 0]
  [('east)  1]
  [('south) 2]
  [('west)  3])

; The neighbors of a room are represented with a single vector, which we treat as a structure.
(define-values (neighbor neighbor-north neighbor-east neighbor-south neighbor-west)
  (values (λ (north east south west) (vector north east south west))
          (λ (a-vector) (vector-ref a-vector 0))
          (λ (a-vector) (vector-ref a-vector 1))
          (λ (a-vector) (vector-ref a-vector 2))
          (λ (a-vector) (vector-ref a-vector 3))))

; Functional setting, updating, and an empty neighborhood.
(define-values (neighbor-set neighbor-update empty-neighborhood)
  (values (λ (vec direction val)
            (define pos (dir->pos direction))
            (for/vector ([(elem indx) (in-indexed (in-vector vec))])
              (if (= indx pos) val elem)))
          (λ (vec direction updater)
            (define pos (dir->pos direction))
            (for/vector ([(elem indx) (in-indexed (in-vector vec))])
              (if (= indx pos) (updater elem) elem)))
          #(() () () ())))

; A Level is [Listof Room]
; A Room is (room Symbol [X -> Y] Posn Level)

;;---------------------------------------------------------------------------------------------------
#| Helpers |#
; Any -> Symbol
(define dummy (const 'dummy))

; [X -> Y] X Number -> Y
(define (self-apply function initial-input times-to-apply)
  (for/fold ([base initial-input])
            ([n (in-range times-to-apply)])
    (function base)))

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
  (f1 (f2 arg) (f3 arg)))

;;---------------------------------------------------------------------------------------------------
#| CONSTANTS |#
(define WIDTH 24) ; the width of a level is 100 logical cells
(define HEIGHT WIDTH)
(define ROOM-WIDTH (/ WIDTH 4)) ; a room can be no larger than a quarter of the map
(define ROOM-HEIGHT ROOM-WIDTH)
(define CELL 100) ; a cell is 100 real units wide/high
(define BLANK-POSN (posn 0 0))
(define BLANK-NEIGHBORS (neighbor '() '() '() '()))

;;---------------------------------------------------------------------------------------------------
#| Functions |#
; [Listof Room] -> [Listof Room]
(define (new-room list-of-room)
  ; To add a new room, we select a random one from a list of valid rooms. Then we cons it onto the
  ; list of rooms. The new room lists its previous room as a neighbor.
  (define prev-room (first list-of-room))
  (define new-name (gensym))
  (define random-slot (select-random (valid-slots prev-room)));-> North, east, south, west
  ;we only pass the name of the previous room, not the entire previous room for now.
  (define new-room (make-room random-slot new-name (room-name prev-room)))
  (cons new-room (cons
                  (add-new-neighbor prev-room random-slot new-room)
                  (rest list-of-room))))

; Room -> [Listof Room]
; We take a room's shape and center, and remove all directions which would lead outside the bounds.
(define (valid-slots a-room)
  (define nbors (room-neighbors a-room))
  (match nbors
    [(vector n e s w) (list n e s w)]))

; [Listof Room] -> Room
(define (select-random nbors)
  (match (random (sub1 (length nbors)))
    [0 'north]
    [1 'east]
    [2 'south]
    [3 'west]))

; Slot Symbol Room -> Room
; consumes a slot, a name, and the last room made.
; Creates a new room with the name of the previous room in the appropriate slot. E.g., if our new room
; is north of our previous one, attach the previous to the south slot.
(define (make-room a-slot name prev-name)
  (room name
        (random-shape)
        (λ (width height) (posn width height))
        (neighbor-set empty-neighborhood a-slot (list prev-name))))

; Room Slot Room -> Room
; adds a new neighbor to a room.
(define (add-new-neighbor prev-room a-slot new-room)
  (match prev-room
    [(room name shape center neighbors)
     (room name shape center (neighbor-update neighbors a-slot (curry cons (room-name new-room))))]))

;;---------------------------------------------------------------------------------------------------
#| Room shapes |#
; A room's shape can be represented as a series of inequality functions. Each shape consumes a point
; and determines whether that point is within the shape.

(define max-radius 10)
(define max-width  10)
(define max-height 10)
(define random1 (curry random 1)) ; 1 <= random-number <= given-number
(define half-of (curryr / 2))

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

; [Listof [Number Number -> Boolean]] -> [Posn -> Boolean]
; A shape consumes a number of inequalities representing lines of a geometric figure. It conjoins all
; functions given, then applies them to the x- and y-points of a posn.
(define ((shape . lines) point)
  [(apply conjoin lines) (posn-x point) (posn-y point)])

; Number Number Number Number -> [Posn -> Boolean]
(define (rectangle/g left top right bottom)
  (shape (λ (x y) (>= x left))
         (λ (x y) (>= y top)) ; the "top" of a screen in Racket is 0,0
         (λ (x y) (<= x right))
         (λ (x y) (<= y bottom)))); and the "bottom" is the maximum

; -> [Posn -> Boolean]
(define (random-rectangle)
  (let* ([left (random (- WIDTH ROOM-WIDTH))]
         [right (random (add1 left) WIDTH)]
         [top (random (- HEIGHT ROOM-HEIGHT))]
         [bottom (random (add1 top) HEIGHT)])
    (rectangle/g left right top bottom)))

; Posn Number -> [Posn -> Boolean]
(define/match (circle/g center rad)
  [((posn h k) rad)
   (shape (λ (x y) (<= (+ (sqr (- x h)) (sqr (- y k))) (sqr rad))))])

; -> [Posn -> Boolean]
(define (random-circle)
  (define radians (random1 (/ ROOM-WIDTH 2)))
  (define center (posn (random radians (- WIDTH radians)) (random radians (- WIDTH radians))))
  (circle/g center radians))

;;---------------------------------------------------------------------------------------------------
#| Grid Creation |#
; In racket, the origin is at the upper left and not the center.

; Level -> [List Grid Grid]
; Consumes a representation of a level, returns two grids: one containing all "empty" cells, and one
; containing all "wall" cells, or those which are not in a room.
(define (make-grid level)
  (define initial-grid (blank-grid WIDTH HEIGHT))
  ; - IN -
  (partition (λ (a-cell) ; there must be a cleaner way of describing this.
               (for/and ([a-room level])
                 (cell-fits? a-cell a-room)))
             initial-grid))

; Cell Room -> Boolean
(define (cell-fits? a-cell a-room)
  (define the-shape (room-function a-room))
  (the-shape (cell-anchor a-cell)))

; Number Number -> Grid
; Consumes 2 numbers representing the number of logical cells each row and column should have.
(define (blank-grid w h)
  (for*/list ([y (in-range h)]
              [x (in-range w)])
    (cell (posn x y))))

;-> [X -> Y]
; selects a random shape with random parameters within certain constraints.
(define (random-shape)
  (define list-of-shapes (list random-circle random-rectangle))
  [(list-ref list-of-shapes (random (length list-of-shapes)))])

; Number -> Level
; Generates a level containing Number amount of rooms.
(define (gen-level number-of-rooms)
  (self-apply new-room (list (room (gensym) (random-shape) BLANK-POSN empty-neighborhood))
              number-of-rooms))

;;---------------------------------------------------------------------------------------------------
#| Rendering |#

; Level -> Image
; To draw a level, we center its shape at its center location. Then, for every neighbor
; it has we overlay a line to them and recurse on each. Maybe I can use pict-finders?
(define (draw-level level)
  (cond
    [(empty? level) (blank WIDTH)]
    [else
     (match-define (room name shape center neighbors) (first level))
     (pin-over (draw-level (rest level))
               (posn-x center)
               (posn-y center)
               (circle 10))]))

; To draw a grid, first generate a blank grid. Then take every cell in the grid and color it
; according to whether it is a member of a room in the level or not. If it is, then it's white.
; If it's not, then it's black. Append each cell in a row horizontally, append each row vertically.

; hey here's an idea, what about "implicit cut" macro? similar to scala fancy app for racket, but
; using an already established srfi
(define (star-it fn args) (apply fn args))
(define (vl-append* images) (apply vl-append images))
(define (ht-append* images) (apply ht-append images))

; Image Cell -> Image
(define (pin-cell-coords img a-cell)
  (match-define (cell (posn x y)) a-cell)
  (pin-over img 5 5 (text (string-append (number->string x) ", " (number->string y)) 'default 30)))

; String -> Image
(define (color-cell color)
  (filled-rectangle CELL CELL
                    #:color color
                    #:border-color "Black"
                    #:border-width 0))
(define black-cell
  (color-cell "Gainsboro"))

(define white-cell
  (color-cell "White"))

; Grid Grid -> image
; Consumes two grids, one of cells which are located in rooms, and one which is not.
(define (draw-grid a-level)
  (define-values (in-room out-room)
    (make-grid a-level))
  ; - IN -
  (lt-superimpose (draw-cells black-cell out-room)
                  (draw-cells white-cell in-room)))

; Grid -> Image
(define (draw-cells colored-cell grid)
  (for/fold ([backg (blank (* WIDTH CELL) (* HEIGHT CELL))])
            ([a-cell grid])
    (define anchor (cell-anchor a-cell))
    (pin-over backg
              (* (posn-x anchor) CELL) (* CELL (posn-y anchor))
              (pin-cell-coords colored-cell a-cell))))

(define excircle
  (circle/g (posn 6 6) 2))
(define circle-level (list (room (gensym) (random-circle) (posn 6 6) empty-neighborhood)))
(define (random-level)
  (list (room (gensym) (random-shape) (posn 6 6) empty-neighborhood)))

;(draw-grid (gen-level 2)) ; why doesn't this work? Gives a blank grid. (random-shape) works.
