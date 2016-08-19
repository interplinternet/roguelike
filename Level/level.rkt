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
(define CELL 200) ; a cell is 100 real units wide/high
(define BLANK-POSN (posn 0 0))
(define BLANK-NEIGHBORS (neighbor '() '() '() '()))

;;---------------------------------------------------------------------------------------------------
#| Level Examples |#
;A level could be a cyclic graph
; However, we would have to abandon the struct form, only lists/vectors/etc. can be cyclic.
(define cyclic-level
  (shared ([a (list 'a dummy (posn 10 10) b c)]
           [b (list 'b dummy (posn 10 15) a d)]
           [c (list 'c dummy (posn 15 10) a)]
           [d (list 'd dummy (posn 10 20) b)])
    a))

; make all rooms available globally.
(define-values (room1 room2 room3 room4)
  (shared ([a (list 'a dummy (posn 10 10) b c)]
           [b (list 'b dummy (posn 10 15) a d)]
           [c (list 'c dummy (posn 15 10) a)]
           [d (list 'd dummy (posn 10 20) b)])
    (values a b c d)))

(define exroom (room (gensym) dummy BLANK-POSN BLANK-NEIGHBORS))

; A Level is a list of nodes, the first one is the most recently created.
(define example-level (list exroom))
(define excell (cell (posn 10 10)))
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
        (λ (width height) (posn width height)) ; call later when generating grid
        ;do we create a room's position and then create a function to fit it,
        ;or create the function and position later?
        ; We create a function for a room, then we take the remaining grid which is not in that room,
        ; and create new rooms from that grid?
        (neighbor-set empty-neighborhood a-slot (list prev-name))))

; Room Slot Room -> Room
; adds a new neighbor to a room.
(define (add-new-neighbor prev-room a-slot new-room)
  (match prev-room
    [(room name shape center neighbors)
     (room name shape center (neighbor-update neighbors a-slot (curry cons (room-name new-room))))]))

;;---------------------------------------------------------------------------------------------------
#| Room shapes |#
; A room's shape can be represented as a function and/or a series of constraints. Each shape consumes
; a central point, and returns a function which takes another point and determines whether that point
; is within the shape.

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

(define (circle-example center rad)
  (define h (posn-x center))
  (define k (posn-y center))
  (λ (pos)
    (define x (posn-x pos))
    (define y (posn-y pos))
    (<= (+ (sqr (- x h)) (sqr (- y k)))
        (sqr rad))))

(define (rectangle-example center width height)
  (define h (posn-x center))
  (define k (posn-y center))
  (λ (pos)
    (define x (posn-x pos))
    (define y (posn-y pos))
    (and (<= (- h (half-of width)) x (+ h (half-of width)))
         (<= (- k (half-of height)) y (+ h (half-of height))))))

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
; Question: Are the lines given in terms of logical points, or real ones?
; It might be a better idea to give in terms of logical points, and convert when drawn to real points.
; That makes determining whether a cell is within a shape much simpler, because we no longer need to
; check every point within it. It will either be within the shape, or not.
(define ((shape . lines) point)
  [(apply conjoin lines) (posn-x point) (posn-y point)])

; Number Number Number Number -> [Posn -> Boolean]
(define (rectangle/g left top right bottom)
  (shape (λ (x y) (>= x left))
         (λ (x y) (>= y top)) ; the "top" of a screen in Racket is 0,
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
  (define radians (random1 ROOM-WIDTH))
  (define center (posn (random radians (- WIDTH radians)) (random radians (- WIDTH radians))))
  (circle/g center radians))

; [Number -> Number] [Number -> Number] [Number -> Number] -> [Posn -> Boolean]
(define (triangle/g left right base)
  ; this seems to work for triangles only in certain positions., swap <= and >= for inversion.
  ; god, that's annoying.
  (shape (λ (x y) (>= y (left x))) ;(hook/dyadic <= left)
         (λ (x y) (<= y (right x)))
         (λ (x y) (<= y (base x)))))

(define (random-triangle)
  ; The width of the triangle's base can be no wider than the max-width of a room, and can be
  ; expressed as the product of the y-intercept and the slope.
  (define base-width (random1 ROOM-WIDTH))
  (define y-intercept (random 7 HEIGHT))
  ; the base of a triangle must be "below" the y-intercept by a reasonable amount, and because in
  ; Racket the upper left of a screen is (0, 0), the lower points on the screen have a higher y-val.
  (define base (random y-intercept HEIGHT))
  (define (random-slope) (expt y-intercept (random 1))) ; something might be giving a negative number
  (triangle/g (λ (x) (+ (* (random-slope) x) y-intercept))
              (λ (x) (+ (* (random-slope) (- x)) y-intercept)) ; how do I generate a random slope?
              (const base)))

; using standard graphing rules, where 0 is at the center & the top is the
; highest value, not the lowest.
; Posn -> Boolean
(define extriangle
  (triangle/g (λ (x) (+ x 2))
              (λ (x) (+ (- x) 2))
              (λ (x) 0)))

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
  (define list-of-shapes (list random-circle random-triangle random-rectangle))
  [(list-ref list-of-shapes (random (length list-of-shapes)))])

; Number -> Level
; Generates a level containing Number amount of rooms.
(define (gen-level number-of-rooms)
  (self-apply new-room (list (room (gensym) (random-shape) BLANK-POSN BLANK-NEIGHBORS))
              number-of-rooms))

(define exlevel (self-apply new-room (list exroom) 5))
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

; Level -> Image
; This is very slow. Grids are drawn incorrectly. Might be possible to fix by passing a list of lists,
; where each list is a row.
(define (draw-grid level)
  (define grid (blank-grid WIDTH HEIGHT))
  ; Grid [Listof Image] -> Image
  (define (draw-rows a-grid images)
    (cond
      [(empty? a-grid) (apply ht-append (reverse images))]
      [(= (length images) WIDTH)
       (define img-row (apply ht-append (reverse images)))
       (vl-append img-row (draw-rows a-grid '()))]
      [else (draw-rows (rest a-grid) (cons (white-or-black-cell (first a-grid) level) images))]))
  ; - IN -
  (draw-rows grid '()))

; Cell Level -> Image
; draws a black or white rectangle with the cell's coords overlaid on it.
(define (white-or-black-cell a-cell level)
  (match-define (cell (posn x y)) a-cell)
  (pin-over (if (andmap (curry cell-fits? a-cell) level)
                (filled-rectangle CELL CELL
                                  #:color "White"
                                  #:border-color "Black"
                                  #:border-width 10)
                (filled-rectangle CELL CELL
                                  #:color "Gainsboro"
                                  #:border-color "Black"
                                  #:border-width 10))
            5 5
            (text (string-append (number->string x) ", " (number->string y)) 'default 30)))

(define extriangle2 ; ugh.
  (triangle/g (λ (x) (- x 12))
              (λ (x) (+ (- x) 12))
              (const 24)))

(define tri-level (list (room (gensym) extriangle2 (posn 6 6) empty-neighborhood)))

(define excircle2
  (circle/g (posn 6 6) 2))
