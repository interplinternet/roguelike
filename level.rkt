#lang racket
(require racket/shared pict)
;;---------------------------------------------------------------------------------------------------
#| Data |#
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

;(struct neighbor (north east south west) #:transparent)
; A node can have a neighbor in any cardinal direction.

; A Level is [Listof Room] or Empty
; A Room is (room Symbol [X -> Y] Posn Level)

;;---------------------------------------------------------------------------------------------------
#| Helpers |#
(define dummy (const 'dummy))
; [X -> Y] X Number -> Y
(define (self-apply function initial-input times-to-apply)
  (for/fold ([base initial-input])
            ([n (in-range times-to-apply)])
    (function base)))

;;---------------------------------------------------------------------------------------------------
#| CONSTANTS |#
(define WIDTH 100) ; in logical, not real, units.
(define HEIGHT 100)
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
        (λ (width height) (posn WIDTH HEIGHT)) ; call later when generating grid
        ;do we create a room's position and then create a function to fit it,
        ;or create the function and position later?
        (neighbor-set empty-neighborhood a-slot (list prev-name))))

; Room Slot Room -> Room
; adds a new neighbor to a room.
(define (add-new-neighbor prev-room a-slot new-room)
  (match prev-room
    [(room name shape center neighbors)
     (room name shape center (neighbor-update neighbors a-slot (curry cons (room-name new-room))))]))

; Room Level -> Room
#;(define (find-neighbors a-room a-level)
    (define neighbors )
    (cond
      [(empty? a-level) '()]
      [(symbol=? (room-name (first a-level)) (room-name a-room))
       (first a-level)]
      [else (find-neighbors a-room (rest a-level))]))

; -> [X -> Y]
; selects a random shape with random parameters within certain constraints.
(define (random-shape)
  dummy)
; Number -> Level
; Generates a level containing Number amount of rooms.
(define (gen-level number-of-rooms)
  (self-apply new-room (list (room (gensym) (random-shape) BLANK-POSN BLANK-NEIGHBORS))
              number-of-rooms))

(define exlevel (self-apply new-room (list exroom) 5))

; Level -> Grid
#;(define (make-grid a-level)
    (define blank-grid (build-list WIDTH (λ (_) 'wall))))

;;---------------------------------------------------------------------------------------------------
#| Room shapes |#
; A room's shape can be represented as a function and/or a series of constraints. Each shape consumes
; a central point, and returns a function which takes another point and determines whether that point
; is within the shape.

(define max-radius 10)
(define max-width  10)
(define max-height 10)
(define random1 (compose1 add1 random)) ; 1 <= random-number <= given-number
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
(define/match ((shape center inequalities) point)
  [((posn h k) inequalities (posn x y)) (inequalities h k x y)])

; Posn Number -> [Posn -> Boolean]
(define (circle center rad)
  (shape center (λ (h k x y)
                  (<= (+ (sqr (- x h)) (sqr (- y k))) (sqr rad)))))

(define (rectangle center width height)
  (shape center (λ (h k x y)
                  (and (<= (- h (half-of width)) x (+ h (half-of width)))
                       (<= (- k (half-of height)) y (+ k (half-of height)))))))

; [Number Number -> Boolean] [Number -> Boolean] -> Posn -> Boolean
; Could be expressed concisely using J-style hooks.
(define ((line oper function) point)
  (oper (posn-y point) (function (posn-x point))))

; Posn [Number -> Number] [Number -> Number] [Number -> Number] -> [Posn -> Boolean]
(define (triangle center base left right)
  (shape center
         (conjoin left right base)
         #;(λ (h k x y)
             (and ((line <= right) (posn x y))
                  ((line <= left) (posn x y))
                  ((line <= base) (posn x y))))))

; [Listof [Number Number -> Boolean]] -> [Posn -> Boolean]
(define/match ((shape/l . lines) point)
  [((list l ...) (posn x y)) [(apply conjoin l) x y]])

; Number Number Number Number -> [Posn -> Boolean]
(define (rect/l #:left left #:top top #:right right #:bottom bottom)
  (shape/l (λ (x y) (>= x left))
           (λ (x y) (<= y top))
           (λ (x y) (<= x right))
           (λ (x y) (>= y bottom))))

; Posn Number -> [Posn -> Boolean]
(define/match (circle/l center rad)
  [((posn h k) rad)
   (shape/l (λ (x y) (<= (+ (sqr (- x h)) (sqr (- y k))) (sqr rad))))])

; [Number -> Number] [Number -> Number] [Number -> Number] -> [Posn -> Boolean]
(define (triangle/l #:left left #:right right #:bottom bottom)
  (shape/l (λ (x y) (<= y (left x)))
           (λ (x y) (<= y (right x)))
           (λ (x y) (>= y (bottom x)))))

(define func
  (λ (x) (+ (- x) 1)))

(define right-side-of-triangle
  (line <= (λ (x) (+ (- x) 1))))

(define left-side-of-triangle
  (line <= (λ (x) (+ x 1))))

; using standard graphing rules, where 0 is at the center & the top is the
; highest value, not the lowest.
(define base-of-triangle
  (line >= (λ (x) 0)))

(define trigon
  (conjoin right-side-of-triangle left-side-of-triangle base-of-triangle))

(define excircle (circle (posn 10 10) (random1 max-radius)))
(define exsquare (rectangle (posn 10 10) 10 10))

; A triangle is a set of inequalities, each representing a side.
; e.g., the right side is a line y = -(x) + 4 where x : 0 <= x <= 4
; the left side is a line y = x + 4, where x : -4 <= x <= 0
; and the bottom is a line y = 0
;;---------------------------------------------------------------------------------------------------
#| Rendering |#

; Maybe you could use the collector idiom here. Each call of the function wraps the base-call with
; another "layer", placing a new circle at (x, y) on the recursive call.
; Or you could just use pin-over.
; Level -> Image
; To draw a level, we center its shape at its center location. Then, for every neighbor
; it has we overlay a line to them and recurse on each. Maybe I can use pict-finders?
(define (draw-level level)
  (cond
    [(empty? level) (blank WIDTH)]
    [else
     (define first-room (first level))
     (match-define (room name shape center neighbors) first-room)
     (pin-over (draw-level (rest level))
               (posn-x center)
               (posn-y center)
               (circle 10))]))
