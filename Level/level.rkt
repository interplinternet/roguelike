#lang racket
(require "data.rkt" "shapes.rkt" "helpers.rkt" racket/shared pict racket/trace rackunit)
(provide (all-defined-out))

;;---------------------------------------------------------------------------------------------------
#| Functions |#
; [Listof Room] -> [Listof Room]
(define (new-room [list-of-room '()])
  ; To add a new room, we select a random one from a list of valid rooms. Then we cons it onto the
  ; list of rooms. The new room lists its previous room as a neighbor.
  (define new-name (gensym))
  (cond
    [(empty? list-of-room)
     (list (random-room))]
    [else (define first-room (first list-of-room))
          ; this will need to be refactored with our new room representation
          ; Neighbors are no longer a list of slots, which may be empty, and represent NESW,
          ; They're just a list of up to four rooms with no regard for cardinal direction. This will
          ; simplify things a lot.
          (define random-slot (select-random-slot (valid-slots first-room)));-> N, E, S, W
          (define new-room (make-room random-slot new-name (room-name first-room)))
          (list* new-room
                 (add-new-neighbor first-room random-slot new-room)
                 (rest list-of-room))]))

; Room -> [Listof Room]
; We take a room's shape and center, and remove all directions which would lead outside the bounds.
(define (valid-slots a-room)
  (define nbors (room-neighbors a-room))
  nbors)

; [Listof Room] -> Room
(define (select-random-slot nbors)
  (select-random '(north east south west)))

; Slot Symbol Room -> Room
; consumes a slot, a name, and the last room made.
; Creates a new room with the name of the previous room in the appropriate slot. E.g., if our new room
; is north of our previous one, attach the previous to the south slot.
(define (make-room a-slot name prev-name)
  (room name
        (random-shape)
        ; does this bit even matter? NESW slots and posn have no correlation, even if a room is "east"
        ; of another, it may not necessarily be to the east of it on the grid.
        (posn (random WIDTH) (random HEIGHT))
        (neighbor-set empty-neighborhood a-slot (list prev-name))))

; Room Slot Room -> Room
; adds a new neighbor to a room.
(define (add-new-neighbor prev-room a-slot new-room)
  (match prev-room
    [(room name shape center neighbors)
     (room name shape center
           (neighbor-update neighbors (opposite-dir a-slot) (curry cons (room-name new-room))))]))

; Symbol -> Symbol
(define (opposite-dir direction)
  (match direction
    ['north 'south]
    ['east  'west]
    ['south 'north]
    ['west  'east]))

(define (random-room)
  (room (gensym) (random-shape) (posn (random WIDTH) (random HEIGHT)) empty-neighborhood))
;;---------------------------------------------------------------------------------------------------
#| Grid Creation |#
; In racket, the origin is at the upper left and not the center.
(define excircle
  (circle/g (posn 6 6) 2))
(define excircle2
  (circle/g (posn 10 10) 2))
(define circle-level (list (room 'alpha excircle (posn 6 6) '(() (beta) () ()))
                           (room 'beta excircle2 (posn 10 10) '(() () () (alpha)))))
; Level -> [List Grid Grid]
; Consumes a representation of a level, returns two grids: one containing all "empty" cells, and one
; containing all "wall" cells, or those which are not in a room.
(define (make-grid2 level)
  (define initial-grid (blank-grid WIDTH HEIGHT))
  ; - IN -
  (partition (λ (a-cell)
               (for/or ([a-room level])
                 (cell-fits? a-cell a-room)))
             initial-grid))

; Level -> [List Grid Grid]
; Consumes a level representation and returns 2 grids: one containing floor cells and one containing
; wall cells. Take all the cells in the grid that are in the first room, and connect them to the last
; existing room and then recurse.
(define (make-grid3 level0)
  (define initial-grid (blank-grid WIDTH HEIGHT))
  (define start (first level0))
  (define seen '())
  ; - IN -
  (walk-and-connect start initial-grid (cons start seen)))


; Level -> Grid
(define/contract (make-grid level0)
  ((listof room?) . -> . (listof cell?))
  (define initial-grid (blank-grid WIDTH HEIGHT))
  (walk-and-connect/level level0 initial-grid))

; Level Grid-> Grid
(define/contract (walk-and-connect/level level grid)
  ((listof room?) (listof cell?) . -> . (listof cell?))
  (foldr (λ (a-room rest-of-level)
           (walk-and-connect a-room rest-of-level level (room-neighbors a-room) (list a-room)))
         grid
         level))

(define (room-or-empty? x) (or (room? x) (empty? x)))

; Room Grid [Listof Room] Level [Listof Room] -> Grid
; only return connected rooms and hallways, don't worry about walls for now.
(define/contract (walk-and-connect home-room grid neighbors0 level seen)
  (room? (listof cell?) (listof room?) (listof room-or-empty?) (listof room-or-empty?) . -> . (listof cell?))
  (define room-grid (punch-through home-room grid))
  (define start-cell (select-random room-grid))
  (define neighbors (remove* seen (remove* '(()) neighbors0)
                             (λ (seens nbors) (equal? seens (first nbors)))))
  (cond
    [(empty? neighbors) room-grid]
    [else
     (define first-n (select-room (first (first neighbors)) level))
     (define new-grid0 (connect room-grid (and (cell? start-cell) start-cell) first-n grid seen))
     (define new-grid (walk-and-connect first-n
                                        new-grid0
                                        (room-neighbors first-n)
                                        (cons first-n seen)))
     (walk-and-connect home-room new-grid (rest neighbors) (cons (room-name first-n) seen))]))

; Grid Cell Room  Grid -> Grid
; Connects a grid representing one room to one neighbor and returns a new grid
(define/contract (connect a-room closest-cell neighbor grid [path '()])
  (((listof cell?) cell? room? (listof cell?)) ((listof symbol?)) . ->* . (listof cell?))
  (define neighbor-grid (punch-through neighbor grid))
  (cond[(inside? closest-cell neighbor-grid) (append path grid)]
       [else (define new-closest-cell (pick-closest closest-cell neighbor-grid grid) )
             (connect a-room new-closest-cell ;ugh
                      neighbor grid (cons (room-name a-room) path))]))

; Room Grid -> Grid
(define (punch-through a-room grid)
  (filter (λ (a-cell) (cell-fits? a-cell a-room)) grid))

; Cell Grid -> Boolean
(define/contract (inside? a-cell grid)
  (cell? (listof cell?) . -> . boolean?)
  (and (cell? a-cell) (member a-cell grid)))

; Cell Grid Grid -> Cell
(define (pick-closest a-cell target-grid grid)
  (define target (select-random target-grid))
  (define target-sum (sum-coords target))
  (define surroundings (orthogonal-cells a-cell grid))
  (argmin (λ (cell-sum) (abs (- cell-sum target-sum))) (map sum-coords surroundings)))

; Cell -> Number
(define (sum-coords a-cell)
  (match a-cell
    [(cell (posn x y)) (+ x y)]))

; Cell Grid -> Grid
(define (orthogonal-cells a-cell grid)
  (match-define (cell (posn x y)) a-cell)
  ; it will be better to select cells by their coordinate, but for now since cells contain
  ; no information it's fine to just "recreate" them
  (map (λ (pos) (cell pos))
       (list (posn (sub1 x) (sub1 y)) ; upper-left
             (posn x (sub1 y)) ; above
             (posn (add1 x) (sub1 y)) ; upper-right
             (posn (sub1 x) y) ; left
             (posn (add1 x) y) ; right
             (posn (sub1 x) (add1 y)) ; lower-left
             (posn x (add1 y)) ; below
             (posn (add1 x) (add1 y))))); lower-right

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
(define (make-level number-of-rooms)
  (self-apply new-room (list (room (gensym) (random-shape) BLANK-POSN empty-neighborhood))
              number-of-rooms))

;;---------------------------------------------------------------------------------------------------
#| HALLWAYS |#
(define ex-level (make-level 5))
#;(define ex-grid
    (let-values ([(rooms walls) (make-grid ex-level)]) ; causing bug room-name: contract violation
      ; expected: room? given: '()
      (list rooms walls)))

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
  (define-values (floor wall)
    (make-grid a-level))
  ; - IN -
  (lt-superimpose (draw-cells (const black-cell) wall)
                  (draw-cells (λ (a-cell) (pin-cell-coords white-cell a-cell)) floor)))

; Grid -> Image
(define (draw-cells colored-cell grid)
  (for/fold ([backg (blank (* WIDTH CELL) (* HEIGHT CELL))])
            ([a-cell grid])
    (define anchor (cell-anchor a-cell))
    (pin-over backg
              (* (posn-x anchor) CELL) (* CELL (posn-y anchor))
              (colored-cell a-cell))))



#;(define (random-level)
    (list (room (gensym) (random-shape) (posn 6 6) empty-neighborhood)))

;(define r-level (list (room (gensym) (random-rectangle) (posn 6 6) empty-neighborhood)))

;(define example-level (draw-grid (make-level 2)))
