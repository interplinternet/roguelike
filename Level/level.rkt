#lang racket
(require "data.rkt" "shapes.rkt" "helpers.rkt" racket/shared pict racket/trace rackunit)
(provide (all-defined-out))

;;---------------------------------------------------------------------------------------------------
#| Functions |#
; [Listof Room] -> [Listof Room]
(define (new-room [list-of-room '()])
  ; To add a new room, we check to see if the neighborhood has room. If it does, then we create a new
  ; room and cons it onto the list of rooms so far, listing the previous room as a neighbor. We
  ; proceed to add rooms only in a linear fashion from now, each one connects directly only to the
  ; last one.
  (cond
    [(empty? list-of-room)
     (list (random-room))]
    [(has-room? (first list-of-room))
     (define first-room (first list-of-room))
     ; this will need to be refactored with our new room representation
     ; Neighbors are no longer a list of slots, which may be empty, and represent NESW,
     ; They're just a list of up to four rooms with no regard for cardinal direction. This will
     ; simplify things a lot.
     (define new-room (make-room (room-name first-room)))
     (list* new-room
            (add-new-neighbor first-room (room-name new-room))
            (list (rest list-of-room)))]
    [else list-of-room]))

; Room -> Boolean
(define (has-room? a-room)
  (<= (length (room-neighbors a-room)) NEIGHBOR-MAX))

; Name -> Room
; Consumes the name of the previous room.
; Creates a new room with the name of the previous room in the neighborhood.
(define (make-room prev-name)
  (room (gensym)
        (random-shape)
        (posn (random WIDTH) (random HEIGHT))
        (list prev-name)))

; Room Name -> Room
; adds a new neighbor to a room.
(define (add-new-neighbor prev-room new-room-name)
  (struct-copy room prev-room
               [neighbors (cons new-room-name (room-neighbors prev-room))]))

(define (random-room)
  (room (gensym) (random-shape) (posn (random WIDTH) (random HEIGHT)) empty-neighborhood))
;;---------------------------------------------------------------------------------------------------
#| Grid Creation |#
; In racket, the origin is at the upper left and not the center.
(define excircle
  (circle/g (posn 6 6) 2))
(define excircle2
  (circle/g (posn 10 10) 2))
(define circle-level (list (room 'alpha excircle (posn 6 6) '(beta))
                           (room 'beta excircle2 (posn 10 10) '(alpha))))
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

(define level? (listof room?))
(define grid? (listof cell?))

; Level -> Grid
(define/contract (make-grid level0)
  (level? . -> . grid?)
  (define initial-grid (blank-grid WIDTH HEIGHT))
  (define start-room (first level0))
  (walk-and-connect start-room level0 initial-grid '()))

(define room-or-empty? (or/c room? empty?))
(define room-name? symbol?)

; Room [Listof Room] [Listof Cell] [Listof Symbol] -> [Listof Cell]
(define (walk-and-connect home-room level walls seen)
  (define home-grid (punch-through home-room walls)) ;[Listof Cell]
  (define neighbors (room-neighbors home-room)) ; [Listof Symbol]
  (define hallway (connect home-grid (select-random home-grid)
                           (select-room (first neighbors) level) walls))
  (append home-grid
          hallway
          (connect-neighbors neighbors level walls (cons (room-name home-room) seen))))

; [Listof Symbol] [Listof Room] [Listof Cell] [Listof Symbol] -> [Listof Cell]
(define (connect-neighbors lon0 level walls seen)
  (define lon (remove* seen lon0))
  (cond
    [(empty? lon) '()]
    [else
     (define seen-now (cons (first lon) seen))
     (append (walk-and-connect (select-room (first lon) level) level walls seen-now)
             (connect-neighbors (rest lon) level walls seen-now))]))
; Grid Cell Room  Grid -> Grid
; Connects a grid representing one room to one neighbor and returns a new grid representing the
; hallway between them.
(define/contract (connect room-grid closest-cell neighbor grid)
  ((listof cell?) cell? room? (listof cell?) . -> . (listof cell?))
  (define neighbor-grid (punch-through neighbor grid))

  (define (connect/helper close-cell cell-path)
    (cond
      [(inside? close-cell neighbor-grid)
       (cons close-cell cell-path)]
      [else (define new-closest-cell (pick-closest close-cell neighbor-grid grid))
            (connect/helper new-closest-cell (cons close-cell cell-path))]))
  ; - IN -
  (connect/helper closest-cell '()))




; Room Grid -> Grid
(define/contract (punch-through a-room grid)
  (room? (listof cell?) . -> . (listof cell?))
  (filter (λ (a-cell) (cell-fits? a-cell a-room)) grid))

; Cell Room -> Boolean
(define/contract (cell-fits? a-cell a-room)
  (cell? room? . -> . boolean?)
  (define the-shape (room-function a-room))
  (the-shape (cell-anchor a-cell)))

; Cell Grid -> Boolean
(define/contract (inside? a-cell grid)
  (cell? (listof cell?) . -> . boolean?)
  (and (member a-cell grid) #t))

; Cell Grid Grid -> Cell
(define (pick-closest a-cell target-grid grid)
  (define target (select-random target-grid))
  (define target-sum (sum-coords target))
  (define surroundings (orthogonal-cells a-cell))
  (argmin (λ (a-cell) (abs (- (sum-coords a-cell) target-sum))) surroundings))

; Cell -> Number
(define (sum-coords a-cell)
  (match a-cell
    [(cell (posn x y)) (+ x y)]))

; Cell -> Grid
(define/contract (orthogonal-cells a-cell)
  (cell? . -> . (listof cell?))
  (match-define (cell (posn x y)) a-cell)
  ; it will be better to select cells by their coordinate, but for now since cells contain
  ; no information it's fine to just "recreate" them
  (map (λ (pos) (cell pos))
       (filter-not (λ (pos) (or (negative? (posn-x pos)) (negative? (posn-y pos))))
                   (list (posn (sub1 x) (sub1 y)) ; upper-left
                         (posn x (sub1 y)) ; above
                         (posn (add1 x) (sub1 y)) ; upper-right
                         (posn (sub1 x) y) ; left
                         (posn (add1 x) y) ; right
                         (posn (sub1 x) (add1 y)) ; lower-left
                         (posn x (add1 y)) ; below
                         (posn (add1 x) (add1 y)))))); lower-right

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
