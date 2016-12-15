#lang racket
(require "../data.rkt" "shapes.rkt" "helpers.rkt" racket/shared pict racket/trace rackunit)
(provide (all-defined-out))

;;---------------------------------------------------------------------------------------------------
#| Functions |#
; The neighbors of a room are represented with a single list, which we treat as a structure.
(define-values (neighbor neighbor-north neighbor-east neighbor-south neighbor-west)
  (values (λ (north east south west) (list north east south west))
          first
          second
          third
          fourth))

(define empty-neighborhood '())
; Functional setting, updating, and an empty neighborhood.
#;(define-values (neighbor-set neighbor-update empty-neighborhood)
    (values (λ (neighborhood direction value)
            (list-set neighborhood (dir->pos direction) value))
          (λ (neighborhood direction updater)
            (list-update neighborhood (dir->pos direction) updater))
          '()))

; Symbol Level -> Room
(define (select-room name level)
  (first (memf (λ (a-room) (symbol=? (room-name a-room) name)) level)))


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
            (rest list-of-room))]
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
(define level? (listof room?))
(define grid? (listof cell?))

; Level -> Grid
; Sometimes this stalls indefinitely, why?
(define/contract (make-grid level0)
  (level? . -> . grid?)
  (define initial-grid (blank-grid WIDTH HEIGHT))
  (define start-room (first level0))
  (remove-duplicates (walk-and-connect start-room level0 initial-grid (list (room-name start-room)))))

(define room-or-empty? (or/c room? empty?))
(define room-name? symbol?)

; Room [Listof Room] [Listof Cell] [Listof Symbol] -> [Listof Cell]
(define/contract (walk-and-connect home-room level walls seen)
  (room? level? grid? (listof symbol?) . -> . (non-empty-listof cell?))
  (define home-grid (punch-through home-room walls)) ;[Listof Cell]
  (define neighbors0 (room-neighbors home-room)) ; [Listof Symbol]
  (define neighbors (remove* seen neighbors0))
  (cond
    [(empty? neighbors) home-grid]
    [else
     (define hallway (connect home-grid (select-random home-grid)
                              (select-room (first neighbors) level) walls))
     (append home-grid
             hallway
             (connect-neighbors neighbors level walls (cons (room-name home-room) seen)))]))

; [Listof Symbol] [Listof Room] [Listof Cell] [Listof Symbol] -> [Listof Cell]
(define/contract (connect-neighbors lon level walls seen)
  ((listof symbol?) level? grid? (listof symbol?) . -> . (non-empty-listof cell?))
  (cond
    [(empty? lon) '()]
    [else
     (define seen-now seen)
     (append (walk-and-connect (select-room (first lon) level) level walls seen-now)
             (connect-neighbors (rest lon) level walls seen-now))]))

; Grid Cell Room  Grid -> Grid
; Connects a grid representing one room to one neighbor and returns a new grid representing the
; hallway between them.
(define/contract (connect room-grid closest-cell neighbor grid)
  ((listof cell?) cell? room? (listof cell?) . -> . (non-empty-listof cell?))
  (define neighbor-grid (punch-through neighbor grid))

  ; Cell [Listof Cell] -> [Listof Cell]
  (define (connect/helper close-cell cell-path)
    (cond
      [(inside? close-cell neighbor-grid)
       (cons close-cell cell-path)]
      [else (define new-closest-cell
              (pick-closest close-cell neighbor-grid cell-path grid))
            (connect/helper new-closest-cell (cons close-cell cell-path))]))
  ; - IN -
  (connect/helper closest-cell '()))

; Room Grid -> Grid
(define/contract (punch-through a-room grid)
  (room? (listof cell?) . -> . (non-empty-listof cell?))
  (filter (λ (a-cell) (cell-fits? a-cell a-room)) grid))

; Cell Room -> Boolean
(define/contract (cell-fits? a-cell a-room)
  (cell? room? . -> . boolean?)
  ((room-function a-room) (cell-anchor a-cell)))

; Cell Grid -> Boolean
(define (inside? a-cell grid)
  (member a-cell grid))

; Cell Grid Grid -> Cell
(define/contract (pick-closest a-cell target-grid cell-path grid0)
  (cell? (non-empty-listof cell?) (listof cell?) (non-empty-listof cell?) . -> . cell?)
  (define target (select-random target-grid))
  (define target-sum (sum-coords target))
  (define grid (remove* cell-path grid0))
  ; if there are no orthogonal cells, then return a random orthogonal cell to prevent hanging.
  (define surroundings (orthogonal-cells a-cell grid))
  (if (empty? surroundings) (select-random (orthogonal-cells a-cell grid0))
      (argmin (λ (a-cell) (abs (- (sum-coords a-cell) target-sum))) surroundings)))

; Cell -> Number
(define/match (sum-coords a-cell)
  [((cell (posn x y) _)) (+ x y)])

; Cell -> Grid
(define/contract (orthogonal-cells a-cell grid)
  (cell? (listof cell?) . -> . (listof cell?))
  (match-define (cell (posn x y) _) a-cell)

  (define orthogonal-c
    (map (curryr cell 'floor)
         (list (posn x (sub1 y)) ; above
               (posn (sub1 x) y) ; left
               (posn (add1 x) y) ; right
               (posn x (add1 y))))); below

  (for/list ([c grid]
             #:when (member c orthogonal-c))
    c))

; Number Number -> Grid
; Consumes 2 numbers representing the number of logical cells each row and column should have.
(define (blank-grid w h)
  (for*/list ([y (in-range h)]
              [x (in-range w)])
    (cell (posn x y) 'floor)))

;-> [X -> Y]
; selects a random shape with random parameters within certain constraints.
(define (random-shape)
  (define list-of-shapes (list random-circle random-rectangle))
  [(list-ref list-of-shapes (random (length list-of-shapes)))])

; Number -> Level
; Generates a level containing Number amount of rooms.
(define (make-level number-of-rooms)
  (self-apply new-room '() number-of-rooms))

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
  (match-define (cell (posn x y) _) a-cell)
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
  (define floor (make-grid a-level))
  (define wall (remove* floor (blank-grid WIDTH HEIGHT)))
  ; - IN -
  (lt-superimpose (draw-cells (const black-cell) wall)
                  (draw-cells (λ (a-cell) (pin-cell-coords white-cell a-cell)) floor)))

(define (draw-grid2 a-level)
  (define grid (make-grid a-level))
  ; - IN -
  (lt-superimpose (draw-cells (λ (a-cell) (pin-cell-coords white-cell a-cell)) grid)
                  (draw-cells (const black-cell) (blank-grid WIDTH HEIGHT))))

; Grid -> Image
(define (draw-cells colored-cell grid)
  (for/fold ([backg (blank (* WIDTH CELL) (* HEIGHT CELL))])
            ([a-cell grid])
    (define anchor (cell-anchor a-cell))
    (pin-over backg
              (* (posn-x anchor) CELL) (* CELL (posn-y anchor))
              (colored-cell a-cell))))
