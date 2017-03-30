#lang racket
(require "../data.rkt" "../helpers.rkt" "shapes.rkt" racket/shared pict racket/trace rackunit graph
         (only-in 2htdp/image save-image))

(provide (all-defined-out))
;;---------------------------------------------------------------------------------------------------
#| LEVEL |#
; the graph library is not functional, maybe later we can create functional wrappers around the
; imperative procedures like add-edge! and add-vertex! but for now clearly indicate.
(define *LEVEL* (unweighted-graph/undirected '()))

;;---------------------------------------------------------------------------------------------------
#| Functions |#
; Symbol -> Room
(define (select-room name [level *LEVEL*])
  (findf (λ (a-room) (symbol=? (room-name a-room) name)) (get-vertices level)))


; [Listof Room] -> [Listof Room]
; Graph -> Graph
(define (new-room! [level (unweighted-graph/undirected '())])
  (cond
    [(empty? (get-vertices level))
     (add-vertex! level (random-room))]
    [else
     ; graphs aren't ordered by insertion date! The first element is not the most recently added
     (define prior-room (argmin (λ (a-room) (length (get-neighbors level a-room)))
                                (get-vertices level)))
     (define new-room (random-room))
     (add-edge! level new-room prior-room)]))

; Room -> Boolean
(define (has-room? a-room)
  (<= (length (get-neighbors *LEVEL* a-room)) NEIGHBOR-MAX))

; -> Room
(define (random-room)
  (room (gensym) (random-shape) (posn (random WIDTH) (random HEIGHT))))

;-> [X -> Y]
; selects a random shape with random parameters within certain constraints.
(define (random-shape)
  (define list-of-shapes (list random-circle random-rectangle))
  ((select-random list-of-shapes)))

; Number -> Level
; Imperatively generates a level containing Number amount of rooms.
(define (make-level! number-of-rooms [level (unweighted-graph/undirected '())])
     (begin (for ([i (in-range number-of-rooms)])
              (new-room! level))
            (set! *LEVEL* level)
            level))

;;---------------------------------------------------------------------------------------------------
#| Grid Creation |#
; In racket, the origin is at the upper left and not the center.
(define excircle
  (circle/g (posn 6 6) 2))
(define excircle2
  (circle/g (posn 10 10) 2))

(define alpha (room 'alpha excircle (posn 6 6)))
(define beta (room 'beta excircle2 (posn 10 10)))
(define exlevel (unweighted-graph/undirected (list alpha beta)))

(define level? (listof room?))
(define grid? (listof cell?))

(define *GRID* (unweighted-graph/undirected '()))
; Number Number -> #void
; Consumes 2 numbers representing the number of logical cells each row and column should have
; and alters *GRID* to add those edges and vertices.
(define (blank-grid! [w WIDTH] [h HEIGHT] [grid (unweighted-graph/undirected '())])
  (for*/list ([y (in-range h)]
              [x (in-range w)])
    ;(cell (posn x y) 'floor)
    (add-edge! grid
               (cell (posn x y) 'floor)
               (cell (posn (csub1 x) y) 'floor))
    (add-edge! grid
               (cell (posn x y) 'floor)
               (cell (posn (cadd1 x) y) 'floor))
    (add-edge! grid
               (cell (posn x y) 'floor)
               (cell (posn x (csub1 y)) 'floor))
    (add-edge! grid
               (cell (posn x y) 'floor)
               (cell (posn x (cadd1 y)) 'floor)))
  (set! *GRID* grid)
  grid)

(define (csub1 n)
  (if (> (sub1 n) 0) (sub1 n) 0))

(define (cadd1 n)
  (if (< (add1 n) HEIGHT) (add1 n) n))
; do I create all cells first, then selectively filter to remove them, or add them in as I go?

; Level -> Grid
(define (make-grid [level *LEVEL*] [grid0 *GRID*])
  ;(level? . -> . grid?)
  (define grid (get-vertices grid0))
  (define neighborhood0 (get-edges level))
  ;remove duplicate edges from the levels, (A B) = (B A).
  (define neighborhood (remove-duplicates neighborhood0 set=?))
  (for/fold ([map '()])
            ([neighbors neighborhood])
    (define room (first neighbors))
    (define next-door (rest neighbors))
    (define room-grid (punch-through room grid))
    ; - IN -
    (append room-grid
            (remove-duplicates (grid-path next-door room-grid level grid0))
            map)))

; [Listof Room] [Listof Cell] Level Grid -> List of cells
; Take a room's neighbors and the room's grid and create a list of cells containing the room's grid
; and the paths to each neighbor
(define (grid-path neighborhood home-grid [level *LEVEL*] [grid *GRID*])
  (for/fold ([grid-so-far '()])
            ([neighbor neighborhood])
    (define neighbor-grid (punch-through neighbor (get-vertices grid)))
    (append
     (find-path (select-random home-grid) (select-random neighbor-grid) grid)
     neighbor-grid
     grid-so-far)))

; Room Grid -> Grid
(define/contract (punch-through a-room grid)
  (room? (listof cell?) . -> . (non-empty-listof cell?))
  (filter (λ (a-cell) (cell-fits? a-cell a-room)) grid))

; Cell Room -> Boolean
(define/contract (cell-fits? a-cell a-room)
  (cell? room? . -> . boolean?)
  ((room-function a-room) (cell-anchor a-cell)))

; 1. Filter all vertices from the level from the grid (punch-through/siphon)
; 2. Append the cells from the first vertice to the path
;    from a cell in there to a cell in the neighbor
; 3. Append that  
(define room-or-empty? (or/c room? empty?))
(define room-name? symbol?)

; Cell Cell Grid -> List
(define (find-path origin destination [grid *GRID*])
  (define-values (distance predecessors) (dijkstra grid origin))
  (define (path-helper pred [path '()])
    (cond
      [(equal? pred origin) path]
      [else (path-helper (hash-ref predecessors pred) (cons pred path))]))
  ;--IN--
  (cons origin (path-helper destination)))

;;---------------------------------------------------------------------------------------------------
#| Rendering |#

; Level -> Image
; To draw a level, we center its shape at its center location. Then, for every neighbor
; it has we overlay a line to them and recurse on each. Maybe I can use pict-finders?
(define (draw-level level0)
  (define (draw-helper level)
  (cond
    [(empty? level) (blank WIDTH)]
    [else
     (match-define (room name shape center) (first level))
     (pin-over (draw-helper (rest level))
               (posn-x center)
               (posn-y center)
               (circle 10))]))
  ; - in -
  (draw-helper (get-vertices level0)))

; hey here's an idea, what about "implicit cut" macro? similar to scala fancy app for racket, but
; using an already established srfi
(define (star-it fn args) (apply fn args))
(define (vl-append* images) (apply vl-append images))
(define (ht-append* images) (apply ht-append images))

; Image Cell -> Image
(define (pin-cell-coords img a-cell)
  (match-define (cell (posn x y) _) a-cell)
  (pin-over img 0 0 (text (string-append (number->string x) ", " (number->string y))
                          'default TEXT-SIZE)))

; String -> Image
(define (color-cell color)
  (filled-rectangle CELL CELL
                    #:color color
                    #:border-color "Black"
                    #:border-width 0))

(define black-cell
  (color-cell "Black"))

(define white-cell
  (color-cell "White"))

; Level -> Image
(define (draw-grid a-level)
  (define grid (make-grid a-level))
  ; - IN -
  (lt-superimpose (draw-cells (const black-cell) (get-vertices (blank-grid!)))
                  (draw-cells (λ (a-cell) white-cell #;(pin-cell-coords white-cell a-cell)) grid)))

; Grid -> Image
(define (draw-cells colored-cell grid)
  (for/fold ([backg (blank (* WIDTH CELL) (* HEIGHT CELL))])
            ([a-cell grid])
    (define anchor (cell-anchor a-cell))
    (pin-over backg
              (* (posn-x anchor) CELL) (* CELL (posn-y anchor))
              (colored-cell a-cell))))

(blank-grid!)
(make-level! 5)
(save-image (pict->bitmap (draw-grid *LEVEL*)) "level.bmp")