#lang racket
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
(struct room [name function anchor neighbors] #:transparent)
; (room Symbol [Posn -> Any] Posn [Listof Symbol])
; The function determines the shape of the room, and
; the anchor is the location of its central point in the level. Neighbors field is a list of symbols
; representing the names of all of its neighbors. -- A room is really a node in a graph.

; Symbol -> Number
; takes a symbol representing a cardinal direction and returns the appropriate index of a vector.
(define/match (dir->pos direction)
  [('north) 0]
  [('east)  1]
  [('south) 2]
  [('west)  3])

; The neighbors of a room are represented with a single vector, which we treat as a structure.
(define-values (neighbor neighbor-north neighbor-east neighbor-south neighbor-west)
  (values (λ (north east south west) (list north east south west))
          first
          second
          third
          fourth)
  #;(values (λ (north east south west) (vector north east south west))
            (λ (a-vector) (vector-ref a-vector 0))
            (λ (a-vector) (vector-ref a-vector 1))
            (λ (a-vector) (vector-ref a-vector 2))
            (λ (a-vector) (vector-ref a-vector 3))))

; Functional setting, updating, and an empty neighborhood.
(define-values (neighbor-set neighbor-update empty-neighborhood)
  (values (λ (neighborhood direction value)
            (list-set neighborhood (dir->pos direction) value))
          (λ (neighborhood direction updater)
            (list-update neighborhood (dir->pos direction) updater))
          '(() () () ()))
  #;(values (λ (vec direction val)
              (define pos (dir->pos direction))
              (for/vector ([(elem indx) (in-indexed (in-vector vec))])
                (if (= indx pos) val elem)))
            (λ (vec direction updater)
              (define pos (dir->pos direction))
              (for/vector ([(elem indx) (in-indexed (in-vector vec))])
                (if (= indx pos) (updater elem) elem)))
            #(() () () ())))

(define (select-room name level)
  (memf (λ (a-room) (symbol=? (room-name a-room) name)) level))

; A Level is [Listof Room]
; A Room is (room Symbol [X -> Y] Posn Level)

;;---------------------------------------------------------------------------------------------------
#| CONSTANTS |#
(define WIDTH 24) ; the width of a level is 100 logical cells
(define HEIGHT WIDTH)
(define ROOM-WIDTH (/ WIDTH 4)) ; a room can be no larger than a quarter of the map
(define ROOM-HEIGHT ROOM-WIDTH)
(define CELL 100) ; a cell is 100 real units wide/high
(define BLANK-POSN (posn 0 0))
(define BLANK-NEIGHBORS (neighbor '() '() '() '()))
