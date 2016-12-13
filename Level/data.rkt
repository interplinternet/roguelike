#lang racket
;(provide (all-defined-out))
(provide (contract-out [struct cell ((anchor posn?))])
         (contract-out [struct posn ((x number?) (y number?))])
         (contract-out [struct room ((name symbol?) (function (-> posn? any/c)) (anchor posn?)
                                     (neighbors (listof symbol?)))])
         (contract-out [dir->pos (-> symbol? number?)])
         (contract-out [pos->dir (-> number? symbol?)])
         (contract-out [neighbor (-> symbol? symbol? symbol? symbol? (listof symbol?))])
         (contract-out [neighbor-north (-> (listof symbol?) symbol?)])
         (contract-out [neighbor-east (-> (listof symbol?) symbol?)])
         (contract-out [neighbor-south (-> (listof symbol?) symbol?)])
         (contract-out [neighbor-west (-> (listof symbol?) symbol?)])
         (contract-out [neighbor-set (-> (listof symbol?) symbol? any/c (listof any/c))])
         (contract-out [neighbor-update (-> (listof symbol?) symbol? (-> any/c any/c) (listof
                                                                                       any/c))])
         (contract-out [empty-neighborhood empty?])
         (contract-out [select-room (-> symbol? (listof room?) room?)])
         (contract-out [select-random (-> (non-empty-listof any/c) any/c)])
         WIDTH HEIGHT ROOM-WIDTH ROOM-HEIGHT CELL BLANK-POSN BLANK-NEIGHBORS NEIGHBOR-MAX)
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

; Number -> Symbol
(define (pos->dir pos)
  (list-ref '(north east south west) pos))

; The neighbors of a room are represented with a single list, which we treat as a structure.
(define-values (neighbor neighbor-north neighbor-east neighbor-south neighbor-west)
  (values (λ (north east south west) (list north east south west))
          first
          second
          third
          fourth))

; Functional setting, updating, and an empty neighborhood.
(define-values (neighbor-set neighbor-update empty-neighborhood)
  (values (λ (neighborhood direction value)
            (list-set neighborhood (dir->pos direction) value))
          (λ (neighborhood direction updater)
            (list-update neighborhood (dir->pos direction) updater))
          '()))

(define (select-room name level)
  (first (memf (λ (a-room) (symbol=? (room-name a-room) name)) level)))

; List -> X
(define (select-random l)
  (list-ref l (random (length l))))
; A Level is [List Room Room Room Room] or [Empty]
; A Room is (room Symbol [X -> Y] Posn Level) such that level has no more than 4 rooms.

;;---------------------------------------------------------------------------------------------------
#| CONSTANTS |#
(define WIDTH 24) ; the width of a level is 100 logical cells
(define HEIGHT WIDTH)
(define ROOM-WIDTH (/ WIDTH 4)) ; a room can be no larger than a quarter of the map
(define ROOM-HEIGHT ROOM-WIDTH)
(define CELL 100) ; a cell is 100 real units wide/high
(define BLANK-POSN (posn 0 0))
(define BLANK-NEIGHBORS (neighbor '() '() '() '()))
(define NEIGHBOR-MAX 4)
