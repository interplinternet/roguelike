#lang racket
(provide WIDTH HEIGHT ROOM-WIDTH ROOM-HEIGHT CELL NEIGHBOR-MAX
         (contract-out [struct cell ((anchor posn?) (terrain symbol?))])
         (contract-out [struct posn ((x number?) (y number?))])
         (contract-out [struct room ((name symbol?) (function (-> posn? any/c)) (anchor posn?)
                                     (neighbors (listof symbol?)))]) 
         )
;;---------------------------------------------------------------------------------------------------
#| CONSTANTS |#
(define WIDTH 24) ; the width of a level is 100 logical cells
(define HEIGHT WIDTH)
(define ROOM-WIDTH (/ WIDTH 4)) ; a room can be no larger than a quarter of the map
(define ROOM-HEIGHT ROOM-WIDTH)
(define CELL 100) ; a cell is 100 real units wide/high
(define NEIGHBOR-MAX 4)

;;---------------------------------------------------------------------------------------------------
#| Data |#
; A Level is [List Room Room Room Room] or [Empty]
; A Room is (room Symbol [X -> Y] Posn Level) such that level has no more than 4 rooms.

; Grid := [Listof Cell]
; Cell := (cell Posn Terrain)
; Where Posn is the anchor of the cell, in the upper left corner, and terrain is a symbol.
(struct cell (anchor terrain) #:transparent) 

; A room is:
; (struct [Number -> Any] Number)
(struct posn [x y] #:transparent)

; A Room is:
(struct room [name function anchor neighbors] #:transparent)
; (room Symbol [Posn -> Any] Posn [Listof Symbol])
; The function determines the shape of the room, and
; the anchor is the location of its central point in the level. Neighbors field is a list of symbols
; representing the names of all of its neighbors. -- A room is really a node in a graph.
