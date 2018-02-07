#lang racket
(provide WIDTH HEIGHT PWIDTH PHEIGHT ROOM-WIDTH ROOM-HEIGHT CELL NEIGHBOR-MAX TEXT-SIZE
         (contract-out [struct cell ((anchor posn?) (terrain symbol?))])
         (contract-out [struct posn ((x number?) (y number?))])
         (contract-out [struct room ((name symbol?) (function (-> posn? any/c)) (anchor posn?))]))
         
;;---------------------------------------------------------------------------------------------------
#| CONSTANTS |#
(define WIDTH 100) ; the width of a level is 100 logical cells
(define HEIGHT WIDTH)
(define PWIDTH 800) ;physical width and height, i.e., in pixels.
(define PHEIGHT PWIDTH)
(define ROOM-WIDTH (round (/ WIDTH 4))) ; a room can be no larger than a quarter of the map
(define ROOM-HEIGHT ROOM-WIDTH)
; a physical cell is wide enough to fit WIDTH number of logical cells in PWIDTH
(define CELL (round (/ PWIDTH WIDTH)))
(define TEXT-SIZE (/ CELL 4))
(define NEIGHBOR-MAX 4)

;;---------------------------------------------------------------------------------------------------
#| Data |#
; A Level is [List Room Room Room Room] or [Empty]
; A Room is (room Symbol [X -> Y] Posn Level) such that level has no more than 4 rooms.

; Grid := [Listof Cell]
; Cell := (cell Posn Terrain)
; Terrain := Symbol
; Where Posn is the anchor of the cell, in the upper left corner, and terrain is the content.
(struct cell (anchor terrain) #:transparent) 

; A posn is:
(struct posn [x y] #:transparent)
; (posn Number Number),
; the X- & Y-Coordinate of an entity or cell on the level

; A Room is:
(struct room [name function anchor] #:transparent)
; (room Symbol [Posn -> Any] Posn)
; The function determines the shape of the room, and
; the anchor is the location of its central point in the level.

; Maybe I should implement some generics for these?
(struct being [hp dmg loc] #:transparent)
; A being is (player Number Number Posn)

(struct world [entities items level] #:transparent)
; A Game is (game [Listof Being] [Listof Item] Grid)

(struct player being [eqp inv] #:transparent)
; A Player is (player Number Number Posn [Listof Item] [Listof Item])

(struct terrain (type))
; The terrain of a cell := (terrain String),
; where String is "floor" or "wall"
