#lang racket/gui
(require "Level/level.rkt" "data.rkt" "helpers.rkt" pict)
(provide (all-defined-out))
; eventually we'll pull out the gui, pict, etc. elements. They're in here for easy testing, it's
; easier to see how everything fits.

;;---------------------------------------------------------------------------------------------------
#| Constants |#
(define UNIT 18)
(define WIDTH 800)
(define HEIGHT 800)
(define UNITSPEED (/ WIDTH 100))
(define FONT (make-font #:size UNIT #:family 'modern))

(define PLAYER-IMG
  (text "@" 'modern UNIT))

;;----------------------------------------------------------------------------------------------------
#| Structs |#
(struct posn [x y] #:transparent)
; A Posn is (posn Number Number),
; the X- & Y-Coordinate of an entity or cell on the level

; Maybe I should implement some generics for these?
(struct being [hp dmg loc] #:transparent)
; A being is: (player Number Number Posn)

(struct world [entities items level] #:transparent)
; A Game is (game [Listof Being] [Listof Item] Grid)

(struct player being [eqp inv] #:transparent)
; A Player is (player Number Number Posn [Listof Item] [Listof Item])

(struct cell [location terrain] #:transparent)
;A Cell is (cell Posn Symbol), where Posn represents its location, and terrain is the content.

(struct terrain (type))
; The terrain of a cell := (terrain String),
; where String is "floor" or "wall"

;;--------------------------------------------------------------------------------------------------
#| Functions |#

; [Listof [Listof Posn Any]] -> [Listof [Listof Posn Any]]
; A grid is a list of lists. We update by applying f to every element.
(define (update-grid a-grid f)
  (deep-map a-grid f))

; Posn Level -> [List [Listof Being] [Listof Item]]
(define (whats-on pos a-level)
  (define item-table (world-items a-level))
  (define entity-table (world-entities a-level))
  (cond
    [(and (hash-has-key? entity-table pos) (hash-has-key? item-table pos))
     (append (hash-ref entity-table pos) (hash-ref item-table pos))]
    [(hash-has-key? entity-table pos)
     (hash-ref entity-table pos)]
    [(hash-has-key? item-table pos)
     (hash-ref item-table pos)]
    [else '()])) ; maybe #f, or '(())?
;;---------------------------------------------------------------------------------------------------
#| Examples |#
(define exgrid (make-grid (make-level 3)))
(define explayer (player 100 10 (select-random (map cell-anchor exgrid)) `(sword shield) `(bag bow arrow)))
(define exmons `(orc goblin doggo))
(define EXMSGLOG '("most recent message" "immediately prior" "last message"))
(define exmonhash
  (hash (posn 0 0) `(orc)
        (posn 2 3) `(goblin)
        (posn 3 3) `(,explayer)
        (posn 1 1) `(doggo)))
(define exmonhash2 (hash-update exmonhash (posn 0 0) (curry cons 'gobbo)))

(define exitemhash
  (hash (posn 0 0) `(chest)
        (posn 2 3) `(dagger potion)
        (posn 3 3) `(arrow)))

;(define exworld (world exmonhash exitemhash exgrid))

;;---------------------------------------------------------------------------------------------------
#| MAIN |#
; A grid is the game-playing map, where each "cell" is one UNIT wide and tall and contains terrain
; information.
(define main%
  (class canvas%
    (init-field [plyr explayer])
    (init-field [monsters exmons])
    (init-field [grid exgrid])

    ; DC -> Void
    (define/public (draw-world dc)
      (match-define (player _ _ (posn x y) _ _) plyr)
      (draw-pict (draw-grid exgrid) dc 0 0)
      ;  the world, overlay the monsters on that, overlay the player on that. 
      )
    ; DC -> Void
    (define/public (draw-player dc)
      (match-define (player _ _ (posn x y) _ _) plyr)
      (draw-pict PLAYER-IMG dc x y))

    ; KeyEvent -> Void
    (define/override (on-char event)
      (update-game! event)
      (send this refresh)) ; (send main-screen refresh)

    ; KeyEvent -> Void
    ; When we want to update an entire game, we process the sequence containing all entities,
    ; moving them as necessary
    (define (update-game! event)
      (match-define (player health damage (posn x y) equipment inventory) plyr)
      (set! plyr
            (player health damage (update-location event x y) equipment inventory)))

    ; KeyEvent Number Number -> Posn
    (define (update-location event x y)
      (match (send event get-key-code)
        ['up    (posn x (- y UNITSPEED))]
        ['down  (posn x (+ y UNITSPEED))]
        ['left  (posn (- x UNITSPEED) y)]
        ['right (posn (+ x UNITSPEED) y)]
        [_      (posn x y)]))
    (super-new)))

;;---------------------------------------------------------------------------------------------------
#| DUMMY |#
(define dummy-frame
  (new frame%
       [label "dummy"]
       [min-width WIDTH]
       [min-height HEIGHT]))

(define dummy (new main%
                   [parent dummy-frame]
                   [paint-callback
                    (λ (canvas dc)
                      (send dummy draw-world dc))]))
