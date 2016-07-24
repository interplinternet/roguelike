#lang racket/gui
(require pict)
(provide (all-defined-out))
;;---------------------------------------------------------------------------------------------------
#| Constants |#
(define UNIT 18)
(define WIDTH 800)
(define HEIGHT 800)
(define UNITSPEED (/ WIDTH 100))
(define FONT (make-font #:size UNIT #:family 'modern))
(define EXMSGLOG '("most recent message" "immediately prior" "last message"))

(define PLAYER-IMG
  (text "@" 'modern UNIT))
;;----------------------------------------------------------------------------------------------------
#| Structs |#
(struct posn [x y] #:transparent)
; A Posn is (posn Number Number),
; the X- & Y-Coordinate of an entity or cell on the level

(struct being [hp dmg loc] #:transparent)
; A being is: (player Number Number Posn)

(struct world [entities level] #:transparent)
; A Game is (game [Listof Being] Grid)

(struct player being [eqp inv] #:transparent)
; A Player is (player Number Number Posn [Listof Item] [Listof Item])

(struct cell [location content] #:transparent)
;A Cell is (cell Posn Any), where Posn represents its location, and Any is the content.

(struct terrain (type))
; The terrain of a cell := (terrain String),
; where String is "floor" or "wall"

#| Functions |#
; Number Any -> [Listof [Listof Posn Any]]
; A nested for/list call is not quite the same as for*/list. The former creates a list of lists, which
; we want for easy reference to each row, and the latter creates a single flattened list.
(define (make-grid len content)
  (for/list ([row (in-range len)])
    (for/list ([col (in-range len)])
      (cell (posn row col) content))))

; [Listof [Listof Posn Any]] -> [Listof [Listof Posn Any]]
; A grid is a list of lists. We update by applying f to every element.
(define (update-grid a-grid f)
  (deep-map a-grid f))

;[Listof [Listof Any]] [Any -> Any] -> [Listof [Listof Any]]
(define (deep-map lol f)
  (cond
    [(empty? lol) '()]
    [(cons? (first lol))
     (cons (deep-map (first lol) f)
           (deep-map (rest lol) f))]
    [else (cons (f (first lol))
                (deep-map (rest lol) f))]))

;;---------------------------------------------------------------------------------------------------
#| Examples |#

(define explayer (player 100 10 (posn 100 100) '(sword shield) '(bag bow arrow)))
(define exmons '(orc goblin doggo))
(define exgrid (make-grid 3 'nada))
(define exmonhash
  (make-immutable-hash `([,(posn 0 0) . (orc)]
                         [,(posn 2 3) . (goblin)]
                         [,(posn 1 1) . (doggo)])))
(define exmonhash2 (hash-update exmonhash (posn 0 0) (curry cons 'gobbo)))
#| MAIN |#
; A grid is the game-playing map, where each "cell" is one UNIT wide and tall and contains terrain
; information.
(define main%
  (class canvas%
    (init-field [plyr explayer])
    (init-field [monsters exmons])
    (init-field [grid exgrid])

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

(define dummy-frame
  (new frame%
       [label "dummy"]
       [min-width 800]
       [min-height 800]))
(define dummy (new main%
                   [parent dummy-frame]
                   [paint-callback
                    (λ (canvas dc)
                      (send dummy draw-player dc))]))
