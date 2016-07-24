#lang racket/gui
(require pict "main.rkt")
;;---------------------------------------------------------------------------------------------------
#| Constants |#
(define UNIT 18)
(define WIDTH 800)
(define HEIGHT 800)
(define UNITSPEED (/ WIDTH 100))
(define FONT (make-font #:size UNIT #:family 'modern))
(define EXMSGLOG ('"most recent message" "immediately prior" "last message"))

(define PLAYER-IMG
  (text "@" 'modern UNIT))
;;----------------------------------------------------------------------------------------------------
#| Structs |#
(struct posn [x y] #:transparent)
; A Posn is (posn Number Number),
; the X- & Y-Coordinate of an entity on the level
(struct being [hp dmg loc] #:transparent)2
; A being is: (player Number Number Posn)

(struct world [entities level] #:transparent)
; A Game is (game [Listof Being] Grid)

(struct player being [eqp inv] #:transparent)
; A Player is (player Number Number Posn [Listof Item] [Listof Item])

(struct terrain (type))
; The terrain of a cell := (terrain String),
; where String is "floor" or "wall"
;;---------------------------------------------------------------------------------------------------
#| Examples |#

(define explayer (player 100 10 (posn 100 100) '(sword shield) '(bag bow arrow)))
(define exmons '(orc goblin doggo))
(define exgrid
  (for*/list ([row (in-range 10)]
              [col (in-range 10)])
    (list (posn row col) 'nothing 'yet)))
; A grid is the game-playing map, where each "cell" is one UNIT wide and tall and contains terrain
; information.

;;---------------------------------------------------------------------------------------------------
#| Objects & Classes |#
; Try to figure out some kind of MVC thing.

#| CONTAINERS|#
(define frame (new frame%
                   [label "hi vlker"]
                   [min-width  WIDTH]
                   [min-height HEIGHT]))

(define game+msgs (new vertical-pane% [parent frame]))
(define game      (new horizontal-pane% [parent game+msgs]))
(define main-pane (new pane%
                       [parent game]
                       [alignment '(left center)]
                       ))
(define inv-pane (new pane%
                      [parent game]
                      [alignment '(right center)]
                      ))
(define msg-panel (new panel%
                       [parent    game+msgs]
                       [alignment '(center bottom)]
                       [style     '(border)]))

#| INTERFACES |#
; there are no first-class methods, string->image when defined in a class cannot be used later in
; that same class expression.
(define text-draw<%> (interface () draw-vl-text textlist->image)) 
(define text-draw-mixin
  (mixin (canvas<%>) (text-draw<%>)
    (super-new)
    
    ; [Listof String] -> Image
    (define/public (draw-vl-text los)
      (apply vl-append (map (curryr text 'modern UNIT) los)))

    ; DC [Listof String] -> Void
    (define/public (textlist->image dc los)
      (draw-pict (draw-vl-text los) dc 0 0))
    ))

#| CLASSES |#
(define msg% (text-draw-mixin (class canvas% (super-new) (init-field [message-log EXMSGLOG]))))

; The inventory screen, where items are listed.
(define inv-canvas% (text-draw-mixin canvas%))

; The Main screen, where the action happens.
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
      (send main-screen refresh))

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

#| OBJECTS |#
(define main-screen
;How do we render a whole screen? We take a map, and for every row we step through each column,
;using its location to pull any entities located there. We render the terrain, then we render the
;objects, followed by monsters/player. Right now we just render the player.
  (new main%
       [parent main-pane]
       [label "Main"]
       [style '(border)]
       [min-width 400]
       [min-height 400]
       [paint-callback
        (λ (canvas dc)
          (send main-screen draw-player dc))]))

(define msg
  ; Eventually, Health & Equipped should be factored into their own panes/canvases.
  ; Then I can label them and wrap all rendering calls with get-label instead of having to cons
  ; & append titles in every paint-callback function.
  ; Options: Split the inventory pane into two panes: One for general inventory,
  ; one for equipped items.
  ; Or change the contents of the pane based on key presses, when "i" set it to general inventory,
  ; When "e" set it to equipped items.
  ; Or have all of them created, but have them hidden based on keypresses like above.
  (new msg%
       [parent msg-panel]
       [paint-callback
        (λ (canvas dc)
          (match-define (player health _ _ equipment _)
            (get-field plyr main-screen))
          (define hp (string-append "Health: " (number->string health)))
          (define eqp (cons "Equipped: " (map symbol->string equipment)))
          (send msg textlist->image
                dc ; this is gross looking.
                (list* hp (append eqp (list " ") (get-field message-log msg)))))]))

(define inv-screen
  (new inv-canvas%
       [parent inv-pane]
       [label "Inventory"]
       [style '(border)]
       [min-width 400]
       [stretchable-width #f]
       [paint-callback
        (λ (canvas dc)
          (define a-player (get-field plyr main-screen))
          (define inven-list (player-inv a-player))
          (send inv-screen textlist->image dc
                (cons "Inventory: " (map symbol->string inven-list))))]))
