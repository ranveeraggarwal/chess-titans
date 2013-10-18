(require graphics/graphics)
(require racket/gui)

(include "pieces.scm")
(include "board.scm")
(include "boardGUI.scm")
;(require "alphabeta.scm")

(open-graphics)

(define imgWidth 75)
(define imgHeight 75)
(define H 8)
(define V 8)
(define horiz-inset 100)
(define vert-inset 35)
(define right-gap 50)
(define bottom-gap 10)

(define width (* H imgWidth))
(define height (* V imgHeight))

(define depth 3)
(define board1 null)
(define mode 2)

;(define turn 'White)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;


;;;;;;;;;;;;;;;;;;;;
(define img-button%
  (class object%
    (init-field button-name)
    (init-field h)
    (init-field v)
    (init-field width)
    (init-field height)
    (init-field action)
    (init-field center)
    
    (super-new)
    (define/public (draw)
      ((draw-pixmap Chess-Window) button-name (make-posn h v) (make-rgb 0 0 0))
      (send center insert this))
    (define/public (inside? posn)
      (define x (posn-x posn))
      (define y (posn-y posn))
      (cond [(and (>= x h) (<= x (+ h width)) (>= y v) (<= y (+ v height))) #t]
            [else #f]))
    
    (define/public (execute!) (action))
    ))

(define imgHold%
  (class object%
    (init-field [imgs '()])
    (super-new)
    (define/public (insert img)
      (set! imgs (append (list img) imgs)))
    (define/public (lookinside pos)
      (define (helper Imgs)
        
        (if (null? Imgs)
            'Empty
             (let* ([fst (send (car Imgs) inside? pos)])
                    (if fst (send (car Imgs) execute!) (helper (cdr Imgs))))))
      (helper imgs))))
;;;;A few important things needed. :p
(define Chess-Window (open-viewport "Kasparov Chess" (+ right-gap horiz-inset width) (+ bottom-gap vert-inset height)))




;A mouse click listener which takes an image(actually it's center)
(define (MouseClickListener img)
  (define posn (mouse-click-posn (get-mouse-click Chess-Window)))
 
  (cond[(equal? (send img lookinside posn) 'Empty) (MouseClickListener img)]))

(define (show)
  ((draw-pixmap Chess-Window) "Images/ChessNG.jpg" (make-posn 0 0) (make-rgb 0 0 0))
  ((draw-pixmap Chess-Window) "Images/Chess.png" (make-posn 330 30) (make-rgb 0 0 0))
  (define mainImgHold (make-object imgHold%))
  (define new-game (make-object img-button% 
                     "Images/new-game.png" 200 165 160 40 
                     (λ() 
                       (begin
                         (sleep 0.1)
                         (send board initialise) 
                         ;(set! turn 'White)
                         (set! board1 board)
                         ((clear-viewport Chess-Window))
                         (new-game-setup)))
                     mainImgHold))
    
    (send new-game draw)
   
    (MouseClickListener mainImgHold)
   )
(define (new-game-setup)
  ((draw-pixmap Chess-Window) "Images/ChessNG.jpg" (make-posn 0 0) (make-rgb 0 0 0))
  ((draw-pixmap Chess-Window) "Images/Chess.png" (make-posn 330 30) (make-rgb 0 0 0))
  ((draw-pixmap Chess-Window) "Images/1-player.png" (make-posn 220 200) (make-rgb 0 0 0))
  (define new-gameHold (make-object imgHold%))
  (define level-1 (make-object img-button% "Images/level-1.png" 230 290 120 30 
                    (λ () 
                      (begin
                        (sleep 0.1)
                        ((clear-viewport Chess-Window))
                        (set! mode '1-player)
                        (set! depth 2)
                         (play)
                        )) new-gameHold))
(begin
    (send level-1 draw)
    (MouseClickListener new-gameHold)))

 (show) 
  
