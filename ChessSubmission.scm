(require graphics/graphics)
(require racket/gui)

(include "boardGUI.scm")
(include "makeMoves.scm")
(include "validMoves.scm")
(include "evaluate.scm")
(include "minimax.scm")
(include "syntax.scm")
(open-graphics)

(define initBoard
  (list (list 
         (cons 'B 'rook) (cons 'B 'knight) (cons 'B 'bishop) (cons 'B 'queen) (cons 'B 'king) (cons 'B 'bishop) (cons 'B 'knight) (cons 'B 'rook))
        (list 
         (cons 'B 'pawn) (cons 'B 'pawn)   (cons 'B 'pawn)   (cons 'B 'pawn)  (cons 'B 'pawn) (cons 'B 'pawn)   (cons 'B 'pawn)   (cons 'B 'pawn))
        (build-list 8 (λ(x) '()))
        (build-list 8 (λ(x) '()))
        (build-list 8 (λ(x) '()))
        (build-list 8 (λ(x) '()))
        (list 
         (cons 'W 'pawn) (cons 'W 'pawn)   (cons 'W 'pawn)   (cons 'W 'pawn)  (cons 'W 'pawn) (cons 'W 'pawn)   (cons 'W 'pawn)   (cons 'W 'pawn))
        (list 
         (cons 'W 'rook) (cons 'W 'knight) (cons 'W 'bishop) (cons 'W 'queen) (cons 'W 'king) (cons `W `bishop) (cons `W `knight) (cons 'W 'rook))))
(define board initBoard)
(define board1 board)

(define player 'W)

(define imgWidth 75)
(define imgHeight 75)
(define H 8)
(define V 8)
(define horiz-inset 100)
(define vert-inset 35)
(define right-gap 150)
(define bottom-gap 30)

(define width (* H imgWidth))
(define height (* V imgHeight))

(define depth 3)
(define mode '2-player)


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
(define Chess-Window (open-viewport "Chess Titans" (+ right-gap horiz-inset width) (+ bottom-gap vert-inset height)))




;A mouse click listener which takes an image(actually it's center)
(define (MouseClickListener img)
  (define posn (mouse-click-posn (get-mouse-click Chess-Window)))
  
  (cond[(equal? (send img lookinside posn) 'Empty) (MouseClickListener img)]))

(define (show)
  ((draw-pixmap Chess-Window) "Images/ChessNG.jpg" (make-posn -80 0) (make-rgb 0 0 0))
  ((draw-pixmap Chess-Window) "Images/Chess.png" (make-posn 220 30) (make-rgb 0 0 0))
  
  (define mainImgHold (make-object imgHold%))
  (define new-game (make-object img-button% 
                     "Images/new-game.png" 60 100 140 44 
                     (λ() 
                       (begin
                         (sleep 0.1)
                         (set! board initBoard) 
                         (set! board1 board)
                         (set! player 'W)
                         
                         ((clear-viewport Chess-Window))
                         (new-game-setup)))
                     mainImgHold))
  
  (send new-game draw)
  
  (MouseClickListener mainImgHold)
  )
(define (new-game-setup)
  ((draw-pixmap Chess-Window) "Images/ChessNG.jpg" (make-posn -80 0) (make-rgb 0 0 0))
  ((draw-pixmap Chess-Window) "Images/1-player.png" (make-posn 40 30) (make-rgb 0 0 0))
  (define new-gameHold (make-object imgHold%))
  (define level-1 (make-object img-button% "Images/level-1.png" 30 150 125 44 
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

