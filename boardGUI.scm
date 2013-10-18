(define (drawImg position tag)
  (define x (car position)) (define y (cdr position))
  ((draw-pixmap Chess-Window) (imgAt (cons x y) tag) (make-posn (+ horiz-inset (* imgWidth (- x 1))) (+ vert-inset (* imgHeight (- y 1)))) (make-rgb 0 0 0)))


(define (makeBoard)
  (define (makeRow j i)
    (define x  (+ 1 (/ (- i horiz-inset) imgWidth)))
    (define y  (+ 1 (/ (- j vert-inset) imgHeight)))
    (if (> (+ horiz-inset (- width imgWidth)) i) 
        (begin 
        
          ((draw-pixmap Chess-Window) (imgAt (cons x y) "") (make-posn i j) (make-rgb 0 0 0))
          (makeRow j (+ i imgWidth)))
        ((draw-pixmap Chess-Window) (imgAt (cons x y) "") (make-posn i j) (make-rgb 0 0 0))))
  (define (makeRows j)
    (if (> (+ vert-inset (- height imgHeight)) j)
        (begin
          (makeRow j horiz-inset)
          (makeRows (+ j imgHeight)))
        (makeRow j horiz-inset)))
  (makeRows vert-inset))


(define (pathStr? square)
  (if (null? (access square)) (string-append "Images/" (if (even? (+ (car square) (cdr square))) "white" "black"  ))    
      (string-append "Images/" (if (even? (+ (car square) (cdr square))) "white" "black") "-" (symbol->string (car (access square))) "-" (symbol->string (cdr (access square))))))

(define (imgAt square tagStr)
  (string-append (pathStr? square) tagStr ".png"))




(define cntr 1)
(define click1 (cons 0 0))
(define click2 (cons 0 0))


(define (makePossibleMoves square board)
  (define possibleMovesForThis (possibleMoves square board))
  (for-each (λ(x) (drawImg (cons (cdr x) (car x)) "-possible-move")) possibleMovesForThis))
  
;A mouse click listener for all the moves on the chessboard
(define (movesMCL)
  (define pos (mouse-click-posn (get-mouse-click Chess-Window)))
  (define x (+ 1 (quotient (- (posn-x pos) horiz-inset) imgWidth)))
  (define y (+ 1 (quotient (- (posn-y pos) vert-inset) imgHeight)))
  (cond
    [(and (odd? cntr) (< horiz-inset (posn-x pos)) (< vert-inset (posn-y pos)) 
          (>= x 1) (>= y 1) (<= x 8) (<= y 8) (null? (access (cons x y)) ))
     (movesMCL)]
    [(and (odd? cntr)  (< horiz-inset (posn-x pos)) (< vert-inset (posn-y pos)) 
          (>= x 1) (>= y 1) (<= x 8) (<= y 8) (not (equal? player (car (access (cons x y))))))
     (movesMCL)] 
    [(and (odd? cntr)  (< horiz-inset (posn-x pos)) (< vert-inset (posn-y pos))
          (>= x 1) (>= y 1) (<= x 8) (<= y 8) (equal? player (car (access (cons x y)))))
     (begin 
       (set! click1 (cons y x))
       (drawImg (cons x y) "-selected")
       (makePossibleMoves click1 board)
       (set! cntr (+ cntr 1))
       (movesMCL))]
    [(and (even? cntr) (< horiz-inset (posn-x pos)) (< vert-inset (posn-y pos))
          (>= x 1) (>= y 1) (<= x 8) (<= y 8) )
     (begin
     
       (set! click2 (cons y x))
       (set! cntr (+ cntr 1))
       (if (and (not (equal? click2 click1)) (passOn? click1 click2 board player))
           (begin
             (set! board board1)
             (makeBoard)
             (if (equal? player 'B) (set! player 'W)
      (set! player 'B))
             (when (in-check? player) (drawImg (cons (cdr kpos) (car kpos)) "-check"))
             (cond
               [(null? (all-valid-moves)) 
                (cond
                  [(and (equal? player 'B) (in-check? player)) (begin
                                                                 (define CheckMate (open-viewport "Check-Mate" 500 375))
                                                                 ((draw-pixmap CheckMate) "Images/Checkmate-M.jpg" (make-posn 0 0) (make-rgb 0 0 0))
                                                                 (close-viewport Chess-Window))]
                  [(and (equal? player 'W) (in-check? player)) (begin
                                                                 (define CheckMate (open-viewport "Check-Mate" 500 375))
                                                                 ((draw-pixmap CheckMate) "Images/Checkmate-M.jpg" (make-posn 0 0) (make-rgb 0 0 0))
                                                                 (close-viewport Chess-Window))]
                  [else (begin
                          (define stale (open-viewport "Stale-Mate" 500 375))
                          ((draw-pixmap stale) "Images/Checkmate-M.jpg" (make-posn 0 0) (make-rgb 0 0 0))
                          
                          (close-viewport Chess-Window))])]
                 [(equal? mode '2-player) (movesMCL)]
                 [(equal? mode '1-player) (AIMove)]))
           (begin
             (makeBoard)
             (when (in-check? player) (drawImg (cons (cdr kpos) (car kpos)) "-check"))
             (movesMCL))))]
     [else (movesMCL)]))
  


(define (AIMove)
  (define move (alpha-beta board depth 'B))

  (define piece (list-ref (list-ref board (- (car (car move)) 1)) (- (cdr (car move)) 1)))
  (when (and (equal? piece (cons 'B 'Pawn)) (= 8 (car (car (cdr move)))))
         (pawnPromote! 'Queen (car move) (cadr move)));;;;;;;;;;;Need to check!!!!
  (begin
    (set! board (makeMove (car move) (cadr move) board))
    (set! board1 board)
    (makeBoard)
    (if (equal? player 'B) (set! player 'W)
      (set! player 'B))
    (when (in-check? player) (drawImg (cons (cdr kpos) (car kpos)) "-check"))
    (cond
      [(null? (all-valid-moves)) 
       (cond
         [(and (equal? player 'B) (in-check? player)) (begin
                                                        (define CheckMate (open-viewport "Check-Mate" 500 375))
                                                        ((draw-pixmap CheckMate) "Images/Checkmate-M.jpg" (make-posn 0 0) (make-rgb 0 0 0))
                                                       
                                                        (close-viewport Chess-Window))]
         [(and (equal? player 'W) (in-check? player)) (begin
                                                        (define CheckMate (open-viewport "Check-Mate" 500 375))
                                                        ((draw-pixmap CheckMate) "Images/Checkmate-M.jpg" (make-posn 0 0) (make-rgb 0 0 0))
                                                        
                                                        (close-viewport Chess-Window))]
         [else (begin
                 (define stale (open-viewport "Stale-Mate" 500 375))
                 ((draw-pixmap stale) "Images/Checkmate-M.jpg" (make-posn 0 0) (make-rgb 0 0 0))
                 (close-viewport Chess-Window))])]
       [else (movesMCL)])))

(define (play)
  (makeBoard)
  (movesMCL))



