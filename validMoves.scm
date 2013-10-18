(define enpB (list 0 0 0 0 0 0 0 0))
(define enpW (list 0 0 0 0 0 0 0 0))


(define canCastleW1 0)
(define canCastleW8 0)
(define canCastleB1 0)
(define canCastleB8 0)


(define (kingPos? board player)
  (define pos (cons player 'king))
  (define bb (append
              (list-ref board 0)
              (list-ref board 1)
              (list-ref board 2)
              (list-ref board 3)
              (list-ref board 4)
              (list-ref board 5)
              (list-ref board 6)
              (list-ref board 7)))
  (define (loop1 b i)
    (cond
      ((null? b) (cons player 'Error))
      ((equal? pos (car b)) i)
      (else (loop1 (cdr b) (+ i 1)))))
  (define whereIs (loop1 bb 1))
  (if (= (remainder whereIs 8) 0) (cons (quotient whereIs 8) 8)
      (cons (+ (quotient whereIs 8) 1) (remainder whereIs 8))))


(define (passOn? pos1 pos2 brd player . z)
  (define piece (list-ref (list-ref brd (- (car pos1) 1)) (- (cdr pos1) 1)))
  (if (null? z) (fun2 pos1 pos2 piece brd player) (fun1 pos1 pos2 piece brd player)))

(define (fun1 pos1 pos2 piece board player . z)
  (if (validmove pos1 pos2 board player)
      (begin 
        (set! board1 board) 
        (is-in-check? (makeMove pos1 pos2 board) player))
      (begin 
        (set! board1 board) #f)))

(define (fun2 pos1 pos2 piece board player)
  (define ans
    (if (validmove pos1 pos2 board player 'yes)
        (begin
          (makeMove pos1 pos2)
          (is-in-check? board1 player))
        #f))
  (begin
    (cond
      [(and ans (equal? pos1 (cons 1 1))) (set! canCastleW1 1)]
      [(and ans (equal? pos1 (cons 1 8))) (set! canCastleW8 1)]
      [(and ans (equal? pos1 (cons 8 1))) (set! canCastleB1 1)]
      [(and ans (equal? pos1 (cons 8 8))) (set! canCastleB8 1)]
      [(and ans (equal? (cdr piece) `king)) (if (equal? (car piece) `W)
                                                (begin (set! canCastleB1 1)
                                                       (set! canCastleB8 1))
                                                (begin (set! canCastleW1 1)
                                                       (set! canCastleW8 1)))]
      [(not ans) (set! board1 board)]
      [else (void)])
    ans))


(define (is-in-check? board1 player)
  (define row 9)
  (define col 9)
  (define kpos (kingPos? board1 player))
  (foldr (λ (x y)
           (and 
            (foldr (λ (a b)
                     (cond 
                       [(or (null? a) (equal? (car a) player)) (begin (set! row (- row 1)) b)]
                       [else (if (validmove (cons col row) kpos board1 
                                            (if (equal? player 'B) 'W 'B)) 
                                 (begin (set! row (- row 1)) #f) 
                                 (begin (set! row (- row 1)) b))]))
                   (begin (set! row 8)
                          (set! col (- col 1))
                          #t)
                   x)
            y))
         #t
         board1))

(define (validmove pos1 pos2 brd player . z)
  (define col1 (cdr pos1))
  (define col2 (cdr pos2))
  (define row1 (car pos1))
  (define row2 (car pos2))
  (define (kingValid)
    (cond
      ((= col1 col2) (if (= 1 (abs (- row1 row2))) #t #f))
      ((= row1 row2) (if (= 1 (abs (- col1 col2))) #t #f))
      ((and (= 1 (abs (- col1 col2))) (= 1 (abs (- row1 row2)))) #t)
      (else #f)))
  
  (define (queenValid)
    (if (or (rookValid) (bishopValid)) #t #f))
  
  (define (bishopValid)
    (define (filt1)
      (define col-init (min col1 col2))
      (define row-init (min row1 row2))
      (define row-fin (max row1 row2))
      (define (ch1 row col)
        (if (= row row-fin) #t
            (if (null? (list-ref (list-ref brd (- row 1)) (- col 1))) (ch1 (+ 1 row) (+ 1 col)) #f)))
      (ch1 (+ row-init 1) (+ col-init 1)))
    
    (define (filt2)
      (define col-init (min col1 col2))
      (define row-init (max row1 row2))
      (define row-fin (min row1 row2))
      (define (ch2 row col)
        (if (= row row-fin) #t
            (if (null? (list-ref (list-ref brd (- row 1)) (- col 1))) (ch2 (- row 1) (+ 1 col)) #f)))
      (ch2 (- row-init 1) (+ col-init 1)))
    
    (cond
      ((= (- col1 col2) (- row1 row2)) (filt1))
      ((= (- col1 col2) (- (- row1 row2))) (filt2))
      (else  #f)))
  
  (define (knightValid)
    (if (or (and (= 1 (abs (- col1 col2))) (= 2 (abs (- row1 row2)))) (and (= 1 (abs (- row1 row2))) (= 2 (abs (- col1 col2))))) #t #f))
  
  (define (rookValid)
    (define (check-col)
      (define rowmax (max row1 row2))
      (define rowmin (min row1 row2))
      (define (ccol row)date
        (if (= row rowmax) #t
            (if (null? (list-ref (list-ref brd (- row 1)) (- col1 1))) (ccol (+ 1 row)) #f)))
      (ccol (+ 1 rowmin)))
    
    (define (check-row)
      (define colmax (max col1 col2))
      (define colmin (min col1 col2))
      (define (crow col)
        (if (= col colmax) #t
            (if (null? (list-ref (list-ref brd (- row1 1)) (- col 1))) (crow (+ 1 col)) #f)))
      (crow (+ 1 colmin)))
    
    (cond
      ((= col1 col2) (check-col))
      ((= row1 row2) (check-row))
      (else #f)))
  
  (define (enpEnable)
    (define tempboard board)
    (or (if (and
             (eq? player 'B)
             (= row1 5)
             (= row2 6)
             (= (abs (- col1 col2)) 1)
             (= 1 (list-ref enpB (- col2 1))))
            (if (not (null? z)) (begin (makeMove pos2 (cons (- (car pos2) 1) (cdr pos2))) #t) #t)
            #f)
        (if (and
             (eq? player 'W)
             (= row1 4)
             (= row2 3)
             (= (abs (- col1 col2)) 1)
             (= 1 (list-ref enpW (- col2 1))))
            (if (not (null? z)) (begin (makeMove pos2 (cons (+ (car pos2) 1) (cdr pos2))) #t) #t)
            #f)))
  
  (define (pawnValid)
    (define square2 (list-ref (list-ref brd (- row2 1)) (- col2 1)))
    (define ans (cond ((and (null? square2) (= col1 col2)) (if (eq? player 'W)
                                                               (cond ((= (- row1 row2) 1) #t)
                                                                     ((and (= row2 5) (= row1 7) (null? (list-ref (list-ref brd 5) (- col2 1)))) #t)
                                                                     (else #f))
                                                               (cond ((= (- row1 row2) -1) #t)
                                                                     ((and (= row2 4) (= row1 2) (null? (list-ref (list-ref brd 2) (- col2 1)))) #t)
                                                                     (else #f))))
                      ((and (= (abs (- col1 col2)) 1) (not (null? square2))) (cond ((and (eq? player 'W) (= (- row1 row2) 1) (eq? 'B (car square2))) #t)
                                                                                   ((and (eq? player 'B) (= (- row1 row2) -1) (eq? 'W (car square2))) #t)
                                                                                   (else #f)))
                      (else #f)))
    
    (if (null? z) ans 
        (pawnvalid? ans)))
  (define (chngBrd)
    (makeMove pos1 pos2)
    (define tempboard board1)
    (set! board1 board)
    (if (equal? player 'B)
        (if (is-in-check? tempboard player)
            (begin (ppromHelper) #t)
            #f)
        (if (is-in-check? tempboard player)
            (begin (ppromHelper) #t)
            #f)))
  
  (define (ppromHelper)
    (define promoteView (open-viewport "Promote a pawn!" 300 360))
    ((draw-pixmap promoteView) "Images/queen-prom.png" (make-posn 0 0) (make-rgb 0 0 0))
    ((draw-pixmap promoteView) "Images/rook-prom.png" (make-posn 0 90) (make-rgb 0 0 0))
    ((draw-pixmap promoteView) "Images/bishop-prom.png" (make-posn 0 180) (make-rgb 0 0 0))
    ((draw-pixmap promoteView) "Images/knight-prom.png" (make-posn 0 270) (make-rgb 0 0 0))
    (define clk (get-mouse-click promoteView))
    (define pmc1 (mouse-click-posn clk))
    (define x (posn-x pmc1))
    (define y (posn-y pmc1))
    (close-viewport promoteView)
    (pawnPromote (cond ((< y 90) 'queen)
                                         ((< y 180) 'rook)
                                         ((< y 270) 'bishop)
                                         (else 'knight))
                                   pos1 pos2))
  
  (define (pawnvalid? ans)
    (if ans
        (cond ((= (- row1 row2) 2) (begin (set! enpB (cond ((= col1 1) (list 1 0 0 0 0 0 0 0))
                                                                      ((= col1 2) (list 0 1 0 0 0 0 0 0))
                                                                      ((= col1 3) (list 0 0 1 0 0 0 0 0))
                                                                      ((= col1 4) (list 0 0 0 1 0 0 0 0))
                                                                      ((= col1 5) (list 0 0 0 0 1 0 0 0))
                                                                      ((= col1 6) (list 0 0 0 0 0 1 0 0))
                                                                      ((= col1 7) (list 0 0 0 0 0 0 1 0))
                                                                      (else (list 0 0 0 0 0 0 0 1))))
                                          ans))
              ((= (- row1 row2) -2) (begin (set! enpW (cond ((= col1 1) (list 1 0 0 0 0 0 0 0))
                                                                       ((= col1 2) (list 0 1 0 0 0 0 0 0))
                                                                       ((= col1 3) (list 0 0 1 0 0 0 0 0))
                                                                       ((= col1 4) (list 0 0 0 1 0 0 0 0))
                                                                       ((= col1 5) (list 0 0 0 0 1 0 0 0))
                                                                       ((= col1 6) (list 0 0 0 0 0 1 0 0))
                                                                       ((= col1 7) (list 0 0 0 0 0 0 1 0))
                                                                       (else (list 0 0 0 0 0 0 0 1))))
                                           ans))
              ((and (eq? player 'W) (= row2 1)) (chngBrd))
              ((and (eq? player 'B) (= row2 8)) (chngBrd))
              (else ans))
        ans))
  
  
  (define (castlingEnable)
    (define tempboard brd)
    (or
     (if (and
          (equal? pos1 (cons 1 5))
          (equal? pos2 (cons 1 7))
          (= canCastleW8 0)
          (null? (list-ref (list-ref brd 0) 6))
          (null? (list-ref (list-ref brd 0) 5))
          (not (in-check? player))
          (begin
            (set! tempboard (makeMove (cons 1 5) (cons 1 7) tempboard))
            (set! tempboard (makeMove (cons 1 8) (cons 1 6) tempboard))
            (is-in-check? tempboard player)))
         (if (not (null? z))
             (begin (makeMove (cons 1 8) (cons 1 6)) (set! board board1) #t)
             (void))
         (begin (set! tempboard brd) #f))
     (if (and
          (equal? pos1 (cons 1 5))
          (equal? pos2 (cons 1 3))
          (= canCastleW1 0)
          (null? (list-ref (list-ref brd 0) 2))
          (null? (list-ref (list-ref brd 0) 3))
          (null? (list-ref (list-ref brd 0) 1))
          (not (in-check? player))
          (begin
            (set! tempboard (makeMove (cons 1 5) (cons 1 3) tempboard))
            (set! tempboard (makeMove (cons 1 1) (cons 1 4) tempboard))
            (is-in-check? tempboard player)))
         (if (not (null? z))
             (begin (makeMove (cons 1 1) (cons 1 4)) (set! board board1) #t)
             (begin  #t))
         (begin (set! tempboard brd) #f))
     (if (and
          (equal? pos1 (cons 8 5))
          (equal? pos2 (cons 8 7))
          (= canCastleB8 0)
          (null? (list-ref (list-ref brd 7) 6))
          (null? (list-ref (list-ref brd 7) 5))
          (not (in-check? player))
          (begin
            (set! tempboard (makeMove (cons 8 5) (cons 8 7) tempboard))
            (set! tempboard (makeMove (cons 8 8) (cons 8 6) tempboard))
            (is-in-check? tempboard player)))
         (if (not (null? z))
             (begin (makeMove (cons 8 8) (cons 8 6)) (set! board board1) #t)
             (begin #t))
         (begin (set! tempboard brd) #f))
     (if (and
          (equal? pos1 (cons 8 5))
          (equal? pos2 (cons 8 3))
          (= canCastleB1 0)
          (null? (list-ref (list-ref brd 7) 1))
          (null? (list-ref (list-ref brd 7) 2))
          (null? (list-ref (list-ref brd 7) 3))
          (not (in-check? player))
          (begin
            (set! tempboard (makeMove (cons 8 5) (cons 8 3) tempboard))
            (set! tempboard (makeMove (cons 8 1) (cons 8 4) tempboard))
            (is-in-check? tempboard player)))
         (if (not (null? z))
             (begin (makeMove (cons 8 1) (cons 8 4)) (set! board board1) #t)
             (begin #t))
         #f)))
  
  (if (not (null? z))
      (if (eq? player 'W) (set! enpB (list 0 0 0 0 0 0 0 0)) (set! enpW (list 0 0 0 0 0 0 0 0)))
      (void))
  (define piece (cdr (list-ref (list-ref brd (- row1 1)) (- col1 1))))
  (cond
    ((not (and (< col1 9) (< col2 9) (< row1 9) (< row2 9) (> col1 0) (> col2 0) (> row1 0) (> row2 0))) #f)
    ((equal? (car (list-ref (list-ref brd (- row1 1)) (- col1 1))) 
             (if (null? (list-ref (list-ref brd (- row2 1)) (- col2 1))) 'S
                 (car (list-ref (list-ref brd (- row2 1)) (- col2 1))))) #f)
    ((equal? piece `queen) (or (rookValid) (bishopValid)))
    ((equal? piece `bishop) (bishopValid))
    ((equal? piece `knight) (knightValid))
    ((equal? piece `pawn) (or (pawnValid) (enpEnable)))
    ((equal? piece `rook) (rookValid))
    ((equal? piece `king) (or (kingValid) (castlingEnable)))
    (else "Error: Should not have happened")))

(define (pawnPromote piece pos1 pos2)
  (if (eq? player 'W)
      (set! board (list (list-ref board 0)
                        (promoteH (list-ref board 1) (cdr pos1) piece)
                        (list-ref board 2)
                        (list-ref board 3)
                        (list-ref board 4)
                        (list-ref board 5)
                        (list-ref board 6)
                        (list-ref board 7)))
      (set! board (list (list-ref board 0)
                        (list-ref board 1)
                        (list-ref board 2)
                        (list-ref board 3)
                        (list-ref board 4)
                        (list-ref board 5)
                        (promoteH (list-ref board 6) (cdr pos1) piece)
                        (list-ref board 7))))
  (set! board1 board))

(define (promoteH lst pos piece)
  (if (= pos 1) (cons (cons player piece) (cdr lst))
      (cons (car lst) (promoteH (cdr lst) (- pos 1) piece))))

(define kpos (cons 1 1))

(define (in-check? player)
  (set! kpos (kingPos? board player))
  (define (enemyPos b i)
    (cond ((null? b) `())
          ((null? (car b)) (enemyPos (cdr b) (+ i 1)))
          ((eq? player (caar b)) (enemyPos (cdr b) (+ i 1)))
          (else (cons (if (= 0 (remainder i 8)) (cons (quotient i 8) 8) (cons (+ 1 (quotient i 8)) (remainder i 8)))
                      (enemyPos (cdr b) (+ i 1))))))
  (define bb (append
              (list-ref board 0)
              (list-ref board 1)
              (list-ref board 2)
              (list-ref board 3)
              (list-ref board 4)
              (list-ref board 5)
              (list-ref board 6)
              (list-ref board 7)))
  (define posen (enemyPos bb 1))
  (define (inch poss)
    (cond ((null? poss) #f)
          ((validmove (car poss) kpos board (if (eq? player 'B) 'W 'B)) #t)
          (else (inch (cdr poss)))))
  (inch posen))

(define (all-valid-moves)
  (define (iter i j)
    (cond ((= i 9) `())
          ((= j 9) (iter (+ i 1) 1))
          ((or (null? (list-ref (list-ref board (- i 1)) (- j 1))) (not (eq? player (car (list-ref (list-ref board (- i 1)) (- j 1)))))) (iter i (+ j 1)))
          ((null? (valid-moves (cons i j))) (iter i (+ j 1)))
          (else (append (valid-moves (cons i j)) (iter i (+ j 1))))))
  (iter 1 1))

(define (valid-moves pos)
  (define (create lamX lamY) 
    (define (predlist cnt l)
      (define posX (lamX cnt))
      (define posY (lamY cnt))
      
      (if (and (> posX 0) (< posX 9) (> posY 0) (< posY 9)) (predlist (+ 1 cnt) (append l (list (cons posX posY))))
          l))
    (predlist 1 '()))
  
  (define r (car pos))
  (define c (cdr pos))
  (define piece (cdr (list-ref (list-ref board (- r 1)) (- c 1))))
  (define color (car (list-ref (list-ref board (- r 1)) (- c 1))))
  (define Moves '())
  
   (set! Moves
        (cond
          ((equal? piece `pawn) (if (equal? color `W) (list
                                                       (cons (- r 1) c)
                                                       (cons (- r 2) c)
                                                       (cons (- r 1) (- c 1))
                                                       (cons (- r 1) (+ c 1)))
                                    (list
                                     (cons (+ r 1) c)
                                     (cons (+ r 2) c)
                                     (cons (+ r 1) (- c 1))
                                     (cons (+ r 1) (+ c 1)))))
          
          ((equal? piece `rook) (append (create (λ(x) x) (λ(y) c))
                                        (create (λ(x) r) (λ(y) y))))
          
          ((equal? piece `knight) (list (cons (- r 2) (+ c 1)) (cons (- r 2) (- c 1))
                                        (cons (+ r 2) (+ c 1)) (cons (+ r 2) (- c 1))
                                        (cons (- r 1) (+ c 2)) (cons (- r 1) (- c 2))
                                        (cons (+ r 1) (+ c 2)) (cons (+ r 1) (- c 2))))
          
          ((equal? piece `bishop) (append
                                   (if (> r c) (create (λ(x) (- 9 x)) (λ(y) (- (+ c 9) (+ r y)))) 
                                       (create (λ(x) (- (+ r 9) (+ c x))) (λ(y) (- 9 y))))
                                   
                                   (if (> (+ r c) 9) (create (λ(x) (- 9 x)) (λ(y) (+ r c y -9)))
                                       (create (λ(x) (- (+ r c) x)) (λ(y) y)))))
          
          ((equal? piece `queen) (append
                                  (append (create (λ(x) x) (λ(y) c))
                                          (create (λ(x) r) (λ(y) y)))
                                  (append
                                   (if (> r c) (create (λ(x) (- 9 x)) (λ (y) (- (+ c 9) (+ r y)))) 
                                       (create (λ(x) (- (+ r 9) (+ c x))) (λ(y) (- 9 y))))
                                   
                                   (if (> (+ r c) 9) (create (λ(x) (- 9 x)) (λ(y) (+ r c y -9)))
                                       (create (λ(x) (- (+ r c) x)) (λ(y) y))))))
          ((equal? piece `king) (append (list (cons (+ r 1) (+ c 1)) (cons (+ r 1) c) (cons (+ r 1) (- c 1))
                                              (cons r (+ c 1)) (cons r (- c 1))
                                              (cons (- r 1) (+ c 1)) (cons (- r 1) c) (cons (- r 1) (- c 1)))
                                        (if (equal? color `W) (list (cons 8 7) (cons 8 3))
                                            (list (cons 1 7) (cons 1 3)))))
          (else "Error : Shouldnt have happened")))
  
  (foldr (λ(x y) (if (passOn? pos x board color 'arbit) (cons x y)
                           y))
         '()
         Moves))
