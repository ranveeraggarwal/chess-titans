;(require "guitest.scm")
;Makes board on GUI
;Self contained
(define (imgAt x y tagStr) 
  
  (let* ([sqState (get-field occupancy (send board board-ref x y))])
    
    (if (not sqState)
        (if (even? (+ x y))
            (string-append "Images/white" tagStr ".png")
            (string-append "Images/black" tagStr ".png"))
        (cond [(is-a? sqState rook%) (string-append "Images/" 
                                                    (if (even? (+ x y)) "white" "black") 
                                                    (if (equal? (get-field color sqState) 'Black) "-B-" "-W-")
                                                    "rook" tagStr ".png")]
              [(is-a? sqState knight%) (string-append "Images/" 
                                                      (if (even? (+ x y)) "white" "black") 
                                                      (if (equal? (get-field color sqState) 'Black) "-B-" "-W-")
                                                      "knight" tagStr ".png")]
              [(is-a? sqState pawn%) (string-append "Images/" 
                                                    (if (even? (+ x y)) "white" "black") 
                                                    (if (equal? (get-field color sqState) 'Black) "-B-" "-W-")
                                                    "pawn" tagStr ".png")]
              [(is-a? sqState queen%) (string-append "Images/" 
                                                     (if (even? (+ x y)) "white" "black") 
                                                     (if (equal? (get-field color sqState) 'Black) "-B-" "-W-")
                                                     "queen" tagStr ".png")]
              [(is-a? sqState king%) (string-append "Images/" 
                                                    (if (even? (+ x y)) "white" "black") 
                                                    (if (equal? (get-field color sqState) 'Black) "-B-" "-W-")
                                                    "king" tagStr".png")]
              [(is-a? sqState bishop%) (string-append "Images/" 
                                                      (if (even? (+ x y)) "white" "black") 
                                                      (if (equal? (get-field color sqState) 'Black) "-B-" "-W-")
                                                      "bishop" tagStr ".png")]))))

(define (drawValidMoves board posn)
  (display posn) (newline)
  (display (get-field occupancy (send board board-ref (car posn) (cdr posn))))
  (let* ([piece (get-field occupancy (send board board-ref (car posn) (cdr posn)))]
         [pieceValidMoves (send piece valid-move)])
    (display "reached here")
    (display pieceValidMoves)
    (define (helper e)
      (let* ([x (car e)]
             [y (cdr e)])
        ((draw-pixmap Chess-Window) (imgAt x y "-possible-move") 
                                    (make-posn (+ horiz-inset (* imgWidth (- x 1))) (+ vert-inset (* imgHeight (- y 1))))  
                                    (make-rgb 0 0 0))))
    (for-each (λ(x) (helper x)) pieceValidMoves)))





(define (make-board)
  (define (makeRow y1 i)
    (define x (+ 1 (/ (- i horiz-inset) imgWidth)))
    (define y (+ 1 (/ (- y1 vert-inset) imgHeight)))
    (if (< i (+ horiz-inset (- width imgWidth)))
        (begin
          ((draw-pixmap Chess-Window) (imgAt x y "") (make-posn i y1) (make-rgb 0 0 0))
          (makeRow y1 (+ i imgWidth)))
        ((draw-pixmap Chess-Window) (imgAt x y "") (make-posn i y1) (make-rgb 0 0 0))))
  (define (makeMultRows i)
    (if (< i (- (+ height vert-inset) imgHeight))
        (begin (makeRow i horiz-inset)
               (makeMultRows (+ i imgHeight)))
        (makeRow i horiz-inset)))
  (makeMultRows vert-inset))
(define temp1 (cons 0 0));click 1
(define temp2 (cons 0 0));click2
(define temp3 1)

(define (movesMCL)
  (define posn (mouse-click-posn (get-mouse-click Chess-Window))) 
  (define x (+ 1 (quotient (- (posn-x posn) horiz-inset) imgWidth)))
  (define y (+ 1 (quotient (- (posn-y posn) vert-inset) imgHeight)))
  (display "x") (display x) (newline) (display "y") (display y) (newline)
  (if (and (odd? temp3) 
           (> (posn-x posn) horiz-inset) (> (posn-y posn) vert-inset) 
           (>= x 1) (>= y 1) (<= x 8) (<= y 8)  (not (get-field occupancy (send board board-ref x y))))
      (begin (display "empty") (movesMCL))
      (if (and (odd? temp3)   
               (> (posn-x posn) horiz-inset) (> (posn-y posn) vert-inset) 
               (>= x 1) (>= y 1) (<= x 8) (<= y 8) (not (equal? (get-field turn board) (get-field color (get-field occupancy (send board board-ref x y))))))
          (begin (display "not ur turn") (movesMCL))
          (if (and (odd? temp3)  
                   (> (posn-x posn) horiz-inset) (> (posn-y posn) vert-inset) 
                   (>= x 1) (>= y 1) (<= x 8) (<= y 8) (equal? (get-field turn board) (get-field color (get-field occupancy (send board board-ref x y)))) )
              (begin
                (display "ur turn")
                
                ((draw-pixmap Chess-Window) (imgAt x y "-selected") 
                                            (make-posn (+ horiz-inset (* imgWidth (- x 1))) (+ vert-inset (* imgHeight (- y 1)))) 
                                            (make-rgb 0 0 0))
                (set! temp1 (cons x y))
                (drawValidMoves board temp1)
                
                
                (set! temp3 (+ temp3 1))
                (movesMCL))
              (if  (and (even? temp3)  (> (posn-x posn) horiz-inset) (> (posn-y posn) vert-inset) 
                        (>= x 1) (>= y 1) (<= x 8) (<= y 8))
                        (begin (set! temp2 (cons x y))
                              
                        (if  (equal? temp1 temp2)
                             (begin
                               (set! temp3 (+ temp3 1))
                               (make-board)
                               (movesMCL))
                             (begin
                               (set! temp3 (+ temp3 1))
                               (send board make-move! temp1 temp2)
                               (make-board)
                               ;(set! board board1)
                               ; (send board print-board)
                               ;(newline)
                               (movesMCL))))
                        (movesMCL)
                        
                        
                        
                        ;(if (and (even? temp3) (> (posn-x posn) horiz-inset) (> (posn-y posn) vert-inset)
                        ;        (>= x 1) (>= y 1) (<= x 8) (<= y 8))
                        ;  (begin
                        ;   (set! temp2 (cons y x))
                        ;  (set! temp3 (+ temp3 1))
                        ; (if (and (not (equal? temp1 temp2)) (check? temp1 temp2 board turn))
                        ;    (begin
                        ;     (
                        
                        
                        )))))
      
      (define (play)
        (make-board)
        (movesMCL))
      
      