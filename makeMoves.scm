(define (modifyBoard brd x y elt)
  (define (modify j e l)       
    (if (= 1 j) (append (list e) (cdr l))
        (append (list (car l)) (modify (- j 1) e (cdr l)))))
  (modify x (modify y elt (list-ref brd (- x 1))) brd))

(define (makeMove pos1 pos2 . l)  ; l contains initial given board, also serves to return makeMoved board
  (let* ([pos1X (car pos1)]
         [pos1Y (cdr pos1)]
         [pos2X (car pos2)]
         [pos2Y (cdr pos2)])
    (cond
      [(null? l) (begin 
                   (define piece 
                   (list-ref (list-ref board1 (- pos1X 1)) (- pos1Y 1)))
                   (set! board1 (modifyBoard 
                                 (modifyBoard board1 pos1X pos1Y '()) 
                                 pos2X pos2Y piece)))]
      [else (begin 
              (define piece (list-ref (list-ref (car l) 
                                                (- pos1X 1))  (- pos1Y 1)))
              (modifyBoard (modifyBoard (car l) 
                                        pos1X pos1Y '()) pos2X pos2Y piece))])))



(define (access square)
  (list-ref (list-ref board (- (cdr square) 1)) (- (car square) 1)))



