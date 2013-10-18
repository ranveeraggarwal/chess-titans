;#lang racket
;(include "pieces.scm")
;(include "syntactic-sugar.scm") Can remove
(include "syntax.scm")
;;;;;;;;;;;;;;;;;;;;;;;;Tarun
(define square%
  (class object%
    (init pos)
    (define position pos)
    (init-field (occupancy #f))
    (super-new)
    (define/public (occupied?)
      occupancy)))
; White goes from 8 to 1, black goes from 1 to 8
(define board%
  (class object%
    (super-new)
    (init-field [white-captured (make-vector 5 0)])	; rook,knight,bishop,queen,pawn
    (init-field [black-captured (make-vector 5 0)])
    (init-field [turn 'White])
    ;teh actual board
    (init-field (board
                 (list->vector 
                  (lc 
                   (list->vector(lc (new square% [pos (cons i j)]) : i <- '(1 2 3 4 5 6 7 8))) : j <- '(1 2 3 4 5 6 7 8)))))
    ;Refer to a particular position on the board
    (define/public (board-ref i j)
      
      (vector-ref (vector-ref board (- i 1)) (- j 1)))
    ; Set on the i,j th position piece(or blank) 
    (define/public (board-set! i j piece)
      (let* ([pos (board-ref i j)])
        (set-field! occupancy pos piece)
        )) ;;;;;;;;;;;;What is curr-pos represented as?
    
    (define/public (print-board)
      (for-each (λ(y) (for-each (λ(x) (display  (get-field occupancy (send this board-ref x y))) (display "  ")) '(1 2 3 4 5 6 7 8)) (newline)) '(1 2 3 4 5 6 7 8)))
    
    ;Move piece to the new position i,j
    (define/public (move-piece! i j piece)
      (let* ([newpos (board-ref i j)]
             [piecepos (get-field curr-pos piece)]
             [piececolor (get-field color piece)]
             [newpospiece (get-field occupancy newpos)]
             [nppcolor (if newpospiece (get-field color newpospiece)
                           #f)])
        ; If piece in new pos, then obviously it is going to be elim.
        (if newpospiece
            (begin  
              (if (equal? nppcolor 'White)
                  (cond [(is-a? newpospiece rook%)
                         (vector-set! white-captured 0 (+ 1 (vector-ref white-captured 0)))]
                        [(is-a? newpospiece knight%)
                         (vector-set! white-captured 1 (+ 1 (vector-ref white-captured 1)))]
                        [(is-a? newpospiece bishop%)
                         (vector-set! white-captured 2 (+ 1 (vector-ref white-captured 2)))]
                        [(is-a? newpospiece queen%)
                         (vector-set! white-captured 3 (+ 1 (vector-ref white-captured 3)))]
                        [(is-a? newpospiece pawn%)
                         (vector-set! white-captured 4 (+ 1 (vector-ref white-captured 4)))])
                  (cond [(is-a? newpospiece rook%)
                         (vector-set! black-captured 0 (+ 1 (vector-ref black-captured 0)))]
                        [(is-a? newpospiece knight%)
                         (vector-set! black-captured 1 (+ 1 (vector-ref black-captured 1)))]
                        [(is-a? newpospiece bishop%)
                         (vector-set! black-captured 2 (+ 1 (vector-ref black-captured 2)))]
                        [(is-a? newpospiece queen%)
                         (vector-set! black-captured 3 (+ 1 (vector-ref black-captured 3)))]
                        [(is-a? newpospiece pawn%)
                         (vector-set! black-captured 4 (+ 1 (vector-ref black-captured 4)))]))
              ;now set on the board's ijth pos that piece
              (board-set! i j piece)
              (board-set! (car piecepos) (cdr piecepos) #f)
              (set-field! curr-pos piece (cons i j)));;;;;Is there anything else?       
            ; Otherwise, either promotion or just a move;;Enp and castling to be done!!!!!!
            ;;;;;;;;;;;;;;;;;;;Made changed color->col (I feel this isnt quite correct) Look it up!
            ;(if (is-a? piece pawn?)
            ;   (when (and (equal? piececolor 'White) (= j 8))
            ;    (let* ([opt (read)])
            ;     (cond [(equal? opt 'Queen) (begin (board-set! i j (new queen% [curr-pos (cons i j)] [col (get-field color piece)])))]
            ;          [(equal? opt 'Rook) (begin (board-set! i j (new rook% [curr-pos (cons i j)] [col (get-field color piece)])))]
            ;         [(equal? opt 'Bishop) (begin (board-set! i j (new bishop% [curr-pos (cons i j)] [col (get-field color piece)])))]
            ;        [(equal? opt 'Knight) (begin (board-set! i j (new knight% [curr-pos (cons i j)] [col (get-field color piece)])))])
            ;  (board-set! (car piecepos) (cdr piecepos) #f)))
            (begin (board-set! i j piece)
                   (board-set! (car piecepos) (cdr piecepos) #f)
                   (set-field! curr-pos piece (cons i j) ))
            ;)
            )))
    ;;;;;;;;;;;;;;;;;;;Made changes in initialise(CHECK)
    (define/public (initialise)
      (define (helper j i)
        (cond[(= i 1)
              (cond [(= j 1) (board-set! j i (new rook% [curr-pos (cons j i)] [col 'Black]))]
                    [(= j 2) (board-set! j i (new knight% [curr-pos (cons j i)] [col 'Black]))]
                    [(= j 3) (board-set! j i (new bishop% [curr-pos (cons j i)] [col 'Black]))]
                    [(= j 4) (board-set! j i (new queen% [curr-pos (cons j i)] [col 'Black]))]
                    [(= j 5) (board-set! j i (new king% [curr-pos (cons j i)] [col 'Black]))]
                    [(= j 6) (board-set! j i (new bishop% [curr-pos (cons j i)] [col 'Black]))]
                    [(= j 7) (board-set! j i (new knight% [curr-pos (cons j i)] [col 'Black]))]
                    [(= j 8) (board-set! j i (new rook% [curr-pos (cons j i)] [col 'Black]))])]
             [(= i 8)
              (cond [(= j 1) (board-set! j i (new rook% [curr-pos (cons j i)] [col 'White]))]
                    [(= j 2) (board-set! j i (new knight% [curr-pos (cons j i)] [col 'White]))]
                    [(= j 3) (board-set! j i (new bishop% [curr-pos (cons j i)] [col 'White]))]
                    [(= j 4) (board-set! j i (new queen% [curr-pos (cons j i)] [col 'White]))]
                    [(= j 5) (board-set! j i (new king% [curr-pos (cons j i)] [col 'White]))]
                    [(= j 6) (board-set! j i (new bishop% [curr-pos (cons j i)] [col 'White]))]
                    [(= j 7) (board-set! j i (new knight% [curr-pos (cons j i)] [col 'White]))]
                    [(= j 8) (board-set! j i (new rook% [curr-pos (cons j i)] [col 'White]))])]
             [(= i 2) (board-set! j i (new pawn% [curr-pos (cons j i)] [col 'Black]))]
             [(= i 7) (board-set! j i (new pawn% [curr-pos (cons j i)] [col 'White]))]
             [else (board-set! j i #f)]))
      (set-field! turn this 'White)
      (for-each (λ(y) (for-each (λ(x) (helper x y)) '(1 2 3 4 5 6 7 8))) '(1 2 3 4 5 6 7 8)))
    
    (define/public (make-move! ppos npos)
      (let* ([posPx (car ppos)]
             [posPy (cdr ppos)]
             [posNx (car npos)]
             [posNy (cdr npos)]
             [piece (get-field occupancy (board-ref posPx posPy))]
             [validMovesList (begin  (send piece get-valid-moves))]
             [isMoveValid? (member (cons posNx posNy) validMovesList)])
       (newline) (display validMovesList) (newline)     
        (when isMoveValid?
            (begin(move-piece! posNx posNy piece))
            ))
      (set-field! turn this (if (equal? turn 'White) 'Black 'White))    
      )
    
    (define (evaluateMaterial)
      (define ans 0)
      (define numWhiteKing 0)
      (define numBlackKing 0)
      (define numWhiteQueens 0)
      (define numBlackQueens 0)
      (define numWhiteKnights 0)
      (define numBlackKnights 0)
      (define numWhiteRooks 0)
      (define numBlackRooks 0)
      (define numWhiteBishops 0)
      (define numBlackBishops 0)
      (define numWhitePawns 0)
      (define numBlackPawns 0)
      (define x 1) (define y 1)
      (define (incrIndex)
        (cond [(= x 8) (begin (set! x 1) (set! y (+ y 1)))]
              [else (set! x (+ x 1))]))
      (define (nums i j)
        (let* ([piece (get-field occupancy (board-ref i j))])
          (if (not piece)
              0
              (if (equal? (get-field color piece) 'Black)
                  (cond [(is-a? piece rook%) (+ 1 numBlackRooks) ]
                        [(is-a? piece bishop%) (+ 1 numBlackBishops)]
                        [(is-a? piece pawn%) (+ 1 numBlackPawns)]
                        [(is-a? piece king%) (+ 1 numBlackKing)]
                        [(is-a? piece queen%) (+ 1 numBlackQueens)]
                        [(is-a? piece knight%) (+ 1 numBlackKnights)])
                  (cond [(is-a? piece rook%) (+ 1 numWhiteRooks) ]
                        [(is-a? piece bishop%) (+ 1 numWhiteBishops)]
                        [(is-a? piece pawn%) (+ 1 numWhitePawns)]
                        [(is-a? piece king%) (+ 1 numWhiteKing)]
                        [(is-a? piece queen%) (+ 1 numWhiteQueens)]
                        [(is-a? piece knight%) (+ 1 numWhiteKnights)])))))
      
      (for (set! x 1) : (and (< x 9) (< y 9)) : (incrIndex) : (nums x y))    
      (define MaterialBlack (+  (* numBlackKing 100000) (* numBlackPawns 100) (* numBlackQueens 980)
                                (* numBlackRooks 520) (* numBlackKnights 330) (* numBlackBishops 330)))
      (define MaterialWhite (+  (* numWhiteKing 100000) (* numWhitePawns 100) (* numWhiteQueens 980)
                                (* numWhiteRooks 520) (* numWhiteKnights 330) (* numWhiteBishops 330)))
      
      (cons MaterialWhite MaterialBlack))
            ))
          
      
      
      ;    (define (evaluateMaterial)
      ;      (define ans 0)
      ;      (define whiteKing 100000)
      ;      (define blackKing 100000)
      ;      (define whiteQueens 980)
      ;      (define blackQueens 980)
      ;      (define whiteKnights 330)
      ;      (define blackKnights 330)
      ;      (define whiteRooks 520)
      ;      (define blackRooks 520)
      ;      (define whiteBishops 330)
      ;      (define blackBishops 330)
      ;      (define whitePawns 100)
      ;      (define blackPawns 100)
      ;      (define x 1) (define y 1)
      ;      (define (incrIndex)
      ;            (cond [(= x 8) (begin (set! x 1) (set! y (+ y 1)))]
      ;                  [else (set! x (+ x 1))]))
      ;      (define (val i j)
      ;        (let* ([piece (get-field occupancy (board-ref i j))])
      ;          (if (not piece)
      ;              0
      ;              (cond [(is-a? piece rook%) 520]
      ;                    [(is-a? piece bishop%) 330]
      ;                    [(is-a? piece pawn%) 100]
      ;                    [(is-a? piece king%) 100000]
      ;                    [(is-a? piece queen%) 980]
      ;                    [(is-a? piece knight%) 330]))))
      ;      (for (set! x 1) : (and (< x 9) (< y 9)) : (incrIndex) : (set! ans (+ ans (val x y))))      
      ;    ans)))
      
      
      ;;;;;;Made changes
      (define board (new board%))
      (send board initialise)
      ;(send board print-board)
      ;(newline)
      ;(display (get-field occupancy (send board board-ref 3 1)))
      ;(newline)
      ;(get-field color (get-field occupancy (send board board-ref 3 1)))
      
      
