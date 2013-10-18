;#lang racket
;(include "syntax.scm")
;(include "pieces.scm")
;(include "board.scm")

;;;Piece rating functions
(define (taxicabDistance pos1 pos2)
  (+ (abs (- (car pos1) (car pos2))) (abs (- (cdr pos1) (cdr pos2)))))



(define (evaluate brd)
  ;King,Queen,Rook,Knight,Bishop,Pawn
  (define blackPieces (vector '() '() '() '() '() '()))
  (define whitePieces (vector '() '() '() '() '() '()))
  ;DONT FORGET TO CALL THIS ALSO!!!!!!
  (define (getAllPieces)
    (define (whereToPush? x y)
      
      (let* ([pc (get-field occupancy (send brd board-ref x y))])
        (if (not pc)
            (void)
            (let* ([col (get-field color pc)])
              (if (equal? col 'Black)
                  (cond [(is-a? pc king%) (vector-set! blackPieces 0 (append (list(cons x y)) (vector-ref blackPieces 0)))]
                        [(is-a? pc queen%) (vector-set! blackPieces 1 (append (list(cons x y))  (vector-ref blackPieces 1)))]
                        [(is-a? pc rook%) (vector-set! blackPieces 2 (append (list(cons x y))  (vector-ref blackPieces 2)))]
                        [(is-a? pc knight%) (vector-set! blackPieces 3 (append (list(cons x y))  (vector-ref blackPieces 3)))]
                        [(is-a? pc bishop%) (vector-set! blackPieces 4 (append (list(cons x y))  (vector-ref blackPieces 4)))]
                        [(is-a? pc pawn%) (vector-set! blackPieces 5 (append (list(cons x y))  (vector-ref blackPieces 5)))])
                  (cond [(is-a? pc king%) (vector-set! whitePieces 0 (append (list(cons x y))  (vector-ref whitePieces 0)))]
                        [(is-a? pc queen%) (vector-set! whitePieces 1 (append (list(cons x y))  (vector-ref whitePieces 1)))]
                        [(is-a? pc rook%) (vector-set! whitePieces 2 (append (list(cons x y))  (vector-ref whitePieces 2)))]
                        [(is-a? pc knight%) (vector-set! whitePieces 3 (append (list(cons x y))  (vector-ref whitePieces 3)))]
                        [(is-a? pc bishop%) (vector-set! whitePieces 4 (append (list(cons x y))  (vector-ref whitePieces 4)))]
                        [(is-a? pc pawn%) (vector-set! whitePieces 5 (append (list(cons x y))  (vector-ref whitePieces 5)))]))))))
    (define x 1) (define y 1)
    (define (incrIndex)
      (cond [(= x 8) (begin (set! x 1) (set! y (+ y 1)))]
            [else (set! x (+ x 1))]))
    (for (void) : (and (< x 9) (< y 9)) : (incrIndex) : (whereToPush? x y)))
  
  (getAllPieces)
  
  ;;;;Call this as well.
  (define NetMaterial
    (+ (* 100 (- (length(vector-ref blackPieces 5))
                 (length(vector-ref whitePieces 5))))
       (* 330 (- (length(vector-ref blackPieces 4))
                 (length(vector-ref whitePieces 4))))
       (* 330 (- (length(vector-ref blackPieces 3))
                 (length(vector-ref whitePieces 3))))
       (* 520 (- (length(vector-ref blackPieces 2))
                 (length(vector-ref whitePieces 2))))
       (* 980 (- (length(vector-ref blackPieces 1))
                 (length(vector-ref whitePieces 1))))
       (* 50000 (- (length(vector-ref blackPieces 0))
                 (length(vector-ref whitePieces 0))))
       ))
  ;(display (length (vector-ref blackPieces 3)))(newline)
 ; (display NetMaterial)(newline)
  (define TotalMaterial
    (+ (* 100 (+ (length(vector-ref blackPieces 5))
                 (length(vector-ref whitePieces 5))))
       (* 330 (+ (length(vector-ref blackPieces 4))
                 (length(vector-ref whitePieces 4))))
       (* 330 (+ (length(vector-ref blackPieces 3))
                 (length(vector-ref whitePieces 3))))
       (* 520 (+ (length(vector-ref blackPieces 2))
                 (length(vector-ref whitePieces 2))))
       (* 980 (+ (length(vector-ref blackPieces 1))
                 (length(vector-ref whitePieces 1))))
       (* 50000 (- (length(vector-ref blackPieces 0))
                 (length(vector-ref whitePieces 0))))
                 ))          
  
  ;(define (game-play)
   ; (cond [(> TotalMaterial 6500) -1]
    ;      [(> TotalMaterial 3500) 0]
     ;     [else 1]))
  
  ;;Everything to do with Pawns
  (define (isoHelper x y player)      
    (cond[(equal? player 'Black) 
                  (if (null? (lc p : p <- (vector-ref blackPieces 5) @ (or (equal? (car p) (- x 1))
                                                                           (equal? (car p) (+ x 1)))))
                      #t #f)]
         [(equal? player 'White) 
                  (if (null? (lc p : p <- (vector-ref whitePieces 5) @ (or (equal? (car p) (- x 1))
                                                                           (equal? (car p) (+ x 1)))))
                      #t #f)]))
  
  (define (isolatedPawns)
    
    (define x 1) (define y 1)
    (define (incrIndex)
      (cond [(= x 8) (begin (set! x 1) (set! y (+ y 1)))]
            [else (set! x (+ x 1))]))
    (define (penalize x y player)
      (cond [(not (isoHelper x y player)) 0]
            [(or (= x 4) (= x 5)) 20]
            [(or (= x 3) (= x 6)) 16]
            [(or (= x 2) (= x 7)) 14]
            [(or (= x 1) (= x 8)) 12]))
    (define blackPenalty 0)
    (define whitePenalty 0)
    (define (penal x y player)
      (if (equal? player 'Black)
          (+ blackPenalty (penalize x y player))
          (+ whitePenalty (penalize x y player))))
    (for (void) : (and (< x 9) (< y 9)) : (incrIndex) : (penal x y 'Black)) 
    (for (void) : (and (< x 9) (< y 9)) : (incrIndex) : (penal x y 'White)) 
    (- whitePenalty blackPenalty))
  
  
  (define (doubledPawns)
    (define blk 0) (define wht 0)
    (define (helper f)
      (let* ([bpInF (length (filter (λ(c) (and (= (car c) f) (not (isoHelper (car c) (cdr c) 'Black)))) 
                                    (lc p : p <- (vector-ref blackPieces 5))))]
             [wpInF (length (filter (λ(c) (and (= (car c) f) (not (isoHelper (car c) (cdr c) 'White)))) 
                                    (lc p : p <- (vector-ref whitePieces 5))))])
        ;for black
        (when (>= bpInF 2) (set! blk (+ blk (* bpInF 6))))
        (when (>= wpInF 2) (set! wht (+ wht (* bpInF 6))))))
    (for-each (λ(f) (helper f)) '(1 2 3 4 5 6 7 8))
    (- wht blk))
  
  (define (rankPenalty)
    (let* ([wd7 (and (send brd board-ref 4 7)
                     (is-a? (get-field occupancy (send brd board-ref 4 7)) pawn%)
                     (equal? 'White (get-field 
                                     color 
                                     (get-field occupancy (send brd board-ref 4 7)))))]
           [we7 (and (send brd board-ref 5 7)
                     (is-a? (get-field occupancy (send brd board-ref 5 7)) pawn%)
                     (equal? 'White (get-field 
                                     color 
                                     (get-field occupancy (send brd board-ref 5 7)))))]
           [bd2 (and (send brd board-ref 4 2)
                     (is-a? (get-field occupancy (send brd board-ref 4 2)) pawn%)
                     (equal? 'Black (get-field 
                                     color 
                                     (get-field occupancy (send brd board-ref 4 2)))))]
           [be2 (and (send brd board-ref 5 2)
                     (is-a? (get-field occupancy (send brd board-ref 5 2)) pawn%)
                     (equal? 'Black (get-field 
                                     color 
                                     (get-field occupancy (send brd board-ref 5 2)))))])
      ( - (+ (if we7 10 0) (if wd7 10 0)) (+ (if bd2 10 0) (if be2 10 0)))))
 
  (define (queenTaxicabPenalty)
    (let* ([wk (car (vector-ref whitePieces 0))]
           [bk (car (vector-ref blackPieces 0))]
           [wql (vector-ref whitePieces 1)]
           [bql (vector-ref blackPieces 1)]
           [penwb (/ (foldr (λ(x y) (+ (taxicabDistance x bk) y)) 0 wql) 3)]
           [penbw (/ (foldr (λ(x y) (+ (taxicabDistance x wk) y)) 0 bql) 3)])
      (- penbw penwb)))
  
  (define (rookTaxicabPenalty)
    (let* ([wk (car (vector-ref whitePieces 0))]
           [bk (car (vector-ref blackPieces 0))]
           [wql (vector-ref whitePieces 2)]
           [bql (vector-ref blackPieces 2)]
           [penwb (/ (foldr (λ(x y) (+ (taxicabDistance x bk) y)) 0 wql) 3)]
           [penbw (/ (foldr (λ(x y) (+ (taxicabDistance x wk) y)) 0 bql) 3)])
      (- penbw penwb)))
 
  (define (knightsCenterProximity)
    (let* ([wkn (vector-ref whitePieces 3)]
           [bkn (vector-ref blackPieces 3)]
           [c1 (cons 4 4)]
           [c2 (cons 4 5)]
           [c3 (cons 5 4)]
           [c4 (cons 5 5)]
           [wval (foldr (λ(x y) (+ (min (taxicabDistance c1 x)
                                        (taxicabDistance c2 x)
                                        (taxicabDistance c3 x)
                                        (taxicabDistance c4 x)) y)) 0 wkn)]
           [bval (foldr (λ(x y) (+ (min (taxicabDistance c1 x)
                                        (taxicabDistance c2 x)
                                        (taxicabDistance c3 x)
                                        (taxicabDistance c4 x)) y)) 0 bkn)])
      (* (- wval bval) 1.5)))
  
  (+ NetMaterial (knightsCenterProximity)
     (rookTaxicabPenalty) (queenTaxicabPenalty)
     (rankPenalty) (doubledPawns) (isolatedPawns)))
                                        
      
      
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
                                     
