(include "syntax.scm")

(define (evaluate brd persp)
  ;Counters to evaluate pawn structure(all size 8 except maxpawncolorbins->2)
  (define MaxPawnFileBins (build-vector 8 (λ(x) 0))) 
  (define MaxPawnColorBins (build-vector 2 (λ(x) 0))) 
  (define MaxTotalPawns 0) 
  (define MaxMostAdvanced (build-vector 8 (λ(x) 0))) 
  (define MaxPassedPawns (build-vector 8 (λ(x) 0))) 
  (define MinPawnFileBins (build-vector 8 (λ(x) 0))) 
  (define MinMostBackward (build-vector 8 (λ(x) 0))) 
  (define PawnRams 0)
  
  (define grain 3)
  ;brd and perspective
  (define (AnalyzePawnStructure bd persp)
    (set! MaxPawnFileBins (build-vector 8 (λ(x) 0))) 
    (set! MaxPawnColorBins (build-vector 2 (λ(x) 0))) 
    (set! MaxTotalPawns 0) 
    (set! MinPawnFileBins (build-vector 8 (λ(x) 0))) 
    (set! PawnRams 0)
    (if (equal? persp 'White)
        (begin 
          (set! MaxMostAdvanced (build-vector 8 (λ(x) (cons 8 8)))) 
          (set! MinMostBackward (build-vector 8 (λ(x) (cons 8 8))))
          (set! MaxPassedPawns (build-vector 8 (λ(x) (cons 8 8))))
          (define x 8) (define y 7)
          (define (decrIndex)
            (cond [(= x 1) (begin (set! x 8) (set! y (- y 1)))]
                  [else (set! x (- x 1))]))
          
          (for (void) : (> y 1) : decrIndex : (begin 
                                                (cond[ (and (get-field occupancy (send bd board-ref x y)) 
                                                            (is-a? (get-field occupancy (send bd board-ref x y)) pawn%)
                                                            (equal? 'White (get-field color (get-field occupancy (send bd board-ref x y)))))
                                                       (begin (define rank y)
                                                              (define file x)
                                                              ;Most advanced on all white pawns on it's file
                                                              (vector-set! MaxPawnFileBins file (+ 1 (vector-ref MaxPawnFileBins file)))
                                                              (set! MaxTotalPawns (+ MaxTotalPawns 1))
                                                              (vector-set! MaxMostAdvanced file (cons x y))
                                                              ;on white square or black square
                                                              (if (= (modulo rank 2) (modulo file 2))
                                                                  (vector-set! MaxPawnColorBins 0 (+ 1 (vector-ref MaxPawnColorBins 0)))
                                                                  (vector-set! MaxPawnColorBins 1 (+ 1 (vector-ref MaxPawnColorBins 1))))
                                                              ;look for a pawn ram(black immediately in front of white pawn
                                                              (cond[ (and (get-field occupancy (send bd board-ref x (- y 1)))
                                                                          (is-a? (get-field occupancy (send bd board-ref x (- y 1))) pawn%)
                                                                          (equal? (get-field color (get-field occupancy (send bd board-ref x (- y 1)))) 'Black))
                                                                     (set! PawnRams (+ PawnRams 1))]))]
                                                     [(and (get-field occupancy (send bd board-ref x y)) 
                                                           (is-a? (get-field occupancy (send bd board-ref x y)) pawn%)
                                                           (equal? 'Black (get-field color (get-field occupancy (send bd board-ref x y)))))
                                                      (begin (define file x)  
                                                             (vector-set! MinPawnFileBins file (+ 1 (vector-ref MinPawnFileBins file)))
                                                             (vector-set! MinMostBackward file (cons x y)))]))))
        (begin 
          (set! MaxMostAdvanced (build-vector 8 (λ(x) (cons 1 1)))) 
          (set! MinMostBackward (build-vector 8 (λ(x) (cons 1 1))))
          (set! MaxPassedPawns (build-vector 8 (λ(x) (cons 1 1))))
          (define x 1) (define y 2)
          (define (incrIndex)
            (cond [(= x 8) (begin (set! x 1) (set! y (+ y 1)))]
                  [else (set! x (+ x 1))]))
          
          (for (void) : (< y 8) : decrIndex : (begin 
                                                (cond[ (and (get-field occupancy (send bd board-ref x y)) 
                                                            (is-a? (get-field occupancy (send bd board-ref x y)) pawn%)
                                                            (equal? 'Black (get-field color (get-field occupancy (send bd board-ref x y)))))
                                                       (begin (define rank y)
                                                              (define file x)
                                                              ;Most advanced of all white pawns on it's file
                                                              (vector-set! MaxPawnFileBins file (+ 1 (vector-ref MaxPawnFileBins file)))
                                                              (set! MaxTotalPawns (+ MaxTotalPawns 1))
                                                              (vector-set! MaxMostAdvanced file (cons x y))
                                                              ;on white square or black square
                                                              (if (= (modulo rank 2) (modulo file 2))
                                                                  (vector-set! MaxPawnColorBins 0 (+ 1 (vector-ref MaxPawnColorBins 0)))
                                                                  (vector-set! MaxPawnColorBins 1 (+ 1 (vector-ref MaxPawnColorBins 1))))
                                                              ;look for a pawn ram(white pawn immediately in front of black pawn
                                                              (cond[ (and (get-field occupancy (send bd board-ref x (+ y 1))) 
                                                                          (is-a? (get-field occupancy (send bd board-ref x (+ y 1))) pawn%)
                                                                          (equal? (get-field color (get-field occupancy (send bd board-ref x (+ y 1)))) 'White))
                                                                     (set! PawnRams (+ PawnRams 1))]))]
                                                     [(and (get-field occupancy (send bd board-ref x y)) 
                                                           (is-a? (get-field occupancy (send bd board-ref x y)) pawn%)
                                                           (equal? 'White (get-field color (get-field occupancy (send bd board-ref x y)))))
                                                      (begin (define file x)  
                                                             (vector-set! MinPawnFileBins file (+ 1 (vector-ref MinPawnFileBins file)))
                                                             (vector-set! MinMostBackward file (cons x y)))]))))) #t)
  ;;;;;;;;;;;;;Finished analyze pawn structure ....Needless to say, it's huge and dirty!
  ;;Will just be called
  (define (materialValue)
    (let* ([pairVal (send brd evaluateMaterial)])
      (if (equal? persp 'White)
          (- (car pairVal) (cdr pairVal))
          (- (cdr pairVal) (car pairVal)))))
  
  (define (evalRookBonus)
    (define ans 0)
    (define (calc x y)
      (let* ([piece (get-field occupancy (send brd board-ref x y))])
        (if (not piece)
            0
            (begin
              (cond [(and (is-a? piece rook%) (equal? persp (get-field color piece)))
                     (if (and (equal? persp 'White) (= y 2))
                         (set! ans (+ ans 22))
                         (and (equal? persp 'Black) (= y 7))
                         (set! ans (+ ans 22)))]
                    [else 0])
              (when (= 0 (vector-ref MaxPawnFileBins x))
                  (if (= 0 (vector-ref MinPawnFileBins x))
                      (set! ans (+ ans 10))
                      (set! ans (+ ans 4)))
              (define x 1) (define y 1)
              (define (incrIndex)
                (cond [(= x 8) (begin (set! x 1) (set! y (+ y 1)))]
                      [else (set! x (+ x 1))]))
              (for (set! x 1) : (and (< x 9) (< y 9)) : incrIndex : (calc x y))
              
              
              
              
              
              
              
              
              
              
              