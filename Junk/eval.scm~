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
                                                (if (and (get-field occupancy (send bd board-ref x y)) 
                                                         (is-a? (get-field occupancy (send bd board-ref x y)) pawn%)
                                                         (get-field color (get-field occupancy (send bd board-ref x y))))
                                                    (begin (define rank y)
                                                           (define file x)
                                                           (vector-set! MaxPawnFileBins file (+ 1 (vector-ref MaxPawnFileBins file)))
                                                           (
                                                                                         
                                                                                         
                                                                                         
                                                                                         
                                                                                         (for-each (λ(x) (for-each (λ(y) (begin 
                                                                                                                           (if (and (get-field occupancy (send bd board-ref x y)) 
                                                                                                                                    (is-a? (get-field occupancy (send bd board-ref x y)) pawn%)
                                                                                                                                    (get-field color (get-field occupancy (send bd board-ref x y))))
                                                                                                                               (begin (define rank y)
                                                                                                                                      (define file x)
                                                                                                                                      (vector-set! MaxPawnFileBins (
                                                                                                                                                                    
                                                                                                                                                                    