(include "eval.scm")
(define i 0)
(define (alphabeta myboard depth curr-player)
  (define tmp board)
  (define my-move 1)
  (define trash 0)
  (define (helper board depthLeft player alpha beta prevAlpha prevBeta)
    (define (loop1 l)
      (if (null? l) (set! trash 0)
          (begin (set! alpha (max alpha (helper (update (caar l) (cadar l) board) (- depthLeft 1) 
                                                (if (equal? player 'Black) 'White 'Black)
                                                alpha beta
                                                prevAlpha prevBeta)))
                 (if (>= alpha beta) (set! trash 0)
                     (begin (cond [(and (not (equal? prev-alpha alpha)) (= depth depthLeft)) (begin (set! my-move (car l))
                                                                                                    (set! prevAlpha alpha))])
                            (loop1 (cdr l)))))))
    
    (define (loop2 l)
      (if (null? l) (set! trash 0)
          (begin
            (set! beta (min beta (helper (update (caar l) (cadar l) board) (- depthLeft 1)
                                         (if (equal? player 'Black) 'White 'Black)
                                         alpha beta
                                         prevAlpha prevBeta)))
            (if (>= alpha beta) (set! trash 0)
                (begin
                  (cond [and (not (equal? prevBeta beta)) (= depth depthLeft) (begin
                                                                                (set! my-move (car l))
                                                                                (set! prevBeta beta))])
                  (loop2 (cdr l)))))))
    (define allPossibleMoves (send board all-possible player))
    (define allPossibleMoves2 allPossibleMoves)
    (cond [(and (null? allPossibleMoves) (equal? player curr-player))
           (cond [(not (inCheck? curr-player))
                  (- (- (evaluate board) 50000000) (* 1000 depth))]
                 [else (let* ([val (evaluate board)])
                         (if (< val 0) (- value 1000) (+ value 1000)))])]
          [(and (null? allPossibleMoves) (not (equal? player curr-player)))
           (cond [(not (inCheck? curr-player)
                       (+ 50000000 (evaluate board) (* 1000 depth)))]
                 [else (let* ([val (evaluate board)])
                         (if (< val 0) (- value 1000) (+ value 1000)))])]
          [(= depth 0) (evaluate board)]
          [(equal? player curr-player)
           (begin (set! i 1)
                  (loop1 allPossibleMoves)
                  alpha)]
          [else (begin (loop2 allPossibleMoves) beta)]))
  
  (begin (helper myboard depth -10000000000 10000000000 -10000000000 10000000000 'Black)
         (set! board tmp)
         (set! board1 tmp)
                 
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  