(define (alphabeta myboard depth curr-player)
  (define store board)
  (define my-move 1)
  (define junk 0)
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
    (define