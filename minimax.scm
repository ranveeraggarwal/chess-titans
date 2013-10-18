(define inf 500000000)
(include "evaluate.scm")
(include "syntax.scm")
(define (alpha-beta brd maxDepth curr-player)
  (define orig board) 
  (define optMove '())
  (define (helper board depthLeft alpha beta prev-alpha prev-beta  player)
    
    (define (maxLoop lst)
      (if (null? lst) (void)
          (begin
            (set! alpha  (max alpha (helper (makeMove (caar lst) (cadar lst) board) (- depthLeft 1) alpha beta prev-alpha prev-beta (if (equal? player 'B) 'W 'B))))
            (if (>= alpha beta) (void)
                (begin
                  (cond 
                    ((and (not (equal? prev-alpha alpha)) (= depthLeft maxDepth)) (begin
                                                                                   (set! optMove (car lst))
                                                                                   (set! prev-alpha alpha))))
                  (maxLoop (cdr lst)))))))
    
    
    (define (minLoop lst)
      (if (null? lst) (void)
          (begin
            (set! beta (min beta (helper (makeMove (caar lst) (cadar lst) board)  (- depthLeft 1) alpha beta prev-alpha prev-beta  (if (equal? player 'B) 'W 'B))))
            (if (>= alpha beta) (void)
                (begin
                  (cond 
                    [(and (not (equal? prev-beta beta)) (= depthLeft maxDepth)) (begin
                                                                                 (set! optMove (car lst))
                                                                                 (set! prev-beta beta))])
                  (minLoop (cdr lst)))))))
    
    
    (define possibleMovesList (allPossible board player))
    (cond
      [(and (null? possibleMovesList) (equal? player curr-player))
       (cond [(not (in-check? curr-player)) 
              (- (evaluate board) 5000000 (* 1000 depth))]
             [else (begin 
             	(define value (evaluate board)) (if (< value 0) (- value 1000) (+ value 1000)))])]
      
      [(and (null? possibleMovesList) (not (equal? player curr-player)))
       (cond [(not (in-check? curr-player))
              (+ 5000000 (evaluate board)) (* 1000 depth)]
             [else (begin 
             	(define value (evaluate board)) (if (< value 0) (- value 1000) (+ value 1000)))])]
      [(= depthLeft 0) (evaluate board)]
      [(equal? player curr-player) (begin 
                       (maxLoop possibleMovesList)
                       alpha)]
      [else (begin
              (minLoop possibleMovesList)
              beta)]))
  
  (begin
    (helper brd maxDepth (- inf) inf (- inf) inf 'B)
    (set! board orig)
    (set! board1 orig)
    optMove))


(define (allPossible brd player)
  (define (iter i j)
    (cond [(= i 0) `()]
          [(= j 0) (iter (- i 1) 8)]
          [(or (null? (list-ref (list-ref brd (- i 1)) (- j 1))) 
               (not (eq? player (car (list-ref (list-ref brd (- i 1)) (- j 1)))))) (iter i (- j 1))]
          [(null? (possibleMoves (cons i j) brd)) (iter i (- j 1))]
          [else (append (foldr (λ(x y) (cons (list (cons i j) x) y)) 
                               '() (possibleMoves (cons i j) brd)) (iter i (- j 1)))]))
  (iter 8 8))

(define (possibleMoves pos brd)
  (define (create lamX lamY)
    (define (predlist count l)
      (define posX (lamX count))
      (define posY (lamY count))
      
      (if (and (>= posX 1) (<= posX 8) (>= posY 1) (<= posY 8)) 
          (predlist (+ 1 count) (append l (list (cons posX posY))))
          l))
    (predlist 1 '()))
  
  (define r (car pos))
  (define c (cdr pos))
  (define piece (cdr (list-ref (list-ref brd (- r 1)) (- c 1))))
  (define color (car (list-ref (list-ref brd (- r 1)) (- c 1))))
  (define moves-list '())
  
  (set! moves-list
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
                                   (if (> r c) (create (λ(x) (- 9 x)) (λ(y) (- (+ c 9) (+ r y))))
                                       (create (λ(x) (- (+ r 9) (+ c x))) (λ(y) (- 9 y))))
                                   
                                   (if (> (+ r c) 9) (create (λ(x) (- 9 x)) (λ(y) (+ r c y -9)))
                                       (create (λ(x) (- (+ r c) x)) (λ(y) y))))))
          ((equal? piece `king) (append (list (cons (+ r 1) (+ c 1)) (cons (+ r 1) c) (cons (+ r 1) (- c 1))
                                              (cons r (+ c 1)) (cons r (- c 1))
                                              (cons (- r 1) (+ c 1)) (cons (- r 1) c) (cons (- r 1) (- c 1)))
                                        (if (equal? color `W) (list (cons 8 7) (cons 8 3))
                                            (list (cons 1 7) (cons 1 3)))))
          (else "Error: This should not have happened!")))
  
  (foldr (λ(x y) (if (passOn? pos x brd color 'not) (cons x y)
                           y))
         '()
         moves-list))


