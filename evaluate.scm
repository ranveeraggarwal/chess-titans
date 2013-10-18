(include "syntax.scm")
(define (evaluate brd)
  (define tempboard board)
  (define tempboard1 board1)
  (set! board brd)
  (define tempplayer player)
  
  (define color `B)
  (define othercolor `W)
  (define (worth? piece)
    (cond
      [(equal? piece `queen) 980]
      [(equal? piece `bishop) 330]
      [(equal? piece `knight) 330]
      [(equal? piece `rook) 520]
      [(equal? piece `pawn) 100]
      [(equal? piece `king) 50000]
      [else "Incorrect piece"]))
  
  (define score 0)
  
  (define (helper i j)
    (let* ([pc (list-ref (list-ref brd (- i 1)) (- j 1))])
      (if (null? pc)
          0
          (if (equal? color (car pc))
              (set! score (+ score (worth? (cdr pc))))
              (set! score (- score (worth? (cdr pc))))))))
  
  
  
  
  (define (addpoints)
    (for-each (λ(i) (for-each (λ(j) (helper i j)) '(1 2 3 4 5 6 7 8))) '(1 2 3 4 5 6 7 8)))
    
  
  
  (begin
    (addpoints)
    (set! board1 tempboard1)
    (set! board tempboard)
    (set! player tempplayer)
    score))
