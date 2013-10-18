#lang racket
(provide (all-defined-out))
(struct node (alpha beta bstate) #:transparent #:mutable)

(define (alphabeta v depth-left)
  (define nb 




;(define (getBestMove brd)
;  (define allPossibleMoves (send brd allPossibleMoves))
;  (define optMove '())
;  (define maxScore 0)
;  (define (whatToDo mv)
;    (set! board1 brd)
;    (define score (negamax board1 depth1 alpha beta 'Black))
;    (if (> score maxScore)
;        (begin (set! maxScore score)
;               (set! optMove mv))
;        (void)))
;  (begin (for-each (λ(x) (whatToDo x)) allPossibleMoves)
;         optMove))
;
;(define (negamax brd dep a b player)
;  (if (or (= dep 0) ( null? (send brd getPossibleMoves player)))
;      (evaluate brd)
;      
;  
;      
;  
