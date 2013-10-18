#lang racket
;;;Piece rating functions
(define (value piece)
  (cond [(is-a? piece rook%) 500]
        [(is-a? piece knight%) 320]
        [(is-a? piece bishop%) 325]
        [(is-a? piece queen%) 980]
        [(is-a? piece king%) 32767]))

;     

(define (evaluate board1)
  (define intBoard1 board) ;Intermediate boards
  (define intBoard2 board)
  (set! board board1)
  (define score 0)
  (define (looping-action! i j)
    (let* ([pc (send board1 board-ref i j)])
      (cond [(not pc) (set! score (+ score 0))]
            [else (let* ([clr (get-field color pc)])
                    ;We assume that Computer is playing Black
                    (if (equal? clr 'Black)
                        (set! score (+ score (value pc)))
                        (set! score (- score (value pc)))))])))
  (define (loop)
    (for-each (λ(i) (for-each (λ(j) (looping-action! i j)) '(1 2 3 4 5 6 7 8))) '(1 2 3 4 5 6 7 8)))
  (begin
    (loop)
    score))
    
                        
           