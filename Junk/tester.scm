#lang racket
(define (concat l) (foldr append `() l))

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc exp : var <- lexp) (map (lambda (var) exp) lexp)]
    [(lc exp : @ guard) (if guard (list exp) `())]
    [(lc exp : @ guard qualifier ...) 
     (concat (lc (lc exp : qualifier ...) : guard))]
    [(lc exp : var <- lexp qualifier ...) 
     (concat (lc (lc exp :  qualifier ... ) : var <- lexp))]))

(define (one-to-n n)
  (if (= n 0) `()
      (append (one-to-n (- n 1)) (list n))))


(define (valid-moves posx posy)
  (append
    (lc (cons posx y) : y <- (one-to-n 8) @ (not(= y posy)))
    (lc (cons x posy) : x <- (one-to-n 8) @ (not (= x posx)))
    (lc (cons x y) : x <- (one-to-n 8) y <- (one-to-n 8) @ (and (= (abs (- x y)) (abs (- posx posy)))  (not (and (equal? x posx) (equal? y posy)))))))