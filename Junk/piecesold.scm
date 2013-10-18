;#lang racket

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc exp : var <- lexp) (map (lambda (var) exp) lexp)]
    [(lc exp : @ guard) (if guard (list exp) `())]
    [(lc exp : @ guard qualifier ...) 
     (concat (lc (lc exp : qualifier ...) : guard))]
    [(lc exp : var <- lexp qualifier ...) 
     (concat (lc (lc exp :  qualifier ... ) : var <- lexp))]))

;(define square%
;  (class object%
;    (init pos)
;    (define position pos)
;    (init-field (occupancy #f))
;    (super-new)
;    (define/public (occupied?)
;      occupancy)))

;(define board%
;  (class object%
;    (super-new)
;    (init-field (board
;;                 (for/vector ([i '(1 2 3 4 5 6 7 8)])
;;                   (for/vector ([j '(1 2 3 4 5 6 7 8)])
;;                     (new square% [pos (cons i j)])))))
;                 (list->vector (lc (list->vector(lc (new square% [pos (cons i j)]) : i <- '(1 2 3 4 5 6 7 8))) : j <- '(1 2 3 4 5 6 7 8)))))
;    (define/public (board-ref i j)
;      (vector-ref (vector-ref board (- i 1)) (- j 1)))
;    (define/public (board-set! i j piece)
;      (let* ([pos (board-ref i j)])
;        (set-field! occupancy pos piece)))
;    (define/public (initialise)
;      (define (helper i j)
;        (cond[(= i 1)
;              (cond [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'Black]))]
;                    [(= j 2) (board-set! i j (new knight% [curr-pos (cons i j)] [color 'Black]))]
;                    [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'Black]))]
;                    [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'Black]))]
;                    [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'Black]))])]
;             [(= i 8)
;              (cond [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'White]))]
;                    [(= j 2) (board-set! i j (new knight% [curr-pos (cons i j)] [color 'White]))]
;                    [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'White]))]
;                    [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'White]))]
;                    [(= j 1) (board-set! i j (new rook% [curr-pos (cons i j)] [color 'White]))])]
;             [(= i 2) (board-set! i j (new pawn% [curr-pos (cons i j)] [color 'Black]))]
;             [(= i 7) (board-set! i j (new pawn% [curr-pos (cons i j)] [color 'White]))]
;             [else (board-set! i j #f)]))
;      (for-each (λ(i) (for-each (λ(j) (helper i j)) '(1 2 3 4 5 6 7 8))) '(1 2 3 4 5 6 7 8)))
;    (define/public (print-board)
;      (display board))))
;
;(define board (new board%))
;
;(send board initialise)

(define piece%
  (class object%
    (super-new)
    (init-field color)
    (init-field [curr-pos (void)])
    (define/public (valid-move) (display "override"))))

;;;;;Pieces;;;;;
;King
(define king%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (scan-discreet (mbound (list
                              (cons (+ posx 1) posy)
                              (cons (- posx 1) posy)
                              (cons posx (+ posy 1))
                              (cons posx (- posy 1))
                              (cons (+ posx 1) (+ posy 1))
                              (cons (+ posx 1) (- posy 1))
                              (cons (- posx 1) (+ posy 1))
                              (cons (- posx 1) (- posy 1)))) (get-field color this)))))


;Queen
(define queen%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (mbound (append
               (scan-continuous-b (lc (cons posx y) : y <- (p-to-n 1 posy) @ (not (= y posy))) (get-field color this))
               (scan-continuous-f (lc (cons posx y) : y <- (p-to-n posy 8) @ (not (= y posy))) (get-field color this))
               (scan-continuous-b (lc (cons x posy) : x <- (p-to-n 1 posx) @ (not (= x posx))) (get-field color this))
               (scan-continuous-f (lc (cons x posy) : x <- (p-to-n posx 8) @ (not (= x posx))) (get-field color this))
               (scan-continuous-b (lc (cons x y) : x <- (p-to-n 1 posx) y <- (p-to-n 1 posy) @ 
                                      (and (= (- x y) (- posx posy)) 
                                           (not (and (equal? x posx) (equal? y posy))))) (get-field color this))
               (scan-continuous-f (lc (cons x y) : x <- (p-to-n posx 8) y <- (p-to-n posy 8) @ 
                                      (and (= (- x y) (- posx posy)) 
                                           (not (and (equal? x posx) (equal? y posy))))) (get-field color this))
               (scan-continuous-b (lc (cons x y) : x <- (p-to-n 1 posx) y <- (p-to-n 1 posy) @ 
                                      (and (= (+ x y) (+ posx posy))
                                           (not (and (equal? x posx) (equal? y posy))))) (get-field color this))
               (scan-continuous-f (lc (cons x y) : x <- (p-to-n posx 8) y <- (p-to-n posy 8) @ 
                                      (and (= (+ x y) (+ posx posy)) 
                                           (not (and (equal? x posx) (equal? y posy))))) (get-field color this)))))))

;Bishop
(define bishop%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (mbound (append 
               (scan-continuous-b (lc (cons x y) : x <- (p-to-n 1 posx) y <- (p-to-n 1 posy) @ 
                                      (and (= (- x y) (- posx posy)) 
                                           (not (and (equal? x posx) (equal? y posy))))) (get-field color this))
               (scan-continuous-f (lc (cons x y) : x <- (p-to-n posx 8) y <- (p-to-n posy 8) @ 
                                      (and (= (- x y) (- posx posy)) 
                                           (not (and (equal? x posx) (equal? y posy))))) (get-field color this))
               (scan-continuous-b (lc (cons x y) : x <- (p-to-n 1 posx) y <- (p-to-n 1 posy) @ 
                                      (and (= (+ x y) (+ posx posy))
                                           (not (and (equal? x posx) (equal? y posy))))) (get-field color this))
               (scan-continuous-f (lc (cons x y) : x <- (p-to-n posx 8) y <- (p-to-n posy 8) @ 
                                      (and (= (+ x y) (+ posx posy)) 
                                           (not (and (equal? x posx) (equal? y posy))))) (get-field color this)))))))

;Rook
(define rook%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (mbound (append
               (scan-continuous-b (lc (cons posx y) : y <- (p-to-n 1 posy) @ (not (= y posy))) (get-field color this))
               (scan-continuous-f (lc (cons posx y) : y <- (p-to-n posy 8) @ (not (= y posy))) (get-field color this))
               (scan-continuous-b (lc (cons x posy) : x <- (p-to-n 1 posx) @ (not (= x posx))) (get-field color this))
               (scan-continuous-f (lc (cons x posy) : x <- (p-to-n posx 8) @ (not (= x posx))) (get-field color this)))))))

;Pawn
;Pawn attack filter
(define (pawn-attack-filter posx posy)
  (cond [(and (is-occupied? (cons (+ 1 posx) (+ 1 posy)))
                             (is-occupied? (cons (- posx 1) (+ 1 posy))))
                        (scan-discreet (list
                                 (cons (+ posx 1) (+ posy 1))
                                 (cons (- posx 1) (+ posy 1))))]
                       [(is-occupied? (cons (+ 1 posx) (+ 1 posy)))
                        (scan-discreet (list
                                 (cons (+ posx 1) (+ posy 1))))]
                       [(is-occupied? (cons (- posx 1) (+ 1 posy)))
                        (scan-discreet (list
                                        (cons (- posx 1) (+ posy 1))))]))
;The pawn move
(define pawn%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (if (equal? (get-field color this) 'black)
          (cond [(is-occupied? (cons posx (+ 1 posy))) 
;                 (if (and (is-occupied? (cons (+ 1 posx) (+ 1 posy)))
;                          (not (equal? (get-field color (is-occupied? (+ 1 posx) (+ 1 posy)))
;                                       (get-field color this))))
;                     (cons (+ posx 1) (+ posy 1)) '())
                 (pawn-attack-filter posx posy)]
                [(and (is-occupied? (cons (+ 1 posx) (+ 1 posy)))
                      (not (equal? (get-field color (is-occupied? (cons (+ 1 posx) (+ 1 posy))))
                                   (get-field color this))))
                 (list (cons posx (+ 1 posy))
                       (cons (+ 1 posx) (+ 1 posy)))]
                [(= posy 2) (if (is occupied? (cons posx (+ 2 posy)))
                                (cons posx (+ 1 posy))
                                (mbound (list 
                                         (cons posx (+ 1 posy))
                                         (cons posx (+ 2 posy)))))]
                [else (mbound (list (cons posx (+ 1 posy))))])
          (cond [(is-occupied? (cons posx (- posy 1))) 
                 (if (and (is-occupied? (cons (- posx 1) (- posy 1)))
                          (not (equal? (get-field color (is-occupied? (- posx 1) (- posy 1)))
                                       (get-field color this))))
                     (cons (- posx 1) (- posy 1)) '())]
                [(and (is-occupied? (cons (- posx 1) (- posy 1)))
                      (not (equal? (get-field color (is-occupied? (cons (- posx 1) (- posy 1))))
                                   (get-field color this))))
                 (list (cons posx (- posy 1))
                       (cons (- posx 1) (- posy 1)))]
                [(= posy 7) (if (is occupied? (cons posx (- posy 2)))
                                (cons posx (- posy 1))
                                (mbound (list
                                         (cons posx (- posy 1))
                                         (cons posx (- posy 2)))))]
                [else (mbound (list (cons posx (- 1 posy))))])))))

;Knight
(define knight%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (scan-discreet (mbound (list
                              (cons (+ 1 posx) (+ 2 posy))
                              (cons (+ 1 posx) (- 2 posy))
                              (cons (- 1 posx) (+ 2 posy))
                              (cons (- 1 posx) (- 2 posy))
                              (cons (+ 2 posx) (+ 1 posy))
                              (cons (+ 2 posx) (- 1 posy))
                              (cons (- 2 posx) (+ 1 posy))
                              (cons (- 2 posx) (- 1 posy)))) (get-field col this)))))

;;;;;The functions that bounds a piece inside the board;;;;;
(define (mbound l)
  (define (helper l fl)
    (if (null? l) fl
        (cond [(or (< (caar l) 1)
                   (< (cdar l) 1)
                   (> (caar l) 8)
                   (> (cdar l) 8))
               (helper (cdr l) fl)]
              [else (helper (cdr l) (append fl (list (car l))))])))
  (helper l '()))

;;;;;Definitions;;;;;
(define (is-occupied? squarecoordinates)
  (define x (car squarecoordinates))
  (define y (cdr squarecoordinates))
  (send (send board board-ref x y) occupied?))

(define (scan-discreet movelist colour)
  (define (helper l fl)
    (if (null? l) fl
        (cond [(is-occupied? (car l)) (if (eq? (get-field color (is-occupied? (car l))) colour) 
                                          (helper (cdr l) fl)
                                          (helper (cdr l) (append (list (car l)) fl)))]
              [else (helper (cdr l) (append (list (car l)) fl))])))
  (helper movelist '()))

(define (scan-continuous-f movelist colour)
  (define (occ-square1st movelist)
    (if (null? movelist) 'khaali
        (if (is-occupied? (car movelist)) (car movelist)
            (occ-square1st (cdr movelist)))))
  (if (occ-square1st movelist) 
      (cond [(eq? 'khaali (occ-square1st movelist)) movelist]
            [(eq? colour (get-field color (is-occupied? (occ-square1st movelist)))) 
             (takewhile (lambda (x) (not (is-occupied? x))) movelist)]
            [else (cons (takewhile (lambda (x) (not (is-occupied? x))) movelist) occ-square1st)])
      movelist))

(define (scan-continuous-b movelistp colour)
  (define movelist (reverse movelistp))
  (define (occ-square1st movelist)
    (if (null? movelist) 'khaali
        (if (is-occupied? (car movelist)) (car movelist)
            (occ-square1st (cdr movelist)))))
  (if (occ-square1st movelist) 
      (cond [(eq? 'khaali (occ-square1st movelist)) movelist]
            [(eq? colour (get-field color (is-occupied? (occ-square1st movelist)))) 
             (takewhile (lambda (x) (not (is-occupied? x))) movelist)]
            [else (cons (takewhile (lambda (x) (not (is-occupied? x))) movelist) occ-square1st)])
      movelist))

(define (rev-cdr l)
  (reverse (cdr (reverse l))))

(define (takewhile p l)
  (foldr (lambda (x t) (if (p x) (cons x t) '())) '() l))

(define (concat l) (foldr append `() l))

(define (one-to-n n)
  (if (= n 0) `()
      (append (one-to-n (- n 1)) (list n))))

(define (p-to-n p n)
  (if (= n p) (list p)
      (append (p-to-n p (- n 1)) (list n))))
