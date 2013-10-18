;;;;;Definitions;;;;;
(define (search-n-remove n l)
  (define (helper l fl)
    (cond [(null? l) fl]
          [(equal? (car l) n) (helper (cdr l) fl)]
          [else (helper (cdr l) (append fl (list (car l))))]))
  (helper l '()))

(define (uniquer l)
  (if (null? l) '()
      (let* ([n (car l)]
             [l1 (search-n-remove n (cdr l))])
        (cons n (uniquer l1)))))

(define (movefinder x y color)
  (let* ([square (send board board-ref x y)]
         [pc (send square occupied?)])
    (if (not pc) '()
        (if (equal? (get-field color pc) color)
            (send pc valid-move)
            '()))))

(define (allpossible color)
  (uniquer (flatten (lc (movefinder x y color) : 
                        x <-(list 1 2 3 4 5 6 7 8) 
                        y <- (list 1 2 3 4 5 6 7 8)))))

(define (kill-common l1 l2)
  (if (or (null? l2) (null? l1))
      l1
      (let* ([n (car l2)]
             [l (search-n-remove n l1)])
        (kill-common l (cdr l2)))))

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc exp : var <- lexp) (map (lambda (var) exp) lexp)]
    [(lc exp : @ guard) (if guard (list exp) `())]
    [(lc exp : @ guard qualifier ...) 
     (concat (lc (lc exp : qualifier ...) : guard))]
    [(lc exp : var <- lexp qualifier ...) 
     (concat (lc (lc exp :  qualifier ... ) : var <- lexp))]))

;;;;;The Piece Class;;;;;
(define piece%
  (class object%
    (super-new)
    (init-field color)
    (init-field [curr-pos (void)])
    (define/public (valid-move) (display "override"))))

;;;;;Pieces;;;;;
;King
;;The King Class
(define king%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (scan-discreet 
       (mbound 
        (list
         (cons (+ posx 1) posy)
         (cons (- posx 1) posy)
         (cons posx (+ posy 1))
         (cons posx (- posy 1))
         (cons (+ posx 1) (+ posy 1))
         (cons (+ posx 1) (- posy 1))
         (cons (- posx 1) (+ posy 1))
         (cons (- posx 1) (- posy 1)))) 
       (get-field color this)))
    (define/public (get-valid-moves)
      (let* ([color (get-field color this)]
             [allothers (if (eq? color 'Black)
                            (allpossible 'White)
                            (allpossible 'Black))]
             [selfmoves (send this valid-move)])
          (kill-common selfmoves allothers)))))

;Queen
(define queen%
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
                                           (not (and (equal? x posx) (equal? y posy))))) (get-field color this))
               (scan-continuous-b (lc (cons posx y) : y <- (p-to-n 1 posy) @ (not (= y posy))) (get-field color this))
               (scan-continuous-f (lc (cons posx y) : y <- (p-to-n posy 8) @ (not (= y posy))) (get-field color this))
               (scan-continuous-b (lc (cons x posy) : x <- (p-to-n 1 posx) @ (not (= x posx))) (get-field color this))
               (scan-continuous-f (lc (cons x posy) : x <- (p-to-n posx 8) @ (not (= x posx))) (get-field color this)))))
    (define/public (get-valid-moves)
      (send this valid-move))))

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
                                           (not (and (equal? x posx) (equal? y posy))))) (get-field color this)))))
    (define/public (get-valid-moves)
      (send this valid-move))))

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
               (scan-continuous-f (lc (cons x posy) : x <- (p-to-n posx 8) @ (not (= x posx))) (get-field color this)))))
    (define/public (get-valid-moves)
      (send this valid-move))))


;Pawn
;;Pawn Abstractions
(define (pawn-moves-black posx posy)
  (define (frontocc)
    (cond [(and (is-occupied? (cons (+ 1 posx) (+ 1 posy)))
                (is-occupied? (cons (- posx 1) (+ 1 posy))))
           (scan-discreet (list
                           (cons (+ posx 1) (+ posy 1))
                           (cons (- posx 1) (+ posy 1))) 'black)]
          [(is-occupied? (cons (+ 1 posx) (+ 1 posy)))
           (scan-discreet (list
                           (cons (+ posx 1) (+ posy 1))) 'black)]
          [(is-occupied? (cons (- posx 1) (+ 1 posy)));
           (scan-discreet (list
                           (cons (- posx 1) (+ posy 1))) 'black)]
          [else '()]))
  (define (frocc2)
    (append (list (cons posx (+ 1 posy))) (frontocc)))
  (define (fronoc2)
    (append (list (cons posx (+ posy 2))) (frocc2)))
  (cond [(is-occupied? (cons posx (+ 1 posy))) (frontocc)]
        [(= posy 2)
         (if (is-occupied? (cons posx (+ 2 posy))) (frocc2) (fronoc2))]
        [else (frocc2)]))

(define (pawn-moves-white posx posy)
  (define (frontocc)
    (cond [(and (is-occupied? (cons (+ 1 posx) (- posy 1)))
                (is-occupied? (cons (- posx 1) (- posy 1))))
           (scan-discreet (list
                           (cons (+ posx 1) (- posy 1))
                           (cons (- posx 1) (- posy 1))) 'black)]
          [(is-occupied? (cons (+ 1 posx) (- posy 1)))
           (scan-discreet (list
                           (cons (+ posx 1) (- posy 1))) 'black)]
          [(is-occupied? (cons (- posx 1) (- posy 1)));
           (scan-discreet (list
                           (cons (- posx 1) (- posy 1))) 'black)]
          [else '()]))
  (define (frocc2)
    (append (list (cons posx (- posy 1))) (frontocc)))
  (define (fronoc2)
    (append (list (cons posx (- posy 2))) (frocc2)))
  (cond [(is-occupied? (cons posx (- posy 1))) (frontocc)]
        [(= posy 7)
         (if (is-occupied? (cons posx (- posy 2)))
             (frocc2) (fronoc2))]
        [else (frocc2)]))

;The pawn subclass
(define pawn%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (if (equal? (get-field color this) 'black)
          (pawn-moves-black posx posy)
          (pawn-moves-white posx posy)))
    (define/public (get-valid-moves)
      (send this valid-move))))


;Knight
(define knight%
  (class piece%
    (init col)
    (super-new [color col])
    (define/override (valid-move)
      (define posx (car (get-field curr-pos this)))
      (define posy (cdr (get-field curr-pos this)))
      (scan-discreet (mbound (list
                              (cons (+ posx 1) (+ posy 2))
                              (cons (+ posx 1) (- posy 2))
                              (cons (- posx 1) (+ posy 2))
                              (cons (- posx 1) (- posy 2))
                              (cons (+ posx 2) (+ posy 1))
                              (cons (+ posx 2) (- posy 1))
                              (cons (- posx 2) (+ posy 1))
                              (cons (- posx 2) (- posy 1)))) (get-field color this)))
    (define/public (get-valid-moves)
      (send this valid-move))))

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
  (define finalres (if (occ-square1st movelist) 
                       (cond [(eq? 'khaali (occ-square1st movelist)) movelist]
                             [(eq? colour (get-field color (is-occupied? (occ-square1st movelist)))) 
                              (takewhile (lambda (x) (not (is-occupied? x))) movelist)]
                             [else (append (takewhile (lambda (x) (not (is-occupied? x))) movelist) 
                                           (list (occ-square1st movelist)))])
                       movelist))
  (if (or (pair? finalres) (null? finalres)) finalres
      (list finalres)))

(define (scan-continuous-b movelistp colour)
  (define movelist (reverse movelistp))
  (define (occ-square1st movelist)
    (if (null? movelist) 'khaali
        (if (is-occupied? (car movelist)) (car movelist)
            (occ-square1st (cdr movelist)))))
  (define finalres (if (occ-square1st movelist) 
                       (cond [(eq? 'khaali (occ-square1st movelist)) movelist]
                             [(eq? colour (get-field color (is-occupied? (occ-square1st movelist)))) 
                              (takewhile (lambda (x) (not (is-occupied? x))) movelist)]
                             [else (append (takewhile (lambda (x) (not (is-occupied? x))) movelist) 
                                           (list (occ-square1st movelist)))])
                       movelist))
  (if (or (pair? finalres) (null? finalres)) finalres
      (list finalres)))

(define (rev-cdr l)
  (reverse (cdr (reverse l))))

(define (takewhile p l)
  (foldr (lambda (x t) (if (p x) (append (list x) t) '())) '() l))

(define (flatten l)
  (cond [(null? l) '()]
        [else (if (list? (car l)) (append (flatten (car l)) (flatten (cdr l)))
                  (append (list (car l)) (flatten (cdr l))))]))

(define (concat l) (foldr append `() l))

(define (one-to-n n)
  (if (= n 0) `()
      (append (one-to-n (- n 1)) (list n))))

(define (p-to-n p n)
  (if (= n p) (list p)
      (append (p-to-n p (- n 1)) (list n))))
