(define-syntax for
  (syntax-rules (:)
    [(for init : condition : incr : statement)
     
     (letrec ((loop (lambda ()
                      (cond [condition (begin
                                         statement
                                         incr
                                         (loop))]))))
       (begin init (loop)))]))
       
(define-syntax lc
  (syntax-rules (: <- @)
    [(lc exp : var <- lexp) (map (lambda (var) exp) lexp)]
    [(lc exp : @ guard) (if guard (list exp) `())]
    [(lc exp : @ guard qualifier ...) 
     (concat (lc (lc exp : qualifier ...) : guard))]
    [(lc exp : var <- lexp qualifier ...) 
     (concat (lc (lc exp :  qualifier ... ) : var <- lexp))]))
