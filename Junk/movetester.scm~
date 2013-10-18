(include "pieces.scm")
(include "board.scm")

;(include "syntactic-sugar.scm") Can remove

(send board print-board)
(get-field color (get-field occupancy (send board board-ref 3 1)))
(send board make-move!)
;(let* ([piece (get-field occupancy (send board board-ref 3 2))]
;       [i (begin(display (is-a? piece knight%)) (display piece))]
;       [validMovesList (begin (display(send piece valid-move)) (send piece valid-move))]
;       [isMoveValid? (member (cons posNx posNy) validMovesList)])
;  (begin (display validMovesList) (newline) (display isMoveValid?) (newline)))
(send board print-board)
(get-field occupancy (send board board-ref 4 1))