(include "pieces.scm")
(include "board.scm")

;(include "syntactic-sugar.scm") Can remove

(send board print-board)
(allpossible 'Black)
(get-field color (get-field occupancy (send board board-ref 3 1)))
(send (get-field occupancy (send board board-ref 3 7)) valid-move)
(send board make-move! (cons 3 7) (cons 3 5))
;(let* ([piece (get-field occupancy (send board board-ref 3 2))]
;       [i (begin(display (is-a? piece knight%)) (display piece))]
;       [validMovesList (begin (display(send piece valid-move)) (send piece valid-move))]
;       [isMoveValid? (member (cons posNx posNy) validMovesList)])
;  (begin (display validMovesList) (newline) (display isMoveValid?) (newline)))
;(send board print-board)
(send (get-field occupancy (send board board-ref 1 2)) get-valid-moves)
;(send (get-field occupancy (send board board-ref 1 7)) valid-move)
(get-field occupancy (send board board-ref 4 1))