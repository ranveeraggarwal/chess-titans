Team members: Tarun Kathuria (110110028)
	      Ranveer Aggarwal (120050020)

Brief description of the project: We have implemented the classic game of Chess. By default, the game is supposed to be a 1 player game which uses the minimax algorithm with alpha-beta pruning for finding an optimal move upto a certain depth(default 2). However, both the depth and the mode can be changed to 2 player as well.

How to run: On the terminal, type drracket ChessSubmission.scm & . Hit Ctrl+R , and the GUI will show up. Click on new game image button, then the level 1 image button and start playing!

Files : boardGUI.scm -> This is the file where the GUI of the board(displaying the movement etc. ) is taken care of.
	evaluate.scm -> This is the file which holds the heuristic evaluation function of the board used by the minimax algorithm.(Uses syntax.scm (see below))
	syntax.scm -> Just some macros/syntactic-sugar for list-comprehensions and for loops
	minimax.scm -> The code for implementing the minimax algorithm
	validMoves.scm -> Checks for validity of possible moves at any point of time.
	makeMoves.scm -> Checks for validity(see validMoves) and makes a move.
	ChessSubmission.scm -> The main file(Has all the other forementioned files as dependencies). It defines the basic image buttons and image containers and actually calls the initial GUI. 
