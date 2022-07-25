;;;; cl-connect-four.lisp

(in-package #:cl-connect-four)


;; returns a new board that is the old board with the col-num'd column replaced
(defun replace-board-column (board col-num column)
  (let ((new-board nil))
     (loop for i from 0 to 7
	   do (if (= i col-num)
		  (setq new-board (cons column new-board))
		  (setq new-board (cons (nth i board) new-board))))
     (reverse new-board)))

;; Adds token at col-num column - returns new board if successful, nil otherwise
(defun add-token (token col-num board)
  (if (< (list-length (nth col-num board)) 7)
      (replace-board-column board col-num (cons token (nth col-num board)))
      nil))

(defstruct move
  row
  col
  color)

;; Takes in a move and board state, returns new board unless an illegal move
;; was made, in which case nil is returned
(defun apply-move (move board)
  (if (= (list-length (nth (move-col move) board)) (move-row move))
      (add-token (move-color move) (move-col move) board)
      nil))

(defun print-board (board)
  (loop for depth in (reverse '(0 1 2 3 4 5))
	do (format t "|")
	   (loop for column in '(0 1 2 3 4 5 6)
		 do (if (< depth (length (nth column board)))
			(prin1 (nth depth (nth column board)))
			(prin1 'O))
		    (format t "|"))
	   (terpri)))


(defun greater (x y) (if (> x y) x y))
(defun lesser (x y) (if (< x y) x y))

(defun check-victory-horizontal (board row col)
  (let ((count 0)
	(current-token nil))
    (loop for cur-col from (greater 0 (- col 4)) to (lesser (- (length board) 1) (- (+ col 4) 1))
	  do (if (equal current-token (nth row (nth cur-col board)))
		 (setq count (+ 1 count))
		 (progn (setq count 1)
			(setq current-token (nth row (nth cur-col board)))))
	     (if (>= count 4)
		 (return current-token)
		 nil))))

(defun check-victory-vertical (board row col)
  (let ((count 0)
	(current-token nil))
    (loop for cur-row from (greater 0 (- row 4)) to (lesser (- (length (nth col board)) 1) (- (+ row 4) 1))
	  do (if (equal current-token (nth cur-row (nth col board)))
		 (setq count (+ 1 count))
		 (progn (setq count 1)
			(setq current-token (nth cur-row (nth col board)))))
	     (if (>= count 4)
		 (return current-token)
		 nil))))


;; need to get both upward sloping and downward sloping offset
(defun get-diagonal-range (board row col)
  (let ((upward '())
	(downward '())
	(offset 0))
    ;; upward sloping
    (loop while (nth (+ row offset) (nth (+ col offset) board))
	  do (setq upward (append upward (list offset)))
	     (incf offset))
    ;; set to 1 so as to not double count starting position
    (setf offset 1)
    (loop while (and (>= (- row offset) 0) (>= (- col offset) 0) (nth (- row offset) board) (nth (- col offset) board))
	  do (setq upward (append upward (list (- 0 offset))))
	     (incf offset))
    (setf offset 0)
    ;; downward sloping
    (loop while (nth (+ row offset) (nth (+ col offset) board))
	  do (setq downward (append downward (list offset)))
	     (incf offset))
    ;; again, set to 1 so as to not double count starting position
    (setf offset 1)
    (loop while (and (>= (- col offset) 0)(nth (+ row offset) board) (nth (- col offset) board))
	  do (setq downward (append downward (list (- 0 offset))))
	     (incf offset))
    (list upward downward)))

(defun check-victory-diagonal (board row col)
    (let ((count 0)
	(current-token nil))
    (loop for offset in (nth 0 (get-diagonal-range board row col))
	  do (if (equal current-token (nth (+ row offset) (nth (+ col offset) board)))
		 (progn (setq count (+ 1 count))
			(if (>= count 4)
			    (return-from check-victory-diagonal current-token)
			    nil))
		 (progn (setq count 1)
			(setq current-token (nth (+ row offset) (nth (+ col offset) board))))))
    (setq count 0)
    (loop for offset in (nth 1 (get-diagonal-range board row col))
	  do (if (equal current-token (nth (+ row offset) (nth (+ col offset) board)))
		 (setq count (+ 1 count))
		 (progn (setq count 1)
			(setq current-token (nth (+ row offset) (nth (+ col offset) board)))))
	     (if (>= count 4)
		 (return-from check-victory-diagonal current-token)
		 nil))))


;; TODO!!! Make macro for if expression that stores value of expression for use if true

;; checks if board state is victory - takes board, and row and col of latest move
;; returns winning token if true, nil otherwise
(defun check-victory (board row col)
  (if (or (check-victory-vertical board row col) (check-victory-horizontal board row col) (check-victory-diagonal board row col))
      (or (check-victory-vertical board row col) (check-victory-horizontal board row col) (check-victory-diagonal board row col))
      nil))

(defun get-opposite-token (token)
  (if (equal token 'R1)
      'R2
      'R1))

;; checks if making a move at given row/column is legal
(defun check-legal (board row col token)
  (if (and (not (nth row (nth col board)))(or (= 0 row) (nth (- row 1) (nth col board))))
      (not (equal (check-victory board row col) (get-opposite-token token)))
      nil))

(defun print-board (board)
  (loop for i from 0 to 6
	do (progn (format t "\|")
	    (loop for j from 0 to 6
		 do (cond ((nth (- 6 i) (nth j board))
			   (progn (prin1 (nth (- 6 i) (nth j board)))
							(format t "\|")))
			  (t (progn (prin1 'OO)
				    (format t "\|")))))
	    (terpri))))

(defun turn-lookup (turn)
  (cond ((= turn 0) 'R1)
	((= turn 1) 'R2)))

(defun play ()
  (let ((turn 0)
	(move-input '())
	(board '(() () () () () () ())))
  (loop while t
	do (progn ()
		  (print-board board)
		  (format t "Player ~d's turn:" (+ 1 turn))
		  (setq move-input (read))
		  (make-move (nth 0 move-input) (nth 1 move-input) (turn-lookup turn))
		  (setq turn (mod (1+ turn) 2))))))
