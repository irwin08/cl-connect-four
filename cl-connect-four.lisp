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

;; checks if board state is victory - takes board, and row and col of latest move
;; returns winning token if true, nil otherwise
(defun check-victory (board row col)
  )