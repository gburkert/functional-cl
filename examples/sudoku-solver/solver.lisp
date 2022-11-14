;;; Dependencies

(require :functional-cl)
(require :str)

(defvar row-length 9)
(setf *random-state* (make-random-state t))



;;; Logic

(defun check-row (board nbr row &optional (from 0) (to row-length))
  (cond
   ((null (member (digit-char nbr) (subseq
                                    (nth row board) from to))) t)
   (t nil)))

(defun check-col (board nbr col)
  (check-row (functional-cl:transpose board) nbr col))

(defun subtract-mod (dividend divisor)
  (- dividend (mod dividend divisor)))

(defun check-box (board nbr row col step)
  (let ((box-row (subtract-mod row 3))
        (box-col (subtract-mod col 3)))
    (cond
     ((>= step 3) t)
     ((check-row board nbr (+ box-row step) box-col (+ box-col 3))
      (check-box board nbr row col (1+ step)))
     (t nil))))

(defun valid-placement (board nbr row col)
  (and (check-row board nbr row)
       (check-col board nbr col)
       (check-box board nbr row col 0)))

(defun first-empty (board &optional (row 0))
  (let ((col (position #\. (nth row board))))
    (if col
        (values row col)
        (if (< row 9)
            (first-empty board (1+ row))
            nil))))

(defun place-nbr (board nbr row col)
  (let ((board-new (functional-cl:copy-list-deep board)))
    (setf (nth col
               (nth row board-new)) (digit-char nbr))
    board-new))

(defun solve (board &optional (nbr 1))
  (multiple-value-bind (row col) (first-empty board)
    (cond
     ((null row) board) ; solved
     ((>= nbr 10) nil)
     (t (cond
         ((valid-placement board nbr row col)
          (let* ((board-new (place-nbr board nbr row col))
                 (solution (solve board-new)))
            (if solution
                solution
                (solve board (1+ nbr)))))
         (t (solve board (1+ nbr))))))))



;;; IO stuff

(defun load-puzzles (filename)
  (functional-cl:filter-list 
    (functional-cl:compose-simple #'not #'str:blankp) (uiop:read-file-lines filename)))

(defun pick-puzzle (puzzles)
  (functional-cl:partition row-length (string-to-chars (nth (random 1000) puzzles))))

(defun join-string-list (string-list)
  (format nil "~{~A~^ ~}" string-list))

(defun string-to-chars (str)
  (coerce str 'list))

(defun format-row (row)
  (format nil "~{~a~a~a~^ | ~}~%" row))

(defun print-board (board)
  (mapcar (lambda (row)
            (format t (format-row row))
            (let ((row-nr (1+ (position row board))))
              (when (and (< row-nr 9) (zerop (mod row-nr 3)))
                    (format t "----+-----+----~%")))) board))

(functional-cl:setfun tap-print-board (functional-cl:tap #'print-board))

(defun tap-format (pattern &rest args)
  (functional-cl:tap (lambda (x)
                       (declare (ignore x))
                       (apply #'format t pattern args))))


;;; Main

(functional-cl:setfun main
                      (functional-cl:pipeline
                      #'load-puzzles
                      #'pick-puzzle
                      (tap-format "~%~a~%" "A randomly selected to be solved:")
                      #'tap-print-board
                      #'solve
                      (tap-format "~%~a~%" "Solution:")
                      #'tap-print-board
                      (constantly t)))


;;; Must be loaded from the root directory of functional-cl. There is no point
;;; in creating an .asd file just for this small example.
; (main "./examples/sudoku-solver/data/kaggle_puzzles.csv")
