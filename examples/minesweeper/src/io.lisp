(defvar *game-over-str* "!!!     GAME OVER     !!!")
(defvar *game-won-str* "!!!     YOU HAVE WON THE GAME     !!!")
(defvar *usage*
  "Options:
  - click a cell:  <row><col>   => e.g. b3
  - place a flag:  f <row><col> => e.g. f b3
  - remove a flag: r <row><col> => e.g. r b3")
(defvar *get-move-str* "Please choose a valid option.")
(defvar *invalid-format-str* "Invalid format. Please try again.")

(defvar *alphabet* (coerce "abcdefghijklmnopqrstuvwxyz" 'list))
(defvar *flag-op* (make-hash-table :test 'equal))
(setf (gethash "f" *flag-op*) 'place)
(setf (gethash "r" *flag-op*) 'remove)

(defun cell-to-str (cell)
  (cond ((cell-revealed cell)
         (cond ((cell-mine cell) "M")
               (t (if (> (cell-adjacent cell) 0)
                      (write-to-string (cell-adjacent cell))
                      "-"))))
        ((cell-flag cell) "F")
        (t "?")))

(defun format-board (board width)
  (mapcar (lambda (row) (format nil "~{~a~^  ~}" row))
          (functional-cl:apply-to-partlist #'cell-to-str width board)))

(defun add-coordinates (board-str height width)
  (format nil "~a~%~a~%~a"
          (format-colnos width)
          (format nil "      ~v{~a~:*~}" (1- (* 3 width)) '("-"))
          (format nil "~{~a~%~}"
                  (add-rowletters board-str height 0))))

(defun format-colnos (width)
  (format nil "      ~{~2a~^ ~}" (functional-cl:range 1 (1+ width))))

(defun add-rowletters (board-str height step &optional (accu '()))
  (cond ((>= step height) accu)
        (t (add-rowletters board-str height (1+ step)
                           (append accu (list (format nil " ~a |  ~a"
                                                      (nth step *alphabet*)
                                                      (nth step board-str))))))))

(defun format-usage (game-over to-reveal)
  (cond ((<= to-reveal 0) *game-won-str*)
        (game-over *game-over-str*)
        (t *usage*)))

(defun format-state (state)
  (format nil "~%~a~%~%~a~%"
          (add-coordinates
           (format-board (state-board state) (state-width state))
           (state-height state) (state-width state))
          (format-usage (state-exploded state) (state-to-reveal state))))

(defun display-state (state)
  (format t "~a" (format-state state)))

(defun get-move (&optional (prompt-str *get-move-str*))
  (progn (format t "~a~%" prompt-str)
         (multiple-value-bind (x y flag) (prompt-move)
           (values x y flag))))

(defun prompt-move ()
  (multiple-value-bind (x y flag) (parse-input (read-line))
    (if x
        (values x y flag)
        (get-move *invalid-format-str*))))

(defun string-to-low-char (str)
  (coerce (string-downcase str) 'character))

(defun parse-input (move-str)
  (multiple-value-bind (str captures)
      (ppcre:scan-to-strings "^(([FfRr]) )?([A-Za-z])(\\d+)$" move-str)
    (when captures
          (values (1- (parse-integer (aref captures 3)))
                  (position (string-to-low-char (aref captures 2)) *alphabet*)
                  (gethash (string-downcase (aref captures 1)) *flag-op*)))))
