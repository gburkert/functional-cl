(defstruct (state
            (:constructor create-state (height width mines
                                       &optional 
                                        (board (init-board height width mines))
                                        (to-reveal (- (* height width) mines))
                                        (exploded nil))))
  board height width to-reveal exploded)

(defstruct (cell
            (:constructor create-cell
                          (&optional (mine nil) (revealed nil) (flag nil) (adjacent nil))))
  mine revealed flag adjacent)

(defun init-board (height width mines)
  (let* ((num-cells (* height width))
         (positions (functional-cl:n-times-in-list (create-cell) num-cells)))
    (add-adjs-to-board
     (functional-cl:partition width (place-mines positions num-cells mines)) height width)))

(defun place-mines (positions num-cells mines &optional (step 0))
  (cond ((>= step mines) positions)
        (t (let ((copy (copy-seq positions))
                 (rand (random num-cells)))
             (cond ((null (cell-mine (nth rand positions)))
                    (place-mines (functional-cl:replace-at (create-cell t nil nil nil) rand copy)
                                 num-cells mines (1+ step)))
                   (t (place-mines positions num-cells mines step)))))))

(defun add-adjs-to-board (board height width)
  (labels ((walk-board (board x y)
              (cond ((>= x width)
                     (if (>= y (1- height))
                         board
                         (walk-board board 0 (1+ y))))
                    (t (walk-board
                        (add-adj-to-cell board width x y)
                        (1+ x) y)))))
    (walk-board board 0 0)))

(defun add-adj-to-cell (board width x y)
  (change-cell-prop-in-board board width x y 'adjacent (get-adj-mines board x y)))

(defun change-cell-prop-in-board (board width x y prop arg)
  (let ((board-copy (apply-to-cells (functional-cl:copy-list-deep board) width #'copy-structure))
        (cell (get-cell board x y)))
    (setf (nth x (nth y board-copy)) (functional-cl:change-slot prop arg cell))
    board-copy))

(defun get-adj-mines (board x y)
  (labels ((walk-adj (x y &optional (step-x 0) (step-y 0) (mines 0))
              (cond ((>= step-x 3)
                     (if (>= step-y 2)
                         mines
                         (walk-adj x y 0 (1+ step-y) mines)))
                    (t (walk-adj x y (1+ step-x) step-y
                                 (+ mines (bool-to-bin
                                           (minep (get-cell board (+ x step-x) (+ y step-y))))))))))
    (walk-adj (1- x) (1- y))))

(defun minep (cell)
  (when cell (cell-mine cell)))

(defun bool-to-bin (bool)
  (if bool 1 0))

(defun get-cell (board x y)
  (when (and (>= x 0) (>= y 0))
        (let ((row (nth y board)))
          (when row (nth x row)))))

(functional-cl:setfun flatten1 (functional-cl:flatten-c 1))

(defun apply-to-cells (board width fun)
  (funcall (functional-cl:pipeline #'flatten1
                                   (functional-cl:map-list-c fun)
                                   (functional-cl:partition-c width)) board))

(defun hit-mine (state)
  (change-board-in-state (functional-cl:change-slot 'exploded t state)
                         (apply-to-cells (state-board state) (state-width state) #'reveal-if-mine)))

(defun reveal-if-mine (cell)
  (if (minep cell)
      (functional-cl:change-slot 'revealed t cell)
      cell))

(defun change-board-in-state (state board-new)
  (functional-cl:change-slot 'board board-new state))

(defun one-less-to-reveal (state)
  (functional-cl:change-slot 'to-reveal (1- (state-to-reveal state)) state))

(defun reveal-cell (board width x y)
  (change-cell-prop-in-board board width x y 'revealed t))

(defun hit-adj (state width x y)
  (change-board-in-state state (reveal-cell (state-board state) width x y)))

(defun place-flag (state x y flag)
  (change-board-in-state state
                         (change-cell-prop-in-board (state-board state) (state-width state) x y 
                                                    'flag (if (eql flag 'place) t nil))))

(defun reveal-area (state x y)
  (let ((state-copy (change-board-in-state state (reveal-cell (state-board state) 
                                                              (state-width state) x y))))
    (labels ((walk-neigh (state-c x y &optional (step-x 0) (step-y 0))
                (cond ((>= step-x 3)
                       (if (>= step-y 2)
                           state-c
                           (walk-neigh state-c x y 0 (1+ step-y))))
                      ((and (eql step-x 1) (eql step-y 1)) (walk-neigh state-c x y 2 1))
                      (t (walk-neigh
                          (if (move-valid state-c (+ x step-x) (+ y step-y))
                              (click-cell state-c (+ x step-x) (+ y step-y))
                              state-c)
                          x y (1+ step-x) step-y)))))
      (walk-neigh state-copy (1- x) (1- y)))))

(defun click-cell (state x y &optional (flag nil))
  (if flag
      (place-flag state x y flag)
      (let ((cell (get-cell (state-board state) x y)))
        (cond ((minep cell) (hit-mine state))
              ((> (cell-adjacent cell) 0) (hit-adj (one-less-to-reveal state) 
                                                   (state-width state) x y))
              (t (reveal-area (one-less-to-reveal state) x y))))))

(defun move-valid (state x y &optional (flag nil))
  (when (and (>= x 0) (>= y 0) (> (state-width state) x) (> (state-height state) y))
        (let ((cell (get-cell (state-board state) x y)))
          (not (or (cell-revealed cell) (if flag
                                            (cond ((equal flag 'place) (cell-flag cell))
                                                  (t (not (cell-flag cell))))
                                            (cell-flag cell)))))))

(defun get-player-move (state)
  (multiple-value-bind (x y flag) (get-move)
    (if (move-valid state x y flag)
        (list state x y flag)
        (get-player-move state))))
    
(functional-cl:setfun lte0 (functional-cl:lte-c 0))
   
(functional-cl:setfun game-over
  (functional-cl:or-checker #'state-exploded
                            (functional-cl:compose-simple #'lte0 #'state-to-reveal)))

(defun move (state)
  (when (not (game-over state))
        (apply #'click-cell (get-player-move state))))

(functional-cl:setfun tap-display-state (functional-cl:tap #'display-state))

(defun main (state)
  (if state
      (funcall (functional-cl:pipeline #'tap-display-state
                                       #'move
                                       #'main) state)
      t))

(defun init-state-valid (state x y flag)
  (or flag (and (not (minep (get-cell (state-board state) x y)))
                (not (> (cell-adjacent (get-cell (state-board state) x y)) 0)))))

(defun init-until-valid (state x y flag)
  (if (init-state-valid state x y flag)
      state
      (init-until-valid (create-state (state-height state) (state-width state)
                                    (state-to-reveal state)) x y flag)))

(functional-cl:setfun first-move 
                      (functional-cl:pipeline #'create-state
                                              #'tap-display-state
                                              #'get-player-move
                                              (functional-cl:keep-args-c #'init-until-valid 0)
                                              (functional-cl:splat #'click-cell)
                                              #'main))

;;; Start a game:
; (first-move 10 15 15)
