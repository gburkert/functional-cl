(in-package :functional-cl)

(defun zip-to-alist (lst1 lst2)
  "Merges two lists into an a-list (association list).
For example:

    (zip-to-alist '(1 2 3) '(4 5 6)) ; ==> ((1 . 4) (2 . 5) (3 . 6))
"
  (cond ((or (null lst1) (null lst2)) nil)
        (t (cons (cons (car lst1) (car lst2))
                 (zip-to-alist (cdr lst1) (cdr lst2))))))

(_curry-libfun zip-to-alist 2)

(defun flatten (lst &optional depth)
  "Flattens a list structure. The optional parameter DEPTH can be used to
specify a maximum depth to which the list should be flattened. This function is 
non-destructive.
For example:

    (flatten '((1 ((2)) (3 4)) (5 6)))   ; ==> (1 2 3 4 5 6)
    (flatten '((1 ((2)) (3 4)) (5 6)) 2) ; ==> (1 ((2)) (3 4) 5 6)
"
  (cond ((and depth (<= depth 0)) lst)
        ((null lst) nil)
        ((listp (car lst))
         (append (flatten (car lst) (when depth (1- depth))) (flatten (cdr lst) depth)))
        (t (cons (car lst) (flatten (cdr lst) depth)))))

(_curry-libfun flatten 2 :fun (switch-params #'flatten) :extra-doc
  "In this version the paramter DEPTH is not optional and the parameter at the 
first position.")

(defun transpose (lst)
  "Transposes a 2D nested lst. It also works for jagged arrays.
For example:

    (transpose '((1 2) (3 4)))        ; ==> ((1 3) (2 4))
    (transpose '((11 12) (21 23 24 25) ())) 
                                      ; ==> ((11 21 41) (12 23 42) (24 43) (25))
"
  (let ((max-len (apply #'max (mapcar #'length lst)))
        (res '()))
    (loop for x in (reverse (range max-len)) do
            (push (mapcan (lambda (sub-lst)
                            (let ((el (nth x sub-lst)))
                              (when el (list el)))) lst) res))
    res))

(defun partition (size lst)
  "Partitions a list into a list of partitions with the length SIZE. This 
function is non-destructive.
For example:

    (partition 2 '(1 2 3 4)) ; ==> ((1 2) (3 4))
    (partition 2 '(1 2 3 4 5)) ; ==> ((1 2) (3 4) (5))
"
  (cond ((null lst) lst)
        ((<= (length lst) size) (list lst))
        (t (let ((end (subseq lst size)))
             (cons (subseq lst 0 size)
                   (if (>= (length end) size)
                       (partition size end)
                       (list end)))))))

(_curry-libfun partition 2)

;;; Internal helper function
(setfun _flatten1 (flatten-c 1))

(defun apply-to-partlist (fun size lst)
  "Applies a function to the elements of a partitioned list. The parameter SIZE
specifies the length of the partitions. This function is non-destructive. 
For example:

    (setf partitioned '((1 2) (3 4)))
    (apply-to-partlist #'1+ 2 partitioned) ; ==> ((2 3) (4 5))
"
  (funcall (pipeline #'_flatten1 (map-list-c fun) (partition-c size)) lst))

(_curry-libfun apply-to-partlist 3)

; Source: https://stackoverflow.com/a/36165653
(defun rot-left (n lst)
  "Rotates the elements of a list from left to right N times. This function is
non-destructive."

  (append (nthcdr n lst) (butlast lst (- (length lst) n))))

(_curry-libfun rot-left 2)

; Source: https://stackoverflow.com/a/36165653
(defun rot-right (n lst)
  "Rotates the elements of a list from right to left N times. This function is
non-destructive."
  (rot-left (- (length lst) n) lst))

(_curry-libfun rot-right 2)

(defun swap (i j lst)
  "Swaps the Ith and the Jth element of a list. This function is
non-destructive."
  (let ((copy (copy-seq lst)))
    (rotatef (nth i copy) (nth j copy))
    copy))
:fun 
(_curry-libfun swap 3)

(defun copy-list-deep (lst)
  "Makes a deep copy of a nested list."
  (cond ((not (listp lst)) lst)
        (t (let ((copy (copy-seq lst)))
             (mapcar #'copy-list-deep copy)))))

(defun split-at (idx lst)
  "Splits a list at the position IDX and returns the two parts inside a list.
The element at the position IDX is contained in the second part. This function
is non-destructive. 
For example:

    (split-at 2 '(1 2 3 4 5)) ; ==> ((1 2) (3 4 5))
"
  (if (or (>= idx (length lst)) (<= idx 0))
      lst
      (list (subseq lst 0 idx) (subseq lst idx))))

(_curry-libfun split-at 2)

(defun insert-at (els idx lst)
  "Inserts the elemets ELS into a list a the position IDX. ELS can be a single
element or a list of elements. This function is non-destructive.
For example:

    (insert-at '(100 200) 2 '(1 2 3 4)) ; ==> (1 2 100 200 3 4)
"
  (if (or (> idx (length lst)) (< idx 0))
      lst
      (let ((els (if (listp els) els (list els))))
        (concatenate 'list (subseq lst 0 idx) els (subseq lst idx)))))

(_curry-libfun insert-at 3)

(defun replace-at (el idx lst)
  "Replaces the element at position IDX in a list with the element EL. This
function is non-destructive.
For example:

    (replace-at 200 1 '(1 2 3 4)) ; ==> (1 200 3 4)
"
  (let ((copy (copy-seq lst)))
    (setf (nth idx copy) el)
    copy))

(_curry-libfun replace-at 3)

(defun mod-at (fun idx lst)
  "Modifies an element in a list at position IDX with a transformation function 
FUN. This function is non-destructive.
For example:

    (mod-at (lambda (x) (+ x 100)) 1 '(1 2 3 4)) ; ==> (1 102 3 4)
"
  (replace-at (funcall fun (nth idx lst)) idx lst))

(_curry-libfun mod-at 3)

(defun remove-at (idx lst)
  "Removes the element at position IDX in a list. This function is 
non-desctructive"
  (append (subseq lst 0 idx) (subseq lst (1+ idx) (length lst))))

(_curry-libfun remove-at 2)

(defun slice (start end lst)
  "A more functional version of SUBSEQ: It has a different order of paramters 
and if END exceeds the maximum index, it makes a slice up to the last element."
  (let* ((len (length lst))
         (end (if (> end len) len end)))
    (subseq lst start end)))

(_curry-libfun slice 3)

(defun lsort (lst &optional (pred #'<))
  "Non-destructive version of the standard SORT function."
  (sort (copy-list-deep lst) pred))

(_curry-libfun lsort 3 :fun (switch-params #'lsort) :extra-doc
  "In this version PRED is the parameter at the first position.")

(_setfun-doc take
  "Returns the first N elements of a list."
  (slice-c 0))

(_curry-libfun take 2)

(defun back (lst)
  "Returns the last element of a list."
  (first (last lst)))