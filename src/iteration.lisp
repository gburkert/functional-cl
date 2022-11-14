(in-package :functional-cl)

(defun repeatedly (times fun)
  "Applies a function repeatedly and returns the results in a list. The input 
to FUN is the index of the iteration, which means that FUN must be unary.
For example:

    (repeatedly 5 #'1+) ; ==> (1 2 3 4 5)
"
  (mapcar fun (range times)))

(_curry-libfun repeatedly 2)

(defun iterate (fun count init)
  "Creates a list by applying a function COUNT times. With INIT either an
initial element can be specified, or alterantively a list from whose last
element the iteration starts.
For example:

    (iterate #'1+ 5 5)                 ; ==> (5 6 7 8 9 10)
    (iterate (lambda (n) (* 2 n)) 7 1) ; ==> (1 2 4 8 16 32 64 128)
    ; with an initial list:
    (iterate #'1+ 5 '(2 3 4 5))        ; ==> (2 3 4 5 6 7 8 9 10)
"
  (let ((res (if (listp init) init (list init))))
    (labels ((iteration (count res)
                (cond ((<= count 0) (reverse res))
                      (t (iteration (1- count) (cons (funcall fun (car res)) res))))))
      (iteration count (reverse res)))))

(_curry-libfun iterate 3)

(defun iterate-while (pred fun init)
  "Creates a list by applying a function as long as PRED returns T. The argument
provided to PRED is the resulting list. With INIT either an initial element can
be specified, or alterantively a list from whose last element the iteration
starts.
For example:

  (iterate-while (lambda (l) (> 10 (first (last l)))) #'1+ 5) ; ==> (5 6 7 8 9)
"
  (let ((res (if (listp init) init (list init))))
    (labels ((iteration (res)
                (let ((reversed (reverse res)))
                  (cond ((funcall pred reversed) (iteration (cons (funcall fun (car res)) res)))
                        (t (butlast reversed))))))
      (iteration (reverse res)))))

(defun n-times-in-list (arg n)
  "Returns a list containing the element ARG N times.
For example:

    (n-times-in-list 1 5) ; ==> (1 1 1 1 1)
"
  (repeatedly n (constantly arg)))

(_curry-libfun n-times-in-list 2)


(defun range (start &optional to (step 1))
  "Returns a list with a sequence of numbers from START through TO (exclusive).
If the parameter TO is not specified, the sequence goes from 0 to START.
For example:

    (range 1 5) ; ==> (1 2 3 4)
    (range 3)   ; ==> (0 1 2)
"
  (cond ((null to) (range 0 start step))
        ((> step 0)
         (loop for n from start below to by step collect n))
        ((< step 0)
         (loop for n from start above to by (- 0 step) collect n))
        (t nil)))

(_curry-libfun range 2 :extra-doc
"In this version the parameter TO is not optional but mandatory.")