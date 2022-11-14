(in-package :functional-cl)

;;; Internal helper function:
;;; Converts a string to a symbol.
(defun _str-to-symb (str)
  (nth-value 0 (read-from-string str)))

;;; Internal helper macro:
;;; Curries a library function and adds the suffix -c to its name. With the
;;; keyword parameter FUN you can specify an adapted version of the function,
;;; e.g. with a different order of paramteres. EXTRA-DOC can be used to add
;;; further content to the documentation string.
(defmacro _curry-libfun (symb numargs &key(fun nil) (extra-doc nil))
  `(let* ((symb-fun (symbol-function ',symb))
          (symb-str (string ',symb))
          (new-symb (_str-to-symb (concatenate 'string symb-str "-c"))))
     (prog1 new-symb 
            (setf (symbol-function new-symb) (curry-n (if (not ,fun) symb-fun ,fun) ,numargs))
            (setf (documentation (symbol-function new-symb) t)
                  (format nil "~a~a~%~a~%~a" "[Curried version] of " symb-str
                          (if ,extra-doc (format nil "~%~a~%" ,extra-doc) "")
                          (documentation symb-fun t))))))
                                            
(defun partial (f &rest args)
  "Partial application of a function: Returns a function of the remaining
arguments.
For example:

    (defun foo (a b c)
      (+ a (* b c)))
    (setfun my-partial (partial #'foo 1 2))
    (my-partial 3) ; ==> 7
"
  (lambda (&rest more-args)
    (apply f (append args more-args))))

(defun partialr (f &rest args)
  "Partial application of a function from the right: Returns a function of the 
remaining arguments.
For example:

    (defun foo (a b c)
      (+ a (* b c)))
    (setfun my-partial (partialr #'foo 1 2)) ; b=2, c=1
    (my-partial 3) ; ==> 5
"
  (lambda (&rest more-args)
    (apply f (append more-args (reverse args)))))


(defun curry2 (f)
  "Curries a function of two arguments, starting from the left:

    f(a,b) => f(a)(b)"
  (lambda (a)
    (lambda (b)
      (funcall f a b))))

(defun curry3 (f)
  "Curries a function of three arguments, starting from the left:

    f(a,b,c) => f(a)(b)(c)"
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (funcall f a b c)))))

(defun curry2r (f)
  "Curries a function of two arguments, starting from the right:
    f(a,b) => f(b)(a)"
  (lambda (b)
    (lambda (a)
      (funcall f a b))))

(defun curry3r (f)
  "Curries a function of three arguments, starting from the right:

    f(a,b,c) => f(c)(b)(a)"
  (lambda (c)
    (lambda (b)
      (lambda (a)
        (funcall f a b c)))))

(defun curry-n (f numargs)
  "Curries a function with NUMARGS parameters, starting from the left.
For example:

    (defun foo (a b c d)
      (+ (* a b) (* c d)))
    (setfun fooc (curry-n #'foo 4)) ; ==> foo(a)(b)(c)(d)
"
  (lambda (&rest args)
    (if (>= (length args) numargs)
        (apply f args)
        (curry-n
         (lambda (&rest restargs) (apply f (append args restargs)))
         (- numargs (length args))))))

(defun curry (f numargs)
  "Curries a function with NUMARGS parameters, starting from the left. This
enhanced version of CURRY-N allows placeholder arguments '_ to be used for
arguments, which are not yet available.
For example:

    (setfun exptc (curry #'expt 2))
    (setfun square (exptc '_ 2)) ; use placeholder for first argument
    (square 3) ; ==> 9
"
  ;; This is necessary because the symbol '_ is otherwise bound to the package.
  (let ((pholder (_str-to-symb "_")))
    (flet ((fill-args (seq values)
              (append (loop for el in seq
                            for idx from 0
                            collect (cond ((null values) el)
                                          ((eq el pholder) (pop values))
                                          (t el))) values))
          (take-args (n seq)
              (loop for i to (1- n)
                    for el in seq
                    collect el)))
      (lambda (&rest args)
        (let* ((currargs (take-args numargs args))
              (countargs (- (length currargs) (count pholder args))))
          (if (>= countargs numargs)
              (apply f args)
              (curry (lambda (&rest restargs) 
                       (apply f (fill-args currargs restargs))) (- numargs countargs))))))))


(defun uncurry2 (f)
  "Uncurries a function of two arguments, starting from the left:

    f(a)(b) => f(a,b)"
  (lambda (a b)
    (funcall (funcall f a) b)))

(defun uncurry3 (f)
  "Uncurries a function of three arguments, starting from the left:

    f(a)(b)(c) => f(a,b,c)"
  (lambda (a b c)
    (funcall (funcall (funcall f a) b) c)))

(defun uncurry2r (f)
  "Uncurries a function of two arguments, starting from the right:

    f(a)(b) => f(b,a)"
  (lambda (a b)
    (funcall (funcall f b) a)))

(defun uncurry3r (f)
  "Uncurries a function of three arguments, starting from the right:

    f(a)(b)(c) => f(a,b,c)"
  (lambda (a b c)
    (funcall (funcall (funcall f c) b) a)))

(defun uncurry-n (f numargs)
  "Uncurries the function F with NUMARGS parameters, starting from the left.
For example:

    (setfun sum3c (curry-n #'+ 3))
    (setfun sum2-add5c (sum3c 5))
    (setfun sum2-add5 (uncurry-n #'sum2-add5c 2))
    (sum2-add5 1 2) ; ==> 8
    (sum2-add5 1)   ; ==> ERROR (as sum2-add5 expects 2 arguments)
"
  (lambda (&rest args)
    (let ((res f))
      (loop for x in (range numargs) do
              (setf res (funcall res (nth x args))))
      res)))
