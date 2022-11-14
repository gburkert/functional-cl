(in-package :functional-cl)

(defmacro setfun (symb fun &key (doc nil))
  "Simplifies the binding of the function FUN to the symbol SYMB, such that the
function can be called without the use of FUNCALL or APPLY.
For example:

    (setfun my-symb (lambda (n) (* n 5)))
    (my-symb 3) ; ==> 15
"
  `(prog1 ',symb 
          (setf (symbol-function ',symb) ,fun)
          (when ,doc (setf (documentation (symbol-function ',symb) T) ,doc))))

;;; Internal helper macro:
;;; Like SETFUN but with a documentation string as the seconde paramter.
(defmacro _setfun-doc (symb doc-str fun)
  `(setfun ,symb ,fun :doc ,doc-str))

; Source: P. Graham - On Lisp
(defun memoize (fn)
  "Returns a memoizing version of a function: Already calculated results are
temporarily stored in a hash table. Caution: To work properly Caution: To work 
properly, the memoized function must be bound to the symbol-function of the 
original function. 
For example:

    (defun fibo (n) 
      (cond ((< n 2) n)
        (t (+ (fibo (- n 1)) (fibo (- n 2))))))
    (setfun fibo (memoize #'fibo))
"
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
        (if win 
            val
            (setf (gethash args cache)
                  (apply fn args)))))))