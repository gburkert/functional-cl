(in-package :functional-cl)

(defun splat (f)
  "Changes a function to accept a list with parameters instead of indvidual 
parameters. 
For example:

    (setfun mapcar-sp (splat #'mapcar))
    (mapcar-sp '(1+ (1 2 3 4))) ; ==> (2 3 4 5)
"
  (lambda (arglist)
    (apply f arglist)))

(defun unsplat (f)
  "Changes a function to accept individual parameters instead of a list."
  (lambda (&rest args)
    (funcall f args)))

(defun switch-params (f &optional (i 0) (j 1))
  "Flips the the Ith and the Jth parameter of a function. 
For example:

    (setfun mapcar-switched (switch-params #'mapcar 0 1))
    (mapcar-switched '(1 2 3 4) #'1-) ; ==> (0 1 2 3)
"
  (lambda (&rest args)
    (apply f (swap i j args))))

(_curry-libfun switch-params 3 :extra-doc
"In this version the parameters I and J are not optional but mandatory.")

(defun args (f n)
  "Adapts a function with &optional or &rest parameters to a function with
exactly N parameters."
  (lambda (&rest args)
    (apply f (take n args))))

(_curry-libfun args 2)

(defun rot-paramsr (f n)
  "Rotates the arguments of a function from right to left N times. With N=1:
  
    f(a,b,c) => f(c,a,b)"
  (lambda (&rest args)
    (funcall (splat f) (rot-right n args))))

(_curry-libfun rot-paramsr 2)

(defun rot-paramsl (f n)
  "Rotates the arguments of a function from left to right N times. With N=1:
  
    f(a,b,c) => f(b,c,a)"
  (lambda (&rest args)
    (funcall (splat f) (rot-left n args))))

(_curry-libfun rot-paramsl 2)
