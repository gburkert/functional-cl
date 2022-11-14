(in-package :functional-cl)

(defun compose-simple (f g)
  "Returns the composition of exactly two functions F and G: 
  
    f(g(args))."
  (lambda (&rest args)
    (funcall f (apply g args))))

(_curry-libfun compose-simple 2)

(defun compose-funcs (f g &optional npar)
  "Returns the composition of two functions F and G: f(g(args)).
The optional parameter NPAR speciefies the number of arguments the function 
called first should consume. The remaining arguments are consumed by the second 
function."
  (lambda (&rest args)
    (let ((nargs (if npar npar (length args))))
      (apply f (cons (apply g (subseq args 0 nargs))
                     (nthcdr nargs args))))))

(_curry-libfun compose-funcs 3 :extra-doc
  "In this version the parameter NPAR is not optional but mandatory.")

(defun pipeline (&rest funcs)
  "Returns the composition of a sequence of functions. The order of  the 
functions corresponds to their order in the call of PIPELINE."
  (lambda (&rest args)
    (car (reduce
          (lambda (ar f) (list (apply f ar)))
          funcs
          :initial-value args))))

(defun and-checker (&rest validators)
  "Returns a function that returns T if every function in the seqeuence of 
functions VALIDATORS returns T for a given argument. Otherwise it returns NIL.
For example:

    (setf my-checker (and-checker #'evenp #'plusp))
    (funcall my-checker 2) ; ==> T  
    (funcall my-checker 3) ; ==> NIL
"
  (lambda (elem)
    (every (lambda (check) (funcall check elem)) validators)))

(defun or-checker (&rest validators)
  "Returns a function that returns T if at least one function in the seqeuence 
of functions VALIDATORS returns T for a given argument. Otherwise it returns
NIL.
For example:

    (setf my-checker (or-checker #'evenp #'plusp))
    (funcall my-checker 3) ; ==> T  
    (funcall my-checker -3) ; ==> NIL
"
  (lambda (elem)
    (some (lambda (check) (funcall check elem)) validators)))

(defun dispatch (&rest funcs)
  "Returns a function from a list of functions FUNCS, which returns the result
of the first of these that returns a non-NIL value."
  (lambda (&rest args)
    (if (null funcs)
        nil
        (let ((result (apply (car funcs) args)))
          (if result
              result
              (apply (apply #'dispatch (cdr funcs)) args))))))

(defun decorate (f pre &optional (post #'identity))
  "Returns a function that is decorated with a PRE and an optional POST 
processing function. Calling a wrapped function with the argument :ORIG returns
the original function. Caution: PRE must return a list.
For example:

    (defun print-args (&rest args)
      (format t \"Called with args ~S~%\" args)
      args)

    (defun print-res (res)
      (format t \"Result: ~S~%\" res)
      res)

    (setfun my-decorate (decorate #'+ #'print-args #'print-res))
    (my-decorate 1 2)  ; ==> Called with args (1 2)
                       ; ==> Result: 3
                       ; ==> 3
"
  (lambda (&rest args)
    (cond ((equal args '(:orig)) f)
          (t (funcall post (apply f (apply pre args)))))))

(_curry-libfun decorate 3 :extra-doc
  "In this version the parameter POST is not optional but mandatory. If not 
needed, just use the IDENTITY function.")

(defun defaults (&rest defaults)
  "Returns a function with defaults for its arguments.
For example: 

    (setfun pow (decorate #'expt (defaults 1 2)))
    (pow 2 3) ; ==> 8
    (pow 2)   ; ==> 4
    (pow)     ; ==> 1
"
  (labels ((merge-defaults (seq defaults)
              (if (null defaults)
                  seq
                  (cons (if (or (null seq) (null (car seq))) (car defaults) (car seq))
                        (merge-defaults (cdr seq) (cdr defaults))))))
    (lambda (&rest args)
      (merge-defaults args defaults))))

; Inpired by the :pre and :post conditions from Clojure.
(defun constrained (f &optional (pre (constantly t)) (post (constantly t)))
  "Returns a function that makes an optional PRE resp. POST assertion call
before resp. after calling the function F.
    (setfun my-constrained (constrained #'expt #'all-plusp #'evenp))
    (my-constrained 2 3)  ; ==> 8
    (my-constrained 2 -2) ; ==> ERROR (as -2 is negative)
    (my-constrained 3 3)  ; ==> ERROR (as the result 27 is not even)
"
  (decorate f (_assert-pre pre) (_assert-post post)))

(_curry-libfun constrained 3 :extra-doc
  "In this version the parameter PRE and POST are not optional but mandatory.
If not needed, just use the closure (CONSTANTLY T).")

;;; Internal helper function
(defun _assert-pre (fun)
  (lambda (&rest args)
    (assert (apply fun args))
    args))

;;; Internal helper function
(defun _assert-post (fun)
  (lambda (&rest args)
    (assert (apply fun args))
    (values-list args)))

(defun keep-args (fun idx args)
  "Modifies an element in a list at position IDX by applying the whole list 
to FUN. The curried version can be used together with PIPELINE to transform 
only one element of list and pass the rest unmodified through the pipe:
'(a b c) = keep-args at pos. 1 => (list a (fun a b c) c)
For example (curried version!):

    (setf my-args '(1 0 2))
    (funcall (pipeline 
               (keep-args-c (lambda (a b c) (+ a b c)) 1)
               (foldr-c #'+ 0)) my-args) ; ==> 6
"
  (replace-at (apply fun args) idx args))

(_curry-libfun keep-args 3)

(defun tap (fun)
  "Applies FUNC to ARGS, but returns the input ARGS. This function is typically
used inside a PIPELINE to print an intermediate result, particularly for 
debugging.
Note: In non-purely functional programming languages sometimes side effects 
which concerning IO can only be encapsulated with a lot of boilerplate code like
monads. This function provides an impure but pragmatic solution.
For example:

    (funcall (tap (lambda (x) (print (1+ x)))) 99) ; ==> *prints 100* 
                                                   ;     but returns 99
"
  (lambda (&rest args)
    (progn (apply fun args) (values-list args))))