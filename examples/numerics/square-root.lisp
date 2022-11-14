;;;; 
;;;; Square root of a number using the  Newton-Raphson Method
;;;;
;;;; Inspired by the popular paper "Why Functional Programming Matters", written 
;;;; by John Hughes.
;;;;

(require :functional-cl)

(defun next (n x)
  (/ (+ x (/ n x)) 2))

(functional-cl:setfun next-c (functional-cl:curry2 #'next))

(defun withinp (eps calcs)
  (if (cadr calcs)
    (let ((a (functional-cl:back calcs))
          (b (functional-cl:back (butlast calcs))))
      (> (abs (- a b)) eps))
    t))

(functional-cl:setfun result (functional-cl:compose-simple #'float #'functional-cl:back))

(defun square-root (eps n a0)
  (result (functional-cl:iterate-while (functional-cl:partial #'withinp eps) (next-c n) a0)))

; (square-root 1e-20 2 5)
