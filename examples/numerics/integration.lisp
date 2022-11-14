;;;; 
;;;; Numerical integration
;;;;
;;;; Inspired by the popular paper "Why Functional Programming Matters", written 
;;;; by John Hughes.
;;;;

(require :functional-cl)

;;; Must be loaded from the root directory of functional-cl. There is no point
;;; in creating an .asd file just for this small example.
(load "./examples/numerics/square-root.lisp")

(defun easy-integrate (f a b)
  (* (+ (funcall f a) (funcall f b)) (/ (- b a) 2)))

(defun add-pair (cons-pair)
  (+ (car cons-pair) (cdr cons-pair)))

(defun integrate (eps-fun f a b)
  (labels ((integ (a b)
    (if (funcall eps-fun a b)
      (let ((mid (/ (+ a b) 2)))
        (cons (easy-integrate f a b)
              (functional-cl:map-list #'add-pair 
                                      (functional-cl:zip-to-alist (integ a mid)
                                                                  (integ mid b)))))
      (list (easy-integrate f a b) (easy-integrate f a b)))))
  (integ a b)))

(defun integrate-within (eps f a b)
  (integrate (functional-cl:unsplat (functional-cl:partial #'withinp eps)) f a b))

(functional-cl:setfun integration 
  (functional-cl:compose-simple #'result #'integrate-within))

;; Memoisation
(functional-cl:setfun easy-integrate (functional-cl:memoize #'easy-integrate))

(defun test-func (x) (* 3 (expt x 2)))

; (integration 1e-5  #'test-func 0 2)
