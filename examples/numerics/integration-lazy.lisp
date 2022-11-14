;;;; 
;;;; Numerical integration
;;;;
;;;; Inspired by the popular paper "Why Functional Programming Matters", written 
;;;; by John Hughes.
;;;;

(require :functional-cl)
(require :clazy)

(defun lazy-add-pair (cons-pair)
  (+ (clazy:head cons-pair) (clazy:tail cons-pair)))

(clazy:deflazy integrate (f a b)
  (clazy:call #'integ f a b (funcall f a) (funcall f b)))

(clazy:deflazy integ (f a b fa fb)
  (let* ((mid (/ (+ a b) 2))
        (fm (funcall f mid)))
    (clazy:call #'cons 
                (/ (* (+ fa fb) (- b a)) 2) ; Trapezoidal rule
                (clazy:call #'lazy-map-list #'lazy-add-pair 
                                            (clazy:call #'zip2 (clazy:call #'integ f a mid fa fm)
                                                               (clazy:call #'integ f mid b fm fb))))))

(clazy:deflazy zip2 (lst1 lst2)
  (clazy:call #'cons (clazy:call #'cons 
                                 (clazy:head lst1) (clazy:head lst2)) 
                     (clazy:call #'zip2 (clazy:tail lst1) (clazy:tail lst2))))

(clazy:deflazy lazy-map-list (fun lst)
  (unless (null lst)
    (clazy:call #'cons (funcall fun (clazy:head lst))
                       (clazy:call #'lazy-map-list fun (clazy:tail lst)))))

(clazy:deflazy within (eps lst)
  (let ((a (clazy:head lst)) (b (clazy:head (clazy:tail lst))))
    (if (<= (abs (- a b)) eps)
      b
      (clazy:call #'within eps (clazy:tail lst)))))

(clazy:deflazy elim-error (n lst)
  (let ((a (clazy:head lst)) (b (clazy:head (clazy:tail lst))))
    (clazy:call #'cons (/ (- (* b (expt 2 n)) a) (1- (expt 2 n))) 
                       (clazy:call #'elim-error n (clazy:tail lst)))))

(functional-cl:setfun log2 (functional-cl:log-c 2))

(clazy:deflazy order (lst)
  (let ((a (clazy:head lst)) (b (clazy:head (clazy:tail lst)))
        (c (clazy:head (clazy:tail (clazy:tail lst)))))
    (fround (log2 (1- (/ (- a c) (- b c)))))))

(clazy:deflazy improve (s)
  (clazy:call #'elim-error (clazy:call #'order s) s))

(defun improved-integration (eps f a b)
  (clazy:call #'within eps (clazy:call #'improve (clazy:call #'integrate f a b))))


(defun test-func (x) (* 3 (expt x 2)))

; (improved-integration 1e-5 #'test-func 0 2)
