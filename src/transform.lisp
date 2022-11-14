(in-package :functional-cl)

(_setfun-doc flat-map
  "Applies a transformation to LST and flattens the result into a list.
For example:

    (defun mul2-lst (n)
      (list (* 2 n)))
    ; regular map:
    (mapcar #'mul2-lst '(1 2 3))   ; ==> ((2) (4) (6))
    ; flat-map:
    (flat-map #'mul2-lst '(1 2 3)) ; ==> (2 4 6)
"
  (pipeline #'mapcar (splat #'append)))

(_curry-libfun flat-map 2)

(_setfun-doc map-list
  "Alias for MAPCAR."
  #'mapcar)

(_curry-libfun map-list 2)

(_setfun-doc reduce-list-left
  "Reduce list from left: A more functional version of REDUCE without keyword
parametes."
  (lambda (f init seq)
    (reduce f seq :initial-value init)))

(_curry-libfun reduce-list-left 3)

(_setfun-doc reduce-list-right
  "Reduce list from right: A more functional version of REDUCE without keyword
parametes."
  (lambda (f init seq)
    (reduce f seq :initial-value init :from-end t)))

(_curry-libfun reduce-list-right 3)

(_setfun-doc filter-list
  "Filter list with a predicate."
  (lambda (pred seq)
    (remove-if-not pred seq)))

(_curry-libfun filter-list 2)

(_setfun-doc reduce-list
  "Alias for REDUCE-LIST-RIGHT."
  #'reduce-list-right)

(_curry-libfun reduce-list 3)

(_setfun-doc foldr
  "Alias for REDUCE-LIST-RIGHT."
  #'reduce-list-right)

(_curry-libfun foldr 3)

(_setfun-doc foldl
  "Alias for REDUCE-LIST-LEFT."
  #'reduce-list-left)

(_curry-libfun foldl 3)

(defun map-index (fun lst)
  "MAPCAR with the index provided as the second argument to the function to be 
applied. Unlike MAPCAR only one list can be passed to the function."
  (loop for el in lst
        for idx in (range (length lst))
        collect (funcall fun el idx)))

(_curry-libfun map-index 2)
