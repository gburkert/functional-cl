(in-package :functional-cl)

(defun change-slot (prop arg struct)
  "Changes a slot value of a structure in a non-destructive way."
  (let ((struct-copy (copy-structure struct)))
    (setf (slot-value struct-copy prop) arg)
    struct-copy))

(_curry-libfun change-slot 3)
