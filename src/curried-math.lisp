(in-package :functional-cl)

;;; Internal helper function:
;;; Curries math operators whose parameters are passed as pairs. The CAR of the
;;; the pair designates the operator and the CDR designates the new name. The
;;; resulting function is bound to the new name with the suffix "-c". Optionally
;;; a boolean value can be added to the pair, which states if the original
;;; documentation of the operation should be added to the one of the resulting
;;; function.
;;;
;;; '(+ add) '(- sub) etc. => ADD-C SUB-C etc.
(defun _curry-math-ops (&rest ops-list)
  (loop for pair in ops-list
      do (destructuring-bind (old-symb new-symb &optional (include-doc nil)) pair
            (let ((fun (symbol-function  old-symb))
                  (curry-symb (_str-to-symb (concatenate 'string (string new-symb) "-c"))))
              (prog1
                curry-symb
                (setf (symbol-function curry-symb) (curry2 fun))
                (setf (documentation (symbol-function curry-symb) t)
                      (concatenate 'string "[Curried version] of " (string old-symb)
                                              (if include-doc
                                                  (format nil "~%Doc: ~a" 
                                                          (documentation old-symb t)) ""))))))))

;;;
;;; Redefinitions of existing functions to be curried:
;;;

;;; expt => pow
(_setfun-doc _pow
  "EXPT funciton with switched paramaters. => POW power-number base-number"
  (switch-params #'expt))

(_setfun-doc _log
  "LOG function with switched parameters. => LOG base number"
  (switch-params #'log))


;;; One tuple per function:
;;; '(<function symbol> <new symbol> [include documentation])
(_curry-math-ops

        ; basic
        '(+ add)
        '(- sub)
        '(* mul)
        '(/ div)
        '(_pow pow t)
        '(_log log t)

        ; compare
        '(< gt)
        '(<= gte)
        '(> lt)
        '(>= lte)
        '(= eq)
        '(/= neq))
