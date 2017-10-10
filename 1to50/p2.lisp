(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(defun solve2 ()
  (loop for x in (fibonacci-leq 4000000) if (evenp x) sum x)
  )
