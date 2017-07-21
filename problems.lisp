(load "fibonacci.lisp")
(require 'fibonacci)


(defun problem2 ()
  (loop for x in (fibonacci-leq 4000000) if (evenp x) sum x)
  )
