(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(defun solve97 (&aux (base97 (expt 10 10)))
  (modadd base 1 (modmult base 28433 (modexpt base 2 7830457))))
