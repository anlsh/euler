(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(defun problem10 ()
  (loop for p in (primes-leq 2000000) sum p))
