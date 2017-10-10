(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(defun solve6 ()
  (nth 10000 (first-n-primes 10001)))
