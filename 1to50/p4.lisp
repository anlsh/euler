(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(defun solve4 (&aux (n 20) (multiple 1))
  (loop for p in (primes-leq 20) do
    (setq multiple (* multiple (expt p (floor (log n p))))))
  multiple
  )
