(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(load (merge-pathnames "../macros.lisp" *load-truename*))
(require 'macros)

(defun solve87 (&aux (numbers (make-hash-table)) (max 50000000) (prime-list (primes-leq (sqrt max))))
  (loop for p1 in prime-list do
    (loop for p2 in prime-list while (<= p2 (expt (- max (expt p1 2)) 1/3)) do
      (loop for p3 in prime-list while (<= p3 (expt (- max (expt p1 2) (expt p2 3)) 1/4)) do
        (sethash (+ (expt p1 2) (expt p2 3) (expt p3 4)) numbers t))))
  (hash-table-count numbers))
