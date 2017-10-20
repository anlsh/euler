(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(defun solve35 (&aux (circprimes 0) (primelist (primes-leq 1000000)))
  ;; This solution is O(n^2), runs in about two minutes. Much, MUCH, better
  ;; performance should be achievable by putting the primes in AVL, allowing for
  ;; log(n) contains-checking rather than O(n) contains checking
  (loop for p in primelist do
    (loop for i from 0 to (numdigits p) do (setf p (rotate p)) always (find p primelist)
          finally (incf circprimes)))
  circprimes)

(defun rotate (n &key (base 10) &aux (baselen (numdigits n base)))
  "Given n, rotate n left. Eg given 435, return 543"
  (+ (floor (/ n base)) (* (mod n base) (expt base (- baselen 1)))))
