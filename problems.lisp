(load "fibonacci.lisp")
(require 'fibonacci)


(defun problem2 ()
  (loop for x in (fibonacci-leq 4000000) if (evenp x) sum x)
  )

(defun digit (num exp &optional (base 10))
  "Returns the coefficient of base^exp in base-base representation of num"
  (mod (floor (/ num (expt base exp))) base))

(defun numdigits (n &optional (base 10))
  "Returns the number of digits in the base-representation of n"
  (ceiling (log n base)))

(defun ispalindrome (n &optional (base 10) &aux (len (numdigits n base)))
  "Returns true if n is palindromic in base, false elsewhere"
  (loop for x from 0 to (ceiling (/ len 2)) do
    (if (not (= (digit n x base) (digit n (- len 1 x) base)))
        (return-from ispalindrome nil)))
  t)
