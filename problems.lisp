(load "fibonacci.lisp")
(load "primes.lisp")
(require 'fibonacci)
(require 'primes)


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

(defun problem3 (&aux (maxpalindrome 0))
  (loop for x from 999 downto 100
    until (< (* x x) maxpalindrome) do
    (loop for y from x downto 100 do
      (let* ((test (* x y)))
        (if (and (ispalindrome test) (> test maxpalindrome))
            (setq maxpalindrome test)))))
  maxpalindrome)

(defun problem4 (&aux (n 20) (multiple 1))
  (loop for p in (primes-leq 20) do
    (setq multiple (* multiple (expt p (floor (log n p))))))
  multiple
  )

(defun problem5 ()
  (- (expt (loop for i from 1 to 100 sum i) 2) (loop for i from 1 to 100 sum (* i i))))

(defun problem6 ()
  (nth 10000 (first-n-primes 10001)))
