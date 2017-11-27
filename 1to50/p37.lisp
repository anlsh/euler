(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(defun truncate-right (n &optional (base 10))
  "Truncate the right-most digit of a number n in base"
  (floor (/ n base)))

(defun truncate-left (n &optional (base 10) &aux n-digits most-significant-digit)
  "Truncate the left-most digit of a number n in the given base"
  (setf n-digits (numdigits n base))
  (setf most-significant-digit (digit n (- n-digits 1) base))
  (- n (* most-significant-digit (expt base (- n-digits 1)))))

(defun primes-leq (n)
  ;; Sourced from https://codereview.stackexchange.com/questions/28826/generating-and-using-a-list-of-prime-numbers
  "return the list of primes not greater than n.
   build it by means of the sieve of Eratosthenes."
  (do ((arr (make-array (+ n 1) :element-type 'boolean :initial-element t))
       (result (list 2))
       (p 3 (+ p 2)))   ; a candidate possible prime
      ((> p (/ n p))
       (nconc (nreverse result)
              (loop for i from p to n by 2 if (aref arr i) collect i)))
    (when (aref arr p)              ; not marked as composite: p is prime
      (push p result)
      (loop for i from (* p p) to n by (* 2 p)  ; mark the multiples
            do (setf (aref arr i) nil)))))

(snakes:defgenerator make-prime-generator (&aux (prime-list nil) (sieve-array nil) (block-num -1) block-size (real-num 0))

  (setf block-size 1000000)

  (defun kill-multiples (factor-num &aux marker-index)
    (setf marker-index (rem (* block-num block-size) factor-num))
    (loop while (< marker-index block-size) do
      (setf (aref sieve-array marker-index) nil)
      (incf marker-index factor-num)))

  (loop while t do
    (incf block-num)
    (setf sieve-array (make-array block-size :element-type 'boolean :initial-element t))
    (loop for prime in prime-list do
      (kill-multiples prime))
    (loop for index from 0 to (- (array-dimension sieve-array 0) 1) do
      (when (aref sieve-array index)
        (setf real-num (+ index (* block-num block-size)))
        (when (> real-num 1)
          ;; "Yield"
          (snakes:yield real-num)
          (push real-num prime-list)
          (kill-multiples real-num))))))
