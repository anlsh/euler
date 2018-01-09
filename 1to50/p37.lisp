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

(defun assert-in-range (num left right)
  "Assert that num is in [left, right)"
  (assert (and (>= num left) (< num right))))

(defun append-digit (n to-append &optional (base 10))
  "Given a number n, return the number given by appending to-insert (as the least-significant digit)
   in the relevant base"
  (assert-in-range to-append 0 base)
  (+ (* n base) to-append))

(defun prepend-digit (n to-prepend &optional (base 10))
  (assert-in-range to-prepend 0 base)
  (+ (* (expt base (numdigits n base)) to-prepend) n))

; The following miller-rabin code is sourced from
; https://rosettacode.org/wiki/Miller%E2%80%93Rabin_primality_test#Common_Lisp

(defun factor-out (number divisor)
  "Return two values R and E such that NUMBER = DIVISOR^E * R,
  and R is not divisible by DIVISOR."
  (do ((e 0 (1+ e))
       (r number (/ r divisor)))
      ((/= (mod r divisor) 0) (values r e))))

(defun mult-mod (x y modulus) (mod (* x y) modulus))

(defun expt-mod (base exponent modulus)
  "Fast modular exponentiation by repeated squaring."
  (labels ((expt-mod-iter (b e p)
             (cond ((= e 0) p)
                   ((evenp e)
                    (expt-mod-iter (mult-mod b b modulus)
                                   (/ e 2)
                                   p))
                   (t
                    (expt-mod-iter b
                                   (1- e)
                                   (mult-mod b p modulus))))))
    (expt-mod-iter base exponent 1)))

(defun random-in-range (lower upper)
  "Return a random integer from the range [lower..upper]."
  (+ lower (random (+ (- upper lower) 1))))

(defun miller-rabin-test (n k)
  "Test N for primality by performing the Miller-Rabin test K times.
  Return NIL if N is composite, and T if N is probably prime."
  (cond ((= n 1)   nil)
        ((< n 4)     t)
        ((evenp n) nil)
        (t
         (multiple-value-bind (d s) (factor-out (- n 1) 2)
           (labels ((strong-liar? (a)
                      (let ((x (expt-mod a d n)))
                        (or (= x 1)
                            (loop repeat s
                                  for y = x then (mult-mod y y n)
                                  thereis (= y (- n 1)))))))
             (loop repeat k
                   always (strong-liar? (random-in-range 2 (- n 2)))))))))

(snakes:defgenerator make-prime-generator (&aux (prime-list nil) (sieve-array nil) (block-num -1) block-size (real-num 0))

  (setf block-size 100000)

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
    (loop for index from 0 to (array-dimension sieve-array 0) do
      (when (aref sieve-array index)
        (setf real-num (+ index (* block-num block-size)))
        (when (> real-num 1)
          ;; "Yield"
          (snakes:yield real-num)
          (push real-num prime-list)
          (kill-multiples real-num))))))

(defun primes-leq-g (upbound)
  (loop with primes = (make-prime-generator) for prime = (funcall primes)
        while (<= prime upbound) collect prime))

(defun q-create ()
  (list nil))

(defun q-is-empty (queue)
  (null (car queue)))

(defun q-as-list (queue)
  (car queue))

(defun q-peep (queue)
  (caar queue))

(defun q-pop (queue)
  (pop (car queue)))

(defun q-add (queue item)
  (let ((new-last (list item)))
    (if (null (car queue))
        (setf (car queue) new-last)
        (setf (cddr queue) new-last))
    (setf (cdr queue) new-last)))

(defun solve37 (&aux (test-level 100) (truncatable-primes nil) (candidate-q nil))
  (setf candidate-q (q-create))
  (q-add candidate-q 37)
  (q-add candidate-q 97)
  (loop while (not (q-is-empty candidate-q)) for num = (q-pop candidate-q) do
    (push num truncatable-primes)
    (loop for d from 1 to 9
          for prepend-num = (prepend-digit num d)
          for append-num = (append-digit num d) do
            (when (and (miller-rabin-test prepend-num test-level)
                        (member (truncate-right prepend-num) truncatable-primes))
              (q-add candidate-q prepend-num))
            (when (and (miller-rabin-test append-num test-level)
                        (member (truncate-left append-num) truncatable-primes))
              (q-add candidate-q append-num))))
  truncatable-primes)
