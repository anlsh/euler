(provide 'numbers)

(defun factorial (n)
  (if (= n 0) (return-from factorial 1)
      (return-from factorial (* n (factorial (- n 1))))))

(defun divisor-in-list (i testlist)
  "Evaluates every element in testlist to see if any divide i"
  (dolist (n testlist)
    (if (= 0 (mod i n))
        (return-from divisor-in-list t)
        )
    )
  (return-from divisor-in-list nil)
  )

(defun divisor-in-slist (i testlist &aux (upbound (sqrt i)))
  "Given a forward-sorted list, see if any elements divide i"
  ;; TODO Use binary search to take advantage of sortedness
  (loop for p in testlist while (<= p upbound) do
    (if (zerop (mod i p))
        (return-from divisor-in-slist t)))
  (if (member i testlist)
      (return-from divisor-in-slist t))
  (return-from divisor-in-slist nil)
  )

;; Prime generation, factorization, etc

(defun primes (&optional (known-primes nil) &aux (plist (copy-list known-primes)) tail n f)
  "Lazily generate primes based on a given list of known primes"
  ;; https://stackoverflow.com/questions/46496083/lazily-generating-prime-in-common-lisp
  (defun fbare ()
    (setf f #'fgen)   ; setting f to the next function
    (setq plist (list 2))
    (setf tail (last plist))
    (setq n 3)
    2)
  (defun fgen ()
    (loop while (divisor-in-slist n plist)
          do (incf n 2))
    (setf (cdr tail) (cons n nil))
    (setf tail (cdr tail))
    n)
  (when plist
    (setq n (car (last plist)))
    (setf tail (last plist)))
  (if (not plist)
      (setf f #'fbare)
      (setf f #'fgen))
  (lambda ()
    (funcall f)))

(defun factor (n &optional (prime-list nil) (factors (cons 0 nil)) (tail factors) (pgen (primes prime-list)) p)
  "Given a natural number, return (as a list) its prime factorization. If
   prime-list is non-nil, it must be a sorted list containing at least all
   primes less than or equal to i"
  (loop for p in prime-list while (not (= n 1)) do
    (loop while (zerop (mod n p)) do
      (setf (cdr tail) (cons p nil))
      (setf tail (cdr tail))
      (setq n (/ n p))
          )
        )
  (loop while (not (= n 1)) do
    (setq p (funcall pgen))
    (loop while (zerop (mod n p)) do
      (setf (cdr tail) (cons p nil))
      (setf tail (cdr tail))
      (setq n (/ n p))
          )
        )
  (return-from factor (cdr factors))
  )

(defun primes-leq (upbound &optional (known-primes nil)
                   &aux (pgen (primes known-primes))
                     (leqprimes (list 0)) (tail (last leqprimes)) p)

  "Return a list of all prime numbers less than or equal to upbound.
  If prime-list is non-nil, it must be a sorted list containing exactly the
  the prime numbers <=less than the last entry"
  (loop for p in known-primes
        do (when (> p upbound)
             (return-from primes-leq (cdr leqprimes)))
           (setf (cdr tail) (cons p nil))
           (setf tail (cdr tail)))

  (setf (cdr leqprimes)
        (loop do (setq p (funcall pgen))
              while (<= p upbound)
              collect p))
  (cdr leqprimes))

(defun first-n-primes (n &optional prime-list &aux (size 0) (p (primes prime-list))
                                                (llist (cons 0 nil)) (tail llist))
  (loop for u in prime-list
        while (< size n)
        do (setf (cdr tail) (cons u nil))
           (setf tail (cdr tail))
           (incf size)
        )
  (loop while (< size n)
        do (setf (cdr tail) (cons (funcall p) nil))
           (setf tail (cdr tail))
           (incf size)
        )
  (return-from first-n-primes (cdr llist))
  )

;; Fibonacci numbers

;; TODO Add in methods which use binet's formula for O(1) calculation :(

(defun fibonacci-leq (upbound)

  (if (< upbound 0)
      (return-from fibonacci-leq nil))
  (if (= upbound 0)
      (return-from fibonacci-leq (list 0)))
  (if (= upbound 1)
      (return-from fibonacci-leq (list 1 0)))

  (let* ((fib-list (list 1 0)) (tmp 0))
    (loop while (< (first fib-list) upbound) do
      (setq tmp (+ (nth 0 fib-list) (nth 1 fib-list)))
      (if (<= tmp upbound)
          (setq fib-list (cons tmp fib-list))
          (return-from fibonacci-leq (reverse fib-list))))
    )
  )

(defun first-n-fibonacci (n)

  (if (< n 0)
      (return-from first-n-fibonacci nil))
  (if (= n 1)
      (return-from first-n-fibonacci (list 0)))

  (let* ((fib-list (list 1 0)) (tmp 0))
    (loop while (< (length fib-list) n) do
      (setq tmp (+ (nth 0 fib-list) (nth 1 fib-list)))
      (setq fib-list (cons tmp fib-list)))
    (return-from first-n-fibonacci (reverse fib-list))
    )
  )

;; Modular arithmetic

(defun digit (num exp &optional (base 10))
  "Returns the coefficient of base^exp in base-base representation of num"
  (mod (floor (/ num (expt base exp))) base))

(defun numdigits (n &optional (base 10))
  "Returns the number of digits in the base-representation of n"
  (floor (+ 1 (log n base))))

(defun ispalindrome (n &optional (base 10) &aux (len (numdigits n base)))
  "Returns true if n is palindromic in base, false elsewhere"
  (loop for x from 0 to (ceiling (/ len 2)) do
    (if (not (= (digit n x base) (digit n (- len 1 x) base)))
        (return-from ispalindrome nil)))
  t)

(defun palindromes-length (length &optional (base 10) &aux palindrome-list)
  "Return all palindromes of length length in base"
  (when (= length 1)
    (return-from palindromes-length (loop for d from 1 to (- base 1) collect d)))

  (when (= length 2)
    (return-from palindromes-length (loop for d from 1 to (- base 1) collect (+ d (* base d)))))

  (loop for i from 1 to (- base 1) do
    (loop for p in (concatenate 'list (palindromes-length (- length 2) base) '(0)) do
      (push (+ (* base p) i (* i (expt base (- length 1)))) palindrome-list)))

  palindrome-list
  )
