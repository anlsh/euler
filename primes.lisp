(provide 'primes)

(defun divisor-in-list (i testlist)
  "Evaluates every element in testlist to see if any divide i"
  (dolist (n testlist)
    (if (= 0 (mod i n))
        (return-from divisor-in-list t)
        )
    )
  (return-from divisor-in-list nil)
  )

(defun primes (&aux (p (list 2)) (n 2) f)
  "Lazily generate primes"
  ;; https://stackoverflow.com/questions/46496083/lazily-generating-prime-in-common-lisp
  (labels ((f2 ()            ; a function for the first iteration
             (incf n)
             (setf f #'fn)   ; setting f to the next function
             2)
           (fn ()            ; a function for all other iterations
             (loop while (divisor-in-list n p)
                   do (incf n 2))
             (push n p)
             n))
    (setf f #'f2)            ; setting f to the first function
    (lambda ()               ; returning a closure
      (funcall f))))         ;   which calls the current f

(defun divisor-in-slist (i testlist)
  "Given a forward-sorted list, see if any elements divide i"
  ;; TODO Use binary search to take advantage of sortedness
  (let* ((upbound (sqrt i)))
    (dolist (n testlist)
      (when (> n upbound) (return))
      (if (= 0 (mod i n))
          (return-from divisor-in-slist t)
          )
      ))
  (when (member i testlist) (return-from divisor-in-slist t))
  (return-from divisor-in-slist nil)
  )

(defun factor (n &optional (prime-list nil))
  "Given a natural number, return (as a list) its prime factorization. If
   prime-list is non-nil, it must be a sorted list containing at least all
   primes less than or equal to i"
  (if (< n 2)
      (return-from factor nil))
  (if (= n 2)
      (return-from factor (list 2)))
  (if (= n 3)
      (return-from factor (list 3)))

  ;; TODO This is mostly-duplicated from primes-leq, maybe refactor primes-leq
  ;; with generators or something to stop that
  (let* ((i 6) (factor-list nil) (biggest (car (last prime-list))))
    (dolist (q '(2 3))
      (loop while (zerop (mod n q)) do
        (nconc factor-list (list q))
        (setq n (/ n q))
            )
      )
    (setq i
          (if (or (not prime-list) (member biggest '(2 3)))
              6
              (if (= (mod biggest 6) 1) (+ biggest 5) (+ biggest 1))
              )
          )
    (unless prime-list (setq prime-list (list 2 3)))
    (loop while (/= n 1) do
      (unless (divisor-in-slist (- i 1) prime-list)
        (nconc prime-list (list (- i 1)))
        (loop while (zerop (mod n (- i 1))) do
          (nconc factor-list (list (- i 1)))
          (setq n (/ n (- i 1)))
          )
        )
      (unless (divisor-in-slist (+ i 1) prime-list)
        (nconc prime-list (list (+ i 1)))
        (loop while (zerop (mod n (+ i 1))) do
          (nconc factor-list (list (+ i 1)))
          (setq n (/ n (+ i 1)))
          )
        )
      (incf i 6))
    (return-from factor factor-list)
    )
  )

(defun primes-leq (upbound &optional (prime-list nil))
  "Return a list of all prime numbers less than or equal to upbound.
  If prime-list is non-nil, it must be a sorted list containing exactly the
  the prime numbers <=less than the last entry"
  (let ((p (primes)) (n 2))
    (funcall p)
    (loop while (<= n upbound)
          collect n
          do (setq n (funcall p))
          )
    )
  )

(defun first-n-primes (n)
  (let ((p (primes)))
    (loop for x from 1 to n
          collect (funcall p))
    )
  )
