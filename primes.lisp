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

(defun primes (&optional (plist nil) &aux (p plist) (n 2) f)
  "Lazily generate primes"
  ;; https://stackoverflow.com/questions/46496083/lazily-generating-prime-in-common-lisp
  (labels ((fbare ()            ; a function for the first iteration
             (setf f #'fgen)   ; setting f to the next function
             (push 2 p)
             (incf n)
             2)
           (fgen ()            ; a function for all other iterations
             (loop while (divisor-in-list n p)
                   do (incf n 2))
             (push n p)
             n))
    (if plist (setq n (car (last plist))))
    (if (= n 2)
        (setf f #'fbare)     ; setting f to the first function
        (setf f #'fgen)
        )
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

(defun primes-leq (upbound &optional (prime-list nil)
                   &aux (p (primes prime-list)) (llist (cons 0 nil)) (tail llist) n)
  "Return a list of all prime numbers less than or equal to upbound.
  If prime-list is non-nil, it must be a sorted list containing exactly the
  the prime numbers <=less than the last entry"
  (loop for x in prime-list
        do (if (<= x upbound)
            (progn (setf (cdr tail) (cons x nil))
                   (setf tail (cdr tail)))
            (return-from primes-leq (cdr llist))
            )
        )
  (setq n (funcall p))
  (loop while (<= n upbound)
        do (progn (setf (cdr tail) (cons n nil))
                  (setf tail (cdr tail))
                  (setq n (funcall p))
                  )
        )
  (return-from primes-leq (cdr llist))
  )

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
