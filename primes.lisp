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
