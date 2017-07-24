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

(defun divisor-in-slist (i testlist)
  "Given a forward-sorted list, see if any elements divide i"
  (let* ((upbound (sqrt i)))
    (dolist (n testlist)
      (when (> i upbound) (return))
      (if (= 0 (mod i n))
          (return-from divisor-in-slist t)
          )
      ))
  (return-from divisor-in-slist nil)
  )

(defun factor (n &optional (prime-list nil))
  (let ((factor-list nil)))
  )

(defun primes-leq (upbound &optional (prime-list nil))

  (if (< upbound 2)
      (return-from primes-leq nil))
  (if (= upbound 2)
      (return-from primes-leq (list 2)))
  (if (= upbound 3)
      (return-from primes-leq (list 2 3)))

  ;; Take advantage of the fact that prime triples dont exist
  ;; TODO Generalize to arbitrary step sizes using FLT for coprimality
  (let* ((i 6) (biggest (car (last prime-list))))
    (if prime-list
        (setq i (+ 6 (if (= 1 (mod biggest 6)) -1 1)))
      (setq prime-list (list 2 3)))
    (loop while (<= (- i 1) upbound) do
      (when (not (divisor-in-slist (- i 1) prime-list))
        (nconc prime-list (list (- i 1))))
      (when (not (divisor-in-slist (+ i 1) prime-list))
        (nconc prime-list (list (+ i 1))))
      (setq i (+ i 6)))
    (return-from primes-leq prime-list)
    )
  )

(defun first-n-primes (n)
  (if (< n 1)
      (return-from first-n-primes nil))

  ;; See note in primes-leq
  (let* ((prime-list (list 2 3)) (i 6))
    (loop while (< (length prime-list) n) do
      (when (not (divisor-in-slist (- i 1) prime-list))
        (nconc prime-list (list (- i 1))))
      (when (not (divisor-in-slist (+ i 1) prime-list))
        (nconc prime-list (list (+ i 1))))
      (setq i (+ i 6)))
    (when (= (length prime-list) (+ n 1))
        (return-from first-n-primes (butlast prime-list)))
    (return-from first-n-primes prime-list)
    )
  )
