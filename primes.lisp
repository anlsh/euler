(provide 'primes)

(defun divisor-in-list (i testlist)
  (dolist (n testlist)
    (if (= 0 (mod i n))
        (return-from divisor-in-list t)
        )
    )
  (return-from divisor-in-list nil)
  )

(defun primes-leq (upbound)

  (if (< upbound 2)
      (return-from primes-leq nil))
  (if (= upbound 2)
      (return-from primes-leq (list 2)))
  (if (= upbound 2)
      (return-from primes-leq (list 2 3)))

  ;; Take advantage of the fact that prime triples dont exist
  ;; TODO Generalize to arbitrary step sizes using FLT for coprimality
  (let* ((prime-list (list 2 3)) (i 6))
    (loop while (<= (- i 1) upbound) do
      (when (not (divisor-in-list (- i 1) prime-list))
        (nconc prime-list (list (- i 1))))
      (when (not (divisor-in-list (+ i 1) prime-list))
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
      (when (not (divisor-in-list (- i 1) prime-list))
        (nconc prime-list (list (- i 1))))
      (when (not (divisor-in-list (+ i 1) prime-list))
        (nconc prime-list (list (+ i 1))))
      (setq i (+ i 6)))
    (when (= (length prime-list) (+ n 1))
        (return-from first-n-primes (butlast prime-list)))
    (return-from first-n-primes prime-list)
    )
  )
