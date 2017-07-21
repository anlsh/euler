(provide 'utils)

(defun divisor-in-list (i testlist)
  (dolist (n testlist)
    (if (= 0 (mod i n))
        (return-from divisor-in-list t)
        )
    )
  (return-from divisor-in-list nil)
  )

(defun inclusive-primes-under (upbound)
  (if (< upbound 2)
      (return-from inclusive-primes-under nil))

  (let* ((prime-list (list 2)) (i 3))
    (loop while (<= i upbound) do
      (if (not (divisor-in-list i prime-list))
          (append prime-list i))
      (setq i (+ i 2)))
    (return-from inclusive-primes-under prime-list)
    )
  )
