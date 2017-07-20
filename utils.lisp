(provide utils)

(defun list-has-divisor (testlist i)
  (dolist (n testlist)
    (if (eq 0 (% i n))
        (return nil))
    )
  (return t)
  )

(defun inclusive-primes-under (upbound)
  (if (< upbound 2)
      (return nil))

  (let* ((prime-list (list 2)) (i 3))
    (loop while (<= i upbound) do
      (if (not (list-has-divisor prime-list i))
          (append prime-list i))
      (setq i (+ i 2)))
    (return prime-list)
    )
  )
