(provide 'fibonacci)

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
