(defun solve1 ()
  (let ((i 3) (sum 0))
    (loop while (< i 1000) do
      (incf sum i)
      (incf i 3))
    (setq i 5)
    (loop while (< i 1000) do
      (incf sum i)
      (incf i 5))
    (setq i 15)
    (loop while (< i 1000) do
      (decf sum i)
      (incf i 15))
    (return-from problem1 sum)
    )
  )
