(defun solve29 (&aux resultset (upbound 100))
  (loop for a from 2 to upbound do
    (loop for b from a to upbound do
      (setf resultset (adjoin (expt a b) resultset))
      (setf resultset (adjoin (expt b a) resultset))))
  (list-length resultset)
  )
