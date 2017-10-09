(load (merge-pathnames "../macros.lisp" *load-truename*))
(require 'macros)

;; Basically ripped from my solution of p34. Same speed issues
(defun solve30 (&aux (power-map (make-hash-table))
                  (max 300000) (sum 0) tmp)
  (loop for i from 0 to 9 do
    (sethash i power-map (expt i 5)))

  (loop for n from 2 to max do
    (setf tmp (loop for d from 0 to (- (numdigits n) 1) sum (gethash (digit n d) power-map)))
    (when (= tmp n)
      (incf sum n)))
  sum
  )
