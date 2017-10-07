(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(defun solve25 (&aux (n1 1) (n2 1) tmp (index 2))
  (loop while (< (numdigits n2) 1000) do
    (setf tmp (+ n1 n2))
    (setf n1 n2)
    (setf n2 tmp)
    (incf index))
  index
  )
