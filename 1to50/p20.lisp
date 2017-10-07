(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(defun solve20 (&aux (num (factorial 100)))
  (loop for i from 0 to (numdigits num)
        sum (digit num i))
  )
