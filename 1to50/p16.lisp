; https://stackoverflow.com/questions/25800283/load-file-with-a-relative-path
(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(defun solve16 (&aux (num (expt 2 1000)))
  (loop for i from 0 to (numdigits num)
        sum (digit num i))
  )
