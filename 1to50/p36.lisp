(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(defun flatten-level (tree)
  ; https://stackoverflow.com/questions/13171928/flatten-list-in-lisp
  "Flatten a single level of a list"
  (loop for e in tree
        nconc
        (if (consp e)
            (copy-list e)
            (list e))))

(defun solve36 (&aux (sum 0))

  (loop for p in (flatten (loop for l from 1 to 6 collect (palindromes-length l))) do
     (when (ispalindrome p 2)
       (incf sum p)
       (format t "~d is palindromic in base 2 and 10~%" p)))
  sum
  )
