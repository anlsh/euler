(load (merge-pathnames "../macros.lisp" *load-truename*))
(require 'macros)

(defun solve18 (&aux (numrows 15) rowarray)
  (with-open-file (pyfile "~/Code/euler/1to50/p18_triangle.txt")
    (setf rowarray (make-array numrows))
    (loop for line = (read-line pyfile nil)
          for i from 0 to numrows while line do
            (setf (aref rowarray i) (make-array (+ i 1)
                                                :initial-contents
                                                (mapcar 'parse-integer
                                                        (cl-utilities:split-sequence #\Space line)))))
    )
  (loop for row from (- (length rowarray) 2) downto 0 do
    (loop for i from 0 to row do
      (setf (multi-index rowarray row i) (+ (multi-index rowarray row i)
                                            (max (multi-index rowarray (+ row 1) i)
                                                 (multi-index rowarray (+ row 1) (+ i 1)))))))
  (multi-index rowarray 0 0)
  )
