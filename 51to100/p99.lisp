(defun solve99 (&aux (maxline 0) (maxnum 0))
  (with-open-file (pairfile "~/Code/euler/51to100/p99_base_exp.txt")
    (loop with tmp for i from 1
          for line = (read-line pairfile nil) while line do
      (setf tmp (cl-utilities:split-sequence #\Comma line))
      (setf tmp (mapcar 'parse-integer tmp))
      (let ((num (* (nth 1 tmp) (log (nth 0 tmp)))))
        (when (> num maxnum)
          (setf maxnum num)
          (setf maxline i)))))
  maxline)
