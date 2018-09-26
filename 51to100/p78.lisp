(let ((bell-table (make-hash-table)))
  (setf (gethash 0 bell-table) 1)
  (setf (gethash 1 bell-table) 1)
  (defun nth-bell (n)
    (or (gethash n bell-table)
        (setf (gethash n bell-table)
              (loop for k from 0 to (- n 1)
                    summing (* (binomial (- n 1) k) (nth-bell k)))))))

(defun factorial (n)
  (if (or (= n 0) (= n 1))
      (return-from factorial 1)
      (return-from factorial (* n (factorial (- n 1))))))

(defun binomial (n k)
  (return-from binomial (/ (factorial n)
                           (* (factorial k) (factorial (- n k))))))

;; The formula for the below function is given by
;; http://www.math.clemson.edu/~kevja/PAPERS/ComputingPartitions-MathComp.pdf
(let ((partition-table (make-hash-table)))
  (setf (gethash 0 partition-table) 1)
  (setf (gethash 1 partition-table) 1)
  (setf (gethash 2 partition-table) 2)
  (defun nth-partition-number (n)
    (when (< n 0) (return-from nth-partition-number 0))
    (or (gethash n partition-table)
        (setf (gethash n partition-table)
              (loop for k from 1 upto n
                    summing (* (expt -1 (+ k 1))
                               (+ (nth-partition-number (- n (* k (/ (+ (* 3 k) 1) 2))))
                                  (nth-partition-number (- n (* k (/ (- (* 3 k) 1) 2)))))))))))

;; The answer is 55374
;; However the code (loop for g from 1 do (when (zerop (mod (nth-partition-number g) 1000000)) (return g)))
;; is totally inadequate to handle the problem, taking about 20 minutes to solve
