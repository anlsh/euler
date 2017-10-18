(defun solve94 (&aux (max-perimeter 1000000000) (perimeter-sum 0))
  (loop for ss from 2 to (/ max-perimeter 3) do
    (loop for su in (list (- ss 1) (+ ss 1)) do
      (when (is-perfect-square (triangle-area-squared (list ss ss su)))
        (format t "~a~%" (list ss ss su))
        (incf perimeter-sum (+ ss ss su)))))
  perimeter-sum)

(defun triangle-area-squared (side-list &aux sp)
  "Calculate the area of triangle using Heron's Formula"
  (setq sp (/ (apply '+ side-list) 2))
  (loop for side in side-list with product = 1 do
    (setf product (* product (- sp side)))
        finally (return-from triangle-area-squared (* product sp))))

(defun is-perfect-square (n)
  (return-from is-perfect-square (= n (expt (floor (sqrt n)) 2))))

(defun triangle-area (side-list)
  (sqrt (triangle-area-squared side-list)))
