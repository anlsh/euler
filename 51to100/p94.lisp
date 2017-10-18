(defun solve94 (&aux (max-perimeter 1000000000) (perimeter-sum 0))
  (loop for ss from 2 upto (/ max-perimeter 3) do
    (loop for su in (list (- ss 1) (+ ss 1)) do
      (when (and (zerop (mod (triangle-area (list ss ss su)) 1)) (<= (+ ss ss su) max-perimeter))
        (incf perimeter-sum (+ ss ss su)))))
  perimeter-sum)

(defun triangle-area (side-list &aux sp)
  "Calculate the area of triangle using Heron's Formula"
  (setq sp (/ (apply '+ side-list) 2))
  (loop for side in side-list with product = 1 do
    (setf product (* product (- sp side)))
        finally (return-from triangle-area (sqrt (* product sp)))))
