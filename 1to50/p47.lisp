(defun dumb-factor (n &aux (factors nil))
  (loop with i = 2
        with factor-count = 0
        while (not (= n 1)) do
          (loop while (zerop (mod n i)) do
            (incf factor-count)
            (setf n (/ n i)))
          (unless (zerop factor-count)
            (push (list i factor-count) factors))
          (setf factor-count 0)
          (incf i))
  (return-from dumb-factor factors))

(defun factor-jump (n)
  (loop for delta in '(0 1 2 3)
        for nmod = (+ n delta) do
          (when (not (= 4 (length (dumb-factor nmod))))
            (return-from factor-jump (+ delta 1))))
  0)

(defun solve47 (&aux (num 2))
  (loop for jump = (factor-jump num)
        while (not (zerop jump)) do
          (incf num jump))
  num)
