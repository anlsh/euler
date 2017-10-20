(defun solve45 ()
  "Turns out that all hexagonal numbers are trianglar, so we dont check for that"
  (loop for n from 144 with h do
    (setf h (* n (- (* 2 n) 1)))
    (when (is-pentagonal h)
      (return-from solve45 h))))

(defun is-integer (n)
  (zerop (mod n 1)))

(defun is-triangular (n &aux (root (nth 1 (solve-quadratic 1/2 1/2 (* -1 n)))))
  (and (is-integer root) (= (* 1/2 root (+ root 1)) n)))

(defun is-pentagonal (n &aux (root (nth 1 (solve-quadratic 3/2 -1/2 (* -1 n)))))
  (and (is-integer root) (= (* 1/2 root (- (* 3 root) 1)) n)))

(defun solve-quadratic (a b c &aux determinant)
  "Precision isn't really a thing, just be warned..."
  (setf determinant (sqrt (- (* b b) (* 4 a c))))
  (list (/ (- (* -1 b) determinant) (* 2 a))
        (/ (+ (* -1 b) determinant) (* 2 a))))
