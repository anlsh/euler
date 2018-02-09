(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(defun solve80 ()
  (loop for n from 1 to 100
        for rootn = (newton-sqrt n :iterations 5)
        with totalsum = 0 do
          (unless (integerp rootn)
            (incf totalsum (loop for d from 0 to 99 sum (digit rootn (* -1 d)))))
        finally (return-from solve80 totalsum)))

(defun newton-sqrt (n &key (iterations 10))
  (loop for i upto iterations
        ;; with guess = (coerce 1 'long-float)
        with guess = (rationalize (sqrt n))
        for fguess = (- (expt guess 2) n)
        for fprimeguess = (* 2 guess) do
          ;; Linear approximation of the function is a(t) = f(x) + f'(x)(t - x)
          ;; So we set a(t) = 0 and solve for t as new guess
          (setf guess (+ guess (/ fguess -1 fprimeguess)))
        finally (return-from newton-sqrt guess)))

(defun weird-sqrt (n &key (iterations 100))
  (loop for i upto iterations
        with a = (* 5 n)
        with b = 5 do
          (cond ((>= a b) (setf a (- a b)) (incf b 10))
                (t (setf a (* 100 a))
                   (setf b (- (* 10 b) 45)))
                (setf a (* 100 a)))
        finally (return-from weird-sqrt b)))
