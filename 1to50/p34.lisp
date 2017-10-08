(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

; 10,000,000 is an upper bound here. The reason is that 8*(9!)
; is around two million, but any 8-digit number is at least ten million
; Adding more digits grows the number faster that sum of digit factorials
; A better upper bound is possible actually

;; TODO: This is super slow, and there are actually only two numbers:
;; 145 and 40585. Perhaps move the upper bound down a bit?

(defun solve34 (&aux (factorial-map (make-hash-table))
                  (max 10000000) (sum 0) tmp)
  ; Calculate factorials for the digits so we dont call factorial a trillion times
  (loop for i from 0 to 9 do
    (setf (gethash i factorial-map) (factorial i)))

  (loop for n from 3 to max do
    (setf tmp (loop for d from 0 to (- (numdigits n) 1) sum (gethash (digit n d) factorial-map)))
    (when (= tmp n)
      (incf sum n)))
  sum
  )
