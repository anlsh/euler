(load (merge-pathnames "../numbers.lisp" *load-truename*))
(require 'numbers)

(defun solve3 (&aux (maxpalindrome 0))
  (loop for x from 999 downto 100
        until (< (* x x) maxpalindrome) do
          (loop for y from x downto 100 do
            (let* ((test (* x y)))
              (if (and (ispalindrome test) (> test maxpalindrome))
                  (setq maxpalindrome test)))))
  maxpalindrome)
