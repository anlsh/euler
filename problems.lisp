(load "fibonacci.lisp")
(load "primes.lisp")
(require 'fibonacci)
(require 'primes)


(defun problem2 ()
  (loop for x in (fibonacci-leq 4000000) if (evenp x) sum x)
  )

(defun digit (num exp &optional (base 10))
  "Returns the coefficient of base^exp in base-base representation of num"
  (mod (floor (/ num (expt base exp))) base))

(defun numdigits (n &optional (base 10))
  "Returns the number of digits in the base-representation of n"
  (ceiling (log n base)))

(defun ispalindrome (n &optional (base 10) &aux (len (numdigits n base)))
  "Returns true if n is palindromic in base, false elsewhere"
  (loop for x from 0 to (ceiling (/ len 2)) do
    (if (not (= (digit n x base) (digit n (- len 1 x) base)))
        (return-from ispalindrome nil)))
  t)

(defun problem3 (&aux (maxpalindrome 0))
  (loop for x from 999 downto 100
    until (< (* x x) maxpalindrome) do
    (loop for y from x downto 100 do
      (let* ((test (* x y)))
        (if (and (ispalindrome test) (> test maxpalindrome))
            (setq maxpalindrome test)))))
  maxpalindrome)

(defun problem4 (&aux (n 20) (multiple 1))
  (loop for p in (primes-leq 20) do
    (setq multiple (* multiple (expt p (floor (log n p))))))
  multiple
  )

(defun problem5 ()
  (- (expt (loop for i from 1 to 100 sum i) 2) (loop for i from 1 to 100 sum (* i i))))

(defun problem6 ()
  (nth 10000 (first-n-primes 10001)))

(defun problem8 (&aux (maxproduct 0) (winlen 13)
                   (lnum 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450))

  (loop for expo from 0 to (numdigits lnum) do
    (let* ((product 1))
      (loop for i from 0 to (- winlen 1) do
        (setq product (* product (digit lnum (+ expo i)))))
      (if (> product maxproduct)
          (setq maxproduct product))))
  maxproduct
  )

(defun problem10 ()
  (loop for p in (primes-leq 2000000) sum p))
