(defun solve14 (&aux (chainmap (make-hash-table :size 1000000))
                  (longest-length 0) seed link)

  (defun chainsize (n)
    (setf link (gethash n chainmap))
    (if (= n 1)
        (return-from chainsize 0))
    (if link
        (return-from chainsize link)
        (if (zerop (mod n 2))
            (setf (gethash n chainmap) (+ 1 (chainsize (/ n 2))))
            (setf (gethash n chainmap) (+ 1 (chainsize (+ 1 (* n 3))))))))

  (loop for i from 1 to 999999 do
    (let ((length 0))
      (unless (gethash i chainmap)
        (setf length (chainsize i))
        (when (> length longest-length)
          (setf longest-length length)
          (setf seed i)))))
  seed
  )
