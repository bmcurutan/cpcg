(defun gcdiv (a b)
  (gcdRec a a b))

(defun gcdRec (g a b)
  (if (/= b 0)
    (gcdRec b b (mod a b))
    a))
      
(defun lcmul (a b)
    (let ((y (* a b))
       )
    (let ((z (gcdiv a b))
         )
      (/ y z))))
    
(print (gcdiv 48 32))
(print (lcmul 3 5))
