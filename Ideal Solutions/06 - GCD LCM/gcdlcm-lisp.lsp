(defun gcdiv (a b)
  (gcd a b))
      
(defun lcmul (a b)
    (/ (* a b) (gcd a b)))
