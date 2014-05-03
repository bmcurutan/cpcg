(defun palindrome (a)
    (let ((b (reverse a)))
      (string= a b)))
      
      (print (palindrome "library"))
    (print (palindrome "racecar"))