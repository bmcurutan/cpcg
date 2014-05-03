(defun expo (a b)
  (expoRec 1 a b))

(defun expoRec (e a b)
  (if (/= b 0)
    (expoRec (* e a) a (- b 1))
    e))

(defun printexp (start end)
  (format t "(expo ~D)=~D~%" start (expo start end)))

(printexp 2 3)

