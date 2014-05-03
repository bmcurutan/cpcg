(defun fibonacci (n)
  (if (< n 2)
    n
    (fibRec n 0 1 2 n)))

(defun fibRec (f n1 n2 i n)
  (if (<= i n)
    (fibRec (+ n1 n2) n2 (+ n1 n2) (+ i 1) n)
    f))

(defun printfib (start end)
  (format t "(fibonacci ~D)=~D~%" start (fibonacci start))
  (if (< start end)
    (printfib (+ start 1) end)))
    
(printfib 0 7)