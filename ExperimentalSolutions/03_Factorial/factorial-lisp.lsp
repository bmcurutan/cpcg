(defun factorial (i)
  facRec 1 i)
    
(defun facRec(f i)
  (if (> i 0)
    (facRec (* f i) (- i 1))
    f))

(defun printfac (start end)
  (format t "(factorial ~D)=~D~%" start (factorial start))
  (if (< start end)
    (printfac (+ start 1) end)))

(printfac 0 6)

