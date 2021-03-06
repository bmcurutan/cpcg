;list, functional paradigm
(defun quicksort (list &aux (pivot (car list)) )
  (if (cdr list)
      (nconc (quicksort (remove-if-not #'(lambda (x) (< x pivot)) list))
             (remove-if-not #'(lambda (x) (= x pivot)) list)
             (quicksort (remove-if-not #'(lambda (x) (> x pivot)) list)))
      list))

;in-place, non-functional paradigm
(defun quicksort (sequence)
  (labels ((swap (a b) (rotatef (elt sequence a) (elt sequence b)))
           (sub-sort (left right)
             (when (< left right)
               (let ((pivot (elt sequence right))
                     (index left))
                 (loop for i from left below right
                       when (<= (elt sequence i) pivot)
                         do (swap i (prog1 index (incf index))))
                 (swap right index)
                 (sub-sort left (1- index))
                 (sub-sort (1+ index) right)))))
    (sub-sort 0 (1- (length sequence)))
    sequence))