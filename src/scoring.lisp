(in-package :icfp-2015-cl)

(defun board-score (board)
  (let ((filled
         (iter :outer
           (for i :below (array-dimension board 0))
           (iter
             (for j :below (array-dimension board 1))
             (when (aref board j i)
               (in :outer (summing 1)))))))
    filled))
