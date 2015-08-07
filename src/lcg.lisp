(in-package :icfp-2015-cl)

(defun lcg (seed)
  (lambda ()
    (prog1 (ash seed -16)
      (setf seed (mod (+ (* 1103515245 seed) 12345)
                      (expt 2 31))))))
