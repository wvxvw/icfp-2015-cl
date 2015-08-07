(in-package :icfp-2015-cl)

(defun lcg (seed)
  (lambda ()
    (ash (setf seed (mod (+ (* 1103515245 seed) 12345) (expt 2 31))) -16)))
