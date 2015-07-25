(in-package :icfp-2015-cl)

(defun entry-point ()
  (format! *standard-output*
           "Hello, world!~%arguments: ~S~%" *command-line-arguments*)
  (quit))
