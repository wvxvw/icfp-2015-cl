(in-package :icfp-2015-cl)

(defparameter *hex-width* 100)
(defparameter *hex-height* (ceiling (/ *hex-width* (/ (sqrt 3) 2))))

(defun draw-board (name width height)
  (svg:with-svg-to-file
      (scene 'svg:svg-1.1-toplevel
             :width width (+ (/ *hex-width* 2) (* *hex-width* width))
             :height (* *hex-width* height))
      ((concatenate 'string name ".svg") :if-exists :supersede)
    (mapcar #'(lambda (col row) (draw-hex scene col row))
            (alexandria:iota width)
            (alexandria:iota height))))

(defun draw-hex (scene column row)
  (svg:draw scene (:polyline :points '(0 0 100 100 100 200))))
