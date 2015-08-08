(in-package :icfp-2015-cl)

(defparameter *hex-width* 10)
(defparameter *hex-height* (ceiling (/ *hex-width* (/ (sqrt 3) 2))))

(defun draw-board (name width height)
  (svg:with-svg-to-file
      (scene 'svg:svg-1.1-toplevel
             :width (+ (/ *hex-width* 2) (* *hex-width* width))
             :height (* *hex-height* height))
      ((make-pathname :type "svg" :defaults (pathname name)) :if-exists :supersede)
    (mapcar #'(lambda (col)
                (mapcar #'(lambda (row) (draw-hex scene col row))
                        (alexandria:iota height)))
            (alexandria:iota width))))

(defparameter *hex-vertices* (list (cons (/ *hex-width* 2) 0)
                                   (cons *hex-width* (/ *hex-height* 4))
                                   (cons *hex-width* (* 3 (/ *hex-height* 4)))
                                   (cons (/ *hex-width* 2) *hex-height*)
                                   (cons 0 (* 3 (/ *hex-height* 4)))
                                   (cons 0 (/ *hex-height* 4))
                                   (cons (/ *hex-width* 2) 0)))

(defun draw-hex (scene column row)
  (svg:draw scene
            (:polyline :points (coord-list->string
                                (mapcar
                                 #'(lambda (vertice)
                                     (transpose vertice column row))
                                 *hex-vertices*))
            :fill "none" :stroke "black")))

(defun transpose (coord column row)
  (cons
   (+ (if (= 0 (mod row 2))
          0
          (/ *hex-width* 2))
      (* *hex-width* column) (car coord))
   (+ (* *hex-height* row) (cdr coord))))

(defun coord-list->string (coord-list)
  (format nil "~{~a ~}" (mapcar #'coord->string coord-list)))

(defun coord->string (coord)
  (format nil "~d,~d" (car coord) (cdr coord)))
