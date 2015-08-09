(in-package :icfp-2015-cl)

(defparameter *hex-width* 10)
(defparameter *hex-height* (ceiling (/ *hex-width* (/ (sqrt 3) 2))))

(defun draw-board (name board)
  (let ((width (array-dimension board 1))
        (height (array-dimension board 0)))
    (svg:with-svg-to-file
        (scene 'svg:svg-1.1-toplevel
               :width (+ (/ *hex-width* 2) (* *hex-width* width))
               :height (* *hex-height* height))
        ((make-pathname :type "svg" :defaults (pathname name)) :if-exists :supersede)
      (svg:draw scene
                (:rect :x 0 :y 0
                       :width (* *hex-width* (+ 1 width))
                       :height (* *hex-height* (+ 1 height))
                       :style "fill:rgb(255,255,255)"))
      (iter
        (for col :below width)
        (iter
          (for row :below height)
          (draw-hex scene col row
                    :fill (if (= 1 (aref board row col))
                              "yellow"
                              "none")))))))

(defparameter *hex-vertices* (list (cons (/ *hex-width* 2) 0)
                                   (cons *hex-width* (/ *hex-height* 4))
                                   (cons *hex-width* (* 3 (/ *hex-height* 4)))
                                   (cons (/ *hex-width* 2) *hex-height*)
                                   (cons 0 (* 3 (/ *hex-height* 4)))
                                   (cons 0 (/ *hex-height* 4))
                                   (cons (/ *hex-width* 2) 0)))

(defun draw-hex (scene column row &key (fill "none"))
  (svg:draw scene
            (:polyline :points (coord-list->string
                                (mapcar
                                 #'(lambda (vertice)
                                     (transpose vertice column row))
                                 *hex-vertices*))
            :fill fill :stroke "black")))

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

;; testing

(defun test-draw-board (input &optional (output "test"))
  (init-game input)
  (draw-board output *board*))

(defparameter *svgs* nil)

;; html svg gameplay view
;;; (with-open-file (f #p"test.svg" :direction :output :if-exists :supersede) (output-svg (game-to-svg) f))
(defun svgs-to-html (name svgs)
  (with-open-file (f name :direction :output :if-exists :supersede)
    (format f "<html><head><title>~A</title></head>" name)
    (format f "<body>")
    (log:info "generating ~d svgs:" (length svgs))
    (loop for svg in (reverse svgs)
	  do (output-svg svg f)
	  do (format f "<br />"))
    (format f "</body></html>")))
