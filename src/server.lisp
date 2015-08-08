(in-package :icfp-2015-cl)

(defparameter *server* nil)

(defun game-to-svg (&key (board *board*) (unit *unit*))
  (unless *board*
    (init-game (asdf:system-relative-pathname
                (asdf:find-system :icfp-2015-cl) #p"./problems/problem_3.json")))
  (let* ((width (array-dimension board 1))
         (height (array-dimension board 0))
         (svg (svg:make-svg-toplevel 'svg:svg-1.1-toplevel
                                     :width (+ (/ *hex-width* 2) (* *hex-width* width))
                                     :height (* *hex-height* height))))
    (svg:draw svg
              (:rect :x 0 :y 0
                     :width (* *hex-width* (+ 1 width))
                     :height (* *hex-height* (+ 1 height))
                     :style "fill:rgb(255,255,255)"))
    (iter
      (for col :below width)
      (iter
        (for row :below height)
        (draw-hex svg col row
                  :fill (if (= 1 (aref board row col))
                            "yellow"
                            "none"))))
    svg))

(defun output-svg (svg &optional (stream *standard-output*))
  (svg:stream-out stream svg))

(tbnl:define-easy-handler (move :uri "/move.svg"
                                :default-request-type :post)
    (direction)
    (with-output-to-string (stream)
      (output-svg (game-to-svg) stream)))

(defun start-server ()
  (setf *server*
        (tbnl:start (make-instance
                     'hunchentoot:easy-acceptor
                     :port 8888
                     :access-log-destination
                     (asdf:system-relative-pathname
                      (asdf:find-system :icfp-2015-cl) #p"./web/access.log")
                     :document-root (asdf:system-relative-pathname
                                     (asdf:find-system :icfp-2015-cl) #p"./web/")))))

(defun stop-server ()
  (ignore-errors (tbnl:stop *server*)))
