(in-package :icfp-2015-cl)

(defparameter *server* nil)

(defun game-to-svg (&key (board *board*) (unit *unit*))
  (if (and board unit)
      (let* ((width (board-dimension board 0))
             (height (board-dimension board 1))
             (tunit (if (listp unit)
                        unit
                        (translate-unit unit :format 'list)))
             (svg (svg:make-svg-toplevel 'svg:svg-1.1-toplevel
                                         :width (+ (/ *hex-width* 2) (* *hex-width* width))
                                         :height (* *hex-height* height))))
        (log:info "tunit: ~s" tunit)
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
                      :fill (cond
                              ((some (alexandria:curry #'equal (list col row))
                                     tunit)
                               "red")
                              ((= 1 (bref board col row))
                               "yellow")
                              (t "none")))))
        (log:info "svg produced")
        svg)
      (error "No active game")))

(defun output-svg (svg &optional (stream *standard-output*))
  (svg:stream-out stream svg))

(tbnl:define-easy-handler (move :uri "/move.svg"
                                :default-request-type :post)
    (direction)
  (issue-command (intern (string-upcase direction) (find-package "KEYWORD")))
  (with-output-to-string (stream)
    (destructuring-bind (pivot members)
        (position-unit *board* *unit* *unit-command-list*)
      (output-svg (game-to-svg :unit members)
                  stream))))

(tbnl:define-easy-handler (play :uri "/play.svg"
                                :default-request-type :post)
    (game)
  (init-game (asdf:system-relative-pathname
              (asdf:find-system :icfp-2015-cl)
              (format nil "./problems/~a" game)))
  (with-output-to-string (stream)
    (destructuring-bind (pivot members)
        (position-unit *board* *unit* *unit-command-list*)
      (output-svg (game-to-svg :unit members)
                  stream))))

(defun start-server (&key (port 8888))
  (setf *server*
        (tbnl:start (make-instance
                     'tbnl:easy-acceptor
                     :port port
                     :access-log-destination
                     (asdf:system-relative-pathname
                      (asdf:find-system :icfp-2015-cl) #p"./web/access.log")
                     :document-root (asdf:system-relative-pathname
                                     (asdf:find-system :icfp-2015-cl) #p"./web/")))))

(defun stop-server ()
  (ignore-errors (tbnl:stop *server*)))
