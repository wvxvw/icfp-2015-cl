(in-package :icfp-2015-cl)

(defparameter *server* nil)

(defun game-to-svg (&key (board *board*) (unit *unit*))
  (if (and board unit)
      (let* ((width (array-dimension board 1))
             (height (array-dimension board 0))
             (tunit (if (listp unit)
                        unit
                        (translate-unit unit :format 'list)))
             (svg (svg:make-svg-toplevel 'svg:svg-1.1-toplevel
                                         :width (+ (/ *hex-width* 2) (* *hex-width* width))
                                         :height (* *hex-height* height))))
        (tbnl:log-message* :error "~&tunit: ~s" tunit)
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
                              ((= 1 (aref board row col))
                               "yellow")
                              (t "none")))))
        svg)
      (error "No active game")))

(defun output-svg (svg &optional (stream *standard-output*))
  (svg:stream-out stream svg))

(tbnl:define-easy-handler (move :uri "/move.svg"
                                :default-request-type :post)
    (direction)
  (tbnl:log-message* :error "~&direction: ~s" direction)
  (tbnl:log-message* :error "~&params: ~s" (tbnl:post-parameters*))
  (issue-command (intern (string-upcase direction) (find-package "KEYWORD")))
  (with-output-to-string (stream)
    (output-svg (game-to-svg :unit
                             (position-unit *board* *unit* *unit-command-list*))
                stream)))

(tbnl:define-easy-handler (play :uri "/play.svg"
                                :default-request-type :post)
    (game)
  ;; sorry, I'm too lazy to set up logging.
  (tbnl:log-message* :error "~&game: ~s" game)
  (tbnl:log-message* :error "~&params: ~s" (tbnl:post-parameters*))
  (init-game (asdf:system-relative-pathname
              (asdf:find-system :icfp-2015-cl)
              (format nil "./problems/~a" game)))
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
