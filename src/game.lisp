
(in-package :icfp-2015-cl)

(defparameter *hex-keymap*
  '((#\d :w)
    (#\t :e)
    (#\b :sw)
    (#\m :se)
    (#\x :cw)
    (#\w :ccw)))

(defmethod glut:keyboard ((window glut:window) key x y)
  (ignore-errors
   (let ((member? (member key *hex-keymap* :key 'first)))
     (when member?
       (issue-command (second (first member?)))))))

(defvar *board*
  nil
  "This holds the state of the grid")

(defclass unit ()
  ((members :initarg :members :initform nil :accessor members)))

(defvar *unit*
  nil
  "This holds the current 'live' unit")

(defvar *unit-array*
  nil
  "Holds the units that may appear in the game")

(defvar *rng*
  nil
  "This holds the RNG for the game")

(defvar *source-length* nil
  "This integer will mark how many units will be generated")

(defvar *command-list* nil
  "This list will hold the history of the commands that got us to where we are
  now.")

(defvar *unit-command-list* nil
  "This list holds the history of how the current unit got to where it is.")

(defun init-board (json-data)
  (let ((board (make-array (list (cdr (assoc :height json-data))
                                 (cdr (assoc :width json-data)))
                           :initial-element 0)))
    (iter (for cell :in (cdr (assoc :filled json-data)))
      (setf (aref board (cdr (assoc :y cell)) (cdr (assoc :x cell)))
            1))
    board))

(defun init-game (file-or-data)
  (let ((data (if (or (pathnamep file-or-data)
                      (stringp file-or-data))
                  (with-open-file (in file-or-data)
                    (cl-json:decode-json in))
                  file-or-data)))
    (setf *board* (init-board data))
    (setf *unit-array*
          (iter (for unit :in (cdr (assoc :units data)))
            (collecting
             (let ((pivot
                     (list (cdr (assoc :x (cdr (assoc :pivot unit))))
                           (cdr (assoc :y (cdr (assoc :pivot unit)))))))
               (make-instance 'unit
                              :members (iter (for mem :in (cdr (assoc :members unit)))
                                         (collect (mapcar
                                                   '-
                                                   (list (cdr (assoc :x mem))
                                                         (cdr (assoc :y mem)))
                                                   pivot)))))
             :result-type 'vector)))
    (when (< 1 (length (cdr (assoc :source-seeds data))))
      (warn "There are seeds that we're ignoring"))
    (setf *rng* (lcg (cadr (assoc :source-seeds data))))
    (setf *source-length* (cdr (assoc :source-length data)))

    ;; Now, place the first unit
    (setf *unit* (aref *unit-array* (funcall *rng*)))
    ;; Reset the command lists
    (setf *unit-command-list* nil
          *command-list* nil)
    (setf envisage:*objects-to-draw*
          (list
           (make-instance 'game-state
                          :board *board*
                          :unit *unit*
                          :command-list *command-list*
                          :unit-command-list *unit-command-list*)))))

(defun issue-command (command)
  (push command *unit-command-list*)
  (setf envisage:*objects-to-draw*
        (list
         (make-instance 'game-state
                        :board *board*
                        :unit *unit*
                        :command-list *command-list*
                        :unit-command-list *unit-command-list*))))

(defclass game-state ()
  ((board :initarg :board :initform nil :accessor board)
   (unit :initarg :unit :initform nil :accessor unit)
   (unit-command-list :initarg :unit-command-list
                      :initform nil
                      :accessor unit-command-list)
   (command-list :initarg :command-list :initform nil :accessor command-list)))

(defmethod envisage:draw-object ((state game-state) w dist)
  (let ((dim (array-dimensions (board state))))
    (iter (for y :below (first dim))
      (gl:with-pushed-matrix* (:modelview)
        (when (oddp y)
          (gl:translate 5 0 0))
        (iter (for x :below (second dim))
          (if (= 1 (aref (board state) y x))
              (gl:color 1 .5 .5)
              (gl:color .25 .25 .25))
          (gl:with-pushed-matrix* (:modelview)
            (gl:translate (* x 10) (* y -8.6) 0)
            (glut:solid-sphere 5 16 3)))))
    ;; Initial position for unit
    (let (y-min x-min x-max)
      (iter (for mem :in (members (unit state)))
        (when (or (not y-min) (> y-min (second mem)))
          (setf y-min (second mem)))
        (when (or (not x-min) (> x-min (first mem)))
          (setf x-min (first mem)))
        (when (or (not x-max) (< x-max (first mem)))
          (setf x-max (first mem))))
      (let* ((rot 0)
             (unit-width (- x-max x-min))
             (east-shift (floor (- (second dim) unit-width) 2))
             (pos (list (- east-shift x-min) (- y-min))))
        (iter (for com :in (reverse (unit-command-list state)))
          (case com
            (:cw (decf rot (/ pi 3)))
            (:ccw (incf rot (/ pi 3)))
            (:e (setf pos (v:+ '(1 0) pos)))
            (:w (setf pos (v:+ '(-1 0) pos)))
            (:se (if (evenp (second pos))
                     (setf pos (v:+ '(0 1) pos))
                     (setf pos (v:+ '(1 1) pos))))
            (:sw (if (evenp (second pos))
                     (setf pos (v:+ '(-1 1) pos))
                     (setf pos (v:+ '(0 1) pos))))))
        (destructuring-bind (x y) pos
          (gl:with-pushed-matrix* (:modelview)
            (when (oddp y)
              (gl:translate 5 0 0))
            (gl:translate (* x 10) (* y -8.6) -10)
            (gl:color .3 1 .4)
            (glut:solid-sphere 1 5 5))
          (iter (for (mem-x mem-y) :in (members (unit state)))
            ;; (print (list mem-x mem-y))
            (let ((mem-xp (if (oddp (+ mem-y y))
                              (+ mem-x 0.5)
                              mem-x))
                  (mem-yp (* mem-y .86)))
              ;; (print (list mem-xp mem-yp))
              (destructuring-bind (mem-xpp mem-ypp)
                  (mv:* (rot-mat rot) (list mem-xp mem-yp))
                (let* ((mem-y (round (/ mem-ypp .86)))
                       (mem-x (round (if (oddp (+ mem-y y))
                                         (- mem-xpp 0.5)
                                         mem-xpp))))
                  (print (list mem-x mem-y))
                  (gl:with-pushed-matrix* (:modelview)
                    (when (oddp (+ mem-y y))
                      (gl:translate 5 0 0))
                    (gl:translate (* (+ x mem-x) 10) (* (+ y mem-y) -8.6) -1)
                    (gl:color .3 .4 1)
                    (glut:solid-sphere 5 16 3)))))))))))

(defun rot-mat (theta)
  (list (list (cos theta) (- (sin theta)))
        (list (+ (sin theta)) (cos theta))))
