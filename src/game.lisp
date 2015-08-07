
(in-package :icfp-2015-cl)

(defparameter *hex-keymap*
  '((west #\d)
    (east #\t)
    (south-west #\b)
    (south-east #\m)
    (rot-clockwise #\x)
    (rot-counterclockwise #\w)))

(defmethod glut:keyboard ((window glut:window) key x y)
  (let ((member? (member key (reduce 'append *hex-keymap* :key 'rest))))
    (when member?
      (issue-command (first member?)))))

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

(defun init-game (file-or-data)
  (let ((data (if (or (pathnamep file-or-data)
                      (stringp file-or-data))
                  (with-open-file (in file-or-data)
                    (cl-json:decode-json in))
                  file-or-data)))
    (setf *board* (make-array (list (cdr (assoc :height data))
                                    (cdr (assoc :width data)))
                              :initial-element 0))
    (iter (for cell :in (cdr (assoc :filled data)))
      (setf (aref *board* (cdr (assoc :y cell)) (cdr (assoc :x cell)))
            1))
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
  (print command))

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
      ;; This isn't the correct placement...
      (let ((init-pos (list y-min (/ (+ x-max x-min) 2))))
        (destructuring-bind (y x) init-pos
          (gl:with-pushed-matrix* (:modelview)
            (when (oddp y)
              (gl:translate 5 0 0))
            (gl:translate (* x 10) (* y -8.6) -10)
            (gl:color .3 1 .4)
            (glut:solid-sphere 1 5 5)))))))
