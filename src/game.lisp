
(in-package :icfp-2015-cl)

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

(defclass game-state ()
  ((board :initarg :board :initform nil :accessor board)
   (unit :initarg :unit :initform nil :accessor unit)
   (unit-command-list :initarg :unit-command-list
                      :initform nil
                      :accessor unit-command-list)
   (command-list :initarg :command-list :initform nil :accessor command-list)))

(defun rot-mat (theta)
  (list (list (cos theta) (- (sin theta)))
        (list (+ (sin theta)) (cos theta))))


(defun issue-command (command)
  (push command *unit-command-list*))

(defun v+ (&rest vecs)
  (apply 'map 'list '+ vecs))

(defun v- (&rest vecs)
  (apply 'map 'list '- vecs))

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
                                         (collect (v-
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
          *command-list* nil)))

(defun position-unit (board unit unit-command-list)
  ;; Initial position for unit
  (let ((dim (array-dimensions board))
        y-min x-min x-max)
    (iter (for mem :in (members unit))
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
      (iter (for com :in (reverse unit-command-list))
        (case com
          (:cw (decf rot 1))
          (:ccw (incf rot 1))
          (:e (setf pos (v+ '(1 0) pos)))
          (:w (setf pos (v+ '(-1 0) pos)))
          (:se (if (evenp (second pos))
                   (setf pos (v+ '(0 1) pos))
                   (setf pos (v+ '(1 1) pos))))
          (:sw (if (evenp (second pos))
                   (setf pos (v+ '(-1 1) pos))
                   (setf pos (v+ '(0 1) pos))))))
      (iter (for mem :in (members unit))
        (destructuring-bind (mem-x mem-y) (v+ pos (apply-rotation mem rot pos))
          (if (or (< mem-x 0) (>= mem-x (second dim))
                  (< mem-y 0) (>= mem-y (first dim))
                  (= 1 (aref board mem-y mem-x)))
              (error "Invalid state, lock at last state: ~A" (rest unit-command-list))
              (collect (list mem-x mem-y))))))))

;; This code applies a rotation.  You can derive this by algebra.  The tricky
;; part here is that shift of the indexes between the style that the spec uses
;; and more traditional triangle lattice indexes.  To covert them, I use the
;; traditional-shift function which applied a shift to the x coordinate.
(defun traditional-shift (pivot-y y)
  (- (floor (+ pivot-y y) 2)
     (floor pivot-y 2)))

(defun apply-rotation (member rot pos)
  (destructuring-bind (mem-x mem-y) member
    (destructuring-bind (x y) pos
      (labels ((%apply-rotation (mem-x mem-y rot x y)
                 (cond ((= rot 0) (list (+ mem-x
                                            (traditional-shift y mem-y))
                                        mem-y))
                       ((> rot 0)
                        (%apply-rotation (+ mem-x mem-y) (- mem-x)
                                         (- rot 1) x y))
                       ((< rot 0)
                        (%apply-rotation (- mem-y) (+ mem-x mem-y)
                                         (+ rot 1) x y)))))
        (%apply-rotation
         (- mem-x (traditional-shift y mem-y))
         mem-y rot x y)))))
