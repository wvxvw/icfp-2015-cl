
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

(defun shift (y) (floor y 2))

(defun vshift- (v)
  (destructuring-bind (x y) v
    (list (- x (shift y)) y)))

(defun vshift+ (v)
  (destructuring-bind (x y) v
    (list (+ x (shift y)) y)))

(defun board-dimensions (board)
  (reverse (array-dimensions board)))

(defun board-dimension (board i)
  (array-dimension board (mod (+ i 1) 2)))

(defun bref (board x y)
  (aref board y
        (mod (+ x (shift y)) (array-dimension board 1))))

(defun set-bref (board x y val)
  (setf
   (aref board y
         (mod (+ x (shift y)) (array-dimension board 1)))
   val))

(defsetf bref set-bref)

(defun init-board (json-data)
  (let ((board (make-array (list (cdr (assoc :height json-data))
                                 (cdr (assoc :width json-data)))
                           :initial-element 0)))
    (iter (for cell :in (cdr (assoc :filled json-data)))
      (setf (bref board (cdr (assoc :x cell)) (cdr (assoc :y cell))) 1))
    board))

(defclass game-state ()
  ((board :initarg :board :initform nil :accessor board)
   (unit :initarg :unit :initform nil :accessor unit)
   (unit-command-list :initarg :unit-command-list
                      :initform nil
                      :accessor unit-command-list)
   (command-list :initarg :command-list :initform nil :accessor command-list)))

(defun issue-command (command)
  (push command *unit-command-list*))

(defun v+ (&rest vecs)
  (apply 'map 'list '+ vecs))

(defun v- (&rest vecs)
  (apply 'map 'list '- vecs))


(defun init-game (file-or-data &optional (seed 0))
  (let* ((data (if (or (pathnamep file-or-data)
                      (stringp file-or-data))
                  (with-open-file (in file-or-data)
                    (cl-json:decode-json in))
                  file-or-data))
	 (board (init-board data))
	 (unit-array
	       (iter (for unit :in (cdr (assoc :units data)))
		 (collecting
		  (let ((pivot
			  (vshift- (list (cdr (assoc :x (cdr (assoc :pivot unit))))
					 (cdr (assoc :y (cdr (assoc :pivot unit))))))))
		    (make-instance
		     'unit
		     :members
		     (iter (for mem :in (cdr (assoc :members unit)))
		       (let ((y (cdr (assoc :y mem))))
			 (collect (v-
				   (list (- (cdr (assoc :x mem)) (shift y)) y)
				   pivot))))))
		  :result-type 'vector)))
	 (rng (lcg (nth seed (cdr (assoc :source-seeds data)))))
	 (source-length (cdr (assoc :source-length data)))
	 (source-list (loop for i in (funcall rng)
			    for j from 0 to source-length
			    collect (elt unit-array (mod i source-length))))
	 (unit (elt unit-array 0)))
    

    ;; Reset the command lists
    ;; don't know what to do with these yet
    ;; (setf *unit-command-list* nil
    ;;       *command-list* nil)
    (setf unit (initial-position board unit))
    (list board unit (cdr source-list))))

(defun translate-coords (board pivot rot unit)
  (let ((dim (board-dimensions board)))
    (list
     (vshift+ pivot)
     (iter (for mem :in (members unit))
       (destructuring-bind (mem-x mem-y)
           (v+ pivot (apply-rotation mem rot pivot))
         (let ((mem-x (+ mem-x (shift mem-y))))
           (if (or (< mem-x 0) (>= mem-x (first dim))
                   (< mem-y 0) (>= mem-y (second dim))
                   (= 1 (bref board mem-x mem-y)))
               (error "Invalid state")
               (collect (list mem-x mem-y)))))))))

(defun initial-position (board unit)
  ;; Initial position for unit
  (let ((dim (board-dimensions board))
        y-min x-min x-max)
    (iter (for mem :in (members unit))
      (when (or (not y-min) (> y-min (second mem)))
        (setf y-min (second mem))))
    (iter (for mem :in (members unit))
      (let ((shifted (+ (first mem) (shift (- (second mem) y-min)))))
        (when (or (not x-min) (> x-min shifted))
          (setf x-min shifted))
        (when (or (not x-max) (< x-max shifted))
          (setf x-max shifted))))
    (let* ((unit-width (- x-max x-min))
           (east-shift (floor (- (first dim) unit-width) 2))
           (pos (list (- east-shift x-min) (- y-min))))
      pos)))

(defun position-unit (board unit unit-command-list)
  (let ((pos (initial-position board unit))
        (rot 0))
    ;; Process the commands
    (iter (for com :in (reverse unit-command-list))
      (case com
        (:cw (decf rot 1))
        (:ccw (incf rot 1))
        (:e (setf pos (v+ '(1 0) pos)))
        (:w (setf pos (v+ '(-1 0) pos)))
        (:se (setf pos (v+ '(0 1) pos)))
        (:sw (setf pos (v+ '(-1 1) pos)))))
    (translate-coords board pos rot unit)))

;; This code applies a rotation.  You can derive this by algebra.  The tricky
;; part here is that shift of the indexes between the style that the spec uses
;; and more traditional triangle lattice indexes.  To covert them, I use the
;; traditional-shift function which applied a shift to the x coordinate.
(defun apply-rotation (member rot pos)
  (destructuring-bind (mem-x mem-y) member
    (destructuring-bind (x y) pos
      (labels ((%apply-rotation (mem-x mem-y rot x y)
                 (cond ((= rot 0) (list mem-x mem-y))
                       ((> rot 0)
                        (%apply-rotation (+ mem-x mem-y) (- mem-x)
                                         (- rot 1) x y))
                       ((< rot 0)
                        (%apply-rotation (- mem-y) (+ mem-x mem-y)
                                         (+ rot 1) x y)))))
        (%apply-rotation mem-x mem-y rot x y)))))
