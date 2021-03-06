
(in-package :icfp-2015-cl)

(defvar *board*
  nil
  "This holds the state of the grid")

(defclass unit ()
  ((members :initarg :members :initform nil :accessor members)
   (symm :initarg :symm :initform nil :accessor symm)))

(defun member-coords (rot unit)
  (sort (iter (for mem :in (members unit))
          (destructuring-bind (mem-x mem-y)
              (apply-rotation mem rot '(0 0))
            (collect (list mem-x mem-y))))
        (lambda (x y)
          (if (/= (first x) (first y))
              (< (first x) (first y))
              (< (second x) (second y))))))

(defun detect-symmetry (unit)
  (let ((initial-coords (member-coords 0 unit)))
    (or (iter (for i :from 1 :to 3)
          (finding i :such-that
                   (equalp initial-coords
                           (member-coords i unit))))
        6)))

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

(defvar *board-id* nil
  "The id of the board currently being played.")

(defvar *seeds* nil
  "All seeds associated with the current board.")

(defvar *json-data* nil
  "Pared JSON game data.")

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

(defun init-game (file-or-data)
  (let ((data (if (or (pathnamep file-or-data)
                      (stringp file-or-data))
                  (with-open-file (in file-or-data)
                    (cl-json:decode-json in))
                  file-or-data)))
    (setf *json-data* data)
    (setf *board* (init-board data))
    (setf *unit-array*
          (iter (for unit :in (cdr (assoc :units data)))
            (collecting
             (let* ((pivot
                      (vshift- (list (cdr (assoc :x (cdr (assoc :pivot unit))))
                                     (cdr (assoc :y (cdr (assoc :pivot unit)))))))
                    (unit-obj
                      (make-instance
                       'unit
                       :members
                       (iter (for mem :in (cdr (assoc :members unit)))
                         (let ((y (cdr (assoc :y mem))))
                           (collect (v-
                                     (list (- (cdr (assoc :x mem)) (shift y)) y)
                                     pivot)))))))
               (setf (symm unit-obj)
                     (detect-symmetry unit-obj))
               unit-obj)
             :result-type 'vector)))
    (setf *rng* (lcg (cadr (assoc :source-seeds data))))
    (setf *source-length* (cdr (assoc :source-length data)))
    (setf *board-id* (cdr (assoc :id data)))
    (setf *seeds* (cdr (assoc :source-seeds data)))

    ;; Now, place the first unit
    (setf *unit* (aref *unit-array* (mod (funcall *rng*)
                                         (length *unit-array*))))
    ;; Reset the command lists
    (setf *unit-command-list* nil
          *command-list* nil)))

(defun blit-unit (&key (board *board*) (unit *unit*) (val 1))
  (iter
    (for (x y) :in (if (listp unit) unit (members unit)))
    (setf (bref board x y) val)))

(defun shift-board (y &key (board *board*))
  (iter
    (for depth :from y :downto 0)
    (iter
      (for x :below (board-dimension board 0))
      (setf (bref board x (1+ depth)) (bref board x depth))))
  (iter
    (for x :below (board-dimension board 0))
    (setf (bref board x 0) 0)))

(defun clear-filled-rows (&key (board *board*))
  (iter
    (for y :from (1- (board-dimension board 1)) :downto 0)
    (when
        (iter
          (for x :below (board-dimension board 0))
          (when (= 0 (bref board x y))
            (return))
          (finally (return t)))
      (shift-board (1- y) :board board)
      (clear-filled-rows :board board)
      (return))))

(defclass listener () ())

(defmethod unit-locked ((this listener) unit)
  (log:info "unit-locked (shouldn't get here)"))

(defmethod reset-commands ((this listener)))

(defun play-game (&optional listener)
  (setf *board* (init-board *json-data*))
  (iter
    (repeat *source-length*)
    (for init-path := (initial-path *unit* *board*))
    (for path := (sample-paths init-path *unit* *board* *moves*))
    (destructuring-bind (pos rot)
        (get-pos path *unit* *board*)
      (destructuring-bind (pivot filled)
          (translate-coords* *board* pos rot *unit*)
        (log:info "filled: ~s" filled)
        (blit-unit :board *board* :unit filled)
        (clear-filled-rows :board *board*)
        (when listener (unit-locked listener filled))))
    (setf *unit*
          (aref *unit-array*
                (mod (funcall *rng*) (length *unit-array*))))
    (log:info "path: ~s" path)
    (collect (nreverse (mapcar 'car path)))))

(defun unit-dimensions (members)
  (iter
    (for (x y) :in members)
    (minimizing x :into min-x)
    (minimizing y :into min-y)
    (maximizing x :into max-x)
    (maximizing y :into max-y)
    (finally
     (return (list (1+ (- max-x min-x))
                   (1+ (- max-y min-y)))))))

(defun possible-sizes (unit)
  (let ((rotations
         (iter
           (for rot :below 4)
           (collect
               (iter
                 (for mem :in (members unit))
                 (destructuring-bind (mem-x mem-y)
                     (apply-rotation mem rot '(0 0))
                   (let ((mem-x (+ mem-x (shift mem-y))))
                     (collect (list mem-x mem-y)))))))))
    (iter
      (for rot :in rotations)
      (collect (unit-dimensions rot)))))

(defun rotate-into-optimal (board unit)
  (let* ((sizes (possible-sizes unit))
         (sizes-sorted (sort (copy-list sizes) '< :key 'first))
         (ranks (board-rank-depths board)))
    (log:debug "ranks: ~s, sizes: ~s" ranks sizes-sorted)
    (iter
      (for rank :in ranks)
      (for depth :from 0)
      (iter
        (for (w h) :in sizes-sorted)
        (for new-sizes :on sizes-sorted)
        (when (<= w rank)
          (setf sizes-sorted new-sizes)
          (return)))
      (finally
       (return (list
                (position (first sizes-sorted) sizes)
                (first sizes-sorted)
                depth))))))

(defun optimal-trajectory (board unit)
  (destructuring-bind (rotations (uw uh) depth)
      (rotate-into-optimal board unit)
    (let* ((offset-w (floor uw 2))
           (offset-h (- depth (floor uh 2)))
           (goal (iter
                   (for pos :from offset-w :below (board-dimension board 0))
                   (handler-case
                       (progn
                         (translate-coords board (list pos offset-h) rotations unit)
                         (return pos))
                     (error (er)
                       (declare (ignore er))
                       (log:debug "couldn't fit: ~d" pos))))))
      (log:debug "rotations: ~s, goal: ~s, ow: ~s, oh: ~s"
                 rotations goal offset-w offset-h)
      (append
       (make-list rotations :initial-element :cw)
       (when goal
         (let ((left (floor (- offset-h goal) 2))
               (right (ceiling (- offset-h goal) 2)))
           (append
            (make-list goal :initial-element :se)
            (if (< left 0)
                (make-list (abs left) :initial-element :e)
                (make-list left :initial-element :se))
            (if (< right 0)
                (make-list (abs right) :initial-element :w)
                (make-list right :initial-element :sw)))))))))

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
           (east-shift (floor (- (first dim) (1+ unit-width)) 2))
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
