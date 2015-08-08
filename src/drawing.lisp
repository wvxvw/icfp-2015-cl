(in-package :icfp-2015-cl)

(defun issue-command (command)
  (push command *unit-command-list*)
  (setf envisage:*objects-to-draw*
        (list
         (make-instance 'game-state
                        :board *board*
                        :unit *unit*
                        :command-list *command-list*
                        :unit-command-list *unit-command-list*))))

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
          *command-list* nil)
    (setf envisage:*objects-to-draw*
          (list
           (make-instance 'game-state
                          :board *board*
                          :unit *unit*
                          :command-list *command-list*
                          :unit-command-list *unit-command-list*)))))

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
        (destructuring-bind (x y) pos
          (gl:with-pushed-matrix* (:modelview)
            (when (oddp y)
              (gl:translate 5 0 0))
            (gl:translate (* x 10) (* y -8.66) -10)
            (gl:color .3 1 .4)
            (glut:solid-sphere 1 5 5))
          (iter (for mem :in (members (unit state)))
            (destructuring-bind (mem-x mem-y) (apply-rotation mem rot pos)
              (gl:with-pushed-matrix* (:modelview)
                (when (oddp (+ mem-y y))
                  (gl:translate 5 0 0))
                (gl:translate (* (+ x mem-x) 10) (* (+ y mem-y) -8.66) -1)
                (gl:color .3 .4 1)
                (glut:solid-sphere 5 16 3)))))))))

(defun apply-rotation (member rot pos)
  (destructuring-bind (mem-x mem-y) member
    (destructuring-bind (x y) pos
      (labels ((%apply-rotation (mem-x mem-y rot x y)
                 (print (list mem-x mem-y rot x y))
                 (cond ((= rot 0) (list (- mem-x
                                           (floor mem-y 2)
                                           (mod (+ mem-y y) 2))
                                        mem-y))
                       ((> rot 0)
                        (%apply-rotation (+ mem-x mem-y) (- mem-x)
                                         (- rot 1) x y))
                       ((< rot 0)
                        (%apply-rotation (- mem-x mem-y) mem-y
                                         (+ rot 1) x y)))))
        (%apply-rotation
         (+ mem-x
            (floor mem-y 2)
            (mod (+ mem-y y) 2))
         mem-y rot x y)))))
