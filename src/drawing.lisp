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
       (issue-command (second (first member?)))))
   (setf envisage:*objects-to-draw*
         (list
          (make-instance 'game-state
                         :board *board*
                         :unit *unit*
                         :command-list *command-list*
                         :unit-command-list *unit-command-list*)))))

(defun display-state (board unit command-list)
  (setf envisage:*objects-to-draw*
        (list
         (make-instance 'game-state
                        :board board
                        :unit unit
                        :command-list command-list
                        :unit-command-list command-list))))

(defun v+ (&rest vecs)
  (apply 'map 'list '+ vecs))

(defun v- (&rest vecs)
  (apply 'map 'list '- vecs))

(defun init-game* (file-or-data)
  (init-game file-or-data))

(defun fill-cell (x y)
  (gl:with-pushed-matrix* (:modelview)
    (when (oddp y)
      (gl:translate 5 0 0))
    (gl:translate (* x 10) (* y -8.66) -1)
    ;; (gl:color .3 .4 1)
    (glut:solid-sphere 5 16 3)))

(defmethod envisage:draw-object ((state game-state) w dist)
  (let ((dim (array-dimensions *board*)))
    (iter (for y :below (first dim))
      (iter (for x :below (second dim))
        (if (= 1 (bref *board* x y))
            (gl:color 1 .5 .5)
            (gl:color .25 .25 .25))
        (fill-cell x y)))
    ;; Initial position for unit
    (gl:with-pushed-matrix* (:modelview)
      (gl:translate 0 0 -10)
      (iter (for member :in (position-unit
                             *board*
                             *unit*
                             *unit-command-list*))
        (destructuring-bind (x y) member
          (gl:color .3 1 .4)
          (fill-cell x y))))))
