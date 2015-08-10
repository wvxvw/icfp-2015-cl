(in-package :icfp-2015-cl)

;; (defun issue-command (command)
;;   (push command *unit-command-list*)
;;   (setf envisage:*objects-to-draw*
;;         (list
;;          (make-instance 'game-state
;;                         :board *board*
;;                         :unit *unit*
;;                         :command-list *command-list*
;;                         :unit-command-list *unit-command-list*))))

(defparameter *hex-keymap*
  '((#\a :w)
    (#\u :e)
    (#\o :sw)
    (#\e :se)
    (#\, :cw)
    (#\. :ccw)))

(defvar *character-log* ())

;; This handler will process any key and translate it into the proper command
;; and save a log of what was pressed.  It allows you to try things out easily.
;; It is commented because it breaks basic interaction.

;; This needs to be implemented on the server.

(defvar *letter-commands-p* nil
  "Controls whether the commands are based on ergonomics or the command
  letters")

(defmethod glut:keyboard ((window envisage::visu-window) key x y)
  (ignore-errors
   (cond (*letter-commands-p*
          (let ((member?
                  (member key *command-mapping*
                          :test (lambda (x y)
                                  (member (char-upcase x)
                                          (rest y))))))
            (when member?
              (push key *character-log*)
              (issue-command (first (first member?))))))
         (t (let ((member? (member key *hex-keymap* :key 'first)))
              (when member?
                (issue-command (second (first member?)))))))
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
    (destructuring-bind (pivot members)
        (position-unit *board* *unit* *unit-command-list*)
      (gl:with-pushed-matrix* (:modelview)
        (gl:translate 0 0 -10)
        (gl:color .3 .4 1)
        (apply 'fill-cell pivot))

      (gl:with-pushed-matrix* (:modelview)
        (gl:translate 0 0 -5)
        (iter (for member :in members)
          (destructuring-bind (x y) member
            (gl:color .3 1 .4)
            (fill-cell x y)))))))
