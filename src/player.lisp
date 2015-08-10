
(in-package :icfp-2015-cl)

(defparameter *command-mapping*
  '((:W #\P #\' #\! #\. #\0 #\3)
    (:E #\B #\C #\E #\F #\Y #\2)
    (:SW #\A #\G #\H #\I #\J #\4)
    (:SE #\L #\M #\N #\O #\5 #\SPACE)
    (:CW #\D #\Q #\R #\V #\Z #\1)
    (:CCW #\K #\S #\T #\U #\W #\X)))



;; Find a valid path through the game, then modify it to be a better path.

(defun initial-path (unit board)
  ;; This produces a random path
  (move-end () unit board))

;; Scores are determined by the size of the unit the number of lines it removes,
;; and the number of each type of power word that is used.  For a given unit,
;; there is no difference in size (and you have no control over what units are
;; given), so we don't have to worry about that.  We only care about how many
;; lines will be removed and what and how many words will be generated.

(defun locked (path)
  (eql :lock (second (first path))))

(defun errored (path)
  (eql :error (second (first path))))

(defun live (path)
  (not (or (locked path) (errored path))))

(defun command-eql (command command-type)
  (let ((type (member command-type *command-mapping* :key 'first)))
    (and (member command (first type))
         (caar type))))

(defun append-commands (more-commands commands board unit)
  (%add-commands (mapcar 'first more-commands) commands board unit))

(defun hanger-grow (path unit board)
  ;; Find possible sites
  (let* ((pots (iter (for (com pos hash) :in (rest path))
                 (for i :from 0)
                 (when (or (command-eql com :sw) (command-eql com :se))
                   (collect i))))
         (i (and pots (alexandria:random-elt pots)))
         (location (and i (nthcdr i path))))
    (cond
      ((not location) path)
      ((and (not (eql :e (caar location)))
            (eql :sw (caadr location)))
       (let ((new (add-commands '(:w :se) (nthcdr 2 location) board unit)))
         (if (live new)
             (append (subseq path 0 (1+ i))
                     new)
             path)))
      ((and (not (eql :w (caar location)))
            (eql :se (caadr location)))
       (let ((new (add-commands '(:e :sw) (nthcdr 2 location) board unit)))
         (if (live new)
             (append (subseq path 0 (1+ i))
                     new)
             path)))
      (t path))))

(defun hanger-shrink (path unit board)
  ;; Find possible sites
  (let* ((pots (iter (for ((com1 . rest1) (com2 . rest2)) :on path)
                 (for i :from 0)
                 (when (or (and (eql :e com1)
                                (eql :sw com2))
                           (and (eql :w com1)
                                (eql :se com2)))
                   (collect i))))
         (i (and pots (alexandria:random-elt pots)))
         (location (and i (nthcdr i path))))
    (cond
      ((not location) path)
      ((and (eql :e (caar location))
            (eql :sw (caadr location)))
       (append (subseq path 0 i)
               (cons-command :se (nthcdr 2 location) board unit)))
      ((and (eql :w (caar location))
            (eql :se (caadr location)))
       (append (subseq path 0 i)
               (cons-command :sw (nthcdr 2 location) board unit)))
      (t path))))

(defun bubble-grow (path unit board)
  ;; Find possible sites
  (let* ((pots (iter (for (com . rest) :in (rest path))
                 (for i :from 0)
                 (when (or (eql :sw com) (eql :se com))
                   (collect i))))
         (i (and pots (alexandria:random-elt pots)))
         (location (and i (nthcdr (+ 1 i) path))))
    (cond
      ((not location) path)
      ((and (not (eql :e (caadr location)))
            (eql :sw (caar location)))
       (let ((new (add-commands '(:se :w) (nthcdr 1 location) board unit)))
         (if (live new) (append (subseq path 0 (1+ i)) new) path)))
      ((and (not (eql :w (caadr location)))
            (eql :se (caar location)))
       (let ((new (add-commands '(:sw :e) (nthcdr 1 location) board unit)))
         (if (live new) (append (subseq path 0 (1+ i)) new) path)))
      (t path))))

(defun bubble-shrink (path unit board)
  (let* ((pots (iter (for ((com1 . rest1) (com2 . rest2)) :on path)
                 (for i :from 0)
                 (when (or (and (eql :sw com1)
                                (eql :e com2))
                           (and (eql :se com1)
                                (eql :w com2)))
                   (collect i))))
         (i (and pots (alexandria:random-elt pots)))
         (location (and i (nthcdr i path))))
    (cond
      ((not location) path)
      ((and (eql :sw (caar location))
            (eql :e (caadr location)))
       (append
        (subseq path 0 i)
        (cons-command :se (nthcdr 2 location) board unit)))
      ((and (eql :se (caar location))
            (eql :w (caadr location)))
       (append
        (subseq path 0 i)
        (cons-command :sw (nthcdr 2 location) board unit)))
      (t path))))

(defun move-end (path unit board)
  "Move undo the final move and replace with random moves until the path locks
again."
  (let ((choice 0))
    (when (> (length path) 0)
      ;; Rejection sampling
      (iter
        (for try := (random 1d0))
        (setf choice (random (1+ (length path))))
        (until (< try (- 1d0 (/ choice (length path)))))))
    (let ((new-path (nthcdr (+ 1 choice) path)))
      (iter (while (not (locked new-path)))
        (let* ((new-command (random-command unit board))
               (potential-path (cons-command new-command new-path board unit)))
          (when (not (errored potential-path))
            (setf new-path potential-path)))
        (finally (return new-path))))))

(defun random-command (unit board)
  (declare (ignore unit board))
  (alexandria:random-elt
   '(:e :w :se :sw :cw :ccw)))

(defun translate-coords* (board pivot rot unit)
  "Compute the translated coordinates but don't do any checking for invalid
states."
  (declare (ignore board))
  (list
   (vshift+ pivot)
   (iter (for mem :in (members unit))
     (destructuring-bind (mem-x mem-y)
         (v+ pivot (apply-rotation mem rot pivot))
       (let ((mem-x (+ mem-x (shift mem-y))))
         (collect (list mem-x mem-y)))))))

(defun get-pos (path unit board)
  "Extract the latest position from a path."
  (cond ((or (null path)
             (and (not (live path))
                  (null (rest path))))
         (initial-position* board unit))
        ((not (live path))
         (second (second path)))
        (t (second (first path)))))

(defun add-commands (commands path board unit)
  (%add-commands (reverse commands) path board unit))

(defun %add-commands (commands path board unit)
  (if commands
      (let ((new (cons-command (first commands) path board unit)))
        (if (live new)
            (%add-commands (rest commands) new board unit)
            new))
      path))

;; you can't have :e after an :w or a :w after an :e.  You can't have a :cw
;; after an :ccw or a :ccw after a :cw.

;; Detect the rotational symmetry and you can't have 3 :cw or :ccw if you have
;; 2-fold, or 2 :cw or :ccw if you have 3-fold, or any :cw or :ccw with 6-fold.

;; It is more complicated than this.  If you have a pivot that is off-center, a
;; rotation can be equivalent to certain translations.  This can only be
;; detected (to my knowledge) by checking placements of the unit on the board.

(defvar *strict-norepeat* nil)

(defmacro with-temp-blit ((board unit pos rot)
                          &body body)
  (alexandria:with-gensyms (board-sym pos-sym rot-sym unit-sym)
    `(let ((,board-sym ,board)
           (,pos-sym ,pos)
           (,rot-sym ,rot)
           (,unit-sym ,unit))
       (unwind-protect
            (progn
              (blit-unit :board ,board-sym
                         :unit (second (translate-coords*
                                        ,board-sym ,pos-sym
                                        ,rot-sym ,unit-sym)))
              ,@body)
         (blit-unit :board ,board-sym
                    :unit (second (translate-coords* ,board-sym ,pos-sym
                                                     ,rot-sym ,unit-sym))
                    :val 0)))))

(defun board-hash (pos rot unit board)
  (if *strict-norepeat*
      (with-temp-blit (board unit pos rot)
        (sxhash (princ-to-string board)))
      nil))

(defun cons-command (command path board unit)
  "Add a command to the path.  Paths are listed in reverse order and each state
in the path is of the form \(C (POS ROT) HASH) where HASH is a hash of the
current board state which is only calculated if *STRICT-NOREPEAT* is non-nil.

If this new command locks the unit, it will be of the form \(C :LOCK NIL).  If
the new command results in an error, the latest state will be \(C :ERROR NIL)."
  (destructuring-bind (pos rot)
      (get-pos path unit board)
    (case command
      ((:W #\P #\' #\! #\. #\0 #\3)
       (setf pos (v+ '(-1 0) pos))
       ;; Check inverse
       (when (member (caar path) '(:E #\B #\C #\E #\F #\Y #\2))
         (return-from cons-command (cons (list command :error nil) path))))
      ((:E #\B #\C #\E #\F #\Y #\2)
       (setf pos (v+ '(1 0) pos))
       ;; Check inverse
       (when (member (caar path) '(:W #\P #\' #\! #\. #\0 #\3))
         (return-from cons-command (cons (list command :error nil) path))))
      ((:SW #\A #\G #\H #\I #\J #\4)
       (setf pos (v+ '(-1 1) pos)))
      ((:SE #\L #\M #\N #\O #\5 #\SPACE)
       (setf pos (v+ '(0 1) pos)))
      ((:CW #\D #\Q #\R #\V #\Z #\1)
       (decf rot 1)
       ;; Check inverse
       (when (or (member (caar path) '(:CCW #\K #\S #\T #\U #\W #\X))
                 (= (iter (for (command position hash) :in path)
                      (repeat (- (symm unit) 1))
                      (counting (member command '(:CW #\D #\Q #\R #\V #\Z #\1))))
                    (- (symm unit) 1)))
         (return-from cons-command (cons (list command :error nil) path))))
      ((:CCW #\K #\S #\T #\U #\W #\X)
       (incf rot 1)
       ;; Check inverse
       (when (or (member (caar path) '(:CW #\D #\Q #\R #\V #\Z #\1))
                 (= (iter (for (command position hash) :in path)
                      (repeat (- (symm unit) 1))
                      (counting (member command '(:CCW #\K #\S #\T #\U #\W #\X))))
                    (- (symm unit) 1)))
         (return-from cons-command (cons (list command :error nil) path)))))
    (let* ((dim (board-dimensions board))
           (members (second (translate-coords* board pos rot unit)))
           (new (if (iter (for (mem-x mem-y) :in members)
                      (thereis (or (< mem-x 0) (>= mem-x (first dim))
                                   (< mem-y 0) (>= mem-y (second dim))
                                   (= 1 (bref board mem-x mem-y)))))
                    (list command :lock nil)
                    (let ((hash (board-hash pos rot unit board)))
                      (if (and *strict-norepeat*
                               (member hash path :key 'third :test 'equal))
                          (return-from cons-command
                             (cons (list command :error nil) path))
                          (list command (list pos rot) hash))))))
      (cons new path))))

(defun initial-position* (board unit)
  (list (initial-position board unit) 0))

(defun filled-rows-with-unit (board unit path)
  (destructuring-bind (pos rot)
      (get-pos path unit board)
    (destructuring-bind (pivot members)
        (translate-coords* board pos rot unit)
      (declare (ignore pivot))
      (let ((rows-to-check nil))
        (iter (for m :in members)
          (pushnew (second m) rows-to-check))
        (iter (for row :in rows-to-check)
          (when (iter (for col :below (board-dimension board 0))
                  (always (or (= 1 (bref board col row))
                              (member (list col row) members :test 'equal))))
            (collect row)))))))

(defun filled-rows (board)
  (iter (for row :below (board-dimension board 1))
    (when (iter (for col :below (board-dimension board 0))
            (always (= 1 (bref board col row))))
      (collect row))))

(defun score (board unit path ls-old)
  (let* ((ls (length (filled-rows-with-unit board unit path)))
         (points
           ;; Score for placement
           (+ (length (members unit))
              ;; Score for removing lines
              (* (/ 100 2) (1+ ls) ls))))
    (+ (score-phrases (encode-solution (reverse (mapcar 'first path))))
       points
       ;; line bonus
       (if (> ls-old 1)
           (floor (* points (- ls-old 1)) 10)
           0))))

(defun mag-l1 (v)
  (reduce '+ (mapcar 'abs v)))

(defun locked-top-p (path unit-pos)
  (and (locked path)
       (some 'zerop (mapcar 'second unit-pos))))

(defun heuristic-score (board unit path ls-old)
  (destructuring-bind (pivot rot)
      (get-pos path unit board)
    (float
     (+ (iter (for m :in (second (translate-coords* board pivot rot unit)))
          (summing
           (/ (mag-l1
               (v- m (list (floor (board-dimension board 0) 2) 0)))
              (mag-l1
               (board-dimensions board)))))
        (* (board-dimension board 0)
           (length (filled-rows-with-unit board unit path)))
        ;; (with-temp-blit (board unit pivot rot)
        ;;   (destructuring-bind (a b c) (board-score board)
        ;;     (declare (ignore a))
        ;;     (+ b c)))
        (score board unit path ls-old)))))

(defun modify-path (path unit board moves
                     &optional (score-fn 'heuristic-score))
  (let* ((original-score (funcall score-fn board unit path 0))
         (move (alexandria:random-elt moves))
         (new-path (funcall move path unit board))
         (score (funcall score-fn board unit new-path 0)))
    ;; (print (list score original-score))
    (if (> score original-score)
        new-path
        path)))

(defun sample-paths (path unit board moves
                      &key
                      (score-fn 'heuristic-score)
                      (iterations 1000))
  (iter (repeat iterations)
    (setf path (modify-path path unit board moves score-fn)))
  path)

(defparameter *moves*
  '(move-end hanger-grow hanger-shrink bubble-grow bubble-shrink))

(defun lock-unit (board unit path)
  (destructuring-bind (pos rot)
      (get-pos path unit board)
    (destructuring-bind (pivot members)
        (translate-coords board pos rot unit)
      (declare (ignore pivot))
      (iter (for (x y) :in members)
        (setf (bref board x y) 1)))))

(defun remove-filled-rows (board)
  (let ((filled-rows (sort (filled-rows board) #'>)))
    (iter (while filled-rows)
      (let ((removed (pop filled-rows)))
        ;; clear out line
        (iter (for col :below (board-dimension board 0))
          (setf (bref board col removed) 0))
        ;; Move all lines above it down
        (iter (for row :from removed :downto 1)
          (iter (for col :below (board-dimension board 0))
            (setf (bref board col row)
                  (bref board col (- row 1)))))
        ;; Clear out the top line
        (iter (for col :below (board-dimension board 0))
          (setf (bref board col 0) 0)))
      ;; Increment all filled rows as they all have shifted down
      (mapcar '1+ filled-rows))
    (length filled-rows)))

(defun place-unit (unit board)
  (let* ((init-path (initial-path unit board))
         (path (sample-paths init-path unit board *moves*)))
    (lock-unit board unit path)
    (let ((ls-old (remove-filled-rows board)))
      (setf *unit* (aref *unit-array* (mod (funcall *rng*)
                                           (length *unit-array*))))
      ls-old)))

(defun zach-player (problem)
  (init-game problem)
  ;; (setf envisage:*redisplay* t)
  (iter (repeat *source-length*)
    (place-unit *unit* *board*)
    ;; (setf envisage:*redisplay* t)
    (sleep 1)))

;; (defun place-unit (board unit unit-command-list)
;;   (let ((pos (initial-position board unit))
;;         (dim (board-dimensions board))
;;         (rot 0))
;;     ;; Process the commands
;;     (iter (for com :in (reverse unit-command-list))
;;       (case com
;;         (:cw (decf rot 1))
;;         (:ccw (incf rot 1))
;;         (:e (setf pos (v+ '(1 0) pos)))
;;         (:w (setf pos (v+ '(-1 0) pos)))
;;         (:se (setf pos (v+ '(0 1) pos)))
;;         (:sw (setf pos (v+ '(-1 1) pos)))))
;;     (list
;;      (vshift+ pos)
;;      (iter (for mem :in (members unit))
;;        (destructuring-bind (mem-x mem-y)
;;            (v+ pos (apply-rotation mem rot pos))
;;          (let ((mem-x (+ mem-x (shift mem-y))))
;;            (if (or (< mem-x 0) (>= mem-x (first dim))
;;                    (< mem-y 0) (>= mem-y (second dim))
;;                    (= 1 (bref board mem-x mem-y)))
;;                (error "Invalid state")
;;                (collect (list mem-x mem-y)))))))))
