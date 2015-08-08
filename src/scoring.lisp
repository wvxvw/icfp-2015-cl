(in-package :icfp-2015-cl)

(defun count-outer-ribs (board)
  (iter :outer
        (with width := (board-dimension board 1))
        (with height := (board-dimension board 0))
        (for j :below height)
        (iter
          (for i :below width)
          (when (= 1 (bref board i j))
            (let ((cnt
                   (cond
                     ;; corners
                     ((or (and (= i 0) (= j 0))
                          (and (oddp height) (and (= i 0) (= j (1- height))))
                          (and (evenp height) (and (= i (1- width)) (= j (1- height)))))
                      4)
                     ;; outermost vertical borders
                     ((or (and (evenp j) (= i 0))
                          (and (oddp j) (= i (1- width))))
                      3)
                     ;; top and bottom
                     ((or (= j 0) (= j (1- height)))
                      2)
                     ;; innermost vertical borders
                     ((or (and (oddp j) (= i 0))
                          (and (evenp j) (= i (1- width))))
                      1))))
              (in :outer (summing (or cnt 0))))))))

(defun count-adjacent-ribs (board)
  (iter :outer
        (with width := (board-dimension board 1))
        (with height := (board-dimension board 0))
        (for j :from 1 :below height)
        (iter
          (for i :from 1 :below width)
          (when (= 1 (bref board i j))
            ;; hexagon directly th the left of the current one
            (when (= 1 (bref board (1- i) j))
              (in :outer (summing 2)))
            ;; haxagon directly on top of the current one
            (when (= 1 (bref board i (1- j)))
              (in :outer (summing 2)))
            (if (oddp i)
                ;; haxagon on top and to the left of the current one
                (when (= 1 (bref board (1- i) (1- j)))
                  (in :outer (summing 2)))
                ;; hexagon on top and to the right of the current one
                (when (and (< (1+ i) width)
                           (= 1 (bref board (1+ i) (1- j))))
                  (in :outer (summing 2))))))))

(defun board-score (board)
  (let ((filled
         (iter :outer
           (for i :below (board-dimension board 1))
           (iter
             (for j :below (board-dimension board 0))
             (in :outer (summing (bref board i j)))))))
    (list filled (count-outer-ribs board)
          (count-adjacent-ribs board))))

(defun ranges (board row)
  (iter
    (with width := (board-dimension board 1))
    (with started := nil)
    (for col :below width)
    (for cell := (bref board col row))
    (cond
      ((and (null started) (= 0 cell))
       (setf started col))
      ((and started (= cell 1))
       (collect (list started (1- col)) :into result)
       (setf started nil)))
    (finally (when started
               (return (append result
                               (list (list started (1- col)))))))))

(defun board-tunnel (board)
  (iter
    (with height := (board-dimension board 0))
    (for row :below height)
    (collect (ranges board row))))

(defun board-to-binary (board)
  (coerce 
   (iter
     (for i :below (board-dimension board 1))
     (collect (iter
                (with row := 0)
                (for j :below (board-dimension board 0))
                (for bit := (bref board i j))
                (setf row (ash row 1)
                      row (logior row bit))
                (finally (return row)))))
   'vector))

(defun translate-unit (unit &key (format 'vector))
  (multiple-value-bind (tx ty width height)
      (iter
        (for (x y) :in (members unit))
        (minimizing x :into min-x)
        (minimizing y :into min-y)
        (maximizing x :into max-x)
        (maximizing y :into max-y)
        (finally
         (return (values min-x min-y
                         (1+ (- max-x min-x))
                         (1+ (- max-y min-y))))))
    (ecase format
      (vector 
       (let ((translated (make-array (list width height) :initial-element 0)))
         (iter
           (for (x y) :in (members unit))
           (setf (aref translated (- x tx) (- y ty)) 1))
         translated))
      (list
       (iter
         (for (x y) :in (members unit))
         (collect (list (- x tx) (- y ty))))))))

(defun unit-to-binary (unit)
  (board-to-binary (translate-unit unit)))

(defun fit (hole peg hole-size peg-size)
  (assert (<= peg-size hole-size))
  (format t "~&fit: ~b, ~b" hole peg)
  (iter
    (for i :below (- hole-size peg-size))
    (when (= (logandc1 hole peg) peg)
      (collect i))
    (setf peg (ash peg 1))))

(defun find-match (board unit)
  (let* ((width (board-dimension board 1))
         (height (board-dimension board 0))
         (binary-board (board-to-binary board))
         (translated (translate-unit unit))
         (binary-unit (board-to-binary translated)))
    (iter
      (with board-tip := (1- height))
      (with unit-tip := (1- (length binary-unit)))
      (with unit-width := (1- (array-dimension translated 1)))
      (with current-board-row := (aref binary-board board-tip))
      (with current-unit-row := (aref binary-unit unit-tip))
      (with positions := (fit current-board-row current-unit-row width unit-width))
      (with offset := 0)
      (while (>= unit-tip 0))
      (if positions
          (progn
            (setf offset (car positions)
                  positions (cdr positions))
            (iter
              (for rows :from board-tip :downto 0)
              (for unit-rows :from unit-tip :downto 0)
              (for current-board-row := (aref binary-board rows))
              (for current-unit-row := (aref binary-unit unit-rows))
              (unless (= (logandc1 current-board-row current-unit-row) current-unit-row)
                (return))
              (finally (return-from find-match (cons board-tip offset)))))
          (progn
            (decf board-tip)
            (setf unit-tip (1- (length binary-unit))
                  positions (fit current-board-row current-unit-row width unit-width)))))))

;; testing

(defun test-scoring (file)
  (init-game file)
  (let* ((score (board-score *board*)))
    (values *board* score)))

(defun test-find-match (file)
  (init-game file)
  (destructuring-bind (border-tip . offset)
      (find-match *board* *unit*)
    (format t "~&~s~&y: ~d, x: ~d" *board* border-tip offset)
    (values)))

(defun test-ranges (file row)
  (init-game file)
  (ranges *board* row))

(defun test-tunnel (file)
  (init-game file)
  (board-tunnel *board*))
