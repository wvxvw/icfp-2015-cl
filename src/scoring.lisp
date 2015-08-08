(in-package :icfp-2015-cl)

(defun count-outer-ribs (board)
  (iter :outer
        (with width := (array-dimension board 1))
        (with height := (array-dimension board 0))
        (for j :below height)
        (iter
          (for i :below width)
          (when (= 1 (aref board j i))
            (let ((cnt
                   (cond
                     ;; corners
                     ((or (and (= i 0) (= j 0))
                          (and (= i (1- width)) (= j (1- height)))
                          (and (= i 0) (= j (1- height)))
                          (and (= i (1- width)) (= j 0)))
                      4)
                     ;; outermost vertical borders
                     ((and (or (= i 0) (= i (1- width))) (oddp j))
                      3)
                     ;; top and bottom
                     ((or (= j 0) (= j (1- height)))
                      2)
                     ;; innermost vertical borders
                     ((or (= i 0) (= i (1- width)))
                      1))))
              (in :outer (summing (or cnt 0))))))))

(defun count-adjacent-ribs (board)
  (iter :outer
        (with width := (array-dimension board 1))
        (with height := (array-dimension board 0))
        (for j :from 1 :below height)
        (iter
          (for i :from 1 :below width)
          (when (= 1 (aref board j i))
            ;; hexagon directly th the left of the current one
            (when (= 1 (aref board j (1- i)))
              (in :outer (summing 2)))
            ;; haxagon directly on top of the current one
            (when (= 1 (aref board (1- j) i))
              (in :outer (summing 2)))
            (if (oddp i)
                ;; haxagon on top and to the left of the current one
                (when (= 1 (aref board (1- j) (1- i)))
                  (in :outer (summing 2)))
                ;; hexagon on top and to the right of the current one
                (when (and (< (1+ i) width)
                           (= 1 (aref board (1- j) (1+ i))))
                  (in :outer (summing 2))))))))

(defun board-score (board)
  (let ((filled
         (iter :outer
           (for i :below (array-dimension board 1))
           (iter
             (for j :below (array-dimension board 0))
             (in :outer (summing (aref board j i)))))))
    (list filled (count-outer-ribs board)
          (count-adjacent-ribs board))))

(defun board-to-binary (board)
  (coerce 
   (iter
     (for i :below (array-dimension board 1))
     (collect (list i
                    (iter
                      (with row := 0)
                      (for j :below (array-dimension board 0))
                      (for bit := (aref board j i))
                      (setf row (ash row 1)
                            row (logior row bit))
                      (finally (return row))))))
   'vector))

(defun unit-to-binary (unit)
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
    (let ((translated (make-array (list width height) :initial-element 0)))
      (iter
        (for (x y) :in (members unit))
        (setf (aref translated (- x tx) (- y ty)) 1))
      (board-to-binary translated))))

(defun find-match (board unit)
  (let ((width (array-dimension board 1))
        (height (array-dimension board 0))
        (binary-board (board-to-binary board))
        (binary-unit (unit-to-binary unit)))
    (iter
      (with i := (1- (length binary-board)))
      (with j := (1- (length binary-unit)))
      (with back-row := i)
      (with back-col := 0)
      (while (>= i 0))
      (let ((match 
                (iter
                  (with tester := j)
                  (with row := (aref binary-board i))
                  (for k :from back-col)
                  (while (< tester (expt 2 width)))
                  (when (logandc1 row tester)
                    (return k))
                  (setf tester (ash j (incf k)))
                  (finally (return -1)))))
        (if (> 0 match)
            (if (= 0 j)
                (return (cons i match))
                (progn
                  (decf i)
                  (decf j)
                  (setf back-col match)))
            (if (< back-col (1- width))
                (progn
                  (incf back-col)
                  (setf i back-row))
                (setf back-col 0
                      i (1- back-row))))))))

;; testing

(defun test-scoring (file)
  (init-game file)
  (let* ((score (board-score *board*)))
    (values *board* score)))

(defun test-find-match (file)
  (init-game file)
  (multiple-value-bind (match-board match-unit)
      (find-match *board* *unit*)
    (format t "~&~s~&~{~{~d:~b~}~^~&~}~&~s"
            *board* (coerce match-board 'list) match-unit)
    (values)))
