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
     (collect (cons i
                    (iter
                      (with row := 0)
                      (for j :below (array-dimension board 0))
                      (for bit := (aref board j i))
                      (setf row (ash row -1)
                            row (logior row bit))))))
   'vector))

(defun unit-to-binary (unit)
  unit)

(defun find-match (board unit)
  (let ((binary-board (board-to-binary board))
        (binary-unit (unit-to-binary unit)))
    (values binary-board binary-unit)))

;; testing

(defun test-scoring (file)
  (with-open-file (stream file)
    (let* ((data (cl-json:decode-json stream))
           (board (init-board data))
           (score (board-score board)))
      (values board score))))

(defun test-find-match (file)
  (with-open-file (stream file)
    (let* ((data (cl-json:decode-json stream))
           (board (init-board data))
           (match (find-match board nil)))
      (values board match))))
