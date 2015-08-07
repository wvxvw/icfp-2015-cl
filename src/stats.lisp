(in-package :icfp-2015-cl)

(defstruct aggregate width height pieces source-length)

(defun parse-file (file aggregate)
  (unless (aggregate-pieces aggregate)
    (setf (aggregate-pieces aggregate)
          (make-hash-table :test 'equal)))
  (with-open-file (stream file)
    (destructuring-bind ((_h . height)
                         (_w . width)
                         _seeds
                         units
                         _id
                         _filled
                         (_sl . source-length))
        (cl-json:decode-json stream)
      (declare (ignore _h _w _seeds _id _filled _sl))
      (push height (aggregate-height aggregate))
      (push width (aggregate-width aggregate))
      (push source-length (aggregate-source-length aggregate))
      (iter
        (for unit :in units)
        (let ((cnt (gethash unit (aggregate-pieces aggregate) 0)))
          (setf (gethash unit (aggregate-pieces aggregate)) (1+ cnt))))
      units)))

(defun avg (data)
  (/ (reduce '+ data) (length data)))

(defun collect-info ()
  (let ((aggregate (make-aggregate)))
    (iter
      (for file :in (directory #p"./problems/*.json"))
      (parse-file file aggregate))
    (let ((max-width (reduce 'max (aggregate-width aggregate)))
          (avg-width (avg (aggregate-width aggregate)))
          (min-width (reduce 'min (aggregate-width aggregate)))
          (max-height (reduce 'max (aggregate-height aggregate)))
          (avg-height (avg (aggregate-height aggregate)))
          (min-height (reduce 'min (aggregate-height aggregate)))
          (max-length (reduce 'max (aggregate-source-length aggregate)))
          (avg-length (avg (aggregate-source-length aggregate)))
          (min-length (reduce 'min (aggregate-source-length aggregate))))
      (format t "~&max width: ~d ~
                 ~&avg width: ~f ~
                 ~&min width: ~d ~
                 ~&------------- ~
                 ~&max height: ~d ~
                 ~&avg height: ~f ~
                 ~&min height: ~d ~
                 ~&-------------- ~
                 ~&max length: ~d ~
                 ~&avg length: ~f ~
                 ~&min length: ~d ~
                 ~&--------------"
              max-width avg-width min-width 
              max-height avg-height min-height
              max-length avg-length min-length)
      (iter
        (with max-value := 0)
        (with max-key := nil)
        (with min-value := 99999)
        (with min-key := nil)
        (with avg-value := 0)
        (for (key value) :in-hashtable (aggregate-pieces aggregate))
        (for cnt :from 0)
        (when (listp key)
          (format t "~&unit: ~s~&" key)
          (print-unit key)
          (format t "~&occurs: ~d times" value))
        (when (> value max-value)
          (setf max-value value
                max-key key))
        (when (< value min-value)
          (setf min-value value
                min-key key))
        (incf avg-value value)
        (finally
         (format t "~&distinct units: ~d ~
                    ~&most popular unit: ~s, ~d ~
                    ~&least popular unit: ~s, ~d ~
                    ~&avg number of units: ~f"
                 cnt max-key max-value
                 min-key min-value
                 (/ avg-value cnt)))))))

(defun print-unit (unit &optional (stream *standard-output*))
  (let ((piece (make-array '(20 20) :element-type 'character :initial-element #\Space))
        (members (first unit))
        (pivot (cdr (second unit))))
    (iter
      (for element :in (cdr members))
      (let ((x (cdr (assoc :x element)))
            (y (cdr (assoc :y element)))
            (offset-x 0) (offset-y 0))
        (setf offset-x (* 2 x)
              offset-y (* 2 y))
        (when (oddp y) (incf offset-x))
        (setf (aref piece offset-y       offset-x)       #\/
              (aref piece offset-y       (+ 1 offset-x)) #\\
              (aref piece (+ 1 offset-y) offset-x)       #\|
              (aref piece (+ 1 offset-y) (+ 2 offset-x)) #\|
              (aref piece (+ 2 offset-y) offset-x)       #\\
              (aref piece (+ 2 offset-y) (+ 1 offset-x)) #\/)))
    (let ((px (* 2 (cdr (assoc :x pivot))))
          (py (* 2 (cdr (assoc :y pivot)))))
      (when (oddp (/ py 2)) (incf px))
    (setf (aref piece (1+ py) (1+ px)) #\.))
    (iter
      (for i :below (array-dimension piece 0))
      (iter
        (for j :below (array-dimension piece 1))
        (format stream "~c" (aref piece i j)))
      (terpri stream))))
        