
(in-package :icfp-2015-cl)

(defparameter *submission-url*
  "https://davar.icfpcontest.org/teams/168/solutions")

(defparameter *api-token*
  (with-open-file (in (asdf:system-relative-pathname
                       (asdf:find-system :icfp-2015-cl)
                       #p"api-token"))
    (read-line in)))

(defparameter *translation-table*
  (let ((table (make-hash-table)))
    (setf (gethash :w table) "p'!.03"
          (gethash :e table) "bcefy2"
          (gethash :sw table) "aghij4"
          (gethash :se table) "lmno 5"
          (gethash :cw table) "dqrvz1"
          (gethash :ccw table) "kstuwx")
    table))

(defun encode-solution (commands)
  (with-output-to-string (stream)
    (iter
      (for cmd :in commands)
      (cond ((gethash cmd *translation-table*)
             (princ (aref (gethash cmd *translation-table*) 0)
                    stream))
            ((characterp cmd)
             (princ cmd stream))
            (t (error "Garbage in command sequence"))))))

(defun solution-to-string (problem-number seed commands &optional (tag "test-submission"))
  (with-output-to-string (out)
    (cl-json:encode-json
     `((("problemId" . ,problem-number)
        ("seed" . ,seed)
        ("tag" . ,tag)
        ("solution" . ,(encode-solution commands))))
     out)))

(defun submit (problem-number seed commands &optional (tag "test-submission"))
  (drakma:http-request
   *submission-url*
   :basic-authorization (list "" *api-token*)
   :method :post
   :content-type "application/json"
   :content (solution-to-string problem-number seed commands tag)))
