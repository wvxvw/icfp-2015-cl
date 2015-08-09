
(in-package :icfp-2015-cl)

(defparameter *submission-url*
  "https://davar.icfpcontest.org/teams/168/solutions")

(defparameter *api-token*
  (with-open-file (in (asdf:system-relative-pathname
                       (asdf:find-system :icfp-2015-cl)
                       #p"api-token"))
    (read-line in)))

(defun solution-to-string (problem-number seed commands &optional (tag "test-submission"))
  (with-output-to-string (out)
    (cl-json:encode-json
     `((("problemId" . ,problem-number)
        ("seed" . ,seed)
        ("tag" . ,tag)
        ("solution" . ,commands))))))

(defun submit (problem-number seed commands &optional (tag "test-submission"))
  (drakma:http-request
   "https://davar.icfpcontest.org/teams/168/solutions"
   :basic-authorization (list "" *api-token*)
   :method :post
   :content-type "application/json"
   :content (solution-to-string problem-number seed commands tag)))
