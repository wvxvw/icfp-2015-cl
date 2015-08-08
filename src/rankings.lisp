(in-package :icfp-2015-cl)

(defparameter *rankings-url*
  "https://davar.icfpcontest.org/rankings.js")


(defun pairme (unpaired)
  (if (and (not (null unpaired)) (evenp (length unpaired)))
      (cons (list (car unpaired) (cadr unpaired)) (pairme (cddr unpaired)))))

(defun regexme (string)
  (let ((matches (pairme (cl-ppcre:all-matches "teamId.?:168," string))))
    (loop for (start end) in matches
	  collect (cl-json:decode-json-from-string (subseq string
							   (position #\{ string :from-end t :end end)
							   (1+ (position #\} string :start start)))))))

(defun rankings ()
  (let* ((rankings-json (subseq (flexi-streams::octets-to-string (drakma:http-request
							   *rankings-url*)) 11)))
    (regexme rankings-json)))

