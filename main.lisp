(in-package :icfp-2015-cl)

;; Flag	Type	Description
;; -f 	FILENAME	File containing JSON encoded input.
;; -t	NUMBER 	Time limit, in seconds, to produce output
;; -m	NUMBER 	Memory limit, in megabytes, to produce output
;; -p	STRING 	Phrase of power, as quoted string
(defun init ()
  (opts:define-opts
      (:name :help
             :description "Print this help text."
             :short #\h
             :long "help")
      (:name :verbose
             :description "Verbosity level."
             :short #\v
             :long "verbose"
             :arg-parser #'parse-integer)
    (:name :board
           :description "File containing JSON encoded input."
           :short #\f
           :long "board"
           :arg-parser #'parse-namestring
           :meta-var "FILE")
    (:name :time-limit
           :description "Time limit, in seconds, to produce output."
           :short #\t
           :long "time-limit"
           :arg-parser #'parse-integer
           :meta-var "NUMBER")
    (:name :memory-limit
           :description "Memory limit, in megabytes, to produce output."
           :short #\m
           :long "memory-limit"
           :arg-parser #'parse-integer
           :meta-var "NUMBER")
    (:name :phrase
           :description "Phrase of power, as quoted string."
           :short #\p
           :long "memory-limit"
           :arg-parser #'identity
           :meta-var "STRING")))

(defun unknown-option (condition)
    (log:warn "~s option is unknown!~%" (opts:option condition))
    (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it ,@body)))

(defun config-logger (level)
  (let ((level (assoc (min level 4)
                      '((4 . :debug)
                        (3 . :info)
                        (2 . :warn)
                        (1 . :error)))))
    (when Level
      ;; (format t "~&log level: ~s" (cdr level))
      (log:config (cdr level)))))

(defclass configuration ()
  ((boards :initarg :boards :initform nil :accessor boards)
   (phrases :initarg :phrases :initform nil :accessor phrases)
   (mem-limits :initarg :mem-limits :initform nil :accessor mem-limits)
   (time-limits :initarg :time-limits :initform nil :accessor time-limits)))

(defun read-arguments ()
  (let ((config (make-instance 'configuration)))
    (multiple-value-bind (options free-args)
        (handler-case
            (handler-bind ((opts:unknown-option #'unknown-option))
              (opts:get-opts))
          (opts:missing-arg (condition)
            (config-logger 4)
            (log:error "Option ~s needs an argument!"
                       (opts:option condition)))
          (opts:arg-parser-failed (condition)
            (config-logger 4)
            (log:error "Cannot parse ~s as argument of ~s"
                       (opts:raw-arg condition)
                       (opts:option condition))))
      (when-option (options :help)
        (opts:describe
         :prefix "ICFP 2015 contest submission by Chaitin's Omega Men team"
         :suffix ""))
      (config-logger (or (getf options :verbose) 1))
      (iter
        (for board := (getf options :board))
        (while board)
        (log:info "selecting board: ~s" board)
        (push board (boards config))
        (remf options :board))
      (when-option (options :time-limit)
        (log:info "time limit: ~s" (getf options :time-limit))
        (setf (time-limits config) (getf options :time-limit)))
      (when-option (options :memory-limit)
        (log:info "memory limit: ~s" (getf options :memory-limit))
        (setf (mem-limits config) (getf options :memory-limit)))
      (iter
        (for phrase := (getf options :phrase))
        (while phrase)
        (log:info "phrase: ~s" phrase)
        (push phrase (phrases config))
        (remf options :phrase)))
    config))

(defun entry-point ()
  (init)
  (let ((config (read-arguments)))
    (iter
      (for board :in (boards config))
      (init-game board)
      (princ (solution-to-string
              *board-id*
              (car *seeds*)
              (optimal-trajectory *board* *unit*)))
      (terpri)))
  (quit))
