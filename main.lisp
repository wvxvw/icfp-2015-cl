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
           :long "phrase"
           :arg-parser #'identity
           :meta-var "STRING")
    (:name :submit
           :description "Submit solution instead of printing it."
           :short #\u
           :long "submit")
    (:name :dry-run
           :description "Only print the log, but not the results."
           :short #\d
           :long "dry-run")
    (:name :tag
           :description "Tag to use when submitting online."
           :short #\g
           :long "tag"
           :arg-parser #'identity
           :meta-var "STRING")
    (:name :range
           :description "Range of the generated command sequence to send."
           :short #\r
           :long "range"
           :arg-parser #'read-from-string 
           :meta-var "LIST")
    (:name :raw
           :description "Raw sequence of commands to send."
           :short #\s
           :long "raw"
           :arg-parser #'identity
           :meta-var "STRING")
    (:name :svg
           :description "Output html with SVG drawing steps."
           :short #\c
           :long "svg"
           :arg-parser #'parse-namestring
           :meta-var "FILE")))

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
   (time-limits :initarg :time-limits :initform nil :accessor time-limits)
   (submit-online :initarg :submit-online :initform nil :accessor submit-online)
   (dry-run :initarg :dry-run :initform nil :accessor dry-run)
   (tag :initarg :tag :initform nil :accessor tag)
   (range :initarg :range :initform (list 0 -1) :accessor range)
   (raw :initarg :raw :initform nil :accessor raw)
   (svg :initarg :svg :initform nil :accessor svg)))

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

      (config-logger (or (getf options :verbose) 1))
      (when-option (options :help)
        (opts:describe
         :prefix "ICFP 2015 contest submission by Chaitin's Omega Men team"
         :suffix ""))
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
      (when-option (options :submit)
        (log:info "will submit online")
        (setf (submit-online config) t))
      (when-option (options :dry-run)
        (log:info "will not submit, only go through motions")
        (setf (dry-run config) t))
      (when-option (options :tag)
        (log:info "Submitting using tag: ~s" (getf options :tag))
        (setf (tag config) (getf options :tag)))
      (when-option (options :range)
        (log:info "Emit commands in range: ~s" (getf options :range))
        (setf (range config) (getf options :range)))
      (when-option (options :raw)
        (log:info "Raw command sequence: ~s" (getf options :raw))
        (setf (raw config) (getf options :raw)))
      (when-option (options :svg)
        (log:info "Output SVG to: ~s" (getf options :svg))
        (setf (svg config) (getf options :svg)))
      (iter
        (for phrase := (getf options :phrase))
        (while phrase)
        (log:info "phrase: ~s" phrase)
        (push phrase (phrases config))
        (remf options :phrase)))
    config))

(defun entry-point ()
  (init)
  (let* ((config (read-arguments))
         (svg-listener
          (when (svg config) (make-instance 'svg-listener))))
    (labels ((%trunkate-commands ()
               (or (raw config)
                   (let ((cmds (alexandria:flatten (play-game svg-listener))))
                     (destructuring-bind (from to) (range config)
                       (when (< to 0) (setf to (length cmds)))
                       (subseq cmds from to))))))

      (iter
        (for board :in (boards config))
        (init-game board)
        (log:info "submitting board: ~s" board)
        (iter
          (for seed :in *seeds*)
          (log:info "submitting seed: ~s" seed)
          (unless (first-time-p)
            (setf *rng* (lcg seed)))
          (cond
            ((dry-run config) (play-game svg-listener))
            ((submit-online config)
             (submit *board-id* seed
                     (%trunkate-commands)
                     (tag config)))
            (t
             (princ (solution-to-string
                     *board-id* seed
                     (%trunkate-commands)
                     (tag config)))
             (terpri))))))
    (when svg-listener
      (svgs-to-html (svg config) (svgs svg-listener)))
    (quit)))
