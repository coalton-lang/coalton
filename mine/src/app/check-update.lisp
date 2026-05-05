(defpackage #:mine/app/check-update
  (:use #:cl)
  (:export
   #:start-update-check-thread
   #:update-check-message
   #:update-check-available-p))

(in-package #:mine/app/check-update)

(defparameter *mine-releases-api-url*
  "https://api.github.com/repos/coalton-lang/coalton/releases?per_page=100")

(defun current-release-tag ()
  "Return the current mine version as a GitHub release tag."
  (concatenate 'string "mine-v" mine/version:*mine-version*))

(defun mine-release-tags (json)
  "Extract mine-v* release tags from GitHub releases API JSON in order."
  (let ((tags '())
        (start 0)
        (needle "\"tag_name\""))
    (loop
      (let ((pos (search needle json :start2 start)))
        (unless pos
          (return))
        (let* ((colon (position #\: json :start (+ pos (length needle))))
               (tag-start (and colon (position #\" json :start (1+ colon))))
               (end (and tag-start (position #\" json :start (1+ tag-start))))
               (tag (and end (subseq json (1+ tag-start) end))))
          (when (and tag
                     (<= 6 (length tag))
                     (string-equal "mine-v" tag :end2 6))
            (push tag tags))
          (setf start (if end (1+ end) (+ pos (length needle)))))))
    (nreverse tags)))

(defun fetch-mine-releases-json ()
  (with-output-to-string (out)
    (let* ((proc (sb-ext:run-program
                  #+win32 "curl.exe" #-win32 "curl"
                  (list "-fsSL"
                        "--max-time" "5"
                        "--connect-timeout" "3"
                        "-H" "Accept: application/vnd.github+json"
                        "-H" "User-Agent: mine-update-check"
                        *mine-releases-api-url*)
                  :search t
                  :output out
                  :error nil
                  :wait t))
           (exit-code (sb-ext:process-exit-code proc)))
      (unless (and exit-code (zerop exit-code))
        (error "GitHub releases request failed")))))

(defun compute-update-check-state ()
  (handler-case
      (let* ((tags (mine-release-tags (fetch-mine-releases-json)))
             (latest (first tags))
             (current (current-release-tag)))
        (cond
          ((null latest)
           (list ':error))
          ((not (string-equal latest current))
           (list ':available latest))
          (t
           (list ':current latest))))
    (error ()
      (list ':error))))

(defun start-update-check-thread (write-state)
  "Start the update check thread, calling WRITE-STATE with status plists."
  (funcall write-state (list ':checking))
  (mine/bindings/thread:make-thread
   "mine-update-check"
   (lambda ()
     (funcall write-state (compute-update-check-state))))
  (values))

(defun update-check-message (state)
  "Return a user-facing update-check line for the welcome screen."
  (cond
    ((and (consp state) (eq (first state) ':checking))
     "Checking for mine updates...")
    ((and (consp state) (eq (first state) ':available))
     (format nil "Update on GitHub: ~A." (second state)))
    (t "")))

(defun update-check-available-p (state)
  (and (consp state) (eq (first state) ':available)))
