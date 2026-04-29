;;;; eval.lisp -- Safe evaluation for the runtime server.
;;;;
;;;; Provides sandboxed evaluation of forms in specified packages.

(in-package #:mine/runtime/eval)

;;; Helpers

(defun find-or-make-package (package-name)
  "Find the package named PACKAGE-NAME, creating it if it does not exist."
  (or (find-package (string-upcase package-name))
      (make-package (string-upcase package-name) :use '(#:cl))))

(defun coalton-form-p (form)
  "Return T if FORM is a Coalton toplevel or inline form."
  (and (consp form)
       (symbolp (car form))
       (member (symbol-name (car form))
               '("COALTON-TOPLEVEL" "COALTON")
               :test #'string-equal)))

(defun %format-result-values (values package)
  "Render VALUES for transport to the TUI as one printed string per value."
  (loop :for val :in values
        :collect (with-output-to-string (s)
                   (let ((*package* package))
                     (write val :stream s)))))

(defun %encode-result-values (values package)
  "Encode VALUES for transport to the TUI while preserving value boundaries."
  (prin1-to-string (list :values (%format-result-values values package))))

(defun %quick-result-payload-string (values output package)
  "Encode output and printed values for Quick Result."
  (prin1-to-string
   (list :quick-result
         :output output
         :values (%format-result-values values package))))

(defun %coalton-readtable ()
  "Return the Coalton named-readtable."
  (named-readtables:ensure-readtable 'coalton:coalton))

;;; Public API

(defun safe-eval (form-string package-name)
  "Evaluate FORM-STRING in the package named PACKAGE-NAME.
Returns (values result-string output-string error-string) where
RESULT-STRING encodes the printed values list and ERROR-STRING is NIL on
success."
  (let ((pkg (find-or-make-package package-name)))
    (handler-case
        (let* ((form (let ((*package* pkg)
                           (*read-eval* nil))
                       (read-from-string form-string)))
               (stdout-capture (make-string-output-stream))
               (stderr-capture (make-string-output-stream))
               (result-values nil))
          ;; Evaluate with output capture
          (setf result-values
                (handler-case
                    (let ((*standard-output* (make-broadcast-stream
                                              *standard-output*
                                              stdout-capture))
                          (*error-output* (make-broadcast-stream
                                           *error-output*
                                           stderr-capture))
                          (*trace-output* (make-broadcast-stream
                                           *trace-output*
                                           stderr-capture))
                          (*package* pkg))
                      (multiple-value-list (eval form)))
                  (error (c)
                    (return-from safe-eval
                      (values nil
                              (get-output-stream-string stdout-capture)
                              (format nil "~A" c))))))
          (let ((result-string (%encode-result-values result-values pkg)))
            (values result-string
                    (get-output-stream-string stdout-capture)
                    nil)))
      ;; Read errors (malformed input)
      (reader-error (c)
        (values nil nil (format nil "Read error: ~A" c)))
      (end-of-file (c)
        (declare (ignore c))
        (values nil nil "Read error: unexpected end of input"))
      (package-error (c)
        (values nil nil (format nil "Package error: ~A" c)))
      (error (c)
        (values nil nil (format nil "Error: ~A" c))))))

(defun debug-eval (form-string package-name &optional wire-stream msg-id coalton-p)
  "Evaluate FORM-STRING in PACKAGE-NAME via plain EVAL for interactive use.
Lets errors propagate to the caller's handler-bind (for debugger support).
When WIRE-STREAM and MSG-ID are provided, binds *standard-input*, *query-io*,
and *terminal-io* to a TUI input stream so interactive reads work.
When COALTON-P is true, binds the Coalton readtable during reading.
Returns (values result-string output-string) on success."
  (let* ((pkg (find-or-make-package package-name))
         (form (let ((*package* pkg)
                     (*read-eval* nil)
                     (*readtable* (if coalton-p (%coalton-readtable) *readtable*)))
                 (read-from-string form-string)))
         (stdout-capture (make-string-output-stream))
         (stderr-capture (make-string-output-stream))
         (tis (when wire-stream
                (make-instance 'mine/protocol/server::tui-input-stream
                  :wire-stream wire-stream :msg-id msg-id
                  :stdout-capture stdout-capture)))
         (result-values
           (let* ((*standard-output* (if tis
                                         stdout-capture
                                         (make-broadcast-stream
                                           *standard-output* stdout-capture)))
                  (*error-output* (make-broadcast-stream
                                    *error-output* stderr-capture))
                  (*trace-output* (make-broadcast-stream
                                    *trace-output* stderr-capture))
                  (*standard-input* (if tis tis *standard-input*))
                  (*query-io* (if tis
                                  (make-two-way-stream tis *standard-output*)
                                  *query-io*))
                  (*terminal-io* (if tis
                                     (make-two-way-stream tis *standard-output*)
                                     *terminal-io*))
                  (*package* pkg))
             ;; Shift form history (+ is always updated)
             (setf +++ ++ ++ + + form)
             (let ((vals (multiple-value-list (eval form))))
               ;; Update value history only when at least one value was produced
               (when vals
                 (setf *** ** ** * * (first vals)
                       /// // // / / vals))
               vals))))
    (let ((all-output (concatenate 'string
                        (get-output-stream-string stdout-capture)
                        (get-output-stream-string stderr-capture))))
      (values (%encode-result-values result-values pkg)
              all-output))))

(defun quick-result (form-string package-name &optional coalton-p)
  "Evaluate FORM-STRING and return compact display text or an error string.
This is for non-modal editor Quick Result: it captures output and values, catches
errors, and deliberately does not enter the interactive debugger."
  (let ((pkg (find-or-make-package package-name))
        (stdout-capture (make-string-output-stream))
        (stderr-capture (make-string-output-stream)))
    (handler-case
        (let* ((form (let ((*package* pkg)
                           (*read-eval* nil)
                           (*readtable* (if coalton-p (%coalton-readtable) *readtable*)))
                       (read-from-string form-string)))
               (result-values
                 (let ((*standard-output* stdout-capture)
                       (*error-output* stderr-capture)
                       (*trace-output* stderr-capture)
                       (*package* pkg))
                   (multiple-value-list (eval form)))))
          (values
           (%quick-result-payload-string
            result-values
            (concatenate 'string
                         (get-output-stream-string stdout-capture)
                         (get-output-stream-string stderr-capture))
            pkg)
           nil))
      (reader-error (c)
        (values nil (format nil "Read error: ~A" c)))
      (end-of-file (c)
        (declare (ignore c))
        (values nil "Read error: unexpected end of input"))
      (package-error (c)
        (values nil (format nil "Package error: ~A" c)))
      (sb-sys:interactive-interrupt (c)
        (declare (ignore c))
        (values nil "Interrupted."))
      (error (c)
        (values nil (format nil "~A" c))))))

(defun compile-string-source-prefix (package-name)
  "Return the exact source prefix written ahead of debug-compile-string input."
  (let ((pkg (find-or-make-package package-name)))
    (format nil "(in-package ~S)~%" (package-name pkg))))

(defun debug-compile-string (form-string package-name &optional wire-stream msg-id coalton-p)
  "Compile FORM-STRING via compile-file + load for correct eval-when semantics.
Lets errors propagate to the caller's handler-bind (for debugger support).
When WIRE-STREAM and MSG-ID are provided, binds *standard-input*, *query-io*,
and *terminal-io* to a TUI input stream so interactive reads work.
When COALTON-P is true, binds the Coalton readtable during compilation.
Returns (values result-string output-string) on success.
Unlike debug-eval, this preserves toplevel form semantics but does not
return expression values (load returns T)."
  (let* ((pkg (find-or-make-package package-name))
         (file-prefix (compile-string-source-prefix package-name))
         (stdout-capture (make-string-output-stream))
         (stderr-capture (make-string-output-stream))
         (tis (when wire-stream
                (make-instance 'mine/protocol/server::tui-input-stream
                  :wire-stream wire-stream :msg-id msg-id
                  :stdout-capture stdout-capture)))
         (result-values nil))
    (uiop:with-temporary-file (:stream tmp-stream
                                :pathname tmp-path
                                :type (if coalton-p "ct" "lisp")
                                :direction :output)
      (write-string file-prefix tmp-stream)
      (write-string form-string tmp-stream)
      :close-stream
      (let* ((*standard-output* (if tis
                                     stdout-capture
                                     (make-broadcast-stream
                                       *standard-output* stdout-capture)))
              (*error-output* (make-broadcast-stream
                                *error-output* stderr-capture))
              (*trace-output* (make-broadcast-stream
                                *trace-output* stderr-capture))
              (*standard-input* (if tis tis *standard-input*))
              (*query-io* (if tis
                              (make-two-way-stream tis *standard-output*)
                              *query-io*))
              (*terminal-io* (if tis
                                 (make-two-way-stream tis *standard-output*)
                                 *terminal-io*))
              (*package* pkg)
              (*readtable* (if coalton-p (%coalton-readtable) *readtable*)))
        (let ((fasl (compile-file tmp-path)))
          (when fasl
            (unwind-protect
                 (setf result-values (multiple-value-list (load fasl)))
              (ignore-errors (delete-file fasl)))))))
    (let ((all-output (concatenate 'string
                        (get-output-stream-string stdout-capture)
                        (get-output-stream-string stderr-capture))))
      (values (%encode-result-values result-values pkg)
              all-output))))

(defun eval-in-package (form package-name)
  "Evaluate an already-read FORM in the context of PACKAGE-NAME.
Returns (values result-string output-string error-string)."
  (let ((pkg (find-or-make-package package-name)))
    (handler-case
        (let ((stdout-capture (make-string-output-stream))
              (stderr-capture (make-string-output-stream))
              (result-values nil))
          (setf result-values
                (handler-case
                    (let ((*standard-output* (make-broadcast-stream
                                              *standard-output*
                                              stdout-capture))
                          (*error-output* (make-broadcast-stream
                                           *error-output*
                                           stderr-capture))
                          (*package* pkg))
                      (multiple-value-list (eval form)))
                  (error (c)
                    (return-from eval-in-package
                      (values nil
                              (get-output-stream-string stdout-capture)
                              (format nil "~A" c))))))
          (let ((result-string (%encode-result-values result-values pkg)))
            (values result-string
                    (get-output-stream-string stdout-capture)
                    nil)))
      (error (c)
        (values nil nil (format nil "Error: ~A" c))))))
