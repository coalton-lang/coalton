;;;; asdf.lisp -- ASDF integration for the runtime server.
;;;;
;;;; Loads systems, queries dependencies, and enumerates source files.

(in-package #:mine/runtime/asdf)

;;; Public API

(defun load-system (system-name)
  "Load the ASDF system named SYSTEM-NAME.
Returns T on success, or (values NIL error-message) on failure."
  (handler-case
      (progn
        (asdf:load-system system-name)
        t)
    (error (c)
      (values nil (format nil "~A" c)))))

(defun beam-system (system-name &optional asd-path)
  "Load ASD-PATH and SYSTEM-NAME, allowing the caller's debugger to handle errors."
  (when (and (stringp asd-path) (plusp (length asd-path)))
    (asdf:load-asd (truename asd-path)))
  (asdf:load-system system-name)
  t)
