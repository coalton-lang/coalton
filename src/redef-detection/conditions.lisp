(defpackage #:coalton-impl/redef-detection/conditions
  (:use #:cl)
  (:local-nicknames
   (#:tc-scheme #:coalton-impl/typechecker/scheme)
   (#:tc-env #:coalton-impl/typechecker/environment)
   (#:source #:coalton-impl/source)
   (#:deps #:coalton-impl/redef-detection/dependencies)
   (#:compat #:coalton-impl/redef-detection/compatibility))
  (:export
   #:incompatible-redefinition
   #:redefinition-function-name
   #:redefinition-old-type
   #:redefinition-new-type
   #:redefinition-affected-functions
   #:raise-redefinition-error
   #:abort-redefinition))
(in-package #:coalton-impl/redef-detection/conditions)

;;;
;;; Helper Functions
;;;

(defun format-function-name (function-name)
  "Format a function name with its package for error messages."
  (declare (type symbol function-name)
           (values string))
  (let ((pkg (symbol-package function-name)))
    (if pkg
        (format nil "~A:~A"
                (package-name pkg)
                (symbol-name function-name))
        ;; Uninterned symbol
        (symbol-name function-name))))

(defun format-location (location)
  "Format a source location compactly for error messages."
  (declare (type source:location location)
           (values string))
  (let* ((src (source:location-source location))
         (span (source:location-span location))
         (start (source:span-start span))
         (name (source:source-name src)))
    ;; For REPL/macro-expanded code, just show <interactive>
    ;; For file-based code, show filename:offset
    (if (string= name "<macroexpansion>")
        "<interactive>"
        (format nil "~A:~D" name start))))

;;;
;;; Conditions
;;;

(define-condition incompatible-redefinition (error)
  ((function-name
    :initarg :function-name
    :reader redefinition-function-name
    :type symbol)
   (old-type
    :initarg :old-type
    :reader redefinition-old-type
    :type tc-scheme:ty-scheme)
   (new-type
    :initarg :new-type
    :reader redefinition-new-type
    :type tc-scheme:ty-scheme)
   (affected-functions
    :initarg :affected-functions
    :reader redefinition-affected-functions
    :type list)
   (environment
    :initarg :environment
    :reader redefinition-environment
    :type tc-env:environment))
  (:report
   (lambda (condition stream)
     (format stream "Redefining ~A with incompatible type~%"
             (format-function-name (redefinition-function-name condition)))
     (format stream "  Old: ~A~%"
             (redefinition-old-type condition))
     (format stream "  New: ~A~%"
             (redefinition-new-type condition))
     (let ((affected (redefinition-affected-functions condition))
           (env (redefinition-environment condition)))
       (when affected
         (format stream "~%  Affected functions (~D):~%"
                 (length affected))
         (dolist (fn affected)
           (let ((location (deps:get-function-location fn env)))
             (if location
                 (format stream "    - ~A (~A)~%"
                         (format-function-name fn)
                         (format-location location))
                 (format stream "    - ~A~%" (format-function-name fn))))))))))

(defun raise-redefinition-error (&rest initargs)
  "Signal an incompatible-redefinition error with the given initargs.
Provides abort-redefinition restart that throws to 'abort-redefinition tag."

  (restart-case
      (apply #'error 'incompatible-redefinition initargs)
    (abort-redefinition ()
      :report "Abort redefinition"
      (throw 'abort-redefinition
             (values :aborted (getf initargs :function-name))))))
