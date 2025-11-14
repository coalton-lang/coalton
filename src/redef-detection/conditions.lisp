(defpackage #:coalton-impl/redef-detection/conditions
  (:use #:cl)
  (:local-nicknames
   (#:tc-scheme #:coalton-impl/typechecker/scheme)
   (#:tc-env #:coalton-impl/typechecker/environment)
   (#:source #:coalton-impl/source)
   (#:deps #:coalton-impl/redef-detection/dependencies))
  (:export
   #:incompatible-redefinition
   #:redefinition-function-name
   #:redefinition-old-type
   #:redefinition-new-type
   #:redefinition-affected-functions
   #:raise-redefinition-error
   #:abort-redefinition
   #:continue-anyway
   #:abort-redef))
(in-package #:coalton-impl/redef-detection/conditions)

;;;
;;; Helper Functions
;;;

(defun format-function-name (function-symbol)
  "Format a function name with its package for error messages."
  (declare (type symbol function-symbol)
           (values string))
  (let ((pkg (symbol-package function-symbol))
        (fn-name (symbol-name function-symbol)))
    (if pkg
        (format nil "~A:~@[:~*~]~A"
                (package-name pkg)
                (eq :internal
                    (nth-value 1
                               (find-symbol fn-name pkg)))
                fn-name)
        ;; Uninterned symbol
        fn-name)))

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
  "Signal an incompatible-redefinition error with the given initargs."

  (restart-case
      (apply #'error 'incompatible-redefinition initargs)
    (abort-redefinition ()
      :report "Abort redefinition"
      (warn "Redefinition of ~A aborted"
            (format-function-name (getf initargs :function-name)))
      (throw 'abort-redef :abort))
    (continue-anyway ()
      :report "Continue anyway"
      (let ((name (getf initargs :function-name))
            (affected (getf initargs :affected-functions)))
        (warn "Continuing with incompatible redefinition of ~A (~D affected function~:P)"
              (format-function-name name)
              (length affected))))))
