(defpackage #:coalton-impl/redef-detection/conditions
  (:use #:cl)
  (:local-nicknames
   (#:tc-scheme #:coalton-impl/typechecker/scheme)
   (#:tc-env #:coalton-impl/typechecker/environment)
   (#:settings #:coalton-impl/settings)
   (#:source #:coalton-impl/source)
   (#:deps #:coalton-impl/redef-detection/dependencies))
  (:export
   #:incompatible-redefinition
   #:incompatible-gadt-redefinition
   #:redefinition-function-name
   #:redefinition-old-type
   #:redefinition-new-type
   #:redefinition-affected-functions
   #:raise-redefinition-error
   #:raise-gadt-redefinition-error
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
    :type (or tc-scheme:ty-scheme list))
   (new-type
    :initarg :new-type
    :reader redefinition-new-type
    :type (or tc-scheme:ty-scheme list))
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
     (report-affected-functions condition stream "Affected functions"))))

(defun report-affected-functions (condition stream label)
  (let ((affected (redefinition-affected-functions condition))
        (env (redefinition-environment condition)))
    (when affected
      (format stream "~%  ~A (~D):~%" label (length affected))
      (dolist (fn affected)
        (let ((location (deps:get-function-location fn env)))
          (if location
              (format stream "    - ~A (~A)~%" (format-function-name fn) (format-location location))
              (format stream "    - ~A~%" (format-function-name fn))))))))

(define-condition incompatible-gadt-redefinition (incompatible-redefinition) ()
  (:report
   (lambda (condition stream)
     (format stream "Redefining ~A invalidates GADT constructor evidence.~%"
             (format-function-name (redefinition-function-name condition)))
     (flet ((report-contract (label contract)
              (format stream "  ~A (~S):~{~%    ~{~S: ~A~}~}~%"
                      label (car contract) (cdr contract))))
       (report-contract "Old representation and constructors" (redefinition-old-type condition))
       (report-contract "New representation and constructors" (redefinition-new-type condition)))
     (report-affected-functions condition stream "Known constructor dependents")
     (format stream "Existing values and compiled matches can still rely on the old definition. ~
Continuing does not update them."))))

(defun raise-gadt-redefinition-error (name old new env)
  ;; The function dependency registry does not track live inhabitants, or
  ;; exhaustiveness decisions made from a constructor's *absence*.
  (let* ((dependencies (remove-duplicates
                        (mapcan (lambda (entry) (deps:find-affected-functions (first entry)))
                                (rest old))))
         (condition (make-condition 'incompatible-gadt-redefinition
                                    :function-name name :old-type old :new-type new
                                    :affected-functions dependencies :environment env)))
    (flet ((continue-with-warning ()
             (warn "Continuing despite an incompatible redefinition.~%~A" condition)))
      (restart-case
          (if settings:*auto-continue-redefinition*
              (continue-with-warning)
              (error condition))
        (abort-redefinition ()
          :report "Abort this compilation or load and keep the previous definition"
          ;; Do not just skip the environment update: the rest of a FASL
          ;; contains code compiled against the incompatible definition.
          (let ((abort (find-restart 'abort)))
            (when abort (invoke-restart abort))
            (error "Aborted incompatible redefinition of ~S." name)))
        (continue-anyway ()
          :report "Continue anyway without updating existing values or compiled matches"
          (continue-with-warning))))))

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
