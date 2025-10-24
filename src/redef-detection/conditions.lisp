(defpackage #:coalton-impl/redef-detection/conditions
  (:use #:cl)
  (:local-nicknames
   (#:tc-scheme #:coalton-impl/typechecker/scheme)
   (#:compat #:coalton-impl/redef-detection/compatibility))
  (:export
   #:incompatible-redefinition
   #:redefinition-function-name
   #:redefinition-old-type
   #:redefinition-new-type
   #:redefinition-affected-functions
   #:prompt-for-redefinition-action
   #:abort-redefinition))
(in-package #:coalton-impl/redef-detection/conditions)

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
    :type list))
  (:report
   (lambda (condition stream)
     (format stream "Redefining ~A with incompatible type~%"
             (redefinition-function-name condition))
     (format stream "  Old: ~A~%"
             (compat:format-type-for-user (redefinition-old-type condition)))
     (format stream "  New: ~A~%"
             (compat:format-type-for-user (redefinition-new-type condition)))
     (let ((affected (redefinition-affected-functions condition)))
       (when affected
         (format stream "~%  Affected functions (~D):~%"
                 (length affected))
         (dolist (fn affected)
           (format stream "    - ~A~%" fn)))))))

(defun prompt-for-redefinition-action (condition)
  "Prompt user for action when incompatible redefinition detected.
Provides abort-redefinition restart that throws to 'abort-redefinition tag."
  (declare (type incompatible-redefinition condition))

  (restart-case
      (error condition)
    (abort-redefinition ()
      :report "Abort redefinition"
      (throw 'abort-redefinition
             (values :aborted (redefinition-function-name condition))))))
