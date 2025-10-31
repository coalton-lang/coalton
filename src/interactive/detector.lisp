(defpackage #:coalton-impl/interactive/detector
  (:use #:cl)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:redef #:coalton-impl/redef-detection))
  (:export
   #:check-function-redefinition
   #:handle-redefinition-warning))
(in-package #:coalton-impl/interactive/detector)

;;;
;;; Redefinition Detection
;;;

(defun check-function-redefinition (function-name new-type env)
  "Check if redefining FUNCTION-NAME is safe.
Returns:
  NIL if safe (no error needed)
  INCOMPATIBLE-REDEFINITION condition if unsafe"
  (declare (type symbol function-name)
           (type tc:ty-scheme new-type)
           (type tc:environment env)
           (values (or null redef:incompatible-redefinition)))

  (let ((old-type (tc:lookup-value-type env function-name :no-error t)))

    (cond
      ;; New function - always safe
      ((null old-type)
       nil)

      ;; Type unchanged - safe
      ((redef:types-compatible-p old-type new-type env)
       nil)

      ;; Type changed - check for affected functions
      (t
       (let ((affected (redef:find-affected-functions function-name)))
         (make-condition 'redef:incompatible-redefinition
                         :function-name function-name
                         :old-type old-type
                         :new-type new-type
                         :affected-functions affected))))))

(defun handle-redefinition-warning (function-name new-type env)
  "Check for incompatible redefinition and handle user interaction.
Signals error with abort restart if incompatible change detected."
  (declare (type symbol function-name)
           (type tc:ty-scheme new-type)
           (type tc:environment env))

  (let ((condition (check-function-redefinition function-name new-type env)))
    (when condition
      ;; This will signal the error and show abort restart
      (redef:prompt-for-redefinition-action condition))))
