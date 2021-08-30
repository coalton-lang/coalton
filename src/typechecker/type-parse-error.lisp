(in-package #:coalton-impl/typechecker)

(define-condition coalton-type-parse-error (coalton-parse-error)
  ()
  (:report (lambda (c s)
             (let ((*print-circle* nil)  ; Prevent printing using reader macros
                   (*print-pretty* nil)) ; Prevent newlines in the middle of our lists
               (format s "Failed to parse type ~S because~%~?"
                       (coalton-parse-error-form c)
                       (coalton-parse-error-reason-control c)
                       (coalton-parse-error-reason-args c))))))

(defun error-parsing-type (form reason-control &rest reason-args)
  (error 'coalton-type-parse-error
         :form form
         :reason-control reason-control
         :reason-args reason-args))
