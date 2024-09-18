(defpackage #:coalton-impl/typechecker/specialize
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/parse-type)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:toplevel-specialize                ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/specialize)

(defun toplevel-specialize (specializations env)

  (declare (type parser:toplevel-specialize-list specializations)
           (type tc:environment env)
           (values tc:environment))

  (loop :for spec :in specializations :do
    (setf env (process-specialize spec env)))

  env)

(defun process-specialize (specialize env)
  (declare (type parser:toplevel-specialize specialize)
           (type tc:environment env)
           (values tc:environment &optional))

  (let* ((from-name (parser:node-variable-name (parser:toplevel-specialize-from specialize)))
         (to-name (parser:node-variable-name (parser:toplevel-specialize-to specialize)))

         (from-ty (tc:lookup-value-type env from-name :no-error t))
         (to-ty (tc:lookup-value-type env to-name :no-error t))

         (from-name-entry (tc:lookup-name env from-name :no-error t))
         (to-name-entry (tc:lookup-name env to-name :no-error t))

         (type (parse-type (parser:toplevel-specialize-type specialize) env))
         (scheme (tc:quantify (tc:type-variables type)
                              (tc:qualify nil type))))

    (unless from-ty
      (tc-error "Invalid specialization"
                (tc-note (parser:toplevel-specialize-from specialize)
                         "unknown function or variable")))
    (unless to-ty
      (tc-error "Invalid specialization"
                (tc-note (parser:toplevel-specialize-to specialize)
                         "unknown function or variable")))

    (unless (eq :value (tc:name-entry-type from-name-entry))
      (tc-error "Invalid specialization"
                (tc-note (parser:toplevel-specialize-from specialize)
                         "must be a function or variable, not a ~A" (tc:name-entry-type from-name-entry))))
    (unless (eq :value (tc:name-entry-type to-name-entry))
      (tc-error "Invalid specialization"
                (tc-note (parser:toplevel-specialize-to specialize)
                         "must be a function or variable, not a ~A" (tc:name-entry-type from-name-entry))))

    (let ((from-qual-ty (tc:fresh-inst from-ty))
          (to-qual-ty (tc:fresh-inst to-ty)))

      (when (null (tc:qualified-ty-predicates from-qual-ty))
        (tc-error "Invalid specialization"
                  (tc-note (parser:toplevel-specialize-from specialize)
                           "must be a function or variable with class constraints")))

      (unless (equalp to-ty scheme)
        (tc-error "Invalid specialization"
                  (tc-note specialize "function ~S does not match declared type" to-name)))

      (when (equalp from-ty to-ty)
        (tc-error "Invalid specialization"
                  (tc-note specialize "specialize must result in a more specific type")))

      (handler-case
          (tc:match (tc:qualified-ty-type from-qual-ty) (tc:qualified-ty-type to-qual-ty))
        (tc:coalton-internal-type-error ()
          (tc-error "Invalid specialization"
                    (tc-note specialize "cannot specialize to declared type"))))

      (let ((entry (tc:make-specialization-entry :from from-name
                                                 :to to-name
                                                 :to-ty type)))
        (handler-case
            (tc:add-specialization env entry)
          (tc:overlapping-specialization-error (c)
            (tc-error "Overlapping specialization"
                      (tc-note specialize "overlaps with specialization ~S"
                               (tc:overlapping-specialization-error-existing c)))))))))
