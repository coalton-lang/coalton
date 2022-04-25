(in-package #:coalton-impl)

(defun process-toplevel-specializations (specializations env)
  (declare (type list specializations)
           (type tc:environment env)
           (values tc:specialization-entry-list tc:environment))

  (let ((out-specializations nil))

    (loop :for elem :in specializations
          :do (multiple-value-bind (specialization new-env)
                  (process-specialization elem env)
                (push specialization out-specializations)
                (setf env new-env)))

    (values
     out-specializations
     env)))

(defun process-specialization (elem env)
  (declare (type list elem)
           (type tc:environment env)
           (values tc:specialization-entry tc:environment &optional))
  (unless (= (length elem) 4)
    (error-parsing elem "Malformed specialization"))

  (let* ((from (second elem))

         (to (third elem))

         (type (fourth elem)))

    (unless (and (symbolp from) (symbolp to))
      (error-parsing elem "Malformed specialization"))

    (let* ((from-ty (tc:lookup-value-type env from :no-error t))

           (to-ty (tc:lookup-value-type env to :no-error t))

           (from-name (tc:lookup-name env from))

           (to-name (tc:lookup-name env to))

           (type (tc:fresh-inst (tc:parse-and-resolve-type env type))))

      (unless from-ty
        (error-parsing elem "Unable to specialize unknown function ~A" from))

      (unless to-ty
        (error-parsing elem "Unable to specialize unknown function ~A" to))

      (unless (eq :value (tc:name-entry-type from-name))
        (error-parsing elem "Unable to specialize ~A because it is a ~A" from (tc:name-entry-type from-name)))

      (unless (eq :value (tc:name-entry-type to-name))
        (error-parsing elem "Unable to specialize ~A to ~A because it is a ~A" from to (tc:name-entry-type to-name)))

      (let* ((from-ty (tc:fresh-inst from-ty))

             (to-ty (tc:fresh-inst to-ty)))

        (with-pprint-variable-context ()
          (when (null (tc:qualified-ty-predicates from-ty))
            (error-parsing elem "Unable to specialize function ~A~%of type ~A. ~%Only functions with type class constraints may be specialized." from from-ty)))

        (unless (null (tc:qualified-ty-predicates to-ty))
          (error-parsing elem "Unable to make ~A a specialization target for ~A. ~%Only functions without type class constraints may be specialization targets." to from))

        (with-pprint-variable-context ()
          (unless (null (tc:qualified-ty-predicates type))
            (error-parsing elem "Unable to specialize ~A to type ~A with type class constraints." from type)))

        (with-pprint-variable-context ()
          (unless (tc:type= (tc:qualified-ty-type to-ty) (tc:qualified-ty-type type))
            (error-parsing elem "Function ~A of type ~A~%does not match declared type ~A" to to-ty type)))

        (when (tc:type= (tc:qualified-ty-type from-ty) (tc:qualified-ty-type to-ty))
          (error-parsing elem "Function ~A does not have a more specefic type than ~A" to from))

        (handler-case 
            (tc:match (tc:qualified-ty-type from-ty) (tc:qualified-ty-type to-ty))
          (tc:coalton-type-error (e)
            (declare (ignore e))
            (with-pprint-variable-context ()
              (error-parsing elem "Function ~A of type ~A is not a valid specialization ~%for ~A of type ~A" to to-ty from from-ty))))

        (let ((entry (tc:make-specialization-entry
                      :from from
                      :to to
                      :to-ty (tc:qualified-ty-type to-ty))))
          (values
           entry 
           (tc:add-specialization env entry)))))))
