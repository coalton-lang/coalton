(in-package #:coalton-impl/doc)

;; TODO: remove this
(defun write-predicate-to-markdown (ctx pred)
  ;; TODO: Only link if the thing is part of the package being documented
  (labels ((base-type (type)
             (typecase type
               (coalton-impl/typechecker::tapp
                (base-type (coalton-impl/typechecker::tapp-from type)))
               (t
                type))))
    (let ((class (ty-predicate-class pred))
          (types (ty-predicate-types pred)))
      (format nil "<code>~{~A ~}~:[~;=> ~]<a href=\"#~A\">~A</a>~{ ~A~}</code>"
              (loop :for pred :in ctx
                    :for class := (ty-predicate-class pred)
                    :for types := (ty-predicate-types pred)
                    :collect (format nil "<a href=\"#~A\">~A~{ ~A~}</a>" class class types))
              ctx
              class class
              (loop :for type :in types
                    :for base-type :in (mapcar #'base-type types)
                    :collect (format nil "<a href=\"#~A\">~A</a>" base-type type))))))

;;;
;;; Methods for WRITE-DOCUMENTATION
;;;

;; TODO: Do the prefixing for file links
(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-file-entry))
  (with-slots (filename value-entries type-entries class-entries link-prefix)
      object
    ;; We only want to write documentation when there is documentation to write.
    (when (or value-entries
              type-entries
              class-entries)
      
      (format stream "## File: [~A](~A)~%~%" filename (concatenate 'string link-prefix filename))

      ;; Print type information
      (when type-entries
        (format stream "### Types~%~%")

        (dolist (entry type-entries)
          (write-documentation backend stream entry)
          (format stream "~%***~%~%")))

      ;; Print class information
      (when class-entries
        (format stream "### Classes~%~%")

        (dolist (entry class-entries)
          (write-documentation backend stream entry)
          (format stream "~%***~%~%")))

      ;; Print value information
      (when value-entries
        (format stream "### Functions~%~%")

        (dolist (entry value-entries)
          (write-documentation backend stream entry)
          (format stream "~%***~%~%"))))))

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-type-entry))
  (let ((name (documentation-entry-name object))
        (type (documentation-type-entry-type object))
        (ctors (documentation-type-entry-constructors object))
        (instances (documentation-type-entry-instances object)))
    (with-pprint-variable-context ()
      (let ((type-vars
              (loop :for i :below (coalton-impl/typechecker::kind-arity
                                   (coalton-impl/typechecker::kind-of type))
                    :collect (coalton-impl/typechecker::make-variable))))
        (format stream
                "#### <code>~A~{ ~A~}</code> <sup><sub>[TYPE]</sub></sup><a name=\"~A\"></a>~%"
                name type-vars name)
          
        (loop :for (ctor-name . entry) :in ctors :do
          (let ((args (coalton-impl/typechecker::function-type-arguments
                       (coalton-impl/typechecker::qualified-ty-type
                        (coalton-impl/typechecker::instantiate
                         type-vars
                         (coalton-impl/typechecker::ty-scheme-type
                          (constructor-entry-scheme entry)))))))
            (if args
                (format stream "- <code>(~A~{ ~A~})</code>~%" ctor-name args)
                (format stream "- <code>~A</code>~%" ctor-name))))

        (let ((docs (documentation name 'type)))
          (when docs
            (format stream "~%~A~%" docs)))
        (format stream "~%")

        (format stream "Constructors:~%")
        (loop :for (ctor-name . entry) :in ctors :do
          (format stream "- <code>~A :: ~A</code>~%"
                  ctor-name
                  (coalton-impl/typechecker::instantiate
                   type-vars
                   (coalton-impl/typechecker::ty-scheme-type
                    (constructor-entry-scheme entry)))))
        (format stream "~%")

        (when instances
          (format stream "<details>~%")
          (format stream "<summary>Instances</summary>~%~%")
          (loop :for instance :in instances :do
            (with-pprint-variable-context ()
              (format stream "- ~A~%"
                      (write-predicate-to-markdown
                       (ty-class-instance-constraints instance)
                       (ty-class-instance-predicate instance)))))
          (format stream "~%</details>~%~%"))))))

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-class-entry))
  (with-slots (name context predicate methods instances documentation location)
      object

    (format stream "#### <code>~A</code> <sup><sub>[CLASS]</sub></sup><a name=\"~A\"></a>~%" name name)

    (with-pprint-variable-context ()
      (format stream "~A~%~%"
              (write-predicate-to-markdown context predicate))

      (when documentation
        (format stream "~A~%~%" documentation))

      (format stream "Methods:~%")
      (loop :for (name . type) :in methods :do
        (format stream "- <code>~A :: ~A</code>~%" name type)))

    (when instances
      (format stream "~%<details>~%")
      (format stream "<summary>Instances</summary>~%~%")
      (loop :for instance :in instances :do
        (with-pprint-variable-context ()
          (format stream "- ~A~%"
                  (write-predicate-to-markdown
                   (ty-class-instance-constraints instance)
                   (ty-class-instance-predicate instance)))))
      (format stream "~%</details>~%~%"))))

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-value-entry))
  (with-slots (name type documentation location)
      object

    (format stream "#### <code>~A</code> <sup><sub>[FUNCTION]</sub></sup><a name=\"~A\"></a>~%" name name)
    
    (format stream "<code>~A</code>~%" type)
    
    (when documentation
      (format stream "~%~A~%~%" documentation))))
