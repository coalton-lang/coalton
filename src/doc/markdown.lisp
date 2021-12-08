(in-package #:coalton-impl/doc)

(defgeneric to-markdown (object)

  ;;
  ;; Type printing (modification of pprint-type)
  ;;

  (:method ((ty coalton-impl/typechecker::tvar))
    (with-output-to-string (stream)
      (coalton-impl/typechecker::pprint-ty stream (coalton-impl/typechecker::pprint-tvar ty))))

  (:method ((ty coalton-impl/typechecker::tcon))
    (let ((tcon-name (coalton-impl/typechecker::tycon-name (coalton-impl/typechecker::tcon-tycon ty))))
      (if (string= "KEYWORD" (package-name (symbol-package tcon-name)))
          (format nil "~S" tcon-name)
          (format nil "<a href=\"#~(~A-type~)\">~:*~A</a>" tcon-name))))

  (:method ((ty coalton-impl/typechecker::tapp))
    (with-output-to-string (stream)
      (cond
        ((function-type-p ty) ;; Print function types
         (write-string "(" stream)
         (write-string (to-markdown (coalton-impl/typechecker::tapp-to (coalton-impl/typechecker::tapp-from ty)))
                       stream)
         (write-string (if *coalton-print-unicode*
                           " → "
                           " -> ")
                       stream)
         ;; Avoid printing extra parenthesis on curried functions
         (labels ((print-subfunction (to)
                    (cond
                      ((function-type-p to)
                       (write-string
                        (to-markdown (coalton-impl/typechecker::tapp-to (coalton-impl/typechecker::tapp-from to)))
                        stream)
                       (write-string (if *coalton-print-unicode*
                                         " → "
                                         " -> ")
                                     stream)
                       (print-subfunction (coalton-impl/typechecker::tapp-to to)))
                      (t
                       (write-string (to-markdown to) stream)))))
           (print-subfunction (coalton-impl/typechecker::tapp-to ty)))
         (write-string ")" stream))
        (t ;; Print type constructors
         (let* ((tcon ty)
                (tcon-args (loop :while (coalton-impl/typechecker::tapp-p tcon)
                                 :collect (coalton-impl/typechecker::tapp-to tcon)
                                 :do (setf tcon (coalton-impl/typechecker::tapp-from tcon)))))
           (cond
             ((and (coalton-impl/typechecker::tcon-p tcon)
                   (coalton-impl/typechecker::simple-kind-p
                    (coalton-impl/typechecker::tycon-kind (coalton-impl/typechecker::tcon-tycon tcon)))
                   (<= (length tcon-args)
                       (coalton-impl/typechecker::kind-arity
                        (coalton-impl/typechecker::tycon-kind (coalton-impl/typechecker::tcon-tycon tcon)))))
              (write-string "(" stream)
              (write-string (to-markdown tcon) stream)
              (dolist (arg (reverse tcon-args))
                (write-string " " stream)
                (write-string (to-markdown arg) stream))
              (write-string ")" stream))
             (t
              (write-string "(" stream)
              (write-string (to-markdown (coalton-impl/typechecker::tapp-from ty)) stream)
              (write-string " " stream)
              (write-string (to-markdown (coalton-impl/typechecker::tapp-to ty)) stream)
              (write-string ")" stream))))))))

  (:method ((object qualified-ty))
    (let ((preds (coalton-impl/typechecker::qualified-ty-predicates object))
          (qual-type (coalton-impl/typechecker::qualified-ty-type object)))
      (format nil "~:[~{~A ~}~;~{(~A) ~}~]~:*~:[~*~;~A ~]~A"
              ;; Get the second element to test if we have more than one predicate
              (second preds)
              (mapcar #'to-markdown preds)
              (if *coalton-print-unicode*
                          "⇒"
                          "=>")
              (to-markdown qual-type))))

  (:method ((object ty-scheme))
    (cond
    ((null (coalton-impl/typechecker::ty-scheme-kinds object))
     (to-markdown (coalton-impl/typechecker::ty-scheme-type object)))
    (t
     (with-pprint-variable-scope ()
       (let* ((types (mapcar (lambda (k) (coalton-impl/typechecker::next-pprint-variable-as-tvar k))
                             (coalton-impl/typechecker::ty-scheme-kinds object)))
              (new-type (coalton-impl/typechecker::instantiate
                         types (coalton-impl/typechecker::ty-scheme-type object))))
         (format nil "~A~{ ~A~}. ~A"
                 (if *coalton-print-unicode*
                     "∀"
                     "FORALL")
                 types
                 (to-markdown new-type)))))))

  (:method ((object ty-predicate))
    (format nil "<a href=\"#~(~A-class~)\">~:*~A</a>~{ ~A~}"
            (ty-predicate-class object)
            (mapcar #'to-markdown (ty-predicate-types object))))

  (:method ((object ty-class-instance))
    (let ((ctx (ty-class-instance-constraints object))
          (pred (ty-class-instance-predicate object)))
      (format nil "~:[~{~A ~}~;~{(~A) ~}~]~:*~:[~*~;~A ~]~A"
              ;; Get the second element to test if we have more than one predicate
              (second ctx)
              (mapcar #'to-markdown ctx)
              (if *coalton-print-unicode*
                          "⇒"
                          "=>")
              (to-markdown pred)))))

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

      (unless (string= "" filename)
        (format stream "## File: [~A](~A)~%~%" filename (concatenate 'string link-prefix filename)))

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
                "#### <code>~A~{ ~A~}</code> <sup><sub>[TYPE]</sub></sup><a name=\"~(~A-type~)\"></a>~%"
                name type-vars name)

        (loop :for (ctor-name . entry) :in ctors :do
          (let ((args (coalton-impl/typechecker::function-type-arguments
                       (coalton-impl/typechecker::qualified-ty-type
                        (coalton-impl/typechecker::instantiate
                         type-vars
                         (coalton-impl/typechecker::ty-scheme-type
                          (constructor-entry-scheme entry)))))))
            (if args
                (format stream "- <code>(~A~{ ~A~})</code>~%" ctor-name (mapcar #'to-markdown args))
                (format stream "- <code>~A</code>~%" ctor-name))))

        (let ((docs (documentation name 'type)))
          (when docs
            (format stream "~%~A~%" docs)))
        (format stream "~%")

        (when ctors
          (format stream "Constructors:~%"))
        (loop :for (ctor-name . entry) :in ctors :do
          (format stream "- <code>~A :: ~A</code>~%"
                  ctor-name
                  (to-markdown
                   (coalton-impl/typechecker::instantiate
                    type-vars
                    (coalton-impl/typechecker::ty-scheme-type
                     (constructor-entry-scheme entry))))))
        (format stream "~%")

        (when instances
          (format stream "<details>~%")
          (format stream "<summary>Instances</summary>~%~%")
          (loop :for instance :in instances :do
            (with-pprint-variable-context ()
              (format stream "- <code>~A</code>~%" (to-markdown instance))))
          (format stream "~%</details>~%~%"))))))

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-class-entry))
  (with-slots (name context predicate methods instances documentation location)
      object

    (format stream "#### <code>~A</code> <sup><sub>[CLASS]</sub></sup><a name=\"~(~A-class~)\"></a>~%"
            name name)

    (with-pprint-variable-context ()
      (format stream "<code>~A</code>~%~%" (to-markdown (ty-class-instance context predicate nil)))

      (when documentation
        (format stream "~A~%~%" documentation))

      (format stream "Methods:~%")
      (loop :for (name . type) :in methods :do
        (format stream "- <code>~A :: ~A</code>~%" name (to-markdown type))))

    (when instances
      (format stream "~%<details>~%")
      (format stream "<summary>Instances</summary>~%~%")
      (loop :for instance :in instances :do
        (with-pprint-variable-context ()
          (format stream "- <code>~A</code>~%" (to-markdown instance))))
      (format stream "~%</details>~%~%"))))

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-value-entry))
  (with-slots (name type documentation location)
      object

    (format stream "#### <code>~A</code> <sup><sub>[FUNCTION]</sub></sup><a name=\"~:*~(~A-function~)\"></a>~%" name)

    (with-pprint-variable-context ()
      (format stream "<code>~A</code>~%" (to-markdown type)))

    (when documentation
      (format stream "~%~A~%~%" documentation))))
