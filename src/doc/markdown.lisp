(in-package #:coalton-impl/doc)

;;
;; The format for the output markdown document is as follows
;;
;; # Reference for ${package}
;; ## File ${filename}                  ; NOTE: We include the empty filename
;;                                      ;       first to emit early types.
;; ### Types
;; #### ${type-name} [TYPE]
;;   ${constructors}
;;
;;   ${docstring}
;;
;;   Instances:                         ; Dropdown hidden by default
;;   ${instances}
;; 
;; ### Classes
;; #### ${class-name} [CLASS]
;;   ${predicate}
;;
;;   ${docstring}
;;
;;   Methods:
;;   - ${method-name} :: ${method-type}
;;
;;   Instances:                         ; Dropdown hidden by default
;;   ${instances}
;;
;; ### Values
;; #### ${value-name} [VALUE|FUNCTION]
;;   ${type}
;;
;;   ${docstring}
;;
;;
;;
;; The following format is used for links to objects in documentation:
;;
;; Package: `${name}-package`
;; File: `${package}-${name}-file`
;;   where `name` is the filepath relative to the provided asdf system
;;   with all non-alphanumeric characters replaced with `-`.
;; Type: `${name}-type`
;; Class: `${name}-class`
;; Value: `${name}-value`
;;

;;;
;;; Methods for WRITE-DOCUMENTATION
;;;

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-package-entries))
  (with-slots (packages asdf-system documentation-by-package) object
    (dolist (package packages)
      (let ((docs-for-package (gethash package documentation-by-package)))
        (when docs-for-package
          (write-documentation backend stream
                               docs-for-package))))))

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-package-entry))
  (let* ((file-entries (documentation-package-entry-documentation-entries-by-file object))
         (valid-files (documentation-package-entry-valid-files object))
         (package (documentation-package-entry-package object)))
    (format stream "# Package `~(~A~)`<a name=\"~:*~(~A-package~)\"></a>~%~%" package)

    ;; NOTE: We are including the empty filename here to allow for
    ;;       symbols without file information to be included.
    (dolist (pathname (append '("") valid-files))
      (let ((file-entry (gethash pathname file-entries)))
        (when file-entry
          (write-documentation :markdown stream file-entry))))))

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-file-entry))
  (with-slots (filename package value-entries type-entries class-entries link-prefix)
      object
    ;; We only want to write documentation when there is documentation to write.
    (when (or value-entries
              type-entries
              class-entries)

      (unless (string= "" filename)
        (format stream "## [~A](~A) <a name=\"~(~A-~A-file~)\"></a>~%~%"
                (html-entities:encode-entities filename)
                (concatenate 'string link-prefix filename)
                package
                (cl-ppcre:regex-replace-all "[^a-zA-Z\d\s:]" filename "-")))

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
        (format stream "### Values~%~%")

        (dolist (entry value-entries)
          (write-documentation backend stream entry)
          (format stream "~%***~%~%"))))))

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-type-entry))
  (with-slots (name type constructors constructor-types instances documentation)
      object
    (let ((type-vars
            (loop :for i :below (coalton-impl/typechecker::kind-arity
                                 (coalton-impl/typechecker::kind-of type))
                  :collect (coalton-impl/typechecker::make-variable))))
      (with-pprint-variable-context ()
        (format stream
                "#### <code>~A~A</code> <sup><sub>[TYPE]</sub></sup><a name=\"~(~A-type~)\"></a>~%"
                (html-entities:encode-entities (symbol-name name))
                (html-entities:encode-entities (format nil "~{ ~A~}" type-vars))
                (html-entities:encode-entities (symbol-name name)))

        (loop :for (ctor-name . entry) :in constructors
              :for ctor-type :in constructor-types :do
                (let ((args (coalton-impl/typechecker::function-type-arguments
                             (coalton-impl/typechecker::qualified-ty-type
                              (coalton-impl/typechecker::instantiate
                               type-vars
                               (coalton-impl/typechecker::ty-scheme-type
                                ctor-type))))))
                  (if args
                      (format stream "- <code>(~A~{ ~A~})</code>~%"
                              (html-entities:encode-entities (symbol-name ctor-name))
                              (mapcar #'to-markdown args))
                      (format stream "- <code>~A</code>~%" (html-entities:encode-entities (symbol-name ctor-name))))))

        (when documentation
          (format stream "~%~A~%"
                  (html-entities:encode-entities
                   documentation
                   :regex "[<>&]")))
        (format stream "~%")

        (when constructors
          (format stream "Constructors:~%"))
        (loop :for (ctor-name . entry) :in constructors
              :for ctor-type :in constructor-types :do
                (format stream "- <code>~A :: ~A</code>~%"
                        (html-entities:encode-entities (symbol-name ctor-name))
                        (to-markdown
                         (coalton-impl/typechecker::instantiate
                          type-vars
                          (coalton-impl/typechecker::ty-scheme-type
                           ctor-type)))))
        (format stream "~%"))

      (when instances
        (format stream "<details>~%")
        (format stream "<summary>Instances</summary>~%~%")
        (loop :for instance :in instances :do
          (with-pprint-variable-context ()
            (format stream "- <code>~A</code>~%" (to-markdown instance))))
        (format stream "~%</details>~%~%")))))

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-class-entry))
  (with-slots (name context predicate methods instances documentation location)
      object

    (format stream "#### <code>~A</code> <sup><sub>[CLASS]</sub></sup><a name=\"~(~:*~A-class~)\"></a>~%"
            (html-entities:encode-entities (symbol-name name)))

    (with-pprint-variable-context ()
      (format stream "<code>~A</code>~%~%" (to-markdown (ty-class-instance context predicate nil (make-hash-table))))

      (when documentation
        (format stream "~A~%~%"
                (html-entities:encode-entities
                 documentation
                 :regex "[<>&]")))

      (format stream "Methods:~%")
      (loop :for (name . type) :in methods :do
        (format stream "- <code>~A :: ~A</code>~%"
                (html-entities:encode-entities (symbol-name name))
                (to-markdown type))))

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

    (let ((function-type? (function-type-p type)))
      (format stream "#### <code>~A</code> <sup><sub>[~:[VALUE~;FUNCTION~]~:*]</sub></sup><a name=\"~:*~(~A-value~)\"></a>~%"
              (html-entities:encode-entities (symbol-name name))
              function-type?)

      (with-pprint-variable-context ()
        (format stream "<code>~A</code>~%" (to-markdown type)))

      (when documentation
        (format stream "~%~A~%~%"
                (html-entities:encode-entities
                 documentation
                 :regex "[<>&]"))))))


;;;
;;; Methods for TO-MARKDOWN
;;;

(defgeneric to-markdown (object)

  ;;
  ;; Type printing (modification of pprint-type)
  ;;

  (:method ((ty coalton-impl/typechecker::tvar))
    (html-entities:encode-entities
     (with-output-to-string (stream)
       (coalton-impl/typechecker::pprint-ty stream (coalton-impl/typechecker::pprint-tvar ty)))))

  (:method ((ty coalton-impl/typechecker::tcon))
    (let ((tcon-name (coalton-impl/typechecker::tycon-name (coalton-impl/typechecker::tcon-tycon ty))))
      (if (string= "KEYWORD" (package-name (symbol-package tcon-name)))
          (html-entities:encode-entities (format nil "~S" tcon-name))
          (format nil "<a href=\"#~(~A-type~)\">~:*~A</a>" (html-entities:encode-entities (symbol-name tcon-name))))))

  (:method ((ty coalton-impl/typechecker::tapp))
    (with-output-to-string (stream)
      (cond
        ((function-type-p ty) ;; Print function types
         (write-string "(" stream)
         (write-string (to-markdown (coalton-impl/typechecker::tapp-to (coalton-impl/typechecker::tapp-from ty)))
                       stream)
         (write-string (html-entities:encode-entities
                        (if *coalton-print-unicode*
                            " → "
                            " -> "))
                       stream)
         ;; Avoid printing extra parenthesis on curried functions
         (labels ((print-subfunction (to)
                    (cond
                      ((function-type-p to)
                       (write-string
                        (to-markdown (coalton-impl/typechecker::tapp-to (coalton-impl/typechecker::tapp-from to)))
                        stream)
                       (write-string (html-entities:encode-entities
                                      (if *coalton-print-unicode*
                                          " → "
                                          " -> "))
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
              (html-entities:encode-entities
               (if *coalton-print-unicode*
                   "⇒"
                   "=>"))
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
           (format nil "~A~A. ~A"
                   (html-entities:encode-entities
                    (if *coalton-print-unicode*
                        "∀"
                        "FORALL"))
                   (html-entities:encode-entities (format nil "~{ ~A~}" types))
                   (to-markdown new-type)))))))

  (:method ((object ty-predicate))
    (format nil "<a href=\"#~(~A-class~)\">~:*~A</a>~{ ~A~}"
            (html-entities:encode-entities (symbol-name (ty-predicate-class object)))
            (mapcar #'to-markdown (ty-predicate-types object))))

  (:method ((object ty-class-instance))
    (let ((ctx (ty-class-instance-constraints object))
          (pred (ty-class-instance-predicate object)))
      (format nil "~:[~{~A ~}~;~{(~A) ~}~]~:*~:[~*~;~A ~]~A"
              ;; Get the second element to test if we have more than one predicate
              (second ctx)
              (mapcar #'to-markdown ctx)
              (html-entities:encode-entities
               (if *coalton-print-unicode*
                   "⇒"
                   "=>"))
              (to-markdown pred)))))
