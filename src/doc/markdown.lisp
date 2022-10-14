(in-package #:coalton-doc)

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
  (with-slots (packages asdf-system by-package) object
    (dolist (package packages)
      (let ((docs-for-package (gethash package by-package)))
        (when docs-for-package
          (write-documentation backend stream
                               docs-for-package))))))

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-package-entry))
  (let* ((file-entries (documentation-package-entry-entries object))
         (package (documentation-package-entry-package object)))
    (format stream "# Package `~(~A~)`<a name=\"~:*~(~A-package~)\"></a>~%~%" package)

    (when (documentation-package-entry-documentation object)
      (format stream "~A~%~%"
              (documentation-package-entry-documentation object)))

    (dolist (pathname (alexandria:hash-table-keys file-entries))
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
            (loop :for i :below (tc:kind-arity
                                 (tc:kind-of type))
                  :collect (tc:make-variable))))
      (tc:with-pprint-variable-context ()
        (format stream
                "#### <code>~A~A</code> <sup><sub>[TYPE]</sub></sup><a name=\"~(~A-type~)\"></a>~%"
                (html-entities:encode-entities (symbol-name name))
                (html-entities:encode-entities (format nil "~{ ~S~}" type-vars))
                (html-entities:encode-entities (symbol-name name)))

        (loop :for (ctor-name . entry) :in constructors
              :for ctor-type :in constructor-types :do
                (let ((args (tc:function-type-arguments
                             (tc:qualified-ty-type
                              (tc:instantiate
                               type-vars
                               (tc:ty-scheme-type
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
        (format stream "~%"))

      (when instances
        (format stream "<details>~%")
        (format stream "<summary>Instances</summary>~%~%")
        (loop :for instance :in instances :do
          (tc:with-pprint-variable-context ()
            (format stream "- <code>~A</code>~%" (to-markdown instance))))
        (format stream "~%</details>~%~%")))))

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-class-entry))
  (with-slots (name context predicate methods instances documentation location)
      object

    (format stream "#### <code>~A</code> <sup><sub>[CLASS]</sub></sup><a name=\"~(~:*~A-class~)\"></a>~%"
            (html-entities:encode-entities (symbol-name name)))

    (tc:with-pprint-variable-context ()
      (format stream "<code>~A</code>~%~%"
              (to-markdown
               (tc:make-ty-class-instance
                :constraints context
                :predicate predicate
                :codegen-sym nil
                :method-codegen-syms (make-hash-table))))

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
        (tc:with-pprint-variable-context ()
          (format stream "- <code>~A</code>~%" (to-markdown instance))))
      (format stream "~%</details>~%~%"))))

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-function-entry))
  (with-slots (name type documentation location param-names)
      object
    (let ((encoded-name (html-entities:encode-entities (symbol-name name))))
      (format stream "#### <code>(~A~{ ~A~})</code> <sup><sub>FUNCTION</sub></sup><a name=\"~(~A-value~)\"></a>~%"
              encoded-name
              param-names
              encoded-name)

      (tc:with-pprint-variable-context ()
        (format stream "<code>~A</code>~%" (to-markdown type)))

      (when documentation
        (format stream "~%~A~%~%"
                (html-entities:encode-entities
                 documentation
                 :regex "[<>&]"))))))

(defmethod write-documentation ((backend (eql ':markdown)) stream (object documentation-value-entry))
  (with-slots (name type documentation location)
      object

    (let ((function-type? (tc:function-type-p type)))
      (format stream "#### <code>~A</code> <sup><sub>[~:[VALUE~;FUNCTION~]~:*]</sub></sup><a name=\"~:*~(~A-value~)\"></a>~%"
              (html-entities:encode-entities (symbol-name name))
              function-type?)

      (tc:with-pprint-variable-context ()
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

  (:method ((ty tc:tyvar))
    (html-entities:encode-entities
     (with-output-to-string (stream)
       (tc:pprint-ty stream (tc:pprint-tvar ty)))))

  (:method ((ty tc:tycon))
    (let ((tcon-name (tc:tycon-name ty)))
      (if (string= "KEYWORD" (package-name (symbol-package tcon-name)))
          (html-entities:encode-entities (format nil "~S" tcon-name))
          (format nil "<a href=\"#~(~A-type~)\">~:*~A</a>" (html-entities:encode-entities (symbol-name tcon-name))))))

  (:method ((ty tc:tapp))
    (with-output-to-string (stream)
      (cond
        ((tc:function-type-p ty) ;; Print function types
         (write-string "(" stream)
         (write-string (to-markdown (tc:tapp-to (tc:tapp-from ty)))
                       stream)
         (write-string (html-entities:encode-entities
                        (if settings:*coalton-print-unicode*
                            " → "
                            " -> "))
                       stream)
         ;; Avoid printing extra parenthesis on curried functions
         (labels ((print-subfunction (to)
                    (cond
                      ((tc:function-type-p to)
                       (write-string
                        (to-markdown (tc:tapp-to (tc:tapp-from to)))
                        stream)
                       (write-string (html-entities:encode-entities
                                      (if settings:*coalton-print-unicode*
                                          " → "
                                          " -> "))
                                     stream)
                       (print-subfunction (tc:tapp-to to)))
                      (t
                       (write-string (to-markdown to) stream)))))
           (print-subfunction (tc:tapp-to ty)))
         (write-string ")" stream))
        (t ;; Print type constructors
         (let* ((tcon ty)
                (tcon-args (loop :while (tc:tapp-p tcon)
                                 :collect (tc:tapp-to tcon)
                                 :do (setf tcon (tc:tapp-from tcon)))))
           (cond
             ((and (tc:tycon-p tcon)
                   (tc:simple-kind-p
                    (tc:tycon-kind tcon))
                   (<= (length tcon-args)
                       (tc:kind-arity
                        (tc:tycon-kind tcon))))
              (write-string "(" stream)
              (write-string (to-markdown tcon) stream)
              (dolist (arg (reverse tcon-args))
                (write-string " " stream)
                (write-string (to-markdown arg) stream))
              (write-string ")" stream))
             (t
              (write-string "(" stream)
              (write-string (to-markdown (tc:tapp-from ty)) stream)
              (write-string " " stream)
              (write-string (to-markdown (tc:tapp-to ty)) stream)
              (write-string ")" stream))))))))

  (:method ((object tc:qualified-ty))
    (let ((preds (tc:qualified-ty-predicates object))
          (qual-type (tc:qualified-ty-type object)))
      (format nil "~:[~{~A ~}~;~{(~A) ~}~]~:*~:[~*~;~A ~]~A"
              ;; Get the second element to test if we have more than one predicate
              (second preds)
              (mapcar #'to-markdown preds)
              (html-entities:encode-entities
               (if settings:*coalton-print-unicode*
                   "⇒"
                   "=>"))
              (to-markdown qual-type))))

  (:method ((object tc:ty-scheme))
    (cond
      ((null (tc:ty-scheme-kinds object))
       (to-markdown (tc:ty-scheme-type object)))
      (t
       (tc:with-pprint-variable-scope ()
         (let* ((types (mapcar (lambda (k) (tc:next-pprint-variable-as-tvar k))
                               (tc:ty-scheme-kinds object)))
                (new-type (tc:instantiate
                           types (tc:ty-scheme-type object))))
           (format nil "~A~A. ~A"
                   (html-entities:encode-entities
                    (if settings:*coalton-print-unicode*
                        "∀"
                        "FORALL"))
                   (html-entities:encode-entities (format nil "~{ ~S~}" types))
                   (to-markdown new-type)))))))

  (:method ((object tc:ty-predicate))
    (format nil "<a href=\"#~(~A-class~)\">~:*~A</a>~{ ~A~}"
            (html-entities:encode-entities (symbol-name (tc:ty-predicate-class object)))
            (mapcar #'to-markdown (tc:ty-predicate-types object))))

  (:method ((object tc:ty-class-instance))
    (let ((ctx (tc:ty-class-instance-constraints object))
          (pred (tc:ty-class-instance-predicate object)))
      (format nil "~:[~{~A ~}~;~{(~A) ~}~]~:*~:[~*~;~A ~]~A"
              ;; Get the second element to test if we have more than one predicate
              (second ctx)
              (mapcar #'to-markdown ctx)
              (html-entities:encode-entities
               (if settings:*coalton-print-unicode*
                   "⇒"
                   "=>"))
              (to-markdown pred)))))
