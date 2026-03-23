
;;;; The format for the output markdown document is:
;;;;
;;;; # Reference for ${package}
;;;; ## File ${filename}                  ; NOTE: We include the empty filename
;;;;                                      ;       first to emit early types.
;;;; ### Types
;;;; #### ${type-name} [TYPE]
;;;;   ${constructors}
;;;;
;;;;   ${docstring}
;;;;
;;;;   Instances:                         ; Dropdown hidden by default
;;;;   ${instances}
;;;;
;;;; ### Classes
;;;; #### ${class-name} [CLASS]
;;;;   ${predicate}
;;;;
;;;;   ${docstring}
;;;;
;;;;   Methods:
;;;;   - ${method-name} :: ${method-type}
;;;;
;;;;   Instances:                         ; Dropdown hidden by default
;;;;   ${instances}
;;;;
;;;; ### Values
;;;; #### ${value-name} [VALUE|FUNCTION]
;;;;   ${type}
;;;;
;;;;   ${docstring}
;;;;
;;;;
;;;;
;;;; The following format is used for links to objects in documentation:
;;;;
;;;; Package: `${sanitized-package-name}-package`
;;;; File: `${package}-${name}-file`
;;;;   where `name` is the filepath relative to the provided asdf system
;;;;   with all non-alphanumeric characters replaced with `-`.
;;;; Type: `${sanitized-package-name}-${name}-type`
;;;; Class: `${sanitized-package-name}-${name}-class`
;;;; Value: `${sanitized-package-name}-${name}-value`

(defpackage #:coalton/doc/markdown
  (:use
   #:cl
   #:coalton/doc/base
   #:coalton/doc/model)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton/doc/markdown)

(defgeneric to-markdown (object)
  (:documentation "Return the Markdown representation of object, including A NAME anchors."))

(defclass markdown-backend ()
  ((stream :initarg :stream
           :reader output-stream)
   (file-line-offsets :initform (make-hash-table :test 'equal)))
  (:documentation "A documentation output backend that writes the contents of Coalton packages in Markdown format."))

(register-backend :markdown 'markdown-backend)

(defmethod write-packages ((backend markdown-backend) packages)
  (dolist (package packages)
    (write-object backend package)))

;; File linking and conversion between character and line offsets.

(defun file-hygienic-ref (file)
  (cl-ppcre:regex-replace-all "[^a-zA-Z\d\s:]" file "-"))

(defun find-line-offsets (stream)
  "Compute the offsets of lines in a stream."
  (file-position stream 0)
  (loop :with index := 0
        :for char := (read-char stream nil nil)
        :unless char
          :return (coerce (cons 0 offsets) 'vector)
        :when (char= char #\Newline)
          :collect (1+ index) :into offsets
        :do (incf index)))

(defmethod write-object ((backend markdown-backend) (object coalton-package))
  (let ((stream (output-stream backend))
        (objects (package-objects object)))
    (format stream "# Package `~A`~A~%~%" (object-name object) (object-anchor object))
    (write-doc backend object)
    (flet ((write-section (type label)
             (let ((objects (remove-if-not (lambda (entry)
                                             (typep entry type))
                                           objects)))
               (unless (null objects)
                 (format stream "### ~A~%~%" label)
                 (dolist (object (sort-objects objects))
                   (write-object backend object)
                   (format stream "~%~%***~%~%"))))))
      (write-section 'coalton-type "Types")
      (write-section 'coalton-struct "Structs")
      (write-section 'coalton-class "Classes")
      (write-section 'coalton-value "Values")
      (write-section 'coalton-macro "Macros"))))

(defun write-instances (backend object)
  (let ((instances (object-instances object)))
    (unless (null instances)
      (let ((stream (output-stream backend)))
        (format stream "<details>~%")
        (format stream "<summary>Instances</summary>~%~%")
        (loop :for instance :in instances
              :do (format stream "- <code>")
                  (write-string (to-markdown instance) stream)
                  (format stream "</code>~:[~;  ~%~:*~A~]~%"
                          (source:docstring instance)))
        (format stream "~%</details>~%~%")))))

(defmethod write-object ((backend markdown-backend) (object coalton-object))
  (let ((stream (output-stream backend)))
    (tc:with-pprint-variable-context ()
      (write-string "#### " stream)
      (write-link stream object)
      (cond
        ((source-available-p object)
         (format stream " <sup><sub>[~A] · <a href=\"~a\">src</a></sub></sup>"
                 (html-entities:encode-entities (object-type object))
                 (html-entities:encode-entities (source-location-link backend object))))
        (t
         (format stream " <sup><sub>[~A]</sub></sup>"
                 (html-entities:encode-entities (object-type object)))))
      (format stream "~A~%" (object-anchor object)))
    (write-object-body backend object)))

(defun write-doc (backend object)
  "When OBJECT has a nonempty docstring, write it using BACKEND's stream."
  (let ((string (object-docstring object))
        (stream (output-stream backend)))
    (when (and string (not (zerop (length string))))
      (terpri stream)
      (write-string string stream)
      (terpri stream)
      (terpri stream))))

;;; coalton-type

(defmethod write-object-body ((backend markdown-backend) (object coalton-type))
  (let ((stream (output-stream backend)))
    (tc:with-pprint-variable-context ()
      (loop :for ctor :in (coalton-type-constructors object)
            :for ctor-name := (tc:constructor-entry-name ctor)
            :for ctor-source-name := (lookup-constructor-source-name ctor-name)
            :for ctor-type := (tc:lookup-value-type entry:*global-environment* ctor-name)
            :for ctor-docstring := (source:docstring ctor)
            :do (let ((args (type-constructor-args object ctor-type)))
                  (cond (args
                         (format stream "- <code>(~A~{ ~A~})</code>~A~%"
                                 (html-entities:encode-entities ctor-source-name)
                                 (mapcar #'to-markdown args)
                                 (if (null ctor-docstring) "" (format nil "~%  - ~A" ctor-docstring))))
                        (t
                         (format stream "- <code>~A</code> ~A~%"
                                 (html-entities:encode-entities ctor-source-name)
                                 (if (null ctor-docstring) "" (format nil "~%  - ~A" ctor-docstring))))))))
    (write-doc backend object)
    (write-instances backend object)))

;;; coalton-struct

(defmethod write-object-body ((backend markdown-backend) (object coalton-struct))
  (let ((stream (output-stream backend)))
    (tc:with-pprint-variable-context ()
      (loop :with entry := (type-entry object)
            :with package := (symbol-package (tc:type-entry-name entry))
            :for (name type docstring) :in (struct-fields object)
            :for symbol := (concatenate 'string "." name)
            :when (eq ':external (nth-value 1 (find-symbol symbol package)))
              :do (format stream "- <code>~A :: ~S</code>~A~%"
                          name
                          type
                          (if docstring
                              (format nil "<br/>~a" docstring)
                              ""))))
    (write-doc backend object)
    (write-instances backend object)))

;;; coalton-class

(defmethod write-object-body ((backend markdown-backend) (object coalton-class))
  (let ((stream (output-stream backend))
        (ctx (class-constraints object))
        (pred (class-predicate object)))
    (format stream "<code>")
    (tc:with-pprint-variable-context ()
      (format stream "~:[~{~A ~}~;~{(~A) ~}~]~:*~:[~*~;~A ~]~A"
              ;; Get the second element to test if we have more than one predicate
              (second ctx)
              (mapcar #'to-markdown ctx)
              (html-entities:encode-entities "⇒")
              (to-markdown pred))
      (format stream "</code>~%~%")
      (write-doc backend object)
      (format stream "Methods:~%")
      (loop :for (name type docstring) :in (class-methods object)
            :do (format stream "- <code>~A :: ~A</code>~@[<br/>~A~]~%"
                        (html-entities:encode-entities name)
                        (to-markdown type)
                        (html-entities:encode-entities docstring))))
    (write-instances backend object)))

;;; coalton-value

(defmethod write-object-body ((backend markdown-backend) (object coalton-value))
  (let ((stream (output-stream backend)))
    (format stream "<code>~A</code>~%" (to-markdown (value-type object)))
    (write-doc backend object)))

;;; coalton-macro

(defmethod write-object-body ((backend markdown-backend) (object coalton-macro))
  (write-doc backend object))

;;; Methods for TO-MARKDOWN

;;; Type printing

(defun markdown-keyword-name (stream keyword)
  (write-string
   (html-entities:encode-entities
    (format nil ":~(~A~)" (symbol-name keyword)))
   stream))

(defun write-markdown-tyvar (stream ty)
  (write-string
   (html-entities:encode-entities
    (with-output-to-string (tyvar-stream)
      (tc:pprint-ty tyvar-stream (tc:pprint-tvar ty))))
   stream))

(defun write-markdown-type-name (stream symbol env)
  (declare (ignore env))
  (if (eq (symbol-package symbol) (find-package '#:keyword))
      (write-string (html-entities:encode-entities (format nil "~S" symbol)) stream)
      (let ((source-name (lookup-type-source-name symbol)))
        (format stream "<a href=\"#~A\">~A</a>"
                (package-qualified-anchor
                 (symbol-package symbol)
                 source-name
                 "type")
                (html-entities:encode-entities source-name)))))

(defun write-markdown-class-name (stream symbol env)
  (declare (ignore env))
  (let ((source-name (lookup-class-source-name symbol)))
    (format stream "<a href=\"#~A\">~A</a>"
            (package-qualified-anchor
             (symbol-package symbol)
             source-name
             "class")
            (html-entities:encode-entities source-name))))

(defun markdown-type-object (object)
  (tc:with-pprint-variable-context ()
    (with-output-to-string (stream)
      (tc:render-type-object stream
                             object
                             :env entry:*global-environment*
                             :type-name-writer #'write-markdown-type-name
                             :class-name-writer #'write-markdown-class-name
                             :tyvar-writer #'write-markdown-tyvar
                             :keyword-name-writer #'markdown-keyword-name
                             :arrow-string (html-entities:encode-entities " → ")
                             :implication-string (html-entities:encode-entities " ⇒ ")
                             :forall-string (html-entities:encode-entities "∀")))))

(defmethod to-markdown ((object tc:ty))
  (markdown-type-object object))

(defmethod to-markdown ((object tc:qualified-ty))
  (markdown-type-object object))

(defmethod to-markdown ((object tc:ty-scheme))
  (markdown-type-object object))

(defmethod to-markdown ((object tc:ty-predicate))
  (markdown-type-object object))

(defmethod to-markdown ((object tc:ty-class-instance))
  (markdown-type-object object))
