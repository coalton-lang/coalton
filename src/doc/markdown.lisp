
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
;;;; Package: `${name}-package`
;;;; File: `${package}-${name}-file`
;;;;   where `name` is the filepath relative to the provided asdf system
;;;;   with all non-alphanumeric characters replaced with `-`.
;;;; Type: `${name}-type`
;;;; Class: `${name}-class`
;;;; Value: `${name}-value`

(defpackage #:coalton/doc/markdown
  (:use
   #:cl
   #:coalton/doc/base
   #:coalton/doc/model)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker)))

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

(defun line-number (backend source offset)
  (let ((line-offsets (gethash (source-error:source-name source)
                        (slot-value backend 'file-line-offsets))))
    (unless line-offsets
      (with-open-stream (stream (source-error:source-stream source))
        (setf line-offsets (find-line-offsets stream)))
      (setf (gethash (source-error:source-name source)
                     (slot-value backend 'file-line-offsets))
            line-offsets))
    (labels ((%find (lo hi)
               (let ((probe (+ lo (floor (/ (- hi lo) 2)))))
                 (when (= probe lo)
                   (return-from line-number (1+ probe)))
                 (cond ((< offset (aref line-offsets probe))
                        (setf hi probe))
                       (t
                        (setf lo probe)))
                 (%find lo hi))))
      (%find 0 (length line-offsets)))))

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
      (write-section 'coalton-value "Values"))))

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

(defun source-location-link (backend object)
  (if (not (source-available-p object))
      ""
      (let ((source (source object))
            (span (source-span object)))
        (format nil "~a#L~a-L~a"
                (source-location-href object)
                (line-number backend source (car span))
                (line-number backend source (cdr span))))))

(defmethod write-object ((backend markdown-backend) (object coalton-object))
  (let ((stream (output-stream backend)))
    (tc:with-pprint-variable-context ()
      (format stream "#### <code><a href=\"~a\">~a</a></code> <sup><sub>[~A]</sub></sup>~A~%"
              (html-entities:encode-entities (source-location-link backend object))
              (html-entities:encode-entities (object-name object))
              (html-entities:encode-entities (object-type object))
              (object-anchor object)))
    (write-object-body backend object)))

(defun write-doc (backend object)
  "When OBJECT has a nonempty docstring, write it using BACKEND's stream."
  (let ((string (source:docstring (coalton-object object)))
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
      (loop :for (ctor-name . ctor-type) :in (type-constructors object)
            :do (let ((args (type-constructor-args object ctor-type)))
                  (cond (args
                         (format stream "- <code>(~A~{ ~A~})</code>~%"
                                 (html-entities:encode-entities (symbol-name ctor-name))
                                 (mapcar #'to-markdown args)))
                        (t
                         (format stream "- <code>~A</code>~%"
                                 (html-entities:encode-entities (symbol-name ctor-name))))))))
    (write-doc backend object)
    (write-instances backend object)))

;;; coalton-struct

(defmethod write-object-body ((backend markdown-backend) (object coalton-struct))
  (let ((stream (output-stream backend)))
    (tc:with-pprint-variable-context ()
      (loop :for (name type docstring) :in (struct-fields object)
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
              (to-markdown pred)))
    (format stream "</code>~%~%")
    (write-doc backend object)
    (format stream "Methods:~%")
    (tc:with-pprint-variable-context ()
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

;;; Methods for TO-MARKDOWN

;;; Type printing (modification of pprint-type)

(defmethod to-markdown ((ty tc:tyvar))
  (html-entities:encode-entities
   (with-output-to-string (stream)
     (tc:pprint-ty stream (tc:pprint-tvar ty)))))

(defmethod to-markdown ((ty tc:tycon))
  (let ((tcon-name (tc:tycon-name ty)))
    (if (string= "KEYWORD" (package-name (symbol-package tcon-name)))
        (html-entities:encode-entities (format nil "~S" tcon-name))
        (object-link ty))))

(defmethod to-markdown ((ty tc:tapp))
  (with-output-to-string (stream)
    (cond
      ((tc:function-type-p ty) ;; Print function types
       (write-string "(" stream)
       (write-string (to-markdown (tc:tapp-to (tc:tapp-from ty))) stream)
       (write-string (html-entities:encode-entities " → ") stream)
       ;; Avoid printing extra parentheses on curried functions
       (labels ((print-subfunction (to)
                  (cond
                    ((tc:function-type-p to)
                     (write-string (to-markdown (tc:tapp-to (tc:tapp-from to))) stream)
                     (write-string (html-entities:encode-entities " → ") stream)
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

(defmethod to-markdown ((object tc:qualified-ty))
  (let ((preds (tc:qualified-ty-predicates object))
        (qual-type (tc:qualified-ty-type object)))
    (format nil "~:[~{~A ~}~;~{(~A) ~}~]~:*~:[~*~;~A ~]~A"
            ;; Get the second element to test if we have more than one predicate
            (second preds)
            (mapcar #'to-markdown preds)
            (html-entities:encode-entities "⇒")
            (to-markdown qual-type))))

(defmethod to-markdown ((object tc:ty-scheme))
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
                 (html-entities:encode-entities "∀")
                 (html-entities:encode-entities (format nil "~{ ~S~}" types))
                 (to-markdown new-type)))))))

(defmethod to-markdown ((object tc:ty-predicate))
  (format nil "<a href=\"#~(~A-class~)\">~:*~A</a>~{ ~A~}"
          (html-entities:encode-entities (symbol-name (tc:ty-predicate-class object)))
          (mapcar #'to-markdown (tc:ty-predicate-types object))))

(defmethod to-markdown ((object tc:ty-class-instance))
  (let ((ctx (tc:ty-class-instance-constraints object))
        (pred (tc:ty-class-instance-predicate object)))
    (tc:with-pprint-variable-context ()
      (format nil "~:[~{~A ~}~;~{(~A) ~}~]~:*~:[~*~;~A ~]~A"
              ;; Get the second element to test if we have more than one predicate
              (second ctx)
              (mapcar #'to-markdown ctx)
              (html-entities:encode-entities "⇒")
              (to-markdown pred)))))
