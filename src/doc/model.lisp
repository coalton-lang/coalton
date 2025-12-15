;;;; There is a shallow hierarchy of classes rooted in coalton-object
;;;; that provides an API for backends that emit markdown, html, text,
;;;; live emacs documentation, etc.
;;;;
;;;;                  coalton-object
;;;;                        |
;;;;        +-------+-------+-------+------+
;;;;        |       |       |       |      |
;;;;   coalton-type | coalton-class | coalton-value
;;;;                |               |
;;;;          coalton-struct coalton-package
;;;;
;;;; Backends can consult functions defined here to ask for the source
;;;; location, github source link, name, type, etc. of these objects,
;;;; and to ask for display-ordered collections of sub-objects.

(defpackage #:coalton/doc/model
  (:documentation "Display model for Coalton types, classes, structs, values instances and packages")
  (:use
   #:cl
   #:coalton/doc/base)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:source #:coalton-impl/source)
   (#:env #:coalton/doc/environment)
   (#:entry #:coalton-impl/entry))
  (:export
   ;; Backend properties
   #:file-line-offsets

   ;; object classes and properties
   #:coalton-object
   #:object-aname
   #:source
   #:source-span
   #:object-docstring
   #:object-instances
   #:object-location
   #:object-name
   #:object-type

   #:type-entry

   #:coalton-struct
   #:struct-fields

   #:coalton-class
   #:class-constraints
   #:class-predicate
   #:class-methods

   #:coalton-value
   #:value-type

   #:coalton-type
   #:coalton-type-constructors
   #:type-constructor-args

   #:coalton-macro
   #:coalton-macro-name

   #:coalton-package
   #:package-objects
   #:sort-objects

   ;; query
   #:find-objects
   #:find-packages

   ;; output utilities
   #:write-anchor
   #:object-anchor
   #:write-link
   #:object-link
   #:source-available-p

   ;; source-name lookup utilities
   #:lookup-type-source-name
   #:lookup-class-source-name
   #:lookup-constructor-source-name

   ;; source-definition lookup utilities
   #:source-location-link))

(in-package #:coalton/doc/model)

(defgeneric object-name (self)
  (:documentation "The name of a thing."))

(defgeneric object-type (self)
  (:documentation "The type of a thing: class, function, value, struct, macro"))

(defgeneric object-location (self)
  (:documentation "The location of an object's definition."))

(defgeneric object-docstring (self)
  (:documentation "The documentation string for an object."))

(defun source (object)
  (let ((location (object-location object)))
    (and location
         (source:location-source location))))

(defun source-available-p (object)
  (and (source object)
       (source:source-available-p (source object))))

(defun source-span (object)
  (source:location-span (object-location object)))

(defgeneric object-aname (self)
  (:documentation "The link target of a thing: <name>-class, e.g."))

(defun ensure-suffix (char string)
  "If STRING doesn't end with CHAR, append it."
  (if (char= (aref string (1- (length string))) char)
      string
      (format nil "~a~a" string char)))

(defun remove-prefix (prefix string)
  "If STRING beings with PREFIX, remove it; otherwise, return unchanged string."
  (if (eql 0 (search prefix string))
      (subseq string (length prefix))
      string))

(defclass coalton-object ()
  ())

(defmethod print-object ((self coalton-object) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (self stream :type t :identity t)
        (format stream "~a (~a) (file: ~a)"
                (object-name self)
                (object-type self)
                (source self)))))

(defun sort-objects (objects)
  (sort (copy-seq objects) #'string< :key #'object-name))

(defclass coalton-type (coalton-object)
  ((type-entry :initarg :type-entry
               :reader type-entry)
   (constructors :initarg :constructors
                 :accessor coalton-type-constructors)
   (instances :initarg :instances
              :reader object-instances)))

(defmethod object-location ((self coalton-type))
  (source:location (type-entry self)))

(defmethod object-docstring ((self coalton-type))
  (source:docstring (type-entry self)))

(defun type-vars (coalton-type)
  (loop :for i :below (tc:kind-arity (tc:kind-of (type-entry coalton-type)))
        :collect (tc:make-variable)))

(defun type-constructor-args (coalton-type ctor-type)
  (tc:function-type-arguments
   (tc:qualified-ty-type (tc:instantiate (type-vars coalton-type)
                                         (tc:ty-scheme-type ctor-type)))))

(defun make-coalton-type (entry constructors applicable-instances)
  (cond ((env:struct-entry-p entry)
         (make-instance 'coalton-struct
           :type-entry entry
           :instances (sort-objects applicable-instances)))
        (t
         (make-instance 'coalton-type
           :type-entry entry
           :constructors constructors
           :instances (sort-objects applicable-instances)))))

(defmethod object-name ((object coalton-type))
  (or (tc:type-entry-source-name (type-entry object))
      (symbol-name (tc:type-entry-name (type-entry object)))))

(defmethod object-type ((object coalton-type))
  "TYPE")

(defmethod object-aname ((object coalton-type))
  (format nil "~(~A-type~)" (object-name object)))

(defclass coalton-struct (coalton-object)
  ((type-entry :initarg :type-entry
               :reader type-entry)
   (instances :initarg :instances
              :reader object-instances)))

(defmethod object-location ((self coalton-struct))
  (source:location (type-entry self)))

(defmethod object-docstring ((self coalton-struct))
  (source:docstring (type-entry self)))

(defun %struct-entry (type-entry)
  (tc:lookup-struct entry:*global-environment* (tc:type-entry-name type-entry) :no-error t))

(defun struct-fields (coalton-struct)
  (let ((struct-entry (%struct-entry (type-entry coalton-struct))))
    (mapcar (lambda (field)
              (list (tc:struct-field-name field)
                    (tc:struct-field-type field)
                    (source:docstring field)))
            (tc:struct-entry-fields struct-entry))))

(defmethod object-name ((self coalton-struct))
  (let* ((entry (type-entry self))
         (name (tc:type-entry-name entry))
         (source-name (or (tc:type-entry-source-name entry)
                          (symbol-name name))))
    (format nil "~A~{ ~S~}"
            source-name
            (tc:type-entry-tyvars entry))))

(defmethod object-type ((self coalton-struct))
  "STRUCT")

(defmethod object-aname ((self coalton-struct))
  (format nil "~(~A-type~)"
          (symbol-name (tc:type-entry-name (type-entry self)))))

(defclass coalton-class (coalton-object)
  ((class-entry :initarg :class
                :reader class-entry)
   (package :initarg :package
            :reader class-package)))

(defmethod object-location ((self coalton-class))
  (source:location (class-entry self)))

(defmethod object-docstring ((self coalton-class))
  (source:docstring (class-entry self)))

(defun make-coalton-class (class &key package)
  (make-instance 'coalton-class
    :class class
    :package package))

(defun class-constraints (coalton-class)
  (let ((class (class-entry coalton-class)))
    (tc:ty-class-superclasses class)))

(defun class-predicate (coalton-class)
  (let ((class (class-entry coalton-class)))
    (tc:ty-class-predicate class)))

(defun class-methods (coalton-class)
  (let ((class (class-entry coalton-class))
        (package (class-package coalton-class)))
    (mapcar (lambda (method)
              (list (symbol-name (tc:ty-class-method-name method))
                    (tc:ty-class-method-type method)
                    (source:docstring method)))
            (remove-if (lambda (method)
                         (and package (exported-symbol-p (tc:ty-class-method-name method) package t)))
                       (tc:ty-class-unqualified-methods class)))))

(defmethod object-instances ((self coalton-class))
  (env:class-instances (class-entry self)))

(defmethod object-name ((self coalton-class))
  (or (tc:ty-class-source-name (class-entry self))
      (symbol-name (tc:ty-class-name (class-entry self)))))

(defmethod object-type ((object coalton-class))
  "CLASS")

(defmethod object-aname ((self coalton-class))
  (format nil "~(~A-class~)" (object-name self)))

;;; class coalton-value

(defclass coalton-value (coalton-object)
  ((name-entry :initarg :name-entry
               :reader name-entry)))

(defmethod object-location ((self coalton-value))
  (source:location (name-entry self)))

(defmethod object-docstring ((self coalton-value))
  (source:docstring (name-entry self)))

(defun make-coalton-value (name-entry)
  (make-instance 'coalton-value :name-entry name-entry))

(defun value-type (coalton-value)
  (env:value-type (name-entry coalton-value)))

(defun %function-p (coalton-value)
  (tc:function-type-p (value-type coalton-value)))

(defmethod object-name ((object coalton-value))
  (let ((name (tc:name-entry-name (name-entry object))))
    (if (%function-p object)
        (format nil "(~A~{ ~A~})"
                (symbol-name name)
                (tc:lookup-function-source-parameter-names ; todo -> env
                 entry:*global-environment* name))
        (lookup-constructor-source-name name))))

(defmethod object-type ((object coalton-value))
  (if (%function-p object)
      "FUNCTION"
      "VALUE"))

(defmethod object-aname ((object coalton-value))
  (format nil "~(~A-value~)" (symbol-name (tc:name-entry-name (name-entry object)))))

;;; class coalton-package

(defclass coalton-package ()
  ((package :initarg :package
            :reader lisp-package)
   (reexported-symbols :initarg :reexported-symbols
                       :initform nil
                       :reader reexported-symbols
                       :documentation "If T, this package's documentation will include re-exported symbols.")))

(defun make-coalton-package (package &key (reexported-symbols nil))
  (make-instance 'coalton-package :package package :reexported-symbols reexported-symbols))

(defmethod object-docstring ((self coalton-package))
  (documentation (lisp-package self) t))

(defmethod object-name ((self coalton-package))
  (string-upcase (package-name (lisp-package self))))

(defmethod object-aname ((self coalton-package))
  (format nil "~A-package" (string-downcase (package-name (lisp-package self)))))

(defun package-objects (coalton-package)
  (let ((package (lisp-package coalton-package)))
    (find-objects :package package :reexported-symbols (reexported-symbols coalton-package))))

;; Helpers for API queries

(defun applicable-p (type-entry instance)
  (some (lambda (pred-type)
          (labels ((check (pred)
                     (typecase pred
                       (tc:tapp (check (tc:tapp-from pred)))
                       (t (equalp pred (tc:type-entry-type type-entry))))))
            (check pred-type)))
        (tc:ty-predicate-types (tc:ty-class-instance-predicate instance))))

(defun constructs-p (type-entry constructor-entry)
  (eq (tc:constructor-entry-constructs constructor-entry)
      (tc:type-entry-name type-entry)))

(defun stdlib-p (symbol)
  "A standard library package is any package with the exact name 'coalton' or whose name starts with 'coalton-library'."
  (let ((name (package-name (symbol-package symbol))))
    (or (string-equal name "COALTON")
        (eql 0 (search "COALTON-LIBRARY" name)))))

;;; class coalton-macro

(defclass coalton-macro (coalton-object)
  ((name :initarg :name :reader coalton-macro-name)))

(defun symbol-exported-p (s)
  (let ((p (symbol-package s)))
    (if (null p)
        nil
        (eq ':external (nth-value 1 (find-symbol (symbol-name s) p))))))

(defun symbol-names-coalton-macro-p (s)
  (and (macro-function s)
       (get s ':coalton-macro)))

(defun find-macros (&key (package nil package-provided-p))
  (let ((macros nil))
    (cond
      ((not package-provided-p)
       (do-all-symbols (v macros)
         (when (and (symbol-exported-p v)
                    (symbol-names-coalton-macro-p v))
           (push (make-instance 'coalton-macro :name v) macros))))
      (t
       (do-external-symbols (v package macros)
         (when (symbol-names-coalton-macro-p v)
           (push (make-instance 'coalton-macro :name v) macros)))))))

(defun print-lambda-list (stream list)
  ;; A special printer that doesn't print package prefixes, but prints
  ;; most everything else as ~S.
  (typecase list
    (cons
     (cond
       ((alexandria:proper-list-p list)
        (write-char #\( stream)
        (let ((first t))
          (dolist (x list)
            (cond
              (first (setf first nil))
              (t (write-char #\Space stream)))
            (print-lambda-list stream x)))
        (write-char #\) stream))
       (t
        (write-char #\( stream)
        (print-lambda-list stream (car list))
        (write-string " . " stream)
        (print-lambda-list stream (cdr list))
        (write-char #\) stream))))
    (symbol
     (format stream "~A" list))
    (t
     (format stream "~S" list)))
  nil)

(defmethod object-name ((x coalton-macro))
  (let ((*print-pretty* nil)
        (*print-circle* nil))
    (with-output-to-string (s)
      (format s "~A " (coalton-macro-name x))
      (print-lambda-list s (get (coalton-macro-name x) ':coalton-macro-lambda-list)))))

(defmethod object-type ((x coalton-macro))
  "MACRO")

(defmethod object-location ((x coalton-macro))
  ;; TODO: Track source location of macro definitions.
  nil)

(defmethod object-docstring ((x coalton-macro))
  (documentation (coalton-macro-name x) 'function))

(defmethod object-aname ((x coalton-macro))
  (substitute
   #\- #\/
   (format nil "~(~A-~A-macro~)"
           (package-name (symbol-package (coalton-macro-name x)))
           (symbol-name (coalton-macro-name x)))))

;;; Public API

(defun has-values-p (package)
  (not (endp (env:find-names :type ':value
                             :package package))))

(defun find-values (&key package reexported-symbols)
  (mapcar #'make-coalton-value
          (env:find-names :type ':value
                          :package package
                          :reexported-symbols reexported-symbols)))

(defun has-classes-p (package)
  (not (endp (env:find-classes :package package))))

(defun find-classes (&key package reexported-symbols)
  (mapcar #'make-coalton-class
          (env:find-classes :package package :reexported-symbols reexported-symbols)))

(defun has-types-p (package)
  "T if package defines any types."
  (not (endp (env:find-types :package package))))

(defun find-types (&key package reexported-symbols)
  "Find all types defined in PACKAGE."
  (mapcar (lambda (entry)
            (make-coalton-type entry
                               (remove-if-not (alexandria:curry #'constructs-p entry)
                                              (env:find-constructors :package package))
                               (remove-if-not (alexandria:curry #'applicable-p entry)
                                              (env:find-instances))))
          (env:find-types :package package :reexported-symbols reexported-symbols)))

(defun has-macros-p (package)
  (do-external-symbols (v package nil)
    (when (symbol-names-coalton-macro-p v)
      (return-from has-macros-p t))))

(defun has-objects-p (package)
  "Return T if PACKAGE defines any Coalton objects."
  (or (has-types-p package)
      (has-values-p package)
      (has-classes-p package)
      (has-macros-p package)))

(defun find-objects (&key package reexported-symbols)
  "Find all Coalton OBJECTS, optionally retstricting to objects defined in PACKAGE."
  (append (find-types :package package :reexported-symbols reexported-symbols)
          (find-values :package package :reexported-symbols reexported-symbols)
          (find-classes :package package :reexported-symbols reexported-symbols)
          (find-macros :package package)))

(defun find-packages ()
  "Return the list of packages in the standard library."
  (let ((packages nil))
    (do-all-symbols (symbol)
      (when (stdlib-p symbol)
        (pushnew (symbol-package symbol) packages)))
    (sort-objects
     (mapcar #'make-coalton-package
             (remove-if-not #'has-objects-p packages)))))

;;; Source-name lookup utilities

(defun lookup-type-source-name (symbol)
  "Look up the source-name for a type symbol, falling back to symbol-name."
  (let ((entry (tc:lookup-type entry:*global-environment* symbol :no-error t)))
    (if entry
        (tc:type-entry-source-name entry)
        (symbol-name symbol))))

(defun lookup-class-source-name (symbol)
  "Look up the source-name for a class symbol, falling back to symbol-name."
  (let ((entry (tc:lookup-class entry:*global-environment* symbol :no-error t)))
    (if entry
        (tc:ty-class-source-name entry)
        (symbol-name symbol))))

(defun lookup-constructor-source-name (symbol)
  "Look up the source-name for a constructor symbol, falling back to symbol-name."
  (let ((entry (tc:lookup-constructor entry:*global-environment* symbol :no-error t)))
    (if entry
        (tc:constructor-entry-source-name entry)
        (symbol-name symbol))))

;;; Source-definition lookup utilities

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

(defun source-location-href (object)
  (when (source-available-p object)
    (format nil "~a/~a"
            *remote*
            (remove-prefix (ensure-suffix #\/ *local*)
                           (source:source-name (source object))))))

(defun line-number (backend source offset)
  (let ((line-offsets (gethash (source:source-name source)
                        (slot-value backend 'file-line-offsets))))
    (unless line-offsets
      (with-open-stream (stream (source:source-stream source))
        (setf line-offsets (find-line-offsets stream)))
      (setf (gethash (source:source-name source)
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

(defun source-location-link (backend object)
  (if (not (source-available-p object))
      ""
      (let ((source (source object))
            (span (source-span object)))
        (format nil "~a#L~a-L~a"
                (source-location-href object)
                (line-number backend source (car span))
                (line-number backend source (cdr span))))))

;;; Output utilities

(defun write-anchor (stream object)
  (format stream "<a name=\"~A\"></a>" (object-aname object)))

(defun object-anchor (object)
  (with-output-to-string (stream)
    (write-anchor stream object)))

(defun write-link (stream object)
  (format stream "<a href=\"#~A\"><code>~A</code></a>"
          (object-aname object)
          (html-entities:encode-entities (object-name object))))

(defun object-link (object)
  (with-output-to-string (stream)
    (write-link stream object)))
