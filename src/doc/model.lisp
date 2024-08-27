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
   ;; object classes and properties
   #:coalton-object
   #:object-aname
   #:object-doc
   #:source
   #:source-span
   #:object-instances
   #:object-name
   #:object-type

   #:coalton-struct
   #:struct-fields

   #:coalton-class
   #:class-constraints
   #:class-predicate
   #:class-methods

   #:coalton-value
   #:value-type

   #:coalton-type
   #:type-constructors
   #:type-constructor-args

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
   #:source-location-href))

(in-package #:coalton/doc/model)

(defgeneric object-name (self)
  (:documentation "The name of a thing."))

(defgeneric object-type (self)
  (:documentation "The type of a thing: class, function, value, struct"))

(defun source (object)
  (let ((location (source:location object)))
    (and location
         (source:location-source location))))

(defun source-available-p (object)
  (let ((source (source object)))
    (and source
         (not (null (source:source-name source))))))

(defun source-location-href (object)
  (when (source-available-p object)
    (format nil "~a/~a"
            *remote*
            (remove-prefix (ensure-suffix #\/ *local*)
                           (source:source-name (source object))))))

(defun source-span (object)
  (source:location-span (source:location object)))

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
  ((type :initarg :type-entry
         :reader type-entry)
   (constructors :initarg :constructors)
   (instances :initarg :instances
              :reader object-instances)))

(defun type-vars (object)
  (loop :for i :below (tc:kind-arity (tc:kind-of (type-entry object)))
        :collect (tc:make-variable)))

(defun type-constructors (object)
  (mapcar (lambda (ctor)
            (let ((name (tc:constructor-entry-name ctor)))
              (cons name
                    (tc:lookup-value-type entry:*global-environment* name))))
          (slot-value object 'constructors)))

(defun type-constructor-args (object ctor-type)
  (tc:function-type-arguments
   (tc:qualified-ty-type (tc:instantiate (type-vars object)
                                         (tc:ty-scheme-type ctor-type)))))

(defmethod object-doc ((self coalton-type))
  (source:docstring (type-entry self)))

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
  (symbol-name (tc:type-entry-name (type-entry object))))

(defmethod object-type ((object coalton-type))
  "TYPE")

(defmethod object-aname ((object coalton-type))
  (format nil "~(~A-type~)" (object-name object)))

(defclass coalton-struct (coalton-object)
  ((type-entry :initarg :type-entry
               :reader type-entry)
   (instances :initarg :instances
              :reader object-instances)))

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
         (name (tc:type-entry-name entry)))
    (format nil "~A~{ ~S~}"
            (symbol-name name)
            (tc:type-entry-tyvars entry))))

(defmethod object-doc ((self coalton-struct))
  (source:docstring (type-entry self)))

(defmethod object-type ((self coalton-struct))
  "STRUCT")

(defmethod object-aname ((self coalton-struct))
  (format nil "~(~A-type~)"
          (symbol-name (tc:type-entry-name (type-entry self)))))

(defclass coalton-class (coalton-object)
  ((class :initarg :class
          :reader ty-class)
   (package :initarg :package
            :reader class-package)))

(defmethod object-doc ((self coalton-class))
  (source:docstring (ty-class self)))

(defun make-coalton-class (class &key package)
  (make-instance 'coalton-class
    :class class
    :package package))

(defun class-constraints (coalton-class)
  (let ((class (ty-class coalton-class)))
    (tc:ty-class-superclasses class)))

(defun class-predicate (coalton-class)
  (let ((class (ty-class coalton-class)))
    (tc:ty-class-predicate class)))

(defun class-methods (coalton-class)
  (with-slots (class package) coalton-class
    (mapcar (lambda (method)
              (list (symbol-name (tc:ty-class-method-name method))
                    (tc:ty-class-method-type method)
                    (source:docstring method)))
            (remove-if (lambda (method)
                         (and package (exported-symbol-p (tc:ty-class-method-name method) package t)))
                       (tc:ty-class-unqualified-methods class)))))

(defmethod object-instances ((self coalton-class))
  (env:class-instances (ty-class self)))

(defmethod object-name ((self coalton-class))
  (symbol-name (tc:ty-class-name (ty-class self))))

(defmethod object-type ((object coalton-class))
  "CLASS")

(defmethod object-aname ((self coalton-class))
  (format nil "~(~A-class~)" (object-name self)))

;;; class coalton-value

(defclass coalton-value (coalton-object)
  ((name-entry :initarg :name-entry
               :reader name-entry)))

(defun make-coalton-value (name-entry)
  (make-instance 'coalton-value :name-entry name-entry))

(defun value-type (coalton-value)
  (env:value-type (name-entry coalton-value)))

(defun %function-p (coalton-value)
  (tc:function-type-p (value-type coalton-value)))

(defmethod object-doc ((self coalton-value))
  (source:docstring (name-entry self)))

(defmethod object-name ((object coalton-value))
  (let ((name (tc:name-entry-name (name-entry object))))
    (if (%function-p object)
        (format nil "(~A~{ ~A~})"
                (symbol-name name)
                (tc:lookup-function-source-parameter-names
                 entry:*global-environment* name))
        (symbol-name name))))

(defmethod object-type ((object coalton-value))
  (if (%function-p object)
      "FUNCTION"
      "VALUE"))

(defmethod object-aname ((object coalton-value))
  (format nil "~(~A-value~)" (symbol-name (tc:name-entry-name (name-entry object)))))

;;; class coalton-package

(defclass coalton-package (coalton-object)
  ((package :initarg :package
            :reader package-lisp-package)))

(defun make-coalton-package (package)
  (make-instance 'coalton-package :package package))

(defmethod object-doc ((object coalton-package))
  (documentation (package-lisp-package object) t))

(defmethod object-name ((object coalton-package))
  (string-upcase (package-name (package-lisp-package object))))

(defmethod object-aname ((object coalton-package))
  (format nil "~A-package" (string-downcase (package-name (package-lisp-package object)))))

(defun package-objects (coalton-package)
  (let ((package (package-lisp-package coalton-package)))
    (find-objects :package package)))

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

;;; Public API

(defun has-values-p (package)
  (not (endp (env:find-names :type ':value
                             :package package))))

(defun find-values (&key package)
  (mapcar #'make-coalton-value
          (env:find-names :type ':value
                          :package package)))

(defun has-classes-p (package)
  (not (endp (env:find-classes :package package))))

(defun find-classes (&key package)
  (mapcar #'make-coalton-class
          (env:find-classes :package package)))

(defun has-types-p (package)
  "T if package defines any types."
  (not (endp (env:find-types :package package))))

(defun find-types (&key package)
  "Find all types defined in PACKAGE."
  (mapcar (lambda (entry)
            (make-coalton-type entry
                               (remove-if-not (alexandria:curry #'constructs-p entry)
                                              (env:find-constructors :package package))
                               (remove-if-not (alexandria:curry #'applicable-p entry)
                                              (env:find-instances))))
          (env:find-types :package package)))

(defun has-objects-p (package)
  "Return T if PACKAGE defines any Coalton objects."
  (or (has-types-p package)
      (has-values-p package)
      (has-classes-p package)))

(defun find-objects (&key package)
  "Find all Coalton OBJECTS, optionally retstricting to objects defined in PACKAGE."
  (append (find-types :package package)
          (find-values :package package)
          (find-classes :package package)))

(defun find-packages ()
  "Return the list of packages in the standard library."
  (let (packages)
    (do-all-symbols (symbol)
      (when (stdlib-p symbol)
        (pushnew (symbol-package symbol) packages)))
    (sort-objects
     (mapcar #'make-coalton-package
             (remove-if-not #'has-objects-p packages)))))

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
