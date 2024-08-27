;;;; The documentation generator queries the global environment for
;;;; entries to emit. These are helper functions that support query by
;;;; package, and also insulate callers from fset datatypes.

(defpackage #:coalton/doc/environment
  (:documentation "Environment access helpers for doc generator.")
  (:use
   #:cl
   #:coalton/doc/base)
  (:local-nicknames
   (#:algo #:coalton-impl/algorithm)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry))
  (:export
   #:value-type
   #:class-instances
   #:struct-entry-p
   #:find-classes
   #:find-constructors
   #:find-instances
   #:find-names
   #:find-types))

(in-package #:coalton/doc/environment)

(defun %values (immutable-map)
  "Coerce an FSET map to a list."
  (fset:convert 'list (fset:range (algo:immutable-map-data immutable-map))))

(defun %lm-values (immutable-listmap)
  "Coerce an FSET 'listmap' to a list."
  (apply #'append
         (mapcar (lambda (value)
                   (fset:convert 'list value))
                 (fset:convert 'list (fset:range (algo:immutable-listmap-data immutable-listmap))))))

(defun value-type (name-entry)
  (tc:lookup-value-type entry:*global-environment*
                        (tc:name-entry-name name-entry)))

(defun class-instances (ty-class)
  (fset:convert 'list
                (tc:lookup-class-instances entry:*global-environment*
                                           (tc:ty-class-name ty-class)
                                           :no-error t)))

(defun struct-entry-p (type-entry)
  (let ((name (tc:type-entry-name type-entry)))
    (not (null (tc:lookup-struct entry:*global-environment* name :no-error t)))))


(defun find-classes (&key (environment entry:*global-environment*)
                          (package nil))
  "Return all class definitions in ENVIRONMENT.

By default the global environment is queried.
If PACKAGE is non-nil, restrict to classes defined in that package."
  (remove-if (lambda (type-entry)
               (and package
                    (not (exported-symbol-p (tc:ty-class-name type-entry) package t))))
             (%values (tc:environment-class-environment environment))))

(defun find-types (&key (environment entry:*global-environment*)
                        (package nil))
  "Return all type definitions in ENVIRONMENT.

By default the global environment is queried.
If PACKAGE is non-nil, restrict to types defined in that package."
  (remove-if (lambda (type-entry)
               (and package
                    (not (exported-symbol-p (tc:type-entry-name type-entry) package t))))
             (%values (tc:environment-type-environment environment))))

(defun find-constructors (&key (environment entry:*global-environment*)
                               (package nil))
  "Return all constructors in ENVIRONMENT.

By default the global environment is queried.
If PACKAGE is non-nil, restrict to constructors defined in that package."
  (remove-if-not (lambda (constructor-entry)
                   (and package
                        (not (exported-symbol-p (tc:constructor-entry-name constructor-entry) package t))))
                 (%values (tc:environment-constructor-environment environment))))

(defun symbol-match (symbol substring)
  (integerp (search (string-downcase substring)
                    (string-downcase (symbol-name symbol)))))

(defun find-names (&key (environment entry:*global-environment*)
                        (type nil)
                        (name nil)
                        (package nil))
  "Return all names in ENVIRONMENT.

By default the global environment is queried.

If NAME is non-nil, restrict to names containing the substring.
If PACKAGE is non-nil, restrict to names defined in that package."
  (remove-if (lambda (entry)
               (or (and type
                        (not (eql type (tc:name-entry-type entry))))
                   (and name
                        (not (symbol-match (tc:name-entry-name entry) name)))
                   (and package
                        (not (exported-symbol-p (tc:name-entry-name entry) package t)))))
             (%values (tc:environment-name-environment environment))))

(defun find-instances (&key (environment entry:*global-environment*)
                            (package nil))
  "Return all instances in ENVIRONMENT.

By default the global environment is queried.
If PACKAGE is non-nil, restrict to instances defined in that package."
  (remove-if (lambda (class-instance)
               (and package
                    (not (exported-symbol-p (tc:ty-predicate-class
                                             (tc:ty-class-instance-predicate class-instance))
                                            package t))))
             (%lm-values (tc:instance-environment-instances
                          (tc:environment-instance-environment environment)))))
