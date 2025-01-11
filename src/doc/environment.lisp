;;;; The documentation generator queries the global environment for
;;;; entries to emit. These are helper functions that support query by
;;;; package, and also insulate callers from fset datatypes.

(defpackage #:coalton/doc/environment
  (:documentation "Environment access helpers for doc generator.")
  (:use
   #:cl
   #:coalton/doc/base)
  (:local-nicknames
   (#:map #:coalton-impl/algorithm/hamt)
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

(defun value-type (name-entry)
  (tc:lookup-value-type entry:*global-environment*
                        (tc:name-entry-name name-entry)))

(defun class-instances (ty-class)
  (tc:lookup-class-instances entry:*global-environment*
                             (tc:ty-class-name ty-class)))

(defun struct-entry-p (type-entry)
  (let ((name (tc:type-entry-name type-entry)))
    (not (null (tc:lookup-struct entry:*global-environment* name :no-error t)))))


(defun find-classes (&key (environment entry:*global-environment*)
                          (package nil))
  "Return all class definitions in ENVIRONMENT.

By default the global environment is queried.
If non-nil, restrict to classes defined in PACKAGE."
  (remove-if (lambda (type-entry)
               (and package
                    (not (exported-symbol-p (tc:ty-class-name type-entry) package t))))
             (map:vals (tc:environment-class-environment environment))))

(defun find-types (&key (environment entry:*global-environment*)
                        (package nil))
  "Return all type definitions in ENVIRONMENT.

By default the global environment is queried.
If non-nil, restrict to types defined in PACKAGE."
  (remove-if (lambda (type-entry)
               (and package
                    (not (exported-symbol-p (tc:type-entry-name type-entry) package t))))
             (map:vals (tc:environment-type-environment environment))))

(defun find-constructors (&key (environment entry:*global-environment*)
                               (package nil))
  "Return all constructors in ENVIRONMENT.

By default the global environment is queried.
If non-nil, restrict to constructors defined in PACKAGE."
  (remove-if-not (lambda (constructor-entry)
                   (and package
                        (not (exported-symbol-p (tc:constructor-entry-name constructor-entry) package t))))
                 (map:vals (tc:environment-constructor-environment environment))))

(defun find-names (&key (environment entry:*global-environment*)
                        (type nil)
                        (package nil))
  "Return all names in ENVIRONMENT.

By default the global environment is queried.
If non-nil, restrict to names defined in PACKAGE."
  (remove-if (lambda (entry)
               (or (and type
                        (not (eql type (tc:name-entry-type entry))))
                   (and package
                        (not (exported-symbol-p (tc:name-entry-name entry) package t)))))
             (map:vals (tc:environment-name-environment environment))))

(defun find-instances (&key (environment entry:*global-environment*)
                            (package nil))
  "Return all instances in ENVIRONMENT.

By default the global environment is queried.
If non-nil, restrict to instances defined in PACKAGE."
  (remove-if (lambda (class-instance)
               (and package
                    (not (exported-symbol-p (tc:ty-predicate-class
                                             (tc:ty-class-instance-predicate class-instance))
                                            package t))))
             (apply #'append
                    (%values (tc:environment-instance-environment environment)))))
