(in-package #:coalton-impl/doc)

(defstruct (documentation-entry
            (:constructor nil))
  (name (required 'name) :type symbol)
  (documentation nil :type (or null string))
  location)

(defstruct (documentation-type-entry
            (:include documentation-entry)
            (:constructor make-documentation-type-entry
                (name type constructors constructor-types instances documentation location)))
  (type (required 'type) :type ty)
  constructors
  constructor-types
  instances)

(defstruct (documentation-class-entry
            (:include documentation-entry)
            (:constructor make-documentation-class-entry
                (name context predicate methods instances documentation location)))
  context
  predicate
  methods
  instances)

(defstruct (documentation-value-entry
            (:include documentation-entry)
            (:constructor make-documentation-value-entry
                (name type documentation location)))
  (type (required 'type) :type ty-scheme))

(defstruct (documentation-file-entry
            (:constructor make-documentation-file-entry
                (filename package value-entries type-entries class-entries link-prefix)))
  filename
  package
  value-entries
  type-entries
  class-entries
  link-prefix)

(defstruct (documentation-package-entry
            (:constructor make-documentation-package-entry
                (package valid-files documentation-entries-by-file)))
  package
  valid-files
  documentation-entries-by-file)

(defstruct (documentation-package-entries
            (:constructor make-documentation-package-entries
                (packages asdf-system documentation-by-package)))
  packages
  asdf-system
  documentation-by-package)

(defgeneric write-documentation (backend stream object)
  (:documentation "Write the given OBJECT to output STREAM. This is
  specialized on the given BACKEND."))

(defun write-documentation-for-packages (&key
                                           (env coalton-impl::*global-environment*)
                                           (stream t)
                                           (backend ':markdown)
                                           (packages
                                            '(coalton
                                              coalton-library/classes
                                              coalton-library/builtin
                                              coalton-library/boolean
                                              coalton-library/bits
                                              coalton-library/arith
                                              coalton-library/char
                                              coalton-library/string
                                              coalton-library/tuple
                                              coalton-library/optional
                                              coalton-library/list
                                              coalton-library/result
                                              coalton-library/functions
                                              coalton-library/cell
                                              coalton-library/vector
                                              coalton-library/slice
                                              coalton-library/hashtable
                                              coalton-library/monad/state
                                              coalton-library/iterator))
                                           (base-package 'coalton-prelude)
                                           (asdf-system ':COALTON)
                                           (file-link-prefix ""))
  (let* ((component (asdf:find-component asdf-system 'library))
         (component-path (asdf:component-pathname component))
         (filenames (mapcar (lambda (file)
                              (file-namestring (asdf:component-relative-pathname file)))
                            (asdf:component-children component)))

         (*package* (find-package base-package)))

    ;; For each package, we just need to collect the require
    ;; documentation and call out to the backend.
    (let ((documentation-by-package (make-hash-table)))
      (loop :for package :in packages :do
        (setf (gethash package documentation-by-package)
              (make-documentation-package-entry
               package filenames
               (collect-documentation-by-file
                (truename component-path)
                file-link-prefix
                env package))))

      (write-documentation backend stream
                           (make-documentation-package-entries
                            packages asdf-system
                            documentation-by-package)))))

(defun collect-documentation (&optional
                                (env coalton-impl::*global-environment*)
                                (package "COALTON-LIBRARY"))
  "Collect all documentation entries related to coalton code in PACKAGE."
  (values (get-doc-value-info env package)
          (get-doc-type-info env package)
          (get-doc-class-info env package)))

(defun collect-documentation-by-file (&optional
                                        basepath
                                        link-prefix
                                        (env coalton-impl::*global-environment*)
                                        (package "COALTON-LIBRARY"))
  (multiple-value-bind (value-entries type-entries class-entries)
      (collect-documentation env package)
    (sort-documentation-by-file basepath link-prefix package value-entries type-entries class-entries)))

;; TODO: We should sort everything here
(defun sort-documentation-by-file (basepath link-prefix package value-entries type-entries class-entries)
  (let ((file-entries (make-hash-table :test #'equalp)))
    ;; Sort the functions by file
    (loop :for entry :in value-entries
          :for filename := (enough-namestring (documentation-entry-location entry) basepath) :do
            (push entry
                  (documentation-file-entry-value-entries
                   (alexandria:ensure-gethash
                    filename
                    file-entries
                    (make-documentation-file-entry
                     filename package nil nil nil link-prefix)))))

    ;; Sort the types by file
    (loop :for entry :in type-entries
          :for filename := (enough-namestring (documentation-entry-location entry) basepath) :do
            (push entry
                  (documentation-file-entry-type-entries
                   (alexandria:ensure-gethash
                    filename
                    file-entries
                    (make-documentation-file-entry
                     filename package nil nil nil link-prefix)))))

    ;; Sort the classes by file
    (loop :for entry :in class-entries
          :for filename := (enough-namestring (documentation-entry-location entry) basepath) :do
            (push entry
                  (documentation-file-entry-class-entries
                   (alexandria:ensure-gethash
                    filename
                    file-entries
                    (make-documentation-file-entry
                     filename package nil nil nil link-prefix)))))

    file-entries))



(defun get-doc-value-info (env package)
  (let ((values nil)
        (package (find-package package)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (immutable-map-data (coalton-impl/typechecker::environment-name-environment env)))
      ;; Only include exported symbols from our package
      (when (and (equalp (symbol-package sym) package)
                 (multiple-value-bind (symbol status)
                     (find-symbol (symbol-name sym) package)
                   (declare (ignore symbol))
                   (eql :external status)))
        (push (cons sym entry) values)))

    (mapcar
     (lambda (e)
       (make-documentation-value-entry
        (car e)
        (lookup-value-type env (car e))
        (coalton-impl/typechecker::name-entry-docstring (cdr e))
        (coalton-impl/typechecker::name-entry-location (cdr e))))
     (remove-if
      (lambda (x)
        (coalton-impl/typechecker::lookup-constructor env (car x) :no-error t))
      (remove-if-not
       (lambda (x)
         (eql :value (name-entry-type (cdr x))))
       values)))))

(defun get-doc-type-info (env package)
  (let ((types nil)
        (ctors nil)
        (package (find-package package)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (immutable-map-data (coalton-impl/typechecker::environment-type-environment env)))
      ;; Only include exported symbols from our packages
      (when (and (equalp (symbol-package sym) package)
                 (multiple-value-bind (symbol status)
                     (find-symbol (symbol-name sym) package)
                   (declare (ignore symbol))
                   (eql :external status)))
        (push (cons sym entry) types)))
    (fset:do-map (sym entry (immutable-map-data (coalton-impl/typechecker::environment-constructor-environment env)))
      (when (equalp (symbol-package sym) package)
        (push (cons sym entry) ctors)))

    (let ((instance-list
            (fset:convert 'list
                          (coalton-impl/typechecker::instance-environment-data
                           (coalton-impl/typechecker::environment-instance-environment env)))))

      (mapcar (lambda (e)
                (let* ((ctors (remove-if-not
                               (lambda (ctor)
                                 (and (eql (constructor-entry-constructs (cdr ctor))
                                           (car e))))
                               ctors))

                       (exported-ctors
                         (remove-if-not
                          (lambda (ctor)
                            (multiple-value-bind (symbol status)
                                (find-symbol (symbol-name (car ctor)) (symbol-package (car ctor)))
                              (declare (ignore symbol))
                              (eq :external status)))
                          ctors))

                       (applicable-instances
                         (loop :for (class . instances) :in instance-list
                               :append
                               (loop :for instance :in (fset:convert 'list instances)
                                     :append
                                     (when (some
                                            (lambda (pred-type)
                                              (labels ((check (pred)
                                                         (typecase pred
                                                           (coalton-impl/typechecker::tapp
                                                            (check (coalton-impl/typechecker::tapp-from pred)))
                                                           (t
                                                            (equalp (type-entry-type (cdr e)) pred)))))
                                                (check pred-type)))
                                            (ty-predicate-types (ty-class-instance-predicate instance)))
                                       (list instance))))))
                  (make-documentation-type-entry
                   (car e)
                   (type-entry-type (cdr e))
                   exported-ctors
                   (mapcar
                    (lambda (ctor)
                      (lookup-value-type env (car ctor)))
                    exported-ctors)
                   applicable-instances
                   (type-entry-docstring (cdr e))
                   ;; Here we will assume that all constructors
                   ;; share the same location as the type.
                   (if (first ctors)
                       (coalton-impl/typechecker::name-entry-location
                        (lookup-name env (car (first ctors))))
                       ""))))
              types))))

(defun get-doc-class-info (env package)
  (let ((values nil)
        (package (find-package package)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (immutable-map-data (coalton-impl/typechecker::environment-class-environment env)))
      ;; Only include exported symbols from our package
      (when (and (equalp (symbol-package sym) package)
                 (multiple-value-bind (symbol status)
                     (find-symbol (symbol-name sym) package)
                   (declare (ignore symbol))
                   (eql :external status)))
        (push entry values)))

    (mapcar (lambda (e)
              (make-documentation-class-entry
               (ty-class-name e)
               (ty-class-superclasses e)
               (ty-class-predicate e)
               (ty-class-unqualified-methods e)
               (reverse (fset:convert 'list (lookup-class-instances env (ty-class-name e) :no-error t)))
               (coalton-impl/typechecker::ty-class-docstring e)
               (coalton-impl/typechecker::ty-class-location e)))
            values)))
