(in-package #:coalton-impl/doc)

;;;
;;; This is the beginning of the end
;;;

(defstruct documentation-entry
  (name (required 'name) :type symbol)
  (documentation nil :type (or null string))
  location)

(defstruct (documentation-type-entry
            (:include documentation-entry)
            (:constructor make-documentation-type-entry
                (name type constructors instances documentation location)))
  (type (required 'type) :type ty)
  constructors
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
                (filename value-entries type-entries class-entries link-prefix)))
  filename
  value-entries
  type-entries
  class-entries
  link-prefix)

(defgeneric write-documentation (backend stream object)
  (:documentation "Write the given OBJECT to output STREAM. This is
  specialized on the given BACKEND."))

(defun write-documentation-for-packages (&key
                                           (env coalton-impl::*global-environment*)
                                           (stream t)
                                           (packages '(coalton coalton-library))
                                           (base-package 'coalton-library)
                                           (asdf-system ':COALTON)
                                           (file-link-prefix ""))
  (let* ((component (asdf:find-component asdf-system 'library))
         (component-path (asdf:component-pathname component))
         (filenames (mapcar (lambda (file)
                              (file-namestring (asdf:component-relative-pathname file)))
                            (asdf:component-children component)))

         (*package* (find-package base-package)))

    (dolist (package packages)
      (let ((file-entries (collect-documentation-by-file (truename component-path) file-link-prefix env package)))
        (format stream "# Reference for ~A~%~%" package)

        ;; NOTE: We are including the empty filename here to allow for
        ;;       symbols without file information to be included.
        (dolist (file (append '("") filenames))
          (let* ((pathname file)
                 (file-entry (gethash pathname file-entries)))
            (when file-entry
              (write-documentation :markdown stream file-entry))))))))

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
    (sort-documentation-by-file basepath link-prefix value-entries type-entries class-entries)))

;; TODO: We should sort everything here
(defun sort-documentation-by-file (basepath link-prefix value-entries type-entries class-entries)
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
                     filename nil nil nil link-prefix)))))

    ;; Sort the types by file
    (loop :for entry :in type-entries
          :for filename := (enough-namestring (documentation-entry-location entry) basepath) :do
            (push entry
                  (documentation-file-entry-type-entries
                   (alexandria:ensure-gethash
                    filename
                    file-entries
                    (make-documentation-file-entry
                     filename nil nil nil link-prefix)))))

    ;; Sort the classes by file
    (loop :for entry :in class-entries
          :for filename := (enough-namestring (documentation-entry-location entry) basepath) :do
            (push entry
                  (documentation-file-entry-class-entries
                   (alexandria:ensure-gethash
                    filename
                    file-entries
                    (make-documentation-file-entry
                     filename nil nil nil link-prefix)))))

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
     (remove-if-not
      (lambda (x)
        (eql :value (name-entry-type (cdr x))))
      values))))

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
                (let ((ctors (remove-if-not
                              (lambda (ctor)
                                (eql (constructor-entry-constructs (cdr ctor))
                                     (car e)))
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
                   ctors
                   applicable-instances
                   nil ; TODO: grab type documentation
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
