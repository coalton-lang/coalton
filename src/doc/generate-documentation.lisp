(in-package #:coalton-doc)

(defstruct (documentation-entry
            (:constructor nil)
            (:predicate nil))
  (name          (required 'name)          :type symbol           :read-only t)
  (documentation (required 'documentation) :type (or null string) :read-only t)
  (location      (required 'location)      :type t                :read-only t))

(defstruct (documentation-type-entry
            (:include documentation-entry))
  (type              (required 'type)              :type ty :read-only t)
  (constructors      (required 'constructors)      :type t  :read-only t)
  (constructor-types (required 'constructor-types) :type t  :read-only t)
  (instances         (required 'instances)         :type t  :read-only t))

(defun documentation-type-entry-list-p (x)
  (and (every #'documentation-type-entry-p x)
       (alexandria:proper-list-p x)))

(deftype documentation-type-entry-list ()
  '(satisfies documentation-type-entry-list-p))

(defstruct (documentation-class-entry
            (:include documentation-entry))
  (context   (required 'context)   :type t :read-only t)
  (predicate (required 'predicate) :type t :read-only t)
  (methods   (required 'methods)   :type t :read-only t)
  (instances (required 'instances) :type t :read-only t))

(defun documentation-class-entry-list-p (x)
  (and (every #'documentation-class-entry-p x)
       (alexandria:proper-list-p x)))

(deftype documentation-class-entry-list ()
  '(satisfies documentation-class-entry-list-p))

(defstruct (documentation-value-entry
            (:include documentation-entry))
  (type (required 'type) :type ty-scheme))

(defun documentation-value-entry-list-p (x)
  (and (every #'documentation-value-entry-p x)
       (alexandria:proper-list-p x)))

(deftype documentation-value-entry-list ()
  '(satisfies documentation-value-entry-list-p))

(defstruct (documentation-function-entry
            (:include documentation-value-entry))
  (param-names (required 'param-names) :type list :read-only t))

(defstruct documentation-file-entry
  (filename      (required 'filename)      :type t                              :read-only t)
  (package       (required 'package)       :type symbol                         :read-only t)
  (value-entries (required 'value-entries) :type documentation-value-entry-list :read-only nil)
  (type-entries  (required 'type-entries)  :type documentation-type-entry-list  :read-only nil)
  (class-entries (required 'class-entries) :type documentation-class-entry-list :read-only nil)
  (link-prefix   (required 'link-prefix)   :type string                         :read-only t))

(defun documentation-file-entry-list-p (x)
  (and (every #'documentation-file-entry-p x)
       (alexandria:proper-list-p x)))

(deftype documentation-file-entry-list ()
  '(satisfies documentation-file-entry-list-p))

(defstruct documentation-package-entry
  (package       (required 'package)       :type symbol           :read-only t)
  (documentation (required 'documentation) :type (or null string) :read-only t)
  (entries       (required 'entries)       :type hash-table       :read-only t))

(defstruct documentation-package-entries
  (packages    (required 'packages)    :type list       :read-only t)
  (by-package  (required 'by-package)  :type hash-table :read-only t))

(defgeneric write-documentation (backend stream object)
  (:documentation "Write the given OBJECT to output STREAM. This is
  specialized on the given BACKEND."))

(defun write-stdlib-documentation-to-file (filename &key (backend :markdown))
  (write-documentation-to-file
   filename
   :backend backend
   :packages '(coalton
               coalton-library/classes
               coalton-library/types
               coalton-library/builtin
               coalton-library/functions
               coalton-library/math
               coalton-library/math/arith
               coalton-library/math/num
               coalton-library/math/bounded
               coalton-library/math/conversions
               coalton-library/math/fraction
               coalton-library/math/integral
               coalton-library/math/real
               coalton-library/math/complex
               coalton-library/math/elementary
               coalton-library/boolean
               coalton-library/bits
               coalton-library/char
               coalton-library/string
               coalton-library/tuple
               coalton-library/optional
               coalton-library/list
               coalton-library/result
               coalton-library/cell
               coalton-library/vector
               coalton-library/slice
               coalton-library/hashtable
               coalton-library/monad/state
               coalton-library/iterator
               coalton-library/ord-tree
               coalton-library/ord-map)
   :asdf-system :coalton/library
   :file-link-prefix "https://github.com/coalton-lang/coalton/tree/main/library/"))

(defun write-documentation-to-file (filename &key
                                               (backend :markdown)
                                                packages
                                                asdf-system
                                                (file-link-prefix ""))
  (declare (type (or pathname symbol string) filename)
           (type t backend)
           (type symbol-list packages)
           (type (or symbol string) asdf-system)
           (type string file-link-prefix))
  (with-open-file (out filename :direction :output :if-exists :supersede :if-does-not-exist :create)
    (write-documentation-for-packages
     :stream out
     :backend backend
     :packages packages
     :asdf-system asdf-system
     :file-link-prefix file-link-prefix)))

(defun write-documentation-for-packages (&key
                                           (env coalton-impl::*global-environment*)
                                           (stream t)
                                           backend
                                           packages
                                           (base-package 'coalton-prelude)
                                           asdf-system
                                           (file-link-prefix ""))
  (let* ((component (asdf:find-system asdf-system))
         (component-path (asdf:component-pathname component))
         (*package* (find-package base-package)))

    ;; For each package, we just need to collect the require
    ;; documentation and call out to the backend.
    (let ((documentation-by-package (make-hash-table :test #'eq)))
      (loop :for package :in packages
            :for package-documentation := (documentation (find-package package) t)
            :for documentation-entries
              := (collect-documentation-by-file
                  (truename component-path)
                  file-link-prefix
                  env
                  package)
            :do (unless (zerop (hash-table-count documentation-entries))
                  (setf (gethash package documentation-by-package)
                        (make-documentation-package-entry
                         :package package
                         :documentation package-documentation
                         :entries documentation-entries))))

      (write-documentation
       backend
       stream
       (make-documentation-package-entries
        :packages (loop :for package :in packages
                        :if (gethash package documentation-by-package)
                          :collect package)
        :by-package documentation-by-package)))))

(defun collect-documentation (env package)
  "Collect all documentation entries related to coalton code in PACKAGE."
  (declare (type environment env)
           (type symbol package)
           (values
            documentation-value-entry-list
            documentation-type-entry-list
            documentation-class-entry-list))
  (values (get-doc-value-info env package)
          (get-doc-type-info env package)
          (get-doc-class-info env package)))

(defun collect-documentation-by-file (basepath link-prefix env package)
  (declare (type t basepath)
           (type string link-prefix)
           (type environment env)
           (type symbol package)
           (values hash-table &optional))
  (multiple-value-bind (value-entries type-entries class-entries)
      (collect-documentation env package)
    (sort-documentation-by-file basepath link-prefix package value-entries type-entries class-entries)))

;; TODO: We should sort everything here
(defun sort-documentation-by-file (basepath link-prefix package value-entries type-entries class-entries)
  (declare (type t basepath )
           (type string link-prefix)
           (type symbol package)
           (type documentation-value-entry-list value-entries)
           (type documentation-type-entry-list type-entries)
           (type documentation-class-entry-list class-entries)
           (values hash-table))
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
                     :filename filename
                     :package package
                     :value-entries nil
                     :type-entries nil
                     :class-entries nil
                     :link-prefix link-prefix)))))

    ;; Sort the types by file
    (loop :for entry :in type-entries
          :for filename := (enough-namestring (documentation-entry-location entry) basepath) :do
            (push entry
                  (documentation-file-entry-type-entries
                   (alexandria:ensure-gethash
                    filename
                    file-entries
                    (make-documentation-file-entry
                     :filename filename
                     :package package
                     :value-entries nil
                     :type-entries nil
                     :class-entries nil
                     :link-prefix link-prefix)))))

    ;; Sort the classes by file
    (loop :for entry :in class-entries
          :for filename := (enough-namestring (documentation-entry-location entry) basepath) :do
            (push entry
                  (documentation-file-entry-class-entries
                   (alexandria:ensure-gethash
                    filename
                    file-entries
                    (make-documentation-file-entry
                     :filename filename
                     :package package
                     :value-entries nil
                     :type-entries nil
                     :class-entries nil
                     :link-prefix link-prefix)))))

    file-entries))


(defun name-entry->documentation-value-entry (env name name-entry)
  (let ((documentation (coalton-impl/typechecker::name-entry-docstring name-entry))
        (location (coalton-impl/typechecker::name-entry-location name-entry))
        (type (lookup-value-type env name)))
    (alexandria:if-let ((source-param-names
                         (coalton-impl/typechecker:lookup-function-source-parameter-names env name)))
      (make-documentation-function-entry :name name
                                         :type type
                                         :documentation documentation
                                         :location location
                                         :param-names source-param-names)
      (make-documentation-value-entry :name name
                                      :type type
                                      :documentation documentation
                                      :location location))))


(defun get-doc-value-info (env package)
  (declare (type environment env)
           (type symbol package)
           (values documentation-value-entry-list))
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
       (destructuring-bind (name . name-entry) e
         (name-entry->documentation-value-entry env name name-entry)))
     (remove-if
      (lambda (x)
        (coalton-impl/typechecker::lookup-constructor env (car x) :no-error t))
      (remove-if-not
       (lambda (x)
         (eql :value (name-entry-type (cdr x))))
       values)))))

(defun get-doc-type-info (env package)
  (declare (type environment env)
           (type symbol package)
           (values documentation-type-entry-list))
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
                          (coalton-impl/algorithm::immutable-listmap-data
                           (coalton-impl/typechecker::instance-environment-instances
                            (coalton-impl/typechecker::environment-instance-environment env))))))

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
                   :name (car e)
                   :type (type-entry-type (cdr e))
                   :constructors exported-ctors
                   :constructor-types (mapcar
                                       (lambda (ctor)
                                         (lookup-value-type env (car ctor)))
                                       exported-ctors)
                   :instances applicable-instances
                   :documentation (type-entry-docstring (cdr e))
                   ;; Here we will assume that all constructors
                   ;; share the same location as the type.
                   :location (type-entry-location (cdr e)))))
              types))))

(defun get-doc-class-info (env package)
  (declare (type environment env)
           (type symbol package)
           (values documentation-class-entry-list))
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
               :name (ty-class-name e)
               :context (ty-class-superclasses e)
               :predicate (ty-class-predicate e)
               :methods (ty-class-unqualified-methods e)
               :instances (reverse (fset:convert 'list (lookup-class-instances env (ty-class-name e) :no-error t)))
               :documentation (coalton-impl/typechecker::ty-class-docstring e)
               :location (coalton-impl/typechecker::ty-class-location e)))
            values)))
