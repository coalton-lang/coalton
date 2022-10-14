(in-package #:coalton-doc)

(defstruct (documentation-entry
            (:constructor nil)
            (:predicate nil))
  (name          (util:required 'name)          :type symbol           :read-only t)
  (documentation (util:required 'documentation) :type (or null string) :read-only t)
  (location      (util:required 'location)      :type t                :read-only t))

(defstruct (documentation-type-entry
            (:include documentation-entry))
  (type              (util:required 'type)              :type tc:ty :read-only t)
  (constructors      (util:required 'constructors)      :type t     :read-only t)
  (constructor-types (util:required 'constructor-types) :type t     :read-only t)
  (instances         (util:required 'instances)         :type t     :read-only t))

(defun documentation-type-entry-list-p (x)
  (and (every #'documentation-type-entry-p x)
       (alexandria:proper-list-p x)))

(deftype documentation-type-entry-list ()
  '(satisfies documentation-type-entry-list-p))

(defstruct (documentation-class-entry
            (:include documentation-entry))
  (context   (util:required 'context)   :type t :read-only t)
  (predicate (util:required 'predicate) :type t :read-only t)
  (methods   (util:required 'methods)   :type t :read-only t)
  (instances (util:required 'instances) :type t :read-only t))

(defun documentation-class-entry-list-p (x)
  (and (every #'documentation-class-entry-p x)
       (alexandria:proper-list-p x)))

(deftype documentation-class-entry-list ()
  '(satisfies documentation-class-entry-list-p))

(defstruct (documentation-value-entry
            (:include documentation-entry))
  (type (util:required 'type) :type tc:ty-scheme))

(defun documentation-value-entry-list-p (x)
  (and (every #'documentation-value-entry-p x)
       (alexandria:proper-list-p x)))

(deftype documentation-value-entry-list ()
  '(satisfies documentation-value-entry-list-p))

(defstruct (documentation-function-entry
            (:include documentation-value-entry))
  (param-names (util:required 'param-names) :type list :read-only t))

(defstruct documentation-file-entry
  (filename      (util:required 'filename)      :type t                              :read-only t)
  (package       (util:required 'package)       :type symbol                         :read-only t)
  (value-entries (util:required 'value-entries) :type documentation-value-entry-list :read-only nil)
  (type-entries  (util:required 'type-entries)  :type documentation-type-entry-list  :read-only nil)
  (class-entries (util:required 'class-entries) :type documentation-class-entry-list :read-only nil)
  (link-prefix   (util:required 'link-prefix)   :type string                         :read-only t))

(defun documentation-file-entry-list-p (x)
  (and (every #'documentation-file-entry-p x)
       (alexandria:proper-list-p x)))

(deftype documentation-file-entry-list ()
  '(satisfies documentation-file-entry-list-p))

(defstruct documentation-package-entry
  (package       (util:required 'package)       :type symbol           :read-only t)
  (documentation (util:required 'documentation) :type (or null string) :read-only t)
  (entries       (util:required 'entries)       :type hash-table       :read-only t))

(defstruct documentation-package-entries
  (packages    (util:required 'packages)    :type list       :read-only t)
  (by-package  (util:required 'by-package)  :type hash-table :read-only t))

(defgeneric write-documentation (backend stream object)
  (:documentation "Write the given OBJECT to output STREAM. This is
  specialized on the given BACKEND."))

(defun write-stdlib-documentation-to-file (filename &key (backend :markdown))
  (write-documentation-to-file
   filename
   :backend backend
   :packages '(coalton
               coalton-library/classes
               coalton-library/hash
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
           (type util:symbol-list packages)
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
                                           (env entry:*global-environment*)
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
  (declare (type tc:environment env)
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
           (type tc:environment env)
           (type symbol package)
           (values hash-table &optional))
  (multiple-value-bind (value-entries type-entries class-entries)
      (collect-documentation env package)
    (sort-documentation-by-file basepath link-prefix package value-entries type-entries class-entries)))

;; HACK: We should sort everything here
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
  (let ((documentation (tc:name-entry-docstring name-entry))
        (location (tc:name-entry-location name-entry))
        (type (tc:lookup-value-type env name)))
    (alexandria:if-let ((source-param-names
                         (tc:lookup-function-source-parameter-names env name)))
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
  (declare (type tc:environment env)
           (type symbol package)
           (values documentation-value-entry-list))
  (let ((values nil)
        (package (find-package package)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (algo:immutable-map-data (tc:environment-name-environment env)))
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
        (tc:lookup-constructor env (car x) :no-error t))
      (remove-if-not
       (lambda (x)
         (eql :value (tc:name-entry-type (cdr x))))
       values)))))

(defun get-doc-type-info (env package)
  (declare (type tc:environment env)
           (type symbol package)
           (values documentation-type-entry-list))
  (let ((types nil)
        (ctors nil)
        (package (find-package package)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (algo:immutable-map-data (tc:environment-type-environment env)))
      ;; Only include exported symbols from our packages
      (when (and (equalp (symbol-package sym) package)
                 (multiple-value-bind (symbol status)
                     (find-symbol (symbol-name sym) package)
                   (declare (ignore symbol))
                   (eql :external status)))
        (push (cons sym entry) types)))
    (fset:do-map (sym entry (algo:immutable-map-data (tc:environment-constructor-environment env)))
      (when (equalp (symbol-package sym) package)
        (push (cons sym entry) ctors)))

    (let ((instance-list
            (fset:convert 'list
                          (algo:immutable-listmap-data
                           (tc:instance-environment-instances
                            (tc:environment-instance-environment env))))))

      (mapcar (lambda (e)
                (let* ((ctors (remove-if-not
                               (lambda (ctor)
                                 (and (eql (tc:constructor-entry-constructs (cdr ctor))
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
                                                           (tc:tapp
                                                            (check (tc:tapp-from pred)))
                                                           (t
                                                            (equalp (tc:type-entry-type (cdr e)) pred)))))
                                                (check pred-type)))
                                            (tc:ty-predicate-types (tc:ty-class-instance-predicate instance)))
                                       (list instance))))))
                  (make-documentation-type-entry
                   :name (car e)
                   :type (tc:type-entry-type (cdr e))
                   :constructors exported-ctors
                   :constructor-types (mapcar
                                       (lambda (ctor)
                                         (tc:lookup-value-type env (car ctor)))
                                       exported-ctors)
                   :instances applicable-instances
                   :documentation (tc:type-entry-docstring (cdr e))
                   ;; Here we will assume that all constructors
                   ;; share the same location as the type.
                   :location (tc:type-entry-location (cdr e)))))
              types))))

(defun get-doc-class-info (env package)
  (declare (type tc:environment env)
           (type symbol package)
           (values documentation-class-entry-list))
  (let ((values nil)
        (package (find-package package)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (algo:immutable-map-data (tc:environment-class-environment env)))
      ;; Only include exported symbols from our package
      (when (and (equalp (symbol-package sym) package)
                 (multiple-value-bind (symbol status)
                     (find-symbol (symbol-name sym) package)
                   (declare (ignore symbol))
                   (eql :external status)))
        (push entry values)))

    (mapcar (lambda (e)
              (make-documentation-class-entry
               :name (tc:ty-class-name e)
               :context (tc:ty-class-superclasses e)
               :predicate (tc:ty-class-predicate e)
               :methods (tc:ty-class-unqualified-methods e)
               :instances (reverse (fset:convert 'list (tc:lookup-class-instances env (tc:ty-class-name e) :no-error t)))
               :documentation (tc:ty-class-docstring e)
               :location (tc:ty-class-location e)))
            values)))
