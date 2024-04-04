(in-package #:coalton-doc)

(defstruct (documentation-entry
            (:constructor nil)
            (:predicate nil))
  (name          (util:required 'name)          :type symbol           :read-only t)
  (documentation (util:required 'documentation) :type (or null string) :read-only t)
  (location      (util:required 'location)      :type t                :read-only t))

(defstruct (documentation-type-entry
            (:include documentation-entry))
  (type              (util:required 'type)              :type tc:ty                     :read-only t)
  (constructors      (util:required 'constructors)      :type t                         :read-only t)
  (constructor-types (util:required 'constructor-types) :type t                         :read-only t)
  (instances         (util:required 'instances)         :type tc:ty-class-instance-list :read-only t))

(defun documentation-type-entry-list-p (x)
  (and (every #'documentation-type-entry-p x)
       (alexandria:proper-list-p x)))

(deftype documentation-type-entry-list ()
  '(satisfies documentation-type-entry-list-p))

(defstruct (documentation-struct-entry
            (:include documentation-entry))
  (type      (util:required 'type)      :type tc:ty                     :read-only t)
  (tyvars    (util:required 'tyvars)    :type tc:tyvar-list             :read-only t)
  (fields    (util:required 'fields)    :type util:string-list          :read-only t)
  (field-tys (util:required 'field-tys) :type hash-table                :read-only t)
  (instances (util:required 'instances) :type tc:ty-class-instance-list :read-only t))

(defun documentation-struct-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'documentation-struct-entry-p x)))

(deftype documentation-struct-entry-list ()
  '(satisfies documentation-struct-entry-list-p))

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
  (filename       (util:required 'filename)       :type t                               :read-only t)
  (package        (util:required 'package)        :type symbol                          :read-only t)
  (value-entries  (util:required 'value-entries)  :type documentation-value-entry-list  :read-only nil)
  (type-entries   (util:required 'type-entries)   :type documentation-type-entry-list   :read-only nil)
  (struct-entries (util:required 'struct-entries) :type documentation-struct-entry-list :read-only nil)
  (class-entries  (util:required 'class-entries)  :type documentation-class-entry-list  :read-only nil)
  (link-prefix    (util:required 'link-prefix)    :type string                          :read-only t))

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
               coalton-library/math/dyadic
               coalton-library/math/dual
               coalton-library/boolean
               coalton-library/bits
               coalton-library/char
               coalton-library/string
               coalton-library/tuple
               coalton-library/lisparray
               coalton-library/optional
               coalton-library/list
               coalton-library/result
               coalton-library/cell
               coalton-library/randomaccess
               coalton-library/vector
               coalton-library/slice
               coalton-library/hashtable
               coalton-library/queue
               coalton-library/monad/state
               coalton-library/monad/free
               coalton-library/iterator
               coalton-library/ord-tree
               coalton-library/ord-map
               coalton-library/seq)
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
            documentation-struct-entry-list
            documentation-class-entry-list))
  (multiple-value-bind (type-entries struct-entries)
      (get-doc-type-info env package)
    (values (get-doc-value-info env package)
            type-entries
            struct-entries
            (get-doc-class-info env package))))

(defun collect-documentation-by-file (basepath link-prefix env package)
  (declare (type t basepath)
           (type string link-prefix)
           (type tc:environment env)
           (type symbol package)
           (values hash-table &optional))
  (multiple-value-bind (value-entries type-entries struct-entries class-entries)
      (collect-documentation env package)
    (sort-documentation-by-file basepath link-prefix package value-entries type-entries struct-entries class-entries)))

;; HACK: We should sort everything here
(defun sort-documentation-by-file (basepath link-prefix package value-entries type-entries struct-entries class-entries)
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
                     :struct-entries nil
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
                     :struct-entries nil
                     :class-entries nil
                     :link-prefix link-prefix)))))

    ;; Sort structs by file
    (loop :for entry :in struct-entries
          :for filename := (enough-namestring (documentation-entry-location entry) basepath) :do
          (push entry
                (documentation-file-entry-struct-entries
                 (alexandria:ensure-gethash
                  filename
                  file-entries
                  (make-documentation-file-entry
                   :filename filename
                   :package package
                   :value-entries nil
                   :type-entries nil
                   :struct-entries nil
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
                     :struct-entries nil
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
  (let* ((package (find-package package))
         (values (env:entries-for-package env :name package :public t)))
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
           (values documentation-type-entry-list documentation-struct-entry-list))
  (let* ((package (find-package package))
         (types (env:entries-for-package env :type package :public t))
         (all-ctors (env:entries-for-package env :constructor package :public t))
         (instance-list nil))
    (env:do-environment (class instances env :instance)
      (declare (ignore class))
      (dolist (instance instances)
        (push instance instance-list)))

    (let ((output-types nil)
          (output-structs nil))

      (loop :for (name . entry) :in types

            :for ctors := (remove-if-not
                           (lambda (ctor)
                             (and (eql (tc:constructor-entry-constructs (cdr ctor))
                                       name)
                                  (env:exported-symbol-p (car ctor) package)))
                           all-ctors)

            :for applicable-instances := (loop :for instance :in instance-list
                                               :when (some
                                                      (lambda (pred-type)
                                                        (labels ((check (pred)
                                                                   (typecase pred
                                                                     (tc:tapp
                                                                      (check (tc:tapp-from pred)))
                                                                     (t
                                                                      (equalp (tc:type-entry-type entry) pred)))))
                                                          (check pred-type)))
                                                      (tc:ty-predicate-types (tc:ty-class-instance-predicate instance)))
                                                 :collect instance)

            :for struct-entry := (tc:lookup-struct env name :no-error t)

            :if (not struct-entry)
              :do (push
                   (make-documentation-type-entry
                    :name name 
                    :type (tc:type-entry-type entry)
                    :constructors ctors
                    :constructor-types (mapcar
                                        (lambda (ctor)
                                          (tc:lookup-value-type env (car ctor)))
                                        ctors)
                    :instances applicable-instances
                    :documentation (tc:type-entry-docstring entry)
                    ;; Here we will assume that all constructors
                    ;; share the same location as the type.
                    :location (tc:type-entry-location entry))
                   output-types)
            :else
              :do (push
                   (make-documentation-struct-entry
                    :name name
                    :documentation (tc:type-entry-docstring entry)
                    :location (tc:type-entry-location entry)
                    :type (tc:type-entry-type entry)
                    :tyvars (tc:type-entry-tyvars entry)
                    :fields (tc:struct-entry-fields struct-entry)
                    :field-tys (tc:struct-entry-field-tys struct-entry)
                    :instances applicable-instances)
                   output-structs))

      (values
       output-types
       output-structs))))

(defun get-doc-class-info (env package)
  (declare (type tc:environment env)
           (type symbol package)
           (values documentation-class-entry-list))
  (let* ((package (find-package package))
         (values (mapcar #'cdr (env:entries-for-package env :class package :public t))))
    (mapcar (lambda (e)
              (make-documentation-class-entry
               :name (tc:ty-class-name e)
               :context (tc:ty-class-superclasses e)
               :predicate (tc:ty-class-predicate e)
               ;; Only include exported methods from our packages
               :methods (remove-if-not
                         (lambda (binding)
                           (env:exported-symbol-p (car binding) package))
                         (tc:ty-class-unqualified-methods e))
               :instances (reverse (tc:lookup-class-instances env (tc:ty-class-name e) :no-error t))
               :documentation (tc:ty-class-docstring e)
               :location (tc:ty-class-location e)))
            values)))
