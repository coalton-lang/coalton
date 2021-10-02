(in-package #:coalton-impl/doc)

(defun write-library-documentation-to-markdown (&optional (env coalton-impl::*global-environment*) (stream t) (file-prefix ""))
  (let* ((package "COALTON-LIBRARY")
         (system ':COALTON)
         (component (asdf:find-component system 'library))
         (component-path (asdf:component-pathname component))
         (*package* (find-package package))
         (value-info (get-doc-value-info env package))
         (type-info (get-doc-type-info env package))
         (class-info (get-doc-class-info env package)))
    (format stream "# Reference for ~A~%~%" package)


    (let ((type-info-by-file (make-hash-table :test #'equalp))
          (value-info-by-file (make-hash-table :test #'equalp))
          (class-info-by-file (make-hash-table :test #'equalp)))

      ;; Sort the functions by file
      (loop :for entry :in value-info :do
        (push entry (gethash (enough-namestring (fourth entry) component-path) value-info-by-file)))

      ;; Sort the types by file
      (loop :for entry :in type-info :do
        (push entry (gethash (enough-namestring (fifth entry) component-path) type-info-by-file)))

      ;; Sort the classes by file
      (loop :for entry :in class-info :do
        (push entry (gethash (enough-namestring (coalton-impl/typechecker::ty-class-location (car entry)) component-path)
                             class-info-by-file)))


      (let ((filenames (mapcar (lambda (file)
                                 (file-namestring (asdf:component-relative-pathname file)))
                               (asdf:component-children component))))

        (dolist (file filenames)
          (let ((value-info (gethash file value-info-by-file))
                (type-info (gethash file type-info-by-file))
                (class-info (gethash file class-info-by-file)))

            (when (or type-info value-info)
              (format stream "## File: [~A](~A)~%~%" file (concatenate 'string file-prefix file))

              (when type-info
                (format stream "### Types~%~%")

                (loop :for (name type ctors instances location) :in type-info :do
                  (with-pprint-variable-context ()
                    (let ((type-vars (loop :for i :below (coalton-impl/typechecker::kind-arity (coalton-impl/typechecker::kind-of type))
                                           :collect (coalton-impl/typechecker::make-variable))))
                      (if type-vars
                          (format stream "#### `~A~{ ~A~}` <sup><sub>[TYPE]</sub></sup><a name=\"~A\"></a>~%" name type-vars name)
                          (format stream "#### `~A` <sup><sub>[TYPE]</sub></sup><a name=\"~A\"></a>~%" name name))

                      (loop :for (ctor-name . entry) :in ctors :do
                        (let ((args (coalton-impl/typechecker::function-type-arguments
                                     (coalton-impl/typechecker::qualified-ty-type
                                      (coalton-impl/typechecker::instantiate type-vars
                                                                             (coalton-impl/typechecker::ty-scheme-type
                                                                              (constructor-entry-scheme entry)))))))
                          (if args
                              (format stream "- `(~A~{ ~A~})`~%" ctor-name args)
                              (format stream "- `~A`~%" ctor-name))))

                      (let ((docs (documentation name 'type)))
                        (when docs
                          (format stream "~%~A~%" docs)))
                      (format stream "~%")

                      (format stream "Constructors:~%")
                      (loop :for (ctor-name . entry) :in ctors :do
                        (format stream "- `~A :: ~A`~%"
                                ctor-name
                                (coalton-impl/typechecker::instantiate type-vars
                                                                       (coalton-impl/typechecker::ty-scheme-type
                                                                        (constructor-entry-scheme entry)))))
                      (format stream "~%")))

                  (when instances
                    (format stream "<details>~%")
                    (format stream "<summary>Instances</summary>~%~%")
                    (loop :for instance :in instances :do
                      (with-pprint-variable-context ()
                        (format stream "- ~A~%"
                                (write-predicate-to-markdown
                                 (ty-class-instance-constraints instance)
                                 (ty-class-instance-predicate instance)))))
                    (format stream "~%</details>~%~%"))

                  (format stream "***~%")
                  (format stream "~%")))

              (when class-info
                (format stream "### Classes~%~%")
                (loop :for (class instances docs) :in class-info :do
                  (let ((name (ty-class-name class))
                        (context (ty-class-superclasses class))
                        (pred (ty-class-predicate class))
                        (methods (ty-class-unqualified-methods class)))
                    (format stream "#### `~A` <sup><sub>[CLASS]</sub></sup><a name=\"~A\"></a>~%" name name)
                    (with-pprint-variable-context ()
                      (format stream "~A~%~%"
                              (write-predicate-to-markdown context pred))

                      (when docs
                        (format stream "~A~%~%" docs))

                      (format stream "Methods:~%")
                      (loop :for (name . type) :in methods :do
                        (format stream "- `~A :: ~A`~%" name type)))

                    (when instances
                      (format stream "~%<details>~%")
                      (format stream "<summary>Instances</summary>~%~%")
                      (loop :for instance :in instances :do
                        (with-pprint-variable-context ()
                          (format stream "- ~A~%"
                                  (write-predicate-to-markdown
                                   (ty-class-instance-constraints instance)
                                   (ty-class-instance-predicate instance)))))
                      (format stream "~%</details>~%~%")))
                  (format stream "~%***~%~%"))
                (format stream "~%"))

              (when value-info
                (format stream "### Functions~%~%")
                (loop :for (name type docstring location) :in value-info :do
                  (progn
                    (format stream "#### `~A` <sup><sub>[FUNCTION]</sub></sup><a name=\"~A\"></a>~%" name name)
                    (format stream "`~A`~%" type)
                    (when docstring
                      (format stream "~%~A~%~%" docstring))
                    (format stream "~%***~%~%")))
                (format stream "~%")))))))))

(defun write-predicate-to-markdown (ctx pred)
  (labels ((base-type (type)
             (typecase type
               (coalton-impl/typechecker::tapp
                (base-type (coalton-impl/typechecker::tapp-from type)))
               (t
                type))))
    (let ((class (ty-predicate-class pred))
          (types (ty-predicate-types pred)))
      (format nil "~{~A ~}~:[~;`=>` ~][`~A`](#~A)~{ ~A~}"
              (loop :for pred :in ctx
                    :for class := (ty-predicate-class pred)
                    :for types := (ty-predicate-types pred)
                    :collect (format nil "[`~A~{ ~A~}`](#~A)" class types class))
              ctx
              class class
              (loop :for type :in types
                    :for base-type :in (mapcar #'base-type types)
                    :collect (format nil "[`~A`](#~A)" type base-type))))))

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
       (list (car e)
             (lookup-value-type env (car e))
             (coalton-impl/typechecker::name-entry-docstring (cdr e))
             (coalton-impl/typechecker::name-entry-location (cdr e))))
     (remove-if-not
      (lambda (x)
        (eql :value (name-entry-type (cdr x))))
      values))))

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
              (list e
                    (fset:convert 'list (lookup-class-instances env (ty-class-name e) :no-error t))
                    (coalton-impl/typechecker::ty-class-docstring e)))
            values)))

(defun get-doc-type-info (env package)
  (let ((types nil)
        (ctors nil)
        (package (find-package package)))
    ;; Sort the entires by package
    (fset:do-map (sym entry (immutable-map-data (coalton-impl/typechecker::environment-type-environment env)))
      ;; Only include exported symbols from our package
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
                (let ((ctors (remove-if-not (lambda (ctor)
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
                                                           (equalp (cdr e) pred)))))
                                               (check pred-type)))
                                           (ty-predicate-types (ty-class-instance-predicate instance)))
                                      (list instance))))))
                  (list (car e)
                        (cdr e)
                        ctors
                        applicable-instances
                        ;; Here we will assume that all constructors
                        ;; share the same location as the type.
                        (coalton-impl/typechecker::name-entry-location
                         (lookup-name env (car (first ctors)))))))
              types))))
