;;;;
;;;; Handling of toplevel define-type forms. Types are split apart by
;;;; SCC and then type usage in constructors is used to infer the
;;;; kinds of all type variables.
;;;;

(defpackage #:coalton-impl/typechecker/define-alias
  (:use
   #:cl
   #:coalton-impl/source
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/parse-type
   #:coalton-impl/typechecker/partial-type-env)
  (:import-from
   #:coalton-impl/typechecker/base
   #:check-package
   #:check-duplicates)
  (:local-nicknames
   (#:se #:source-error)
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:toplevel-define-alias               ; FUNCTION
   #:alias-definition                    ; STRUCT
   #:make-alias-definition               ; CONSTRUCTOR
   #:alias-definition-base-type              ; ACCESSOR
   #:alias-definition-name               ; ACCESSOR
   #:alias-definition-docstring       ; ACCESSOR
   #:alias-definition-location        ; ACCESSOR
   ))

(in-package #:coalton-impl/typechecker/define-alias)

(defstruct alias-definition
  (base-type (util:required 'base-type) :type tc:ty            :read-only t)
  (name      (util:required 'name)      :type symbol           :read-only t)
  (docstring (util:required 'docstring) :type (or null string) :read-only t)
  (location  (util:required 'location)  :type location         :read-only t))

(defmethod docstring ((self alias-definition))
  (alias-definition-docstring self))

(defun toplevel-define-alias (types structs aliases env)
  (declare (type parser:toplevel-define-type-list aliases)
           (type parser:toplevel-define-struct-list structs)
           (type tc:environment env)
           (values type-definition-list parser:toplevel-define-instance-list tc:environment))

  ;; Ensure that all types are defined in the current package
  (check-package (append types structs aliases)
                 (alexandria:compose #'parser:identifier-src-name
                                     #'parser:type-definition-name)
                 (alexandria:compose #'parser:identifier-src-location
                                     #'parser:type-definition-name))

  ;; Ensure that all constructors are defined in the current package
  (check-package (mapcan (alexandria:compose #'copy-list #'parser:toplevel-define-type-ctors)
                         types)
                 (alexandria:compose #'parser:identifier-src-name
                                     #'parser:constructor-name)
                 (alexandria:compose #'parser:identifier-src-location
                                     #'parser:constructor-name))

  ;; Ensure that there are no duplicate type definitions
  (check-duplicates
   (append types structs aliases)
   (alexandria:compose #'parser:identifier-src-name #'parser:type-definition-name)
   #'parser:type-definition-location
   (lambda (first second)
     (error 'tc:tc-error
            :err (source-error
                  :location (parser:type-definition-location first)
                  :message "Duplicate type definitions"
                  :primary-note "first definition here"
                  :notes (list (se:make-source-error-note
                                :type :primary
                                :span (location-span
                                       (parser:type-definition-location second))
                                :message "second definition here"))))))

  (let* ((type-names (mapcar (alexandria:compose #'parser:identifier-src-name
                                                 #'parser:type-definition-name)
                             (append types structs aliases)))

         (type-dependencies
           (loop :for type :in (append types structs aliases)
                 :for referenced-types := (parser:collect-referenced-types type)
                 :collect (list*
                           (parser:identifier-src-name (parser:type-definition-name type))
                           (intersection type-names (mapcar #'parser:tycon-name referenced-types) :test #'eq))))

         (sccs (algo:tarjan-scc type-dependencies))

         (type-table
           (loop :with table := (make-hash-table :test #'eq)
                 :for type :in (append types structs aliases)
                 :for type-name := (parser:identifier-src-name (parser:type-definition-name type))
                 :do (setf (gethash type-name table) type)
                 :finally (return table)))



         (instances nil))

    (values
     (update-env-for-alias-definition )
     #+ig(loop :for scc :in types-by-scc
           :for partial-env := (make-partial-type-env :env env)

           ;; Register each type in the partial type env
           :do (loop :for type :in scc
                     :for name := (parser:identifier-src-name (parser:type-definition-name type))
                     :for vars := (mapcar #'parser:keyword-src-name (parser:type-definition-vars type))

                     ;; Register each type's type variables in the environment. A
                     ;; mapping is stored from (type-name, var-name) to kind-variable
                     ;; because type variable names are not unique between define-types.
                     :for kvars
                       := (loop :for var :in vars
                                :collect (tc:kind-of (partial-type-env-add-var partial-env var)))

                     :for kind := (tc:make-kind-function* kvars tc:+kstar+)
                     :for ty := (tc:make-tycon :name name :kind kind)
                     :do (partial-type-env-add-type partial-env name ty))

           :append  (multiple-value-bind (type-definitions instances_ ksubs)
                        (infer-define-type-scc-kinds scc partial-env)
                      (setf instances (append instances instances_))
                      (loop :for type :in type-definitions

                            :for parser-type := (gethash (type-definition-name type) type-table)

                            :for vars := (tc:apply-ksubstitution
                                          ksubs
                                          (loop :for var :in (parser:type-definition-vars parser-type)
                                                :for var-name := (parser:keyword-src-name var)
                                                :collect (gethash var-name (partial-type-env-ty-table partial-env))))

                            :do (setf env (update-env-for-type-definition type vars parser-type env))
                            :finally (return type-definitions))))
     instances
     env)))

(defun update-env-for-alias-definition (type env)
  (declare (type alias-definition type)
           (type tc:environment env)
           (values tc:environment))

  (setf env
        (tc:set-alias
         env
         (alias-definition-name type)
         (tc:make-alias-entry
          :name (alias-definition-name type)
          :base-type (alias-definition-base-type type)
          :docstring (docstring type)
          :location (alias-definition-location type))))

  env)


#+ig(defun infer-define-type-scc-kinds (types env)
  (declare (type parser:type-definition-list types)
           (type partial-type-env env)
           (values type-definition-list parser:toplevel-define-instance-list))

  (let ((ksubs nil)

        (ctor-table (make-hash-table :test #'eq)))

    ;; Infer the kinds of each type
    (loop :for type :in types
          :for name := (parser:identifier-src-name (parser:type-definition-name type))
          :do (loop :for ctor :in (parser:type-definition-ctors type)
                    :for ctor-name := (parser:identifier-src-name (parser:type-definition-ctor-name ctor))
                    :for fields := (loop :for field :in (parser:type-definition-ctor-field-types ctor)
                                         :collect (multiple-value-bind (type ksubs_)
                                                      (infer-type-kinds field tc:+kstar+ ksubs env)
                                                    (setf ksubs ksubs_)
                                                    type))
                    :do (setf (gethash ctor-name ctor-table) fields)))

    ;; Redefine types with final inferred kinds in the environment
    (loop :for type :in types
          :for name := (parser:identifier-src-name (parser:type-definition-name type))
          :for ty := (gethash name (partial-type-env-ty-table env))
          :for kind := (tc:apply-ksubstitution ksubs (tc:tycon-kind ty))
          :do (setf ksubs (tc:kind-monomorphize-subs (tc:kind-variables kind) ksubs))
          :do (partial-type-env-replace-type env name (tc:make-tycon
                                                       :name name
                                                       :kind (tc:apply-ksubstitution ksubs kind))))

    ;; Build type-definitions for each type in the scc
    (let ((instances nil))
      (values
       (loop :for type :in types
             :for name := (parser:identifier-src-name (parser:type-definition-name type))
             :for tvars := (tc:apply-ksubstitution
                            ksubs
                            (mapcar (lambda (var)
                                      (partial-type-env-lookup-var
                                       env
                                       (parser:keyword-src-name var)
                                       (parser:keyword-src-location var)))
                                    (parser:type-definition-vars type)))

             :for repr := (parser:type-definition-repr type)
             :for repr-type := (and repr (parser:keyword-src-name (parser:attribute-repr-type repr)))
             :for repr-arg := (and repr (eq repr-type :native) (cst:raw (parser:attribute-repr-arg repr)))

             ;; Apply ksubs to find the type of each constructor
             :for constructor-types
               := (loop :for ctor :in (parser:type-definition-ctors type)
                        :for ctor-name := (parser:identifier-src-name (parser:type-definition-ctor-name ctor))
                        :for ty
                          := (tc:make-function-type*
                              (tc:apply-ksubstitution ksubs (gethash ctor-name ctor-table))
                              (tc:apply-type-argument-list
                               (tc:apply-ksubstitution ksubs (gethash name (partial-type-env-ty-table env)))
                               tvars))
                        :collect (tc:quantify-using-tvar-order tvars (tc:qualify nil ty)))

             :for constructor-args
              := (loop :for ctor :in (parser:type-definition-ctors type)
                       :for ctor-name := (parser:identifier-src-name (parser:type-definition-ctor-name ctor))
                       :collect (tc:apply-ksubstitution ksubs (gethash ctor-name ctor-table)))

             ;; Check that repr :enum types do not have any constructors with fields
             :when (eq repr-type :enum)
               :do (loop :for ctor :in (parser:toplevel-define-type-ctors type)
                         :unless (endp (parser:constructor-fields ctor))
                           :do (tc-error (parser:ty-location (first (parser:constructor-fields ctor)))
                                         "Invalid repr :enum attribute"
                                         "constructors of repr :enum types cannot have fields"))

                   ;; Check that repr :transparent types have a single constructor
             :when (eq repr-type :transparent)
               :do (unless (= 1 (length (parser:type-definition-ctors type)))
                     (tc-error (parser:toplevel-define-type-location type)
                               "Invalid repr :transparent attribute"
                               "repr :transparent types must have a single constructor"))

                   ;; Check that the single constructor of a repr :transparent type has a single field
             :when (eq repr-type :transparent)
               :do (unless (= 1 (length (parser:type-definition-ctor-field-types (first (parser:type-definition-ctors type)))))
                     (tc-error (parser:type-definition-ctor-location (first (parser:type-definition-ctors type)))
                               "Invalid repr :transparent attribute"
                               "constructors of repr :transparent types must have a single field"))

             :collect (let* ((ctors
                               (loop :for ctor :in (parser:type-definition-ctors type)

                                     :for ctor-name := (parser:identifier-src-name
                                                        (parser:type-definition-ctor-name ctor))

                                     :for classname := (alexandria:format-symbol
                                                        *package*
                                                        "~A/~A"
                                                        name
                                                        ctor-name)
                                     :collect (tc:make-constructor-entry
                                               :name ctor-name
                                               :arity (length (parser:type-definition-ctor-field-types ctor))
                                               :constructs name
                                               :classname classname
                                               :compressed-repr (if (eq repr-type :enum)
                                                                    classname
                                                                    nil))))

                             (type-definition
                               (make-type-definition
                                :name name
                                :type (gethash name (partial-type-env-ty-table env))
                                :runtime-type (cond
                                                ((eq repr-type :transparent)
                                                 (tc:lisp-type
                                                  (tc:function-type-from
                                                   (tc:qualified-ty-type
                                                    (tc:fresh-inst (first constructor-types))))
                                                  (partial-type-env-env env)))
                                                ((eq repr-type :native)
                                                 repr-arg)
                                                ((eq repr-type :enum)
                                                 `(member ,@(mapcar #'tc:constructor-entry-compressed-repr ctors)))
                                                (t
                                                 name))
                                :explicit-repr (if (eq repr-type :native)
                                                   (list repr-type repr-arg)
                                                   repr-type)
                                :enum-repr (eq repr-type :enum)
                                :newtype (eq repr-type :transparent)

                                :constructors ctors

                                :constructor-types constructor-types
                                :constructor-args constructor-args
                                :docstring (docstring type)
                                :location (parser:type-definition-location type)))

                             (runtime-repr-instance (maybe-runtime-repr-instance type-definition)))

                        (when runtime-repr-instance
                          (push runtime-repr-instance instances))

                        type-definition))
       instances
       ksubs))))
