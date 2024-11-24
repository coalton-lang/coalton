;;;;
;;;; Handling of toplevel define-type forms. Types are split apart by
;;;; SCC and then type usage in constructors is used to infer the
;;;; kinds of all type variables.
;;;;

(defpackage #:coalton-impl/typechecker/define-type
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/parse-type
   #:coalton-impl/typechecker/partial-type-env)
  (:import-from
   #:coalton-impl/typechecker/base
   #:check-package
   #:check-duplicates)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:toplevel-define-type               ; FUNCTION
   #:type-definition                    ; STRUCT
   #:make-type-definition               ; CONSTRUCTOR
   #:type-definition-name               ; ACCESSOR
   #:type-definition-type               ; ACCESSOR
   #:type-definition-runtime-type       ; ACCESSOR
   #:type-definition-explicit-repr      ; ACCESSOR
   #:type-definition-enum-repr          ; ACCESSOR
   #:type-definition-newtype            ; ACCESSOR
   #:type-definition-constructors       ; ACCESSOR
   #:type-definition-constructor-types  ; ACCESSOR
   #:type-definition-docstring          ; ACCESSOR
   #:type-definition-list               ; TYPE
   ))

(in-package #:coalton-impl/typechecker/define-type)

(defstruct type-definition
  (name              (util:required 'name)              :type symbol                   :read-only t)
  (type              (util:required 'type)              :type tc:ty                    :read-only t)
  (runtime-type      (util:required 'runtime-type)      :type t                        :read-only t)

  ;; See the fields with the same name on type-entry
  (explicit-repr     (util:required 'explicit-repr)     :type tc:explicit-repr          :read-only t)
  (enum-repr         (util:required 'enum-repr)         :type boolean                   :read-only t)
  (newtype           (util:required 'newtype)           :type boolean                   :read-only t)

  (constructors      (util:required 'constructors)      :type tc:constructor-entry-list :read-only t)
  (constructor-types (util:required 'constructor-types) :type tc:scheme-list            :read-only t)
  (constructor-args  (util:required 'constructor-args)  :type list                      :read-only t)
  (docstring         (util:required 'docstring)         :type (or null string)          :read-only t)
  (location          (util:required 'location)          :type source:location           :read-only t))

(defmethod source:location ((self type-definition))
  (type-definition-location self))

(defmethod source:docstring ((self type-definition))
  (type-definition-docstring self))

(defstruct field-definition
  (name     (util:required 'name)     :type symbol          :read-only t)
  (type     (util:required 'type)     :type tc:ty           :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self field-definition))
  (field-definition-location self))

(defun type-definition-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'type-definition-p x)))

(deftype type-definition-list ()
  '(satisfies type-definition-list-p))

(defun toplevel-define-type (types structs env)
  (declare (type parser:toplevel-define-type-list types)
           (type parser:toplevel-define-struct-list structs)
           (type tc:environment env)
           (values type-definition-list parser:toplevel-define-instance-list tc:environment))

  ;; Ensure that all types are defined in the current package
  (check-package (append types structs)
                 (alexandria:compose #'parser:identifier-src-name
                                     #'parser:type-definition-name)
                 (alexandria:compose #'source:location
                                     #'parser:type-definition-name))

  ;; Ensure that all constructors are defined in the current package
  (check-package (mapcan (alexandria:compose #'copy-list #'parser:toplevel-define-type-ctors)
                         types)
                 (alexandria:compose #'parser:identifier-src-name
                                     #'parser:constructor-name)
                 (alexandria:compose #'source:location
                                     #'parser:constructor-name))

  ;; Ensure that there are no duplicate type definitions
  (check-duplicates
   (append types structs)
   (alexandria:compose #'parser:identifier-src-name #'parser:type-definition-name)
   (lambda (first second)
     (tc:tc-error "Duplicate type definitions"
                  (tc:tc-note first "first definition here")
                  (tc:tc-note second "second definition here"))))

  ;; Ensure that there are no duplicate constructors
  ;; NOTE: structs define a constructor with the same name
  (check-duplicates
   (mapcan (alexandria:compose #'copy-list #'parser:type-definition-ctors)
           (append types structs))
   (alexandria:compose #'parser:identifier-src-name #'parser:type-definition-ctor-name)
   (lambda (first second)
     (tc:tc-error "Duplicate constructor definitions"
                  (tc:tc-note first "first definition here")
                  (tc:tc-note second "second definition here"))))

  ;; Ensure that no type has duplicate type variables
  (loop :for type :in (append types structs)
        :do (check-duplicates
             (parser:type-definition-vars type)
             #'parser:keyword-src-name
             (lambda (first second)
               (tc:tc-error "Duplicate type variable definitions"
                            (tc:tc-note first "first definition here")
                            (tc:tc-note second "second definition here")))))

  (let* ((type-names (mapcar (alexandria:compose #'parser:identifier-src-name
                                                 #'parser:type-definition-name)
                             (append types structs)))

         (type-dependencies
           (loop :for type :in (append types structs)
                 :for referenced-types := (parser:collect-referenced-types type)
                 :collect (list*
                           (parser:identifier-src-name (parser:type-definition-name type))
                           (intersection type-names (mapcar #'parser:tycon-name referenced-types) :test #'eq))))

         (sccs (algo:tarjan-scc type-dependencies))

         (type-table
           (loop :with table := (make-hash-table :test #'eq)
                 :for type :in (append types structs)
                 :for type-name := (parser:identifier-src-name (parser:type-definition-name type))
                 :do (setf (gethash type-name table) type)
                 :finally (return table)))

         (types-by-scc
           (loop :for scc :in (reverse sccs)
                 :collect (loop :for name :in scc
                                :collect (gethash name type-table))))

         (instances nil))

    (values
     (loop :for scc :in types-by-scc
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

(defun update-env-for-type-definition (type tyvars parsed-type env)
  (declare (type type-definition type)
           (type tc:tyvar-list tyvars)
           (type parser:type-definition parsed-type)
           (type tc:environment env)
           (values tc:environment))

  ;; If the type was previously defined, then undefine all
  ;; constructors that were defined only on the old version of the
  ;; type.
  (let ((old-type (tc:lookup-type env (type-definition-name type) :no-error t)))
    (when old-type
      (loop :for constructor :in (tc:type-entry-constructors old-type)
            :for ctor-entry := (tc:lookup-constructor env constructor)
            :unless (find constructor (type-definition-constructors type) :key #'tc:constructor-entry-name)
              :do (setf env (tc:unset-constructor env constructor))
                  (setf env (tc:unset-value-type env constructor))
                  (setf env (tc:unset-name env constructor))
                  (when (plusp (tc:constructor-entry-arity ctor-entry))
                    (setf env (tc:unset-function env constructor))))))

  (cond ((typep parsed-type 'parser:toplevel-define-struct)
         (let ((fields (loop :for field
                               :in (parser:toplevel-define-struct-fields parsed-type)
                             :for ty
                               :in (first (type-definition-constructor-args type))
                             :for index :from 0
                             :collect (tc:make-struct-field :name (parser:struct-field-name field)
                                                            :type ty
                                                            :index index
                                                            :docstring (source:docstring field)))))
           (setf env (tc:set-struct
                      env
                      (type-definition-name type)
                      (tc:make-struct-entry
                       :name (type-definition-name type)
                       :fields fields
                       :docstring nil)))))
        ((tc:lookup-struct env (type-definition-name type) :no-error t)
         (setf env (tc:unset-struct env (type-definition-name type)))))

  ;; Define type parsed type in the environment
  (setf env
        (tc:set-type
         env
         (type-definition-name type)
         (tc:make-type-entry
          :name (type-definition-name type)
          :runtime-type (type-definition-runtime-type type)
          :type (type-definition-type type)
          :tyvars tyvars
          :constructors (mapcar #'tc:constructor-entry-name (type-definition-constructors type))
          :explicit-repr (type-definition-explicit-repr type)
          :enum-repr (type-definition-enum-repr type)
          :newtype (type-definition-newtype type)
          :docstring (source:docstring type)
          :location (source:location parsed-type))))

  ;; Define the type's constructors in the environment
  (loop :for ctor :in (type-definition-constructors type)
        :for ctor-type :in (type-definition-constructor-types type)

        :for ctor-name := (tc:constructor-entry-name ctor) :do

          ;; Add the constructor to the constructor environment
          (setf env (tc:set-constructor env ctor-name ctor))

          ;; Add the constructor as a value to the value environment
          (setf env (tc:set-value-type env ctor-name ctor-type))

          ;; Register the constructor in the name environment
          (setf env (tc:set-name
                     env
                     ctor-name
                     (tc:make-name-entry
                      :name ctor-name
                      :type :constructor
                      :docstring nil
                      :location (source:location parsed-type))))

          ;; If the constructor takes parameters then
          ;; add it to the function environment
          (cond ((not (= (tc:constructor-entry-arity ctor) 0))
                 (setf env
                       (tc:set-function
                        env
                        ctor-name
                        (tc:make-function-env-entry
                         :name ctor-name
                         :arity (tc:constructor-entry-arity ctor)))))
                ((tc:lookup-function env ctor-name :no-error t)
                 ;; If the constructor does not take
                 ;; parameters then remove it from the
                 ;; function environment
                 (setf env (tc:unset-function env ctor-name)))))

  env)


(defun infer-define-type-scc-kinds (types env)
  (declare (type parser:type-definition-list types)
           (type partial-type-env env)
           (values type-definition-list parser:toplevel-define-instance-list))

  (let ((ksubs nil)

        (ctor-table (make-hash-table :test #'eq))
        (ctor-preds-table (make-hash-table :test #'eq)))

    ;; Infer the kinds of each type
    (loop :for type :in types
          :for name := (parser:identifier-src-name (parser:type-definition-name type))
          :do (loop :for ctor :in (parser:type-definition-ctors type)
                    :for ctor-name := (parser:identifier-src-name (parser:type-definition-ctor-name ctor))
                    :for _ := (when (parser:existential-constructor-p ctor)
                                (loop :for var :in (parser:existential-constructor-vars ctor)
                                      :do (partial-type-env-add-var env (parser:keyword-src-name var))))
                    :for preds := (when (parser:existential-constructor-p ctor)
                                    (loop :for pred :in (parser:existential-constructor-preds ctor)
                                          :collect (multiple-value-bind (pred ksubs_)
                                                       (infer-predicate-kinds pred ksubs env)
                                                     (setf ksubs ksubs_)
                                                     pred)))
                    :for fields := (loop :for field :in (parser:type-definition-ctor-field-types ctor)
                                         :collect (multiple-value-bind (type ksubs_)
                                                      (infer-type-kinds field tc:+kstar+ ksubs env)
                                                    (setf ksubs ksubs_)
                                                    type))
                    :do (progn
                          (setf (gethash ctor-name ctor-table) fields)
                          (when (parser:existential-constructor-p ctor)
                            (setf (gethash ctor-name ctor-preds-table) preds)))))

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
                                       var))
                                    (parser:type-definition-vars type)))

             :for repr := (parser:type-definition-repr type)
             :for repr-type := (and repr (parser:keyword-src-name (parser:attribute-repr-type repr)))
             :for repr-arg := (and repr (eq repr-type :native) (cst:raw (parser:attribute-repr-arg repr))) 

             ;; Apply ksubs to find the type of each constructor
             :for constructor-types
               := (loop :for ctor :in (parser:type-definition-ctors type)
                        :for ctor-name := (parser:identifier-src-name (parser:type-definition-ctor-name ctor))
                        :for ctor-vars := (when (parser:existential-constructor-p ctor)
                                            (tc:apply-ksubstitution
                                             ksubs
                                             (mapcar (lambda (var)
                                                       (partial-type-env-lookup-var
                                                        env
                                                        (parser:keyword-src-name var)
                                                        var))
                                                     (parser:existential-constructor-vars ctor))))
                        :for ctor-preds := (when (parser:existential-constructor-p ctor)
                                             (tc:apply-ksubstitution
                                              ksubs
                                              (gethash ctor-name ctor-preds-table)))
                        :for ty
                          := (tc:make-function-type*
                              (tc:apply-ksubstitution ksubs (gethash ctor-name ctor-table))
                              (tc:apply-type-argument-list
                               (tc:apply-ksubstitution ksubs (gethash name (partial-type-env-ty-table env)))
                               tvars))
                        :for qual-ty := (tc:qualify ctor-preds ty)
                        :collect (tc:quantify-using-tvar-order (append tvars)
                                                               (if (parser:existential-constructor-p ctor)
                                                                   (tc:existentialize ctor-vars qual-ty)
                                                                   qual-ty)))

             :for constructor-args
               := (loop :for ctor :in (parser:type-definition-ctors type)
                        :for ctor-name := (parser:identifier-src-name (parser:type-definition-ctor-name ctor))
                        :collect (tc:apply-ksubstitution ksubs (gethash ctor-name ctor-table)))

             ;; Check that repr :enum types do not have any constructors with fields
             :when (eq repr-type :enum)
               :do (loop :for ctor :in (parser:toplevel-define-type-ctors type)
                         :unless (endp (parser:constructor-fields ctor))
                           :do (tc-error "Invalid repr :enum attribute"
                                         (tc-note (first (parser:constructor-fields ctor))
                                                  "constructors of repr :enum types cannot have fields")))

                   ;; Check that repr :transparent types have a single constructor
             :when (eq repr-type :transparent)
               :do (unless (= 1 (length (parser:type-definition-ctors type)))
                     (tc-error "Invalid repr :transparent attribute"
                               (tc-note type
                                        "repr :transparent types must have a single constructor")))

                   ;; Check that the single constructor of a repr :transparent type has a single field
             :when (eq repr-type :transparent)
               :do (unless (= 1 (length (parser:type-definition-ctor-field-types (first (parser:type-definition-ctors type)))))
                     (tc-error "Invalid repr :transparent attribute"
                               (tc-note (first (parser:type-definition-ctors type))
                                        "constructors of repr :transparent types must have a single field")))

             :collect (let* ((ctors
                               (loop :for ctor :in (parser:type-definition-ctors type)

                                     :for ctor-name := (parser:identifier-src-name
                                                        (parser:type-definition-ctor-name ctor))

                                     :for classname := (alexandria:format-symbol
                                                        *package*
                                                        "~A/~A"
                                                        name
                                                        ctor-name)

                                     :for ctor-docstring := (source:docstring ctor)

                                     :collect (tc:make-constructor-entry
                                               :name ctor-name
                                               :arity (length (parser:type-definition-ctor-field-types ctor))
                                               :existential-p (parser:existential-constructor-p ctor)
                                               :constructs name
                                               :classname classname
                                               :docstring ctor-docstring
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
                                :docstring (source:docstring type)
                                :location (source:location type)))

                             (runtime-repr-instance (maybe-runtime-repr-instance type-definition)))

                        (when runtime-repr-instance
                          (push runtime-repr-instance instances))

                        type-definition))
       instances
       ksubs))))

(defun maybe-runtime-repr-instance (type)
  (declare (type type-definition type))
  (unless (equalp *package* (find-package "COALTON-LIBRARY/TYPES"))
    (make-runtime-repr-instance type)))

(defun make-runtime-repr-instance (type)
  (declare (type type-definition type))

  (let* ((location
           (source:location type))
         (types-package
           (util:find-package "COALTON-LIBRARY/TYPES"))
         (runtime-repr
           (util:find-symbol "RUNTIMEREPR" types-package))
         (runtime-repr-method
           (util:find-symbol "RUNTIME-REPR" types-package))
         (lisp-type
           (util:find-symbol "LISPTYPE" types-package))
         (tvars
           (loop :for i :below (tc:kind-arity (tc:tycon-kind (type-definition-type type)))
                 :collect (parser:make-tyvar
                           :location location
                           :name (alexandria:format-symbol util:+keyword-package+ "~d" i))))
         (ty
           (parser:make-tycon :location location
                              :name (type-definition-name type))))

    (loop :for tvar :in tvars
          :do (setf ty (parser:make-tapp
                        :location location
                        :from ty
                        :to tvar)))

    (parser:make-toplevel-define-instance
     :context nil
     :pred (parser:make-ty-predicate
            :class (parser:make-identifier-src
                    :name runtime-repr
                    :location location)
            :types (list ty)
            :location location)
     :docstring nil
     :methods (list
               (parser:make-instance-method-definition
                :name (parser:make-node-variable
                       :location location
                       :name runtime-repr-method)
                :params (list
                         (parser:make-pattern-wildcard
                          :location location))
                :body (parser:make-node-body
                       :nodes nil
                       :last-node (parser:make-node-lisp
                                   :location location
                                   :type (parser:make-tycon
                                          :location location
                                          :name lisp-type)
                                   :vars nil
                                   :var-names nil
                                   :body (list (util:runtime-quote (type-definition-runtime-type type)))))
                :location location))
     :location location
     :head-location location
     :compiler-generated t)))
