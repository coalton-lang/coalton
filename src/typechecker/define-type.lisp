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
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:error #:coalton-impl/error)
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

;;; TODO: add autogeneration of RuntimeRepr instances

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

  (docstring         (util:required 'docstring)         :type (or null string)       :read-only t))

(defun type-definition-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'type-definition-p x)))

(deftype type-definition-list ()
  '(satisfies type-definition-list-p))

(defun toplevel-define-type (types file env)
  (declare (type parser:toplevel-define-type-list types)
           (type coalton-file file)
           (type tc:environment env)
           (values type-definition-list tc:environment))

  ;; Ensure that all types are defined in the current package
  (check-package
   types
   (alexandria:compose #'parser:identifier-src-name #'parser:toplevel-define-type-name)
   (alexandria:compose #'parser:identifier-src-source #'parser:toplevel-define-type-name)
   file)

  ;; Ensure that all constructors are defined in the current package
  (check-package
   (mapcan (alexandria:compose #'copy-list #'parser:toplevel-define-type-ctors) types)
   (alexandria:compose #'parser:identifier-src-name #'parser:constructor-name)
   (alexandria:compose #'parser:identifier-src-source #'parser:constructor-name)
   file)

  ;; Ensure that there are no duplicate type definitions
  (check-duplicates
   types
   (alexandria:compose #'parser:identifier-src-name #'parser:toplevel-define-type-name)
   (lambda (first second)
     (error 'tc-error
            :err (coalton-error
                  :span (parser:toplevel-define-type-head-src first)
                  :file file
                  :message "Duplicate type definitions"
                  :primary-note "first definition here"
                  :notes
                  (list
                   (make-coalton-error-note
                    :type :primary
                    :span (parser:toplevel-define-type-head-src second)
                    :message "second definition here"))))))

  ;; Ensure that there are no duplicate constructors
  (check-duplicates
   (mapcan (alexandria:compose #'copy-list #'parser:toplevel-define-type-ctors) types)
   (alexandria:compose #'parser:identifier-src-name #'parser:constructor-name)
   (lambda (first second)
     (error 'tc-error
            :err (coalton-error
                  :span (parser:constructor-source first)
                  :file file
                  :message "Duplicate constructor definitions"
                  :primary-note "first definition here"
                  :notes
                  (list (make-coalton-error-note
                         :type :primary
                         :span (parser:constructor-source second)
                         :message "second definition here"))))))

  ;; Ensure that no type has duplicate type variables
  (loop :for type :in types
        :do (check-duplicates
             (parser:toplevel-define-type-vars type)
             #'parser:keyword-src-name
             (lambda (first second)
               (error 'tc-error
                      :err (coalton-error
                            :span (parser:keyword-src-source first)
                            :file file
                            :message "Duplicate type variable definitions"
                            :primary-note "first definition here"
                            :notes
                            (list (make-coalton-error-note
                                   :type :primary
                                   :span (parser:keyword-src-source second)
                                   :message "second definition here")))))))

  (let* ((type-names (mapcar (alexandria:compose #'parser:identifier-src-name
                                                 #'parser:toplevel-define-type-name)
                             types))

         (type-dependencies
           (loop :for type :in types
                 :for referenced-types := (parser:collect-referenced-types type)
                 :collect (list*
                           (parser:identifier-src-name (parser:toplevel-define-type-name type))
                           (intersection type-names (mapcar #'parser:tycon-name referenced-types)))))

         (sccs (algo:tarjan-scc type-dependencies))

         (type-table
           (loop :with table := (make-hash-table :test #'eq)
                 :for type :in types
                 :for type-name := (parser:identifier-src-name (parser:toplevel-define-type-name type))
                 :do (setf (gethash type-name table) type)
                 :finally (return table)))

         (types-by-scc
           (loop :for scc :in sccs
                 :collect (loop :for name :in scc
                                :collect (gethash name type-table)))))

    (values
     (loop :for scc :in types-by-scc
           :for partial-env := (make-partial-type-env :env env)

           ;; Register each type in the partial type env
           :do (loop :for type :in scc
                     :for name := (parser:identifier-src-name (parser:toplevel-define-type-name type))
                     :for vars := (mapcar #'parser:keyword-src-name (parser:toplevel-define-type-vars type))

                     ;; Register each type's type variables in the environment. A
                     ;; mapping is stored from (type-name, var-name) to kind-variable
                     ;; because type variable names are not unique across between
                     ;; define-types.
                     :for kvars
                       := (loop :for var :in vars
                                :collect (tc:kind-of (partial-type-env-add-var partial-env var)))

                     :for kind := (tc:make-kind-function* kvars tc:+kstar+)
                     :for ty := (tc:make-tycon :name name :kind kind)
                     :do (partial-type-env-add-type partial-env name ty))

           :append (loop :with type-definitions := (infer-define-type-scc-kinds scc partial-env file)
                         :for type :in type-definitions
                         :do (setf env (update-env-for-type-definition type env))
                         :finally (return type-definitions)))
     env)))

(defun update-env-for-type-definition (type env)
  (declare (type type-definition type)
           (type tc:environment env)
           (values tc:environment))

  ;; Define type parsed type in the environment
  (setf env
        (tc:set-type
         env
         (type-definition-name type)
         (tc:make-type-entry
          :name (type-definition-name type)
          :runtime-type (type-definition-runtime-type type)
          :type (type-definition-type type)
          :constructors (mapcar #'tc:constructor-entry-name (type-definition-constructors type))
          :explicit-repr (type-definition-explicit-repr type)
          :enum-repr (type-definition-enum-repr type)
          :newtype (type-definition-newtype type)
          :docstring (type-definition-docstring type)
          :location (or *compile-file-pathname* *load-truename*))))

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
                      :location (or *compile-file-pathname*
                                    *load-truename*))))

          ;; If the constructor takes paramaters then
          ;; add it to the function environment
          (if (not (= (tc:constructor-entry-arity ctor) 0))
              (setf env
                    (tc:set-function
                     env
                     ctor-name
                     (tc:make-function-env-entry
                      :name ctor-name
                      :arity (tc:constructor-entry-arity ctor))))

              ;; If the constructor does not take
              ;; parameters then remove it from the
              ;; function environment
              (setf env (tc:unset-function env ctor-name))))

  env)

(defun infer-define-type-scc-kinds (types env file)
  (declare (type parser:toplevel-define-type-list types)
           (type partial-type-env env)
           (type coalton-file file)
           (values type-definition-list))

  (let ((ksubs nil)

        (ctor-table (make-hash-table :test #'eq)))

    ;; Infer the kinds of each type
    (loop :for type :in types
          :for name := (parser:identifier-src-name (parser:toplevel-define-type-name type))
          :do (loop :for ctor :in (parser:toplevel-define-type-ctors type)
                    :for ctor-name := (parser:identifier-src-name (parser:constructor-name ctor))
                    :for fields := (loop :for field :in (parser:constructor-fields ctor)
                                         :collect (multiple-value-bind (type ksubs_)
                                                      (infer-type-kinds field tc:+kstar+ ksubs env file)
                                                    (setf ksubs ksubs_)
                                                    type))
                    :do (setf (gethash ctor-name ctor-table) fields)))
    
    ;; Redefine types with final inferred kinds in the environment
    (loop :for type :in types
          :for name := (parser:identifier-src-name (parser:toplevel-define-type-name type))
          :for ty := (gethash name (partial-type-env-ty-table env))
          :for kind := (tc:apply-ksubstitution ksubs (tc:tycon-kind ty))
          :do (setf ksubs (tc:kind-monomorphize-subs (tc:kind-variables kind) ksubs))
          :do (partial-type-env-replace-type env name (tc:make-tycon :name name :kind (tc:apply-ksubstitution ksubs kind))))

    ;; Build type-definitions for each type in the scc
    (loop :for type :in types
          :for name := (parser:identifier-src-name (parser:toplevel-define-type-name type))
          :for tvars := (tc:apply-ksubstitution
                         ksubs
                         (mapcar (lambda (var)
                                   (partial-type-env-lookup-var
                                    env
                                    (parser:keyword-src-name var)
                                    (parser:keyword-src-source var)
                                    file))
                                 (parser:toplevel-define-type-vars type)))

          :for repr := (parser:toplevel-define-type-repr type)
          :for repr-type := (and repr (parser:keyword-src-name (parser:attribute-repr-type repr)))
          :for repr-arg := (and repr (eq repr-type :native) (cst:raw (parser:attribute-repr-arg repr))) 

          ;; Apply ksubs to find the type of each constructor
          :for constructor-types
            := (loop :for ctor :in (parser:toplevel-define-type-ctors type)
                     :for ctor-name := (parser:identifier-src-name (parser:constructor-name ctor))
                     :for ty
                       := (tc:make-function-type*
                           (tc:apply-ksubstitution ksubs (gethash ctor-name ctor-table))
                           (tc:apply-type-argument-list
                            (tc:apply-ksubstitution ksubs (gethash name (partial-type-env-ty-table env)))
                            tvars))
                     :collect (tc:quantify-using-tvar-order tvars (tc:qualify nil ty)))

          ;; Check that repr :enum types do not have any constructors with fields
          :when (eq repr-type :enum)
            :do (loop :for ctor :in (parser:toplevel-define-type-ctors type)
                      :unless (endp (parser:constructor-fields ctor))
                        :do (error 'tc-error
                                   :err (coalton-error
                                         :span (parser:ty-source (first (parser:constructor-fields ctor)))
                                         :file file
                                         :message "Invalid repr :enum attribute"
                                         :primary-note "constructors of repr :enum types cannot have fields")))

                ;; Check that repr :transparent types have a single constructor
          :when (eq repr-type :transparent)
            :do (unless (= 1 (length (parser:toplevel-define-type-ctors type)))
                  (error 'tc-error
                         :err (coalton-error
                               :span (parser:toplevel-define-type-source type)
                               :file file
                               :message "Invalid repr :transparent attribute"
                               :primary-note "repr :transparent types must have a single constructor")))

                
                ;; Check that the single constructor of a repr :transparent type has a single field
          :when (eq repr-type :transparent)
            :do (unless (= 1 (length (parser:constructor-fields (first (parser:toplevel-define-type-ctors type)))))
                  (error 'tc-error
                         :err (coalton-error
                               :span (parser:constructor-source (first (parser:toplevel-define-type-ctors type)))
                               :file file
                               :message "Invalid repr :transparent attribute"
                               :primary-note "constructors of repr :transparent types must have a single field")))

          :collect (let ((ctors
                           (loop :for ctor :in (parser:toplevel-define-type-ctors type)
                                 :for ctor-name := (parser:identifier-src-name (parser:constructor-name ctor))
                                 :for classname := (alexandria:format-symbol
                                                    *package*
                                                    "~A/~A"
                                                    name
                                                    ctor-name)
                                 :collect (tc:make-constructor-entry
                                           :name ctor-name
                                           :arity (length (parser:constructor-fields ctor))
                                           :constructs name
                                           :classname classname
                                           :compressed-repr (if (eq repr-type :enum)
                                                                classname
                                                                nil)))))
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
                      :docstring (parser:toplevel-define-type-docstring type))))))
