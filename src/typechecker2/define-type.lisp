;;;;
;;;; Handling of toplevel define-type forms. Types are split apart by
;;;; SCC and then type usage in constructors is used to infer the
;;;; kinds of all type variables.
;;;;

(defpackage #:coalton-impl/typechecker2/define-type
  (:use
   #:cl
   #:coalton-impl/typechecker2/base
   #:coalton-impl/typechecker2/parse-type)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:error #:coalton-impl/error)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:toplevel-define-type               ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker2/define-type)

;; TODO: validate that types are defined in the current package

(defun toplevel-define-type (types file env)
  (declare (type parser:toplevel-define-type-list types)
           (type file-stream file)
           (type tc:environment env)
           (values tc:type-definition-list tc:environment))


  ;; Ensure that all type variables are known
  (loop :for type :in types
        :for tyvars := (collect-type-variables type)
        :do (loop :for tyvar :in tyvars
                  :unless (find (parser:tyvar-name tyvar)
                                (parser:toplevel-define-type-vars type)
                                :key #'parser:keyword-src-name)
                    :do (error 'tc-error
                               :err (coalton-error
                                     :span (parser:ty-source tyvar)
                                     :file file
                                     :message "Unknown type variable"
                                     :primary-note (format nil "unknown type variable")))))

  ;; Ensure that there are no duplicate type definitions
  (loop :with table := (make-hash-table :test #'eq)
        :for type :in types
        :for name := (parser:identifier-src-name (parser:toplevel-define-type-name type))
        :if (gethash name table)
          :do (error 'tc-error
                     :err (coalton-error
                           :span (parser:toplevel-define-type-head-src type)
                           :file file
                           :message "Duplicate type definitions"
                           :primary-note "second definition here"
                           :notes
                           (list
                            (make-coalton-error-note
                             :type :primary
                             :span (parser:toplevel-define-type-head-src (gethash name table))
                             :message "first definition here"))))
        :else
          :do (setf (gethash name table) type))

  ;; Ensure that there are no duplicate constructors
  (loop :with table := (make-hash-table :test #'eq)
        :for type :in types
        :do (loop :for ctor :in (parser:toplevel-define-type-ctors type)
                  :for name := (parser:identifier-src-name (parser:constructor-name ctor))
                  :if (gethash name table)
                    :do (error 'tc-error
                               :err (coalton-error
                                     :span (parser:constructor-source ctor)
                                     :file file
                                     :message "Duplicate constructor definitions"
                                     :primary-note "second definition here"
                                     :notes
                                     (list (make-coalton-error-note
                                            :type :primary
                                            :span (parser:constructor-source (gethash name table))
                                            :message "first definition here"))))
                  :else
                    :do (setf (gethash name table) ctor)))

  ;; Ensure that no type has duplicate type variables
  ;; TODO: does this error render incorectly??
  (loop :for type :in types
        :do (loop :with table := (make-hash-table :test #'eq)
                  :for var :in (parser:toplevel-define-type-vars type)
                  :for name := (parser:keyword-src-name var)
                  :if (gethash name table)
                    :do (error 'tc-error
                               :err (coalton-error
                                     :span (parser:keyword-src-source var)
                                     :file file
                                     :message "Duplicate type variable definitions"
                                     :primary-note "second definition here"
                                     :notes
                                     (list (make-coalton-error-note
                                            :type :primary
                                            :span (parser:keyword-src-source (gethash name table))
                                            :message "first definition here"))))
                  :else
                    :do (setf (gethash name table) var)))

  (let* ((type-names (mapcar (alexandria:compose #'parser:identifier-src-name
                                                 #'parser:toplevel-define-type-name)
                             types))

         (type-dependencies
           (loop :for type :in types
                 :for referenced-types := (collect-referenced-types type)
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
                                :collect (tc:kind-of (partial-type-env-add-var partial-env name var)))

                     :for kind := (tc:make-kind-function* kvars tc:+kstar+)
                     :for ty := (tc:make-tycon :name name :kind kind)
                     :do (partial-type-env-add-type partial-env name ty))

           :append (loop :with type-definitions := (infer-define-type-scc-kinds scc partial-env file)
                         :for type :in type-definitions
                         :do (setf env (update-env-for-type-definition type env))
                         :finally (return type-definitions)))
     env)))

(defun update-env-for-type-definition (type env)
  (declare (type tc:type-definition type)
           (type tc:environment env)
           (values tc:environment))

  ;; Define type parsed type in the environment
  (setf env
        (tc:set-type
         env
         (tc:type-definition-name type)
         (tc:make-type-entry
          :name (tc:type-definition-name type)
          :runtime-type (tc:type-definition-runtime-type type)
          :type (tc:type-definition-type type)
          :constructors (mapcar #'tc:constructor-entry-name (tc:type-definition-constructors type))
          :explicit-repr (tc:type-definition-explicit-repr type)
          :enum-repr (tc:type-definition-enum-repr type)
          :newtype (tc:type-definition-newtype type)
          :docstring (tc:type-definition-docstring type)
          :location (or *compile-file-pathname* *load-truename*))))

  ;; Define the type's constructors in the environment
  (loop :for ctor :in (tc:type-definition-constructors type)
        :for ctor-type :in (tc:type-definition-constructor-types type)
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
           (type file-stream file)
           (values tc:type-definition-list))

  (let ((ksubs nil)

        (ctor-table (make-hash-table :test #'eq)))

    ;; Infer the kinds of each type
    (loop :for type :in types
          :for name := (parser:identifier-src-name (parser:toplevel-define-type-name type))
          :do (loop :for ctor :in (parser:toplevel-define-type-ctors type)
                    :for ctor-name := (parser:identifier-src-name (parser:constructor-name ctor))
                    :for fields := (loop :for field :in (parser:constructor-fields ctor)
                                         :collect (multiple-value-bind (type ksubs_)
                                                      (infer-type-kinds field tc:+kstar+ name ksubs env file)
                                                    (setf ksubs ksubs_)
                                                    type))
                    :do (setf (gethash ctor-name ctor-table) fields)))


    ;; Redefine types with final inferred kinds in the environment
    (loop :for type :in types
          :for name := (parser:identifier-src-name (parser:toplevel-define-type-name type))
          :for ty := (gethash name (partial-type-env-ty-table env))
          :for kind := (tc:apply-ksubstitution ksubs (tc:tycon-kind ty))
          :for ksubs_ := (tc:kind-monomorphize-subs (tc:kind-variables kind) ksubs)
          :for tycon := (tc:make-tycon :name name :kind (tc:apply-ksubstitution ksubs_ kind))
          :do (partial-type-env-add-type env name tycon))

    ;; Build type-definitions for each type in the scc
    (loop :for type :in types
          :for name := (parser:identifier-src-name (parser:toplevel-define-type-name type))
          :for tvars := (tc:apply-ksubstitution
                         ksubs
                         (mapcar (lambda (var)
                                   (partial-type-env-lookup-var env name (parser:keyword-src-name var)))
                                 (parser:toplevel-define-type-vars type)))

          :for repr := (parser:toplevel-define-type-repr type)
          :for repr-type := (and repr (parser:keyword-src-name (parser:attribute-repr-type repr)))

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

          :collect (tc:make-type-definition
                    :name name
                    :type (gethash name (partial-type-env-ty-table env))
                    :runtime-type (if (eq repr-type :transparent)
                                      (tc:lisp-type
                                       (tc:function-type-from
                                        (tc:qualified-ty-type
                                         (tc:fresh-inst (first constructor-types))))
                                       (partial-type-env-env env))
                                      name)
                    :explicit-repr (and repr t)
                    :enum-repr (eq repr-type :enum)
                    :newtype (eq repr-type :transparent)

                    :constructors
                    (loop :for ctor :in (parser:toplevel-define-type-ctors type)
                          :for ctor-name := (parser:identifier-src-name (parser:constructor-name ctor))
                          :collect (tc:make-constructor-entry
                                    :name ctor-name
                                    :arity (length (parser:constructor-fields ctor))
                                    :constructs name
                                    :classname (alexandria:format-symbol
                                                *package*
                                                "~A/~A"
                                                name
                                                ctor-name)
                                    :compressed-repr nil))

                    :constructor-types constructor-types
                    :docstring (parser:toplevel-define-type-docstring type)))))
