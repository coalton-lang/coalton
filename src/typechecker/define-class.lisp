(defpackage #:coalton-impl/typechecker/define-class
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
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker/stage-1))
  (:export
   #:toplevel-define-class              ; FUNCTION
   ))

;;;;
;;;; Type Class Definition Processing with SCC-Based Dependency Resolution
;;;;
;;;; This module handles toplevel define-class forms and ensures that type class
;;;; hierarchies are processed in the correct dependency order using strongly
;;;; connected components (SCCs).
;;;;

(in-package #:coalton-impl/typechecker/define-class)

(defstruct partial-class
  (superclasses (util:required 'superclasses) :type tc:ty-predicate-list :read-only t)
  (method-tys   (util:required 'method-tys)   :type tc:qualified-ty-list :read-only t))

(defmethod tc:kind-variables-generic% ((partial partial-class))
  (nconc
   (tc:kind-variables-generic% (partial-class-superclasses partial))
   (tc:kind-variables-generic% (partial-class-method-tys partial))))

(defmethod tc:apply-ksubstitution (ksubs (partial partial-class))
  (declare (type tc:ksubstitution-list ksubs)
           (values partial-class))

  (make-partial-class
   :superclasses (tc:apply-ksubstitution ksubs (partial-class-superclasses partial))
   :method-tys (tc:apply-ksubstitution ksubs (partial-class-method-tys partial))))

;;;
;;; Entrypoint
;;;

(defun toplevel-define-class (classes env)
  (declare (type parser:toplevel-define-class-list classes)
           (type tc:environment env)
           (values tc:ty-class-list tc:environment))

  ;; Check that all class names are in the current package
  (check-package classes (alexandria:compose #'parser:identifier-src-name
                                             #'parser:toplevel-define-class-name)
                 #'parser:toplevel-define-class-head-location)

  ;; Check that all methods are in the current package
  (check-package (mapcan (alexandria:compose #'copy-list #'parser:toplevel-define-class-methods)
                         classes)
                 (alexandria:compose #'parser:identifier-src-name
                                     #'parser:method-definition-name)
                 #'source:location)

  ;; Check for duplicate class definitions
  (check-duplicates
   classes
   (alexandria:compose #'parser:identifier-src-name #'parser:toplevel-define-class-name)
   (lambda (first second)
     (tc:tc-error "Duplicate class definition"
                  (tc:tc-location (parser:toplevel-define-class-head-location first)
                                  "first definition here")
                  (tc:tc-location (parser:toplevel-define-class-head-location second)
                                  "second definition here"))))

  ;; Check for duplicate method definitions
  (check-duplicates
   (mapcan (alexandria:compose #'copy-list #'parser:toplevel-define-class-methods) classes)
   (alexandria:compose #'parser:identifier-src-name #'parser:method-definition-name)
   (lambda (first second)
     (tc:tc-error "Duplicate method definition"
                  (tc:tc-note first "first definition here")
                  (tc:tc-note second "second definition here"))))

  (loop :for class :in classes :do
    ;; Classes cannot have duplicate variables
    (check-duplicates
     (parser:toplevel-define-class-vars class)
     #'parser:keyword-src-name
     (lambda (first second)
       (tc:tc-error "Duplicate class variable"
                    (tc:tc-note first "first usage here")
                    (tc:tc-note second "second usage here")))))

  (let* ((class-table
           (loop :with table := (make-hash-table :test #'eq)

                 :for class :in classes
                 :for name := (parser:identifier-src-name
                               (parser:toplevel-define-class-name class))

                 :do (setf (gethash name table) class)

                 :finally (return table)))

         (class-names (alexandria:hash-table-keys class-table))

         (class-dependencies
           (loop :for class-name :being :the :hash-keys :of class-table
                 :for class :being :the :hash-values :of class-table

                 :for deps := (intersection
                               (remove-duplicates
                                (mapcar #'parser:identifier-src-name
                                        (parser:collect-referenced-classes class))
                                :test #'eq)
                               class-names
                               :test #'eq)

                 :collect (cons class-name deps)))

         (sccs (algo:tarjan-scc class-dependencies))

         (classes-by-scc
           (loop :for scc :in sccs
                 :collect (loop :for name :in scc
                                :collect (gethash name class-table)))))

    (values
     (loop :for scc :in classes-by-scc
           :for scc-names :in sccs

           :for superclass-names
             := (loop :for class :in scc
                      :for referenced-classes
                        := (parser:collect-referenced-classes
                            (parser:toplevel-define-class-preds class))

                      :append (mapcar #'parser:identifier-src-name referenced-classes))

           ;; Classes cannot have cyclic superclasses
           :when (intersection superclass-names scc-names :test #'eq)
             :do (let ((scc (sort (copy-list scc) #'source:location< :key #'source:location)))
                   (apply #'tc:tc-error "Cyclic superclasses"
                          (cons (tc:tc-location (parser:toplevel-define-class-head-location (first scc))
                                                "in class defined here")
                                (loop :for class :in (rest scc)
                                      :collect (tc:tc-location (parser:toplevel-define-class-head-location class)
                                                               "in class defined here")))))

           :append (multiple-value-bind (classes env_)
                       (infer-class-scc-kinds scc env)
                     (setf env env_)
                     classes))
     env)))

(defun infer-class-scc-kinds (classes env)
  (declare (type parser:toplevel-define-class-list classes)
           (type tc:environment env)
           (values tc:ty-class-list tc:environment))

  (let* ((renamed-classes (parser:rename-type-variables classes))

         (partial-env (make-partial-type-env :env env))

         ;; Predefine each class in the environment
         (preds
           (loop :for class :in renamed-classes

                 :for class-name := (parser:identifier-src-name (parser:toplevel-define-class-name class))

                 :for vars := (mapcar #'parser:keyword-src-name
                                      (parser:toplevel-define-class-vars class))

                 :for tvars := (loop :for var :in vars
                                     :collect (partial-type-env-add-var partial-env var))

                 :for pred := (tc:make-ty-predicate
                               :class class-name
                               :types tvars)

                 :do (partial-type-env-add-class partial-env pred)
                 :collect pred))

         (ksubs nil)

         ;; Infer the kinds of each class
         (partial-classes
           (loop :for class :in renamed-classes
                 :collect (multiple-value-bind (partial-class ksubs_)
                              (infer-class-kinds class partial-env ksubs)
                            (setf ksubs ksubs_)
                            partial-class))))

    ;; Monomorphise kind variables
    (setf partial-classes (tc:apply-ksubstitution ksubs partial-classes))
    (setf preds (tc:apply-ksubstitution ksubs preds))
    (setf ksubs (tc:kind-monomorphize-subs (tc:kind-variables (list preds partial-classes)) ksubs))
    (setf partial-classes (tc:apply-ksubstitution ksubs partial-classes))
    (setf preds (tc:apply-ksubstitution ksubs preds))

    (values
     (loop :for class :in classes
           :for pred :in preds
           :for partial :in partial-classes

           :for class-name := (tc:ty-predicate-class pred)
           :for class-vars := (mapcar #'parser:keyword-src-name (parser:toplevel-define-class-vars class))

           :for location := (or *compile-file-pathname* *load-truename*)

           :for codegen-sym := (alexandria:format-symbol *package* "CLASS/~A" class-name)

           :for method-names := (mapcar (alexandria:compose #'parser:identifier-src-name
                                                            #'parser:method-definition-name)
                                        (parser:toplevel-define-class-methods class))

           :for unqualifed-methods
             := (loop :for method-ty :in (partial-class-method-tys partial)
                      :for method-name :in method-names

                      :collect (cons method-name method-ty))

           :for superclass-dict
             := (loop :for super :in (partial-class-superclasses partial)
                      :for i :from 0
                      :collect (cons super
                                     (alexandria:format-symbol *package* "SUPER-~D"
                                                               i)))

           :for superclass-map
             := (loop :for (pred . super-name) :in superclass-dict
                      :for prefixed-name := (alexandria:format-symbol *package* "~A-~A"
                                                                      codegen-sym
                                                                      super-name)
                      :collect (cons prefixed-name super-name))

           :for fundeps
             := (loop :for fundep :in (parser:toplevel-define-class-fundeps class)
                      :collect (tc:make-fundep
                                :from (mapcar #'parser:keyword-src-name (parser:fundep-left fundep))
                                :to (mapcar #'parser:keyword-src-name (parser:fundep-right fundep))))

           :for class-entry
             :=  (tc:make-ty-class
                  :name class-name
                  :source-name (parser:identifier-src-source-name (parser:toplevel-define-class-name class))
                  :predicate pred
                  :superclasses (partial-class-superclasses partial)
                  :class-variables class-vars
                  :fundeps fundeps
                  :unqualified-methods (loop :for method-ty :in (partial-class-method-tys partial)
                                             :for method :in (parser:toplevel-define-class-methods class)

                                             :for method-name := (parser:identifier-src-name
                                                                  (parser:method-definition-name method))

                                             :collect (tc:make-ty-class-method :name method-name
                                                                               :type (tc:quantify nil method-ty)
                                                                               :docstring (source:docstring method)))
                  :codegen-sym codegen-sym
                  :superclass-dict superclass-dict
                  :superclass-map superclass-map
                  :docstring (source:docstring class)
                  :location (source:location class))

           :for method-tys := (loop :for (name . qual-ty) :in unqualifed-methods
                                    :for type := (tc:qualified-ty-type qual-ty)
                                    :for preds := (tc:qualified-ty-predicates qual-ty)
                                    :for new-qual-ty := (tc:qualify (cons pred preds) type)
                                    :collect (tc:quantify (tc:type-variables new-qual-ty) new-qual-ty))

           :for class-arity := (+ (length (partial-class-superclasses partial))
                                  (length method-tys))

           :for prev-class := (tc:lookup-class env class-name :no-error t)

           ;; Fundeps cannot be redefined
           :when (and prev-class (not (equalp (tc:ty-class-fundeps prev-class)
                                              fundeps))) 
             :do (tc-error "Invalid fundep redefinition"
                           (tc-location (parser:toplevel-define-class-head-location class)
                                        "unable to redefine the fundeps of class ~S." class-name))

           :when fundeps
             :do (setf env (tc:initialize-fundep-environment env class-name))

           :do (setf env (tc:set-class env class-name class-entry))

               ;; If the class has a constructor function then define
               ;; it in the function environment
           :if (not (zerop class-arity))
             :do (setf env (tc:set-function env codegen-sym (tc:make-function-env-entry
                                                             :name codegen-sym
                                                             :arity class-arity
                                                             :inline-p nil)))
           :else
             :when (tc:lookup-function env codegen-sym :no-error t)
               :do (setf env (tc:unset-function env codegen-sym))

           :do (loop :for method-ty :in method-tys
                     :for method-name :in method-names

                     :for method-arity := (+ (tc:function-type-arity
                                              (tc:qualified-ty-type
                                               (tc:fresh-inst method-ty)))
                                             (length (tc:qualified-ty-predicates
                                                      (tc:fresh-inst method-ty))))

                     :do (setf env (tc:set-value-type env method-name method-ty))

                     :do (setf env (tc:set-name env method-name
                                                (tc:make-name-entry
                                                 :name method-name
                                                 :type :method
                                                 :docstring nil
                                                 :location (source:location class))))

                     :if (not (zerop method-arity))
                       :do (setf env (tc:set-function env method-name (tc:make-function-env-entry
                                                                       :name method-name
                                                                       :arity method-arity
                                                                       :inline-p nil)))
                     :else
                       :do (setf env (tc:unset-function env method-name))) 

           :collect class-entry)
     env)))


(defun infer-class-kinds (class env ksubs)
  (declare (type parser:toplevel-define-class class)
           (type partial-type-env env)
           (type tc:ksubstitution-list ksubs)
           (values partial-class tc:ksubstitution-list))

  (let* ((var-names (mapcar #'parser:keyword-src-name (parser:toplevel-define-class-vars class)))
         (vars (loop :for var :in (parser:toplevel-define-class-vars class)
                     :for name :in var-names
                     :collect (handler-case (partial-type-env-lookup-var env name var)
                                (error () (util:coalton-bug "missing type variable"))))))

    ;; Ensure fundeps don't have duplicate variables
    (labels ((check-duplicate-fundep-variables (vars)
               (check-duplicates
                vars
                #'parser:keyword-src-name
                (lambda (first second)
                  (tc:tc-error "Duplicate variable in function dependency"
                               (tc:tc-note first "first usage here")
                               (tc:tc-note second "second usage here"))))))
      (loop :for fundep :in (parser:toplevel-define-class-fundeps class)
            :do (check-duplicate-fundep-variables (parser:fundep-left fundep))
            :do (check-duplicate-fundep-variables (parser:fundep-right fundep))))

    ;; Ensure all fundep variables are valid
    (labels ((check-fundep-variables (vars)
               (loop :for var :in vars
                     :unless (find (parser:keyword-src-name var) var-names :test #'eq)
                       :do (tc-error "Unknown type variable"
                                     (tc-note var "unknown type variable ~S"
                                              (parser:keyword-src-name var))))))
      (loop :for fundep :in (parser:toplevel-define-class-fundeps class)
            :do (check-fundep-variables (parser:fundep-left fundep))
            :do (check-fundep-variables (parser:fundep-right fundep))))

    (let* ((fundeps
             (loop :for fundep :in (parser:toplevel-define-class-fundeps class)
                   :collect (tc:make-fundep
                             :from (mapcar #'parser:keyword-src-name (parser:fundep-left fundep))
                             :to (mapcar #'parser:keyword-src-name (parser:fundep-right fundep)))))

           ;; Parse each of the superclasses
           (preds
             (loop :for pred :in (parser:toplevel-define-class-preds class)
                   :collect (multiple-value-bind (pred ksubs_)
                                (infer-predicate-kinds pred ksubs env)
                              (setf ksubs ksubs_)
                              (tc:apply-ksubstitution ksubs pred))))

           ;; Parse each of the methods
           (method-tys
             (loop :for method :in (parser:toplevel-define-class-methods class)

                   :for method-ty := (parser:method-definition-type method)

                   ;; Type variables referenced in ty
                   :for method-tyvars
                     := (remove-duplicates
                         (parser:collect-type-variables method-ty)
                         :test #'eq
                         :key #'parser:tyvar-name)

                   :for method-tyvar-names := (mapcar #'parser:tyvar-name method-tyvars)

                   ;; Type variables referenced in ty as well as the class predicate
                   :for known-method-tyvars
                     := (loop :for tyvar :in method-tyvars
                              :for name :in method-tyvar-names
                              :when (member name var-names :test #'eq)
                                :collect (handler-case (partial-type-env-lookup-var env name tyvar)
                                           (error () (util:coalton-bug "missing type variable")))
                                  :into known-method-tyvars
                              :finally (return (mapcar (alexandria:curry #'tc:apply-ksubstitution ksubs)
                                                       known-method-tyvars)))

                   ;; Ensure that methods are not ambiguous
                   :unless (or
                            ;; First, we check if the class variables are fully determined
                            ;; by the functional dependencies which are immediately provided
                            ;; in this class definition, such as in the following example.
                            ;; (define-class (C :a :b (:a -> :b))
                            ;;   (m (Unit -> :a)) ; :B is determined by :A.
                            ;;   )
                            (subsetp var-names
                                     (tc:closure method-tyvar-names fundeps)
                                     :test #'eq)
                            ;; Otherwise, we collect functional dependencies by recursing into
                            ;; the superclass predicates and computing a generic closure on the
                            ;; types themselves using TC:TY=. This allows for examples like the
                            ;; following.
                            ;; (define-class (C :a :b (:a -> :b)))
                            ;; (define-class (D :a :b)
                            ;;   (m (Unit -> :a)) ; :B is determined by the functional dependency
                            ;;                    ; provided in the class definition of C.
                            ;;   )
                            (subsetp (mapcar (alexandria:curry #'tc:apply-ksubstitution ksubs) vars)
                                     (tc:generic-closure
                                      known-method-tyvars
                                      (tc:collect-fundep-vars (partial-type-env-env env) preds)
                                      :test #'tc:ty=)
                                     :test #'tc:ty=))
                     :do (tc-error "Ambiguous method"
                                   (tc-note method
                                            "the method is ambiguous"))

                   ;; Add new type variables to the environment
                   :do (loop :for new-method-tyvar-name
                               :in (set-difference method-tyvar-names var-names :test #'eq)
                             :do (partial-type-env-add-var env new-method-tyvar-name))

                   :collect (multiple-value-bind (method-ty_ ksubs_)
                                (infer-type-kinds method-ty tc:+kstar+ ksubs env)
                              (setf ksubs ksubs_)
                              (apply-type-alias-substitutions method-ty_ method-ty env)))))

      (values (make-partial-class :superclasses preds
                                  :method-tys method-tys)
              ksubs))))
