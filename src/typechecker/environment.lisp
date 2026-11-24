(defpackage #:coalton-impl/typechecker/environment
  (:use
   #:cl
   #:coalton-impl/algorithm
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/type-errors
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/predicate
   #:coalton-impl/typechecker/scheme
   #:coalton-impl/typechecker/unify)
  (:import-from
   #:coalton-impl/util
   #:project-elements)
  (:import-from
   #:coalton-impl/typechecker/substitutions
   #:apply-substitution
   #:substitution-list
   #:compose-substitution-lists)
  (:import-from
   #:coalton-impl/typechecker/fundeps
   #:fundep
   #:fundep-from
   #:fundep-to
   #:fundep-list
   #:generic-closure
   #:+fundep-max-depth+)
  (:local-nicknames
   (#:avl #:coalton-impl/algorithm/avl-tree)
   (#:util #:coalton-impl/util)
   (#:source #:coalton-impl/source)
   (#:parser #:coalton-impl/parser))
  (:export
   #:*update-hook*                          ; VARIABLE
   #:value-environment                      ; STRUCT
   #:explicit-repr                          ; TYPE
   #:variance-list                          ; TYPE
   #:type-entry                             ; STRUCT
   #:make-type-entry                        ; CONSTRUCTOR
   #:type-entry-name                        ; ACCESSOR
   #:type-entry-source-name                 ; ACCESSOR
   #:type-entry-runtime-type                ; ACCESSOR
   #:type-entry-type                        ; ACCESSOR
   #:type-entry-tyvars                      ; ACCESSOR
   #:type-entry-variances                   ; ACCESSOR
   #:type-entry-constructors                ; ACCESSOR
   #:type-entry-explicit-repr               ; ACCESSOR
   #:type-entry-enum-repr                   ; ACCESSOR
   #:type-entry-newtype                     ; ACCESSOR
   #:type-entry-exception-p                 ; ACCESSOR
   #:type-entry-resumption-p                ; ACCESSOR
   #:type-environment                       ; STRUCT
   #:constructor-entry                      ; STRUCT
   #:make-constructor-entry                 ; ACCESSOR
   #:constructor-entry-name                 ; ACCESSOR
   #:constructor-entry-source-name          ; ACCESSOR
   #:constructor-entry-arity                ; ACCESSOR
   #:constructor-entry-constructs           ; ACCESSOR
   #:constructor-entry-classname            ; ACCESSOR
   #:constructor-entry-compressed-repr      ; ACCESSOR
   #:constructor-entry-list                 ; TYPE
   #:constructor-environment                ; STRUCT
   #:type-alias-entry                       ; STRUCT
   #:make-type-alias-entry                  ; CONSTRUCTOR
   #:type-alias-entry-name                  ; ACCESSOR
   #:type-alias-entry-source-name           ; ACCESSOR
   #:type-alias-entry-tyvars                ; ACCESSOR
   #:type-alias-entry-type                  ; ACCESSOR
   #:type-alias-entry-list                  ; ACCESSOR
   #:type-alias-environment                 ; STRUCT
   #:struct-field                           ; STRUCT
   #:make-struct-field                      ; CONSTRUCTOR
   #:struct-field-name                      ; ACCESSOR
   #:struct-field-type                      ; ACCESSOR
   #:struct-field-index                     ; ACCESSOR
   #:struct-field-list                      ; TYPE
   #:struct-field-accessor-name              ; FUNCTION
   #:struct-entry                           ; STRUCT
   #:make-struct-entry                      ; CONSTRUCTOR
   #:struct-entry-name                      ; ACCESSOR
   #:struct-entry-source-name               ; ACCESSOR
   #:struct-entry-fields                    ; ACCESSOR
   #:struct-entry-list                      ; TYPE
   #:struct-environment                     ; STRUCT
   #:get-field                              ; FUNCTION
   #:ty-class-method                        ; STRUCT
   #:make-ty-class-method                   ; CONSTRUCTOR
   #:ty-class-method-name                   ; ACCESSOR
   #:ty-class-method-type                   ; ACCESSOR
   #:ty-class-method-outer-tvars            ; ACCESSOR
   #:ty-class-method-explicit-tvars         ; ACCESSOR
   #:ty-class                               ; STRUCT
   #:make-ty-class                          ; CONSTRUCTOR
   #:ty-class-name                          ; ACCESSOR
   #:ty-class-source-name                   ; ACCESSOR
   #:ty-class-predicate                     ; ACCESSOR
   #:ty-class-superclasses                  ; ACCESSOR
   #:ty-class-class-variables               ; ACCESSOR
   #:ty-class-fundeps                       ; ACCESSOR
   #:ty-class-unqualified-methods           ; ACCESSOR
   #:ty-class-codegen-sym                   ; ACCESSOR
   #:ty-class-superclass-dict               ; ACCESSOR
   #:ty-class-superclass-map                ; ACCESSOR
   #:ty-class-list                          ; TYPE
   #:class-environment                      ; STRUCT
   #:ty-class-instance                      ; STRUCT
   #:make-ty-class-instance                 ; CONSTRUCTOR
   #:ty-class-instance-constraints          ; ACCESSOR
   #:ty-class-instance-predicate            ; ACCESSOR
   #:ty-class-instance-codegen-sym          ; ACCESSOR
   #:ty-class-instance-overlap-p            ; ACCESSOR
   #:ty-class-instance-derived-p            ; ACCESSOR
   #:lookup-class-instance-by-head         ; FUNCTION
   #:class-has-overlap-p                    ; FUNCTION
   #:*instance-selection-hook*             ; VARIABLE
   #:*defer-instance-validation*           ; VARIABLE
   #:*changed-instance-classes*            ; VARIABLE
   #:register-instance-selections          ; FUNCTION
   #:validate-instance-selections          ; FUNCTION
   #:ty-class-instance-method-codegen-syms  ; ACCESSOR
   #:ty-class-instance-method-codegen-inline-p ; ACCESSOR
   #:ty-class-instance-location             ; ACCESSOR
   #:finalize-instance-signatures           ; FUNCTION
   #:instance-dictionary-signature          ; FUNCTION
   #:instantiate-instance-context           ; FUNCTION
   #:instantiate-superclasses               ; FUNCTION
   #:superclass-closure                     ; FUNCTION
   #:instance-superclass-obligations        ; FUNCTION
   #:predicate-height                       ; FUNCTION
   #:max-class-instance-head-height         ; FUNCTION
   #:predicate-embeds-p                     ; FUNCTION
   #:instance-recursion-blocked-p           ; FUNCTION
   #:ty-class-instance-list                 ; TYPE
   #:instance-environment                   ; STRUCT
   #:instance-environment-instances         ; ACCESSOR
   #:function-env-entry                     ; STRUCT
   #:make-function-env-entry                ; CONSTRUCTOR
   #:function-env-entry-p                   ; FUNCTION
   #:function-env-entry-name                ; ACCESSOR
   #:function-env-entry-arity               ; ACCESSOR
   #:function-env-entry-inline-p            ; ACCESSOR
   #:function-env-entry-primary-convention  ; ACCESSOR
   #:function-env-entry-entrypoints         ; ACCESSOR
   #:abi-leaf                               ; STRUCT
   #:make-abi-leaf                          ; CONSTRUCTOR
   #:abi-leaf-p                             ; FUNCTION
   #:abi-leaf-type                          ; ACCESSOR
   #:abi-product                            ; STRUCT
   #:make-abi-product                       ; CONSTRUCTOR
   #:abi-product-p                          ; FUNCTION
   #:abi-product-type                       ; ACCESSOR
   #:abi-product-constructor                ; ACCESSOR
   #:abi-product-type-name                  ; ACCESSOR
   #:abi-product-fields                     ; ACCESSOR
   #:abi-sum-variant                        ; STRUCT
   #:make-abi-sum-variant                   ; CONSTRUCTOR
   #:abi-sum-variant-p                      ; FUNCTION
   #:abi-sum-variant-constructor            ; ACCESSOR
   #:abi-sum-variant-tag-value              ; ACCESSOR
   #:abi-sum-variant-field-layouts          ; ACCESSOR
   #:abi-sum-variant-slot-indices           ; ACCESSOR
   #:abi-sum                                ; STRUCT
   #:make-abi-sum                           ; CONSTRUCTOR
   #:abi-sum-p                              ; FUNCTION
   #:abi-sum-type                           ; ACCESSOR
   #:abi-sum-type-name                      ; ACCESSOR
   #:abi-sum-tag-type                       ; ACCESSOR
   #:abi-sum-payload-types                  ; ACCESSOR
   #:abi-sum-variants                       ; ACCESSOR
   #:abi-multi-value                        ; STRUCT
   #:make-abi-multi-value                   ; CONSTRUCTOR
   #:abi-multi-value-p                      ; FUNCTION
   #:abi-multi-value-type                   ; ACCESSOR
   #:abi-multi-value-components             ; ACCESSOR
   #:entrypoint-spec                        ; STRUCT
   #:make-entrypoint-spec                   ; CONSTRUCTOR
   #:entrypoint-spec-p                      ; FUNCTION
   #:entrypoint-spec-convention             ; ACCESSOR
   #:entrypoint-spec-symbol                 ; ACCESSOR
   #:entrypoint-spec-arity                  ; ACCESSOR
   #:entrypoint-spec-input-layouts          ; ACCESSOR
   #:entrypoint-spec-output-layout          ; ACCESSOR
   #:entrypoint-spec-function-type          ; ACCESSOR
   #:function-environment                   ; STRUCT
   #:name-entry                             ; STRUCT
   #:make-name-entry                        ; CONSTRUCTOR
   #:name-entry-name                        ; ACCESSOR
   #:name-entry-type                        ; ACCESSOR
   #:name-environment                       ; STRUCT
   #:method-inline-environment              ; STRUCT
   #:code-environment                       ; STRUCT
   #:specialization-entry                   ; STRUCT
   #:make-specialization-entry              ; CONSTRUCTOR
   #:specialization-entry-from              ; ACCESSOR
   #:specialization-entry-to                ; ACCESSOR
   #:specialization-entry-to-ty             ; ACCESSOR
   #:specialization-entry-list              ; TYPE
   #:specialization-environment             ; STRUCT
   #:environment                            ; STRUCT
   #:make-default-environment               ; FUNCTION
   #:environment-value-environment          ; ACCESSOR
   #:environment-type-environment           ; ACCESSOR
   #:environment-type-alias-environment     ; ACCESSOR
   #:environment-constructor-environment    ; ACCESSOR
   #:environment-class-environment          ; ACCESSOR
   #:environment-fundep-environment         ; ACCESSOR
   #:environment-instance-environment       ; ACCESSOR
   #:environment-function-environment       ; ACCESSOR
   #:environment-name-environment           ; ACCESSOR
   #:environment-method-inline-environment  ; ACCESSOR
   #:environment-code-environment           ; ACCESSOR
   #:environment-specialization-environment ; ACCESSOR
   #:lookup-value-type                      ; FUNCTION
   #:set-value-type                         ; FUNCTION
   #:unset-value-type                       ; FUNCTION
   #:lookup-type                            ; FUNCTION
   #:set-type                               ; FUNCTION
   #:lookup-constructor                     ; FUNCTION
   #:set-constructor                        ; FUNCTION
   #:unset-constructor                      ; FUNCTION
   #:lookup-type-alias                      ; FUNCTION
   #:set-type-alias                         ; FUNCTION
   #:unset-type-alias                       ; FUNCTION
   #:lookup-struct                          ; FUNCTION
   #:set-struct                             ; FUNCTION
   #:unset-struct                           ; FUNCTION
   #:set-constructor                        ; FUNCTION
   #:unset-constructor                      ; FUNCTION
   #:lookup-class                           ; FUNCTION
   #:set-class                              ; FUNCTION
   #:lookup-function                        ; FUNCTION
   #:set-function                           ; FUNCTION
   #:unset-function                         ; FUNCTION
   #:lookup-name                            ; FUNCTION
   #:set-name                               ; FUNCTION
   #:unset-name                             ; FUNCTION
   #:lookup-class-instances                 ; FUNCTION
   #:lookup-class-instance                  ; FUNCTION
   #:synthesized-class-instance-constraints ; FUNCTION
   #:lookup-instance-by-codegen-sym         ; FUNCTION
   #:lookup-function-source-parameter-names ; FUNCTION
   #:set-function-source-parameter-names    ; FUNCTION
   #:unset-function-source-parameter-names  ; FUNCTION
   #:constructor-arguments                  ; FUNCTION
   #:add-instance                           ; FUNCTION
   #:set-method-inline                      ; FUNCTION
   #:lookup-method-inline                   ; FUNCTION
   #:set-code                               ; FUNCTION
   #:lookup-code                            ; FUNCTION
   #:add-specialization                     ; FUNCTION
   #:lookup-specialization                  ; FUNCTION
   #:lookup-specialization-by-type          ; FUNCTION
   #:lookup-fundep-environment              ; FUNCTION
   #:initialize-fundep-environment          ; FUNCTION
   #:collect-fundeps                        ; FUNCTION
   #:collect-fundep-vars                    ; FUNCTION
   #:given-superclass-expansions            ; FUNCTION
   #:instantiate-context-superclasses       ; FUNCTION
   #:context-superclasses                   ; FUNCTION
   #:*given-superclass-expansions*          ; VARIABLE
   #:update-instance-fundeps                ; FUNCTION
   #:check-instance-fundep-consistency      ; FUNCTION
   #:solve-fundeps                          ; FUNCTION
   #:synchronize-type-variable-counter      ; FUNCTION
   ))

;;;;
;;;; Coalton Type Checker Environment
;;;;
;;;; This module maintains the global type checking environment, which
;;;; contains all information about types, functions, classes,
;;;; instances, and other compiler state.
;;;;
;;;; Environment contents:
;;;;
;;;; - Value types: Type signatures for functions and variables
;;;; - Type definitions: User-defined types (algebraic data types, type aliases)
;;;; - Type classes: Class definitions with their methods and superclasses  
;;;; - Type instances: Implementations of type classes for specific types
;;;; - Constructor information: Data constructors and their types
;;;; - Function metadata: Arity, inlining directives, specializations
;;;;
;;;; The global environment root is:
;;;;
;;;;   coalton-impl/entry:*global-environment*
;;;;
;;;; Environments are generally treated immutably, in that the various
;;;; functions defined here return new environments.
;;;;
;;;; The *update-hook* variable allows recording environment modifications
;;;; during compilation. When compilation finishes, the update log is used to
;;;; generate code that will replay the same modifications at load time.
;;;;

(in-package #:coalton-impl/typechecker/environment)

;;; *update-hook* may be bound to a function that is called whenever
;;; an environment is updated.
;;;
;;; The bound function must accept 2 arguments:
;;;
;;; - the symbol naming the environment update function
;;; - that function's arg list
;;;
;;; The environment itself, which is always the first argument to an
;;; update function, is not provided in the arg list: just the update
;;; values.

(defvar *update-hook*)

(defmacro define-env-updater (name arg-list &body body)
  "Wrap a function that mutates an environment so that the environment update notification hook is called with the function's name and arg list whenever the function is called."
  `(defun ,name (&rest args)
     (declare (values environment &optional))
     (when (boundp '*update-hook*)
       (funcall *update-hook* ',name (cdr args)))
     (destructuring-bind ,arg-list args ,@body)))

;;;
;;; Value type environments
;;;

(defstruct (value-environment (:include immutable-map)))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type value-environment))

(defmethod apply-substitution (subst-list (env value-environment))
  (immutable-map-image (lambda (value) (apply-substitution subst-list value)) env #'make-value-environment))

(defmethod type-variables ((env value-environment))
  (let ((out nil))
    (do-immutable-map (name type env)
      (declare (ignore name))
      (setf out (append (type-variables type) out)))
    (remove-duplicates out :test #'ty=)))

;;;
;;; Type environments
;;;

(deftype explicit-repr ()
  '(or null
    (member :enum :lisp :transparent)
    (cons (eql :native) (cons t null))))

(defun variance-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (variance)
                (member variance '(:covariant :contravariant :invariant) :test #'eq))
              x)))

(deftype variance-list ()
  '(satisfies variance-list-p))

(defstruct type-entry
  (name          (util:required 'name)          :type symbol                    :read-only t)
  (source-name   nil                            :type (or null string)          :read-only t)
  (runtime-type  (util:required 'runtime-type)  :type t                         :read-only t)
  (type          (util:required 'type)          :type ty                        :read-only t)
  (tyvars        (util:required 'tyvars)        :type tyvar-list                :read-only t)
  ;; Variance of each type parameter in declaration order.
  ;; Entries are one of :COVARIANT, :CONTRAVARIANT, or :INVARIANT.
  ;; Used by relaxed value restriction when deciding whether weak variables
  ;; from expansive bindings can be generalized.
  (variances     (util:required 'variances)     :type variance-list             :read-only t)
  (constructors  (util:required 'constructors)  :type util:symbol-list          :read-only t)
  ;; An explicit repr defined in the source, or nil if none was
  ;; supplied. Computed repr will be reflected in ENUM-REPR, NEWTYPE,
  ;; and/or RUNTIME-TYPE.
  (explicit-repr (util:required 'explicit-repr) :type explicit-repr              :read-only t)

  ;; If this is true then the type is compiled to a more effecient
  ;; enum representation at runtime
  (enum-repr     (util:required 'enum-repr)     :type boolean                    :read-only t)

  ;; If this is true then the type does not exist at runtime See
  ;; https://wiki.haskell.org/Newtype
  ;;
  ;; A type cannot be both enum repr and a newtype
  ;;
  ;; A type that is a newtype has another Coalton type as its
  ;; runtime-type instead of a lisp type. This is to avoid issues with
  ;; recursive newtypes.
  (newtype    (util:required 'newtype)           :type boolean                   :read-only t)
  (docstring  (util:required 'docstring)         :type (or null string)          :read-only t)
  (location   nil                                :type (or null source:location) :read-only t)
  (exception-p nil                               :type boolean                   :read-only nil)
  (resumption-p nil                              :type boolean                   :read-only nil))

(defmethod source:location ((self type-entry))
  (type-entry-location self))

(defmethod source:docstring ((self type-entry))
  (type-entry-docstring self))

(defmethod make-load-form ((self type-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

(defmethod kind-of ((entry type-entry))
  (kind-of (type-entry-type entry)))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type type-entry))

(defstruct (type-environment (:include immutable-map)))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type type-environment))

(defun make-default-type-environment ()
  "Create a TYPE-ENVIRONMENT containing early types."
  (make-type-environment
   :data (avl:make-avl-map
          ;; Early Types
          ('coalton:Boolean
           (make-type-entry
            :name 'coalton:Boolean
            :source-name "Boolean"
            :runtime-type 'cl:boolean
            :type *boolean-type*
            :tyvars nil
            :variances nil
            :constructors '(coalton:True coalton:False)
            :explicit-repr '(:native cl:boolean)
            :enum-repr t
            :newtype nil
            :docstring "Either true or false, internally represented by `cl:t` and `cl:nil` respectively."))

          ('coalton:Unit
           (make-type-entry
            :name 'coalton:Unit
            :source-name "Unit"
            :runtime-type 'coalton-impl/constants:lisp-type-of-unit
            :type *unit-type*
            :tyvars nil
            :variances nil
            :constructors '(coalton:Unit)
            :explicit-repr :enum
            :enum-repr t
            :newtype nil
            :docstring "The \"unit\" type whose only member is the value `Unit`."))

          ('coalton:Char
           (make-type-entry
            :name 'coalton:Char
            :source-name "Char"
            :runtime-type 'cl:character
            :type *char-type*
            :tyvars nil
            :variances nil
            :constructors nil
            :explicit-repr '(:native cl:character)
            :enum-repr nil
            :newtype nil
            :docstring "A character represented by a Common Lisp `cl:character`."))

          ('coalton:Integer
           (make-type-entry
            :name 'coalton:Integer
            :source-name "Integer"
            :runtime-type 'cl:integer
            :type *integer-type*
            :tyvars nil
            :variances nil
            :constructors nil
            :explicit-repr '(:native cl:integer)
            :enum-repr nil
            :newtype nil
            :docstring "Integer of unbounded size. Represented by a Common Lisp `cl:integer`."))

          ('coalton:F32
           (make-type-entry
            :name 'coalton:F32
            :source-name "F32"
            :runtime-type 'cl:single-float
            :type *single-float-type*
            :tyvars nil
            :variances nil
            :constructors nil
            :explicit-repr '(:native cl:single-float)
            :enum-repr nil
            :newtype nil
            :docstring "Single-precision floating point number (32 bits in size). Represented by a Common Lisp `cl:single-float`."))

          ('coalton:F64
           (make-type-entry
            :name 'coalton:F64
            :source-name "F64"
            :runtime-type 'cl:double-float
            :type *double-float-type*
            :tyvars nil
            :variances nil
            :constructors nil
            :explicit-repr '(:native cl:double-float)
            :enum-repr nil
            :newtype nil
            :docstring "Double-precision floating point number (64 bits in size). Represented by a Common Lisp `cl:double-float`."))

          ('coalton:String
           (make-type-entry
            :name 'coalton:String
            :source-name "String"
            :runtime-type 'cl:string
            :type *string-type*
            :tyvars nil
            :variances nil
            :constructors nil
            :explicit-repr '(:native cl:string)
            :enum-repr nil
            :newtype nil
            :docstring "String of characters. Represented by Common Lisp `cl:string`."))

          ('coalton:Fraction
           (make-type-entry
            :name 'coalton:Fraction
            :source-name "Fraction"
            :runtime-type 'cl:rational
            :type *fraction-type*
            :tyvars nil
            :variances nil
            :constructors nil
            :explicit-repr '(:native cl:rational)
            :enum-repr nil
            :newtype nil
            :docstring "A ratio of integers always in reduced form. Represented by a Common Lisp `cl:rational`."))

          ('coalton:Arrow
           (make-type-entry
            :name 'coalton:Arrow
            :source-name "Arrow"
            :runtime-type nil
            :type *arrow-type*
            :tyvars nil
            :variances '(:contravariant :covariant)
            :constructors nil
            :explicit-repr nil
            :enum-repr nil
            :newtype nil
            :docstring "A named constructor for function types. `Arrow :a :b` is equivalent to `:a -> :b`."))

          ('coalton:List
           (make-type-entry
            :name 'coalton:List
            :source-name "List"
            :runtime-type 'cl:list
            :type *list-type*
            :tyvars (list (make-variable))
            :variances '(:covariant)
            :constructors '(coalton:Cons coalton:Nil)
            :explicit-repr '(:native cl:list)
            :enum-repr nil
            :newtype nil
            :docstring "Homogeneous list of objects. Represented as a typical Common Lisp chain of `cl:cons` (or `cl:nil`)."))

          ('coalton:Optional
           (make-type-entry
            :name 'coalton:Optional
            :source-name "Optional"
            :runtime-type 'cl:t
            :type *optional-type*
            :tyvars (list (make-variable))
            :variances '(:covariant)
            :constructors '(coalton:Some coalton:None)
            :explicit-repr '(:native cl:t)
            :enum-repr nil
            :newtype nil
            :docstring "A type that allows indicating the presence or absence of a value. The underlying representation does not allocate when a value is present (i.e., with `Some`).")))))

;;;
;;; Constructor environment
;;;

(defstruct constructor-entry
  (name            (util:required 'name)            :type symbol                         :read-only t)
  (source-name     nil                              :type (or null string)               :read-only t)
  (arity           (util:required 'arity)           :type alexandria:non-negative-fixnum :read-only t)
  (constructs      (util:required 'constructs)      :type symbol                         :read-only t)
  (classname       (util:required 'classname)       :type symbol                         :read-only t)
  (docstring       (util:required 'docstring)       :type (or string null)               :read-only t)

  ;; If this constructor constructs a compressed-repr type then
  ;; compressed-repr is the runtime value of this nullary constructor
  (compressed-repr (util:required 'compressed-repr) :type t                              :read-only t))

(defmethod source:docstring ((self constructor-entry))
  (constructor-entry-docstring self))

(defmethod make-load-form ((self constructor-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type constructor-entry))

(util:define-list-type constructor-entry-list constructor-entry-p)

(defstruct (constructor-environment (:include immutable-map)))

(defun make-default-constructor-environment ()
  "Create a TYPE-ENVIRONMENT containing early constructors"
  (make-constructor-environment
   :data (avl:make-avl-map
          ;; Early Constructors
          ('coalton:True
           (make-constructor-entry
            :name 'coalton:True
            :source-name "True"
            :arity 0
            :constructs 'coalton:Boolean
            :classname 'coalton::Boolean/True
            :docstring "Boolean `True`"
            :compressed-repr 't))

          ('coalton:False
           (make-constructor-entry
            :name 'coalton:False
            :source-name "False"
            :arity 0
            :constructs 'coalton:Boolean
            :classname 'coalton::Boolean/False
            :docstring "Boolean `False`"
            :compressed-repr 'nil))

          ('coalton:Unit
          (make-constructor-entry
            :name 'coalton:Unit
            :source-name "Unit"
            :arity 0
            :constructs 'coalton:Unit
            :classname 'coalton::Unit/Unit
            :docstring "`Unit` is the explicit one-value unit type, distinct from zero-value returns `()`."
            :compressed-repr coalton-impl/constants:+value-of-unit+))

          ('coalton:Cons
           (make-constructor-entry
            :name 'coalton:Cons
            :source-name "Cons"
            :arity 2
            :constructs 'coalton:List
            :classname nil
            :docstring "`Cons` represents a `List` containing a first element (`car`) and a nested `Cons` (`cdr`)."
            :compressed-repr 'nil))

          ('coalton:Nil
           (make-constructor-entry
            :name 'coalton:Nil
            :source-name "Nil"
            :arity 0
            :constructs 'coalton:List
            :classname nil
            :docstring "`Nil` represents an empty `List`."
            :compressed-repr 'nil))

          ('coalton:Some
           (make-constructor-entry
            :name 'coalton:Some
            :source-name "Some"
            :arity 1
            :constructs 'coalton:Optional
            :classname nil
            :docstring "`Some` expresses the presence of a meaningful value."
            :compressed-repr 'nil))

          ('coalton:None
           (make-constructor-entry
            :name 'coalton:None
            :source-name "None"
            :arity 0
            :constructs 'coalton:Optional
            :classname nil
            :docstring "`None` expresses the absence of a meaningful value."
            :compressed-repr 'nil)))))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type constructor-environment))

;;;
;;; Type alias environment
;;;

(defstruct type-alias-entry
  (name      (util:required 'name)      :type symbol           :read-only t)
  (source-name nil                      :type (or null string) :read-only t)
  (tyvars    (util:required 'tyvars)    :type tyvar-list       :read-only t)
  (type      (util:required 'type)      :type ty               :read-only t)
  (docstring (util:required 'docstring) :type (or null string) :read-only t))

(defmethod source:docstring ((self type-alias-entry))
  (type-alias-entry-docstring self))

(defmethod make-load-form ((self type-alias-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type type-alias-entry))

(util:define-list-type type-alias-entry-list type-alias-entry-p)

(defstruct (type-alias-environment (:include immutable-map)))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type type-alias-environment))

;;;
;;; Struct environment
;;;

(defstruct struct-field
  (name      (util:required 'name)      :type string            :read-only t)
  (type      (util:required 'type)      :type ty                :read-only t)
  (index     (util:required 'index)     :type fixnum            :read-only t)
  (docstring (util:required 'docstring) :type (or null string)  :read-only t))

(defmethod source:docstring ((self struct-field))
  (struct-field-docstring self))

(defmethod make-load-form ((self struct-field) &optional env)
  (make-load-form-saving-slots self :environment env))

(util:define-list-type struct-field-list struct-field-p)

(defun struct-field-accessor-name (name index)
  "Return the generated reader name for field INDEX of struct NAME."
  (declare (type symbol name)
           (type fixnum index)
           (values symbol &optional))
  (alexandria:format-symbol (symbol-package name) "~A/~A-_~D" name name index))

(defstruct struct-entry
  (name      (util:required 'name)      :type symbol            :read-only t)
  (source-name nil                      :type (or null string)  :read-only t)
  (fields    (util:required 'fields)    :type struct-field-list :read-only t)
  (docstring (util:required 'docstring) :type (or null string)  :read-only t))

(defmethod source:docstring ((self struct-entry))
  (struct-entry-docstring self))

(defmethod make-load-form ((self struct-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

(util:define-list-type struct-entry-list struct-entry-p)

(defun get-field (struct-entry name &key no-error)
  (or (some (lambda (field)
              (when (string-equal (struct-field-name field) name)
                field))
            (struct-entry-fields struct-entry))
      (unless no-error
        (util:coalton-bug "Unknown field ~S" name))))

(defstruct (struct-environment (:include immutable-map)))

;;;
;;; Class environment
;;;

(defstruct ty-class-method
  (name      (util:required 'name)      :type symbol           :read-only t)
  (type      (util:required 'type)      :type ty-scheme        :read-only t)
  ;; Class-head binders that should be in scope in instance method bodies
  ;; before any method-local explicit FORALL binders are introduced.
  (outer-tvars nil                       :type list             :read-only t)
  ;; Method-local explicit FORALL binders, kept in source order so instance
  ;; methods can preserve their scoped names when typechecking nested declares.
  (explicit-tvars nil                    :type list             :read-only t)
  (docstring (util:required 'docstring) :type (or null string) :read-only t))

(defmethod source:docstring ((self ty-class-method))
  (ty-class-method-docstring self))

(defmethod make-load-form ((self ty-class-method) &optional env)
  (make-load-form-saving-slots self :environment env))

(util:define-list-type ty-class-method-list ty-class-method-p)

(defstruct ty-class
  (name                (util:required 'name)                :type symbol              :read-only t)
  (source-name         nil                                  :type (or null string)    :read-only t)
  (predicate           (util:required 'predicate)           :type ty-predicate        :read-only t)
  (superclasses        (util:required 'superclasses)        :type ty-predicate-list   :read-only t)
  (class-variables     (util:required 'class-variables)     :type util:symbol-list    :read-only t)
  (fundeps             (util:required 'fundeps)             :type fundep-list         :read-only t)

  ;; Methods of the class containing the same tyvars in PREDICATE for
  ;; use in pretty printing
  (unqualified-methods (util:required 'unqualified-methods) :type ty-class-method-list :read-only t)
  (codegen-sym         (util:required 'codegen-sym)         :type symbol               :read-only t)
  (superclass-dict     (util:required 'superclass-dict)     :type list                 :read-only t)
  (superclass-map      (util:required 'superclass-map)      :type list                 :read-only t)
  (docstring           (util:required 'docstring)           :type (or null string)     :read-only t)
  (location            (util:required 'location)            :type source:location      :read-only t))

(defmethod source:location ((self ty-class))
  (ty-class-location self))

(defmethod source:docstring ((self ty-class))
  (ty-class-docstring self))

(defmethod make-load-form ((self ty-class) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type ty-class))

(util:define-list-type ty-class-list ty-class-p)

(defmethod apply-substitution (subst-list (class ty-class))
  (declare (type substitution-list subst-list)
           (values ty-class &optional))
  (make-ty-class
   :name (ty-class-name class)
   :source-name (ty-class-source-name class)
   :predicate (apply-substitution subst-list (ty-class-predicate class))
   :superclasses (apply-substitution subst-list (ty-class-superclasses class))
   :class-variables (ty-class-class-variables class)
   :fundeps (ty-class-fundeps class)
   :unqualified-methods (mapcar (lambda (method)
                                  (make-ty-class-method :name (ty-class-method-name method)
                                                        :type (apply-substitution subst-list (ty-class-method-type method))
                                                        :outer-tvars (apply-substitution subst-list (ty-class-method-outer-tvars method))
                                                        :explicit-tvars (apply-substitution subst-list (ty-class-method-explicit-tvars method))
                                                        :docstring (ty-class-method-docstring method)))
                                (ty-class-unqualified-methods class))
   :codegen-sym (ty-class-codegen-sym class)
   :superclass-dict (mapcar (lambda (entry)
                              (cons (apply-substitution subst-list (car entry))
                                    (cdr entry)))
                            (ty-class-superclass-dict class))
   :superclass-map (ty-class-superclass-map class)
   :docstring (source:docstring class)
   :location (source:location class)))

(defstruct (class-environment (:include immutable-map)))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type class-environment))

;;;
;;; Fundep Environment
;;;

(defstruct (fundep-environment (:include immutable-map)))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type fundep-environment))

;;;
;;; Instance environment
;;;

(defstruct ty-class-instance
  (overlap-p               nil                                      :type boolean              :read-only t)
  (derived-p               nil                                      :type boolean              :read-only t)
  (constraints             (util:required 'constraints)             :type ty-predicate-list      :read-only t)
  (predicate               (util:required 'predicate)               :type ty-predicate           :read-only t)
  (codegen-sym             (util:required 'codegen-sym)             :type symbol                 :read-only t)
  (method-codegen-syms     (util:required 'method-codegen-syms)     :type util:symbol-list       :read-only t)
  (method-codegen-inline-p (util:required 'method-codegen-inline-p) :type list                   :read-only t)
  (docstring               (util:required 'docstring)               :type (or null string)       :read-only t)
  (location                nil                                      :type (or null source:location) :read-only t)
  ;; (HEAD . ORDERED-DICTIONARY-PARAMETERS). New declarations start with the
  ;; source context, then finalize it together before the environment is published.
  ;; NIL means missing metadata, not a nullary constructor (which stores (HEAD)).
  (dictionary-signature    nil :type ty-predicate-list))

(defun instance-dictionary-signature (instance)
  "Return the compiled head and ordered parameters, even for a nullary dictionary."
  (declare (type ty-class-instance instance)
           (values cons &optional))
  (let ((signature (and (slot-boundp instance 'dictionary-signature)
                        (ty-class-instance-dictionary-signature instance))))
    (unless (consp signature)
      (error "No compiled dictionary signature for ~S. Recompile its provider and clients."
             (ty-class-instance-predicate instance)))
    signature))

(defun instantiate-instance-context (instance pred)
  "Return the compiled dictionary parameters at the wanted instance head PRED."
  (let ((signature (instance-dictionary-signature instance)))
    ;; Context-only variables are local to this use of the instance, too.
    (when (cdr signature)
      (destructuring-bind (head . context) (fresh-preds signature)
        (apply-substitution (predicate-match head pred) context)))))

(defun type-children (ty)
  "Return the immediate child types of TY."
  (declare (type ty ty)
           (values ty-list &optional))
  (typecase ty
    (tapp
     (list (tapp-from ty) (tapp-to ty)))
    (function-ty
     (append (function-ty-positional-input-types ty)
             (mapcar #'keyword-ty-entry-type (function-ty-keyword-input-types ty))
             (function-ty-output-types ty)))
    (result-ty
     (result-ty-output-types ty))
    (forall-ty
     (append (mapcan (lambda (pred) (copy-list (ty-predicate-types pred)))
                     (forall-ty-predicates ty))
             (list (forall-ty-body ty))))
    (otherwise nil)))

(defun binder-captures-p (ty container)
  (declare (type ty ty container)
           (values boolean &optional))
  (and (forall-ty-p container)
       (not (null (intersection (type-variables ty)
                                (forall-ty-binders container)
                                :test #'ty=)))))

(defun type-height (ty &optional cache)
  (declare (type ty ty)
           (type (or null hash-table) cache)
           (values fixnum &optional))
  (let ((children (type-children ty)))
    (unless children
      (return-from type-height 1))
    (when cache
      (multiple-value-bind (cached foundp)
          (gethash ty cache)
        (when foundp
          (return-from type-height cached))))
    (let* ((cache (or cache (make-hash-table :test #'eq)))
           (height (1+ (loop :for child :in children
                             :maximize (type-height child cache)))))
      (setf (gethash ty cache) height)
      height)))

(defun predicate-height (pred &optional cache)
  (declare (type ty-predicate pred)
           (type (or null hash-table) cache)
           (values fixnum &optional))
  (let ((cache (or cache (make-hash-table :test #'eq))))
    (loop :for ty :in (ty-predicate-types pred)
          :maximize (type-height ty cache))))

(defun max-class-instance-head-height (env class &optional height-cache)
  (declare (type environment env)
           (type symbol class)
           (type (or null hash-table) height-cache)
           (values fixnum &optional))
  (let ((height-cache (or height-cache (make-hash-table :test #'eq)))
        (pending (list class))
        (visited nil)
        (max-height 0))
    (loop :while pending
          :for current := (pop pending)
          :unless (member current visited :test #'eq)
            :do (push current visited)
                (let ((class-entry (lookup-class env current :no-error t)))
                  (when class-entry
                    (dolist (super (ty-class-superclasses class-entry))
                      (push (ty-predicate-class super) pending))))
                (dolist (inst (lookup-class-instances env current :no-error t))
                  (setf max-height
                        (max max-height
                             (predicate-height (ty-class-instance-predicate inst)
                                               height-cache)))
                  (dolist (constraint (ty-class-instance-constraints inst))
                    (push (ty-predicate-class constraint) pending))
                  (dolist (constraint (cdr (instance-dictionary-signature inst)))
                    (push (ty-predicate-class constraint) pending))))
    max-height))

(defun type-proper-subterm-p (needle haystack &optional height-cache)
  "Does NEEDLE occur as a proper subterm of HAYSTACK?"
  (declare (type ty needle haystack)
           (type (or null hash-table) height-cache)
           (values boolean &optional))
  (let ((height-cache (or height-cache (make-hash-table :test #'eq))))
    (and (< (type-height needle height-cache) (type-height haystack height-cache))
         (not (binder-captures-p needle haystack))
         (some (lambda (child)
                 (or (ty= needle child)
                     (type-proper-subterm-p needle child height-cache)))
               (type-children haystack)))))

(defun predicate-arguments-proper-subterms-p (derived-pred container-pred)
  "Is every type argument of DERIVED-PRED a proper subterm of an argument of CONTAINER-PRED?"
  (declare (type ty-predicate derived-pred container-pred)
           (values boolean &optional))
  (let ((height-cache (make-hash-table :test #'eq)))
    (and (consp (ty-predicate-types derived-pred))
         (every (lambda (derived-arg)
                  (some (lambda (container-arg)
                          (type-proper-subterm-p derived-arg container-arg height-cache))
                        (ty-predicate-types container-pred)))
                (ty-predicate-types derived-pred)))))

(defun type-embeds-p (earlier later fixed-vars &optional height-cache)
  "Does EARLIER homeomorphically embed into LATER with FIXED-VARS kept distinct?"
  (declare (type ty earlier later)
           (type tyvar-list fixed-vars)
           (type (or null hash-table) height-cache)
           (values boolean &optional))
  (let ((memo (make-hash-table :test #'equal))
        (height-cache (or height-cache (make-hash-table :test #'eq))))
    (labels ((embeds-p (earlier later)
               (let ((key (cons earlier later)))
                 (multiple-value-bind (cached foundp)
                     (gethash key memo)
                   (when foundp
                     (return-from embeds-p cached))
                   (let ((earlier-height (type-height earlier height-cache))
                         (later-height (type-height later height-cache)))
                     (when (> earlier-height later-height)
                       (return-from embeds-p (setf (gethash key memo) nil)))
                     (setf (gethash key memo)
                           (or
                            ;; Diving: EARLIER embeds into an immediate child of LATER.
                            (and (not (binder-captures-p earlier later))
                                 (some (lambda (child) (embeds-p earlier child))
                                       (type-children later)))
                            ;; Coupling / leaf match at the root.
                            (typecase earlier
                              (tyvar
                               (and (tyvar-p later)
                                    (if (member earlier fixed-vars :test #'ty=)
                                        (ty= earlier later)
                                        (not (member later fixed-vars :test #'ty=)))))
                              ((or tycon tgen)
                               (ty= earlier later))
                              (tapp
                               (and (tapp-p later)
                                    (embeds-p (tapp-from earlier) (tapp-from later))
                                    (embeds-p (tapp-to earlier) (tapp-to later))))
                              (function-ty
                               (and (function-ty-p later)
                                    (eq (function-ty-keyword-open-p earlier)
                                        (function-ty-keyword-open-p later))
                                    (= (length (function-ty-positional-input-types earlier))
                                       (length (function-ty-positional-input-types later)))
                                    (= (length (function-ty-output-types earlier))
                                       (length (function-ty-output-types later)))
                                    (equal (mapcar #'keyword-ty-entry-keyword
                                                   (function-ty-keyword-input-types earlier))
                                           (mapcar #'keyword-ty-entry-keyword
                                                   (function-ty-keyword-input-types later)))
                                    (every #'embeds-p
                                           (type-children earlier)
                                           (type-children later))))
                              (result-ty
                               (and (result-ty-p later)
                                    (= (length (result-ty-output-types earlier))
                                       (length (result-ty-output-types later)))
                                    (every #'embeds-p
                                           (result-ty-output-types earlier)
                                           (result-ty-output-types later))))
                              (forall-ty
                               (and (forall-ty-p later)
                                    (= (length (forall-ty-predicates earlier))
                                       (length (forall-ty-predicates later)))
                                    (call-with-aligned-forall-binders
                                     earlier later
                                     (lambda (qual1 qual2)
                                       (and (every (lambda (p1 p2)
                                                     (predicate-embeds-p p1 p2 fixed-vars height-cache))
                                                   (qualified-ty-predicates qual1)
                                                   (qualified-ty-predicates qual2))
                                            (embeds-p (qualified-ty-type qual1)
                                                      (qualified-ty-type qual2)))))))))))))))
      (embeds-p earlier later))))

(defun predicate-embeds-p (earlier later fixed-vars &optional height-cache)
  "Does EARLIER homeomorphically embed into LATER with FIXED-VARS kept distinct?"
  (declare (type ty-predicate earlier later)
           (type tyvar-list fixed-vars)
           (type (or null hash-table) height-cache)
           (values boolean &optional))
  (let ((height-cache (or height-cache (make-hash-table :test #'eq))))
    (and (eq (ty-predicate-class earlier)
             (ty-predicate-class later))
         (= (length (ty-predicate-types earlier))
            (length (ty-predicate-types later)))
         (every (lambda (a b) (type-embeds-p a b fixed-vars height-cache))
                (ty-predicate-types earlier)
                (ty-predicate-types later)))))

(defun predicate-key (pred fixed-vars sentinel)
  "Quantify variables in PRED not in FIXED-VARS to test equivalence up to renaming."
  (declare (type ty-predicate pred)
           (type tyvar-list fixed-vars)
           (type tyvar sentinel)
           (values ty-scheme &optional))
  (quantify (set-difference (type-variables pred) fixed-vars :test #'ty=)
            (qualify (list pred) sentinel)))

(defun superclass-closure (env pred)
  "Recursively get all superclasses of PRED. Requires acyclic superclasses."
  (declare (type environment env)
           (type ty-predicate pred)
           (values ty-predicate-list))
  (let ((class (lookup-class env (ty-predicate-class pred))))
    (cons pred
          (loop :for super :in (context-superclasses class pred)
                :append (superclass-closure env super)))))

(defun instance-superclass-obligations (env pred instance-context)
  "Instantiate and fundep-improve the direct superclass obligations of PRED."
  (declare (type environment env)
           (type ty-predicate pred)
           (type ty-predicate-list instance-context)
           (values ty-predicate-list &optional))
  (let* ((raw-supers (instantiate-superclasses
                      (lookup-class env (ty-predicate-class pred))
                      pred))
         (super-subs (when raw-supers
                       (handler-case
                           (nth-value 1 (solve-fundeps env (append raw-supers instance-context) nil))
                         (coalton-internal-type-error () nil)))))
    (apply-substitution super-subs raw-supers)))

(defun type-disagreement-height (left right height-cache)
  "Measure the remaining height mismatch after cancelling paired type structure.
This is a progress measure, not a test for type equality."
  (declare (type ty left right)
           (type hash-table height-cache)
           (values fixnum &optional))
  (let ((memo (make-hash-table :test #'equal)))
    (labels ((measure (left right)
               (let ((ls (type-children left))
                     (rs (type-children right)))
                 (cond
                   ((and ls (eq (class-of left) (class-of right)) (= (length ls) (length rs)))
                    (loop :for l :in ls :for r :in rs :maximize (distance l r)))
                   ((and (null ls) (null rs) (ty= left right)) 0)
                   (t (max (type-height left height-cache) (type-height right height-cache))))))
             (distance (left right)
               (if (eq left right)
                   0
                   (let ((key (cons left right)))
                     (or (gethash key memo)
                         (setf (gethash key memo) (measure left right)))))))
      (distance left right))))

(defun head-equality-distances (head pred height-cache)
  "One distance per repeated variable occurrence in HEAD; NIL if its operands
are not yet exposed in PRED. These observations guide search, never selection."
  (declare (type ty-predicate head pred)
           (type hash-table height-cache)
           (values list &optional))
  (let ((bindings nil)
        (distances nil))
    (labels ((visit (pattern ty)
               (cond
                 ((tyvar-p pattern)
                  (let ((first (assoc pattern bindings :test #'ty=)))
                    (if first
                        (push (and ty (cdr first)
                                   (type-disagreement-height (cdr first) ty height-cache))
                              distances)
                        (push (cons pattern ty) bindings))))
                 (t
                  (loop :with children := (and ty (type-children ty))
                        :for child :in (type-children pattern)
                        :do (visit child (pop children)))))))
      (mapc #'visit (ty-predicate-types head) (ty-predicate-types pred)))
    distances))

(defun instance-input-progress-p (instance pred ancestors height-cache)
  "Did the gap between any two carried inputs narrow since each visit in ANCESTORS?"
  (declare (type ty-class-instance instance)
           (type ty-predicate pred)
           (type ty-predicate-list ancestors)
           (type hash-table height-cache)
           (values boolean &optional))
  (destructuring-bind (head . parameters) (instance-dictionary-signature instance)
    (let ((carried (intersection (type-variables head) (type-variables parameters) :test #'ty=)))
      (when (cdr carried)
        (flet ((gaps (goal)
                 (let* ((inputs (apply-substitution (predicate-match head goal) carried))
                        (heights (mapcar (lambda (ty) (type-height ty height-cache)) inputs)))
                   ;; A prerequisite can move a carried input into a tested position.
                   ;; Keep both directions: trading places is different from moving
                   ;; farther apart on the same side.
                   (loop :for (h . rest) :on heights
                         :append (loop :for k :in rest
                                       :collect (max 0 (- h k))
                                       :collect (max 0 (- k h)))))))
          (let ((current (gaps pred)))
            (every (lambda (old) (some #'< current (gaps old))) ancestors)))))))

(defun instance-equality-progress-p (env instance pred stack height-cache)
  "Has a tested equality or one of its possible inputs improved on each prior visit?"
  (declare (type environment env)
           (type (or ty-class-instance symbol) instance)
           (type ty-predicate pred)
           (type list stack)
           (type hash-table height-cache)
           (values boolean &optional))
  (when (symbolp instance)
    (return-from instance-equality-progress-p nil))
  (let ((others (remove instance (lookup-class-instances env (ty-predicate-class pred)) :test #'eq)))
    (flet ((distances (goal)
             (loop :for other :in others
                   :append (head-equality-distances (ty-class-instance-predicate other)
                                                    goal height-cache)))
           (closer-p (new old)
             (and new (or (null old) (< new old)))))
      (let ((current (distances pred)))
        (when (some #'identity current)
          ;; Against each ancestor, at least one of a fixed number of nonnegative
          ;; distances must strictly decrease. A plateau alone is not progress.
          (let ((stalled (loop :for (old-inst . old) :in stack
                               :when (and (eq instance old-inst)
                                          (not (some #'closer-p current (distances old))))
                                 :collect old)))
            (or (null stalled)
                (instance-input-progress-p instance pred stalled height-cache))))))))

(defun instance-recursion-blocked-p (env instance pred stack fixed-vars max-given-height
                                     &optional height-cache)
  "Stop on a repeated goal, or on growth with no progress around the cycle."
  (declare (type environment env)
           (type (or ty-class-instance symbol) instance)
           (type ty-predicate pred)
           (type list stack)
           (type tyvar-list fixed-vars)
           (type fixnum max-given-height)
           (type (or null hash-table) height-cache)
           (values boolean &optional))
  (unless (assoc instance stack)
    (return-from instance-recursion-blocked-p nil))
  (let* ((height-cache (or height-cache (make-hash-table :test #'eq)))
         (sentinel (make-variable))
         (key (predicate-key pred fixed-vars sentinel))
         (height (predicate-height pred height-cache)))
    (or (loop :for (old-inst . old) :in stack
              :thereis (and (eq instance old-inst)
                            (ty-scheme= key (predicate-key old fixed-vars sentinel))))
        (and (> height (+ max-given-height
                          (if (symbolp instance)
                              0
                              (max-class-instance-head-height env (ty-predicate-class pred)
                                                              height-cache))))
             (loop :for (old-inst . old) :in stack
                   :thereis (and (eq instance old-inst)
                                 (> height (predicate-height old height-cache))
                                 (predicate-embeds-p old pred fixed-vars height-cache)))
             ;; A different class on this path may be approaching its base
             ;; case. Check every step back to the last use of INSTANCE.
             (loop :for ((inst . goal) . history) :on (acons instance pred stack)
                   :never (instance-equality-progress-p env inst goal history height-cache)
                   :until (eq (caar history) instance))))))

(defstruct expansion-frame
  (depth            0                         :type fixnum            :read-only t)
  (pred             (util:required 'pred)     :type ty-predicate      :read-only t)
  (key              (util:required 'key)      :type ty-scheme         :read-only t)
  (instance         (util:required 'instance) :type ty-class-instance :read-only t)
  (constraint-index nil                       :type (or null fixnum)  :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type expansion-frame))

(defun valid-derived-cycle-p (prior pred key stack)
  "Return T when revisiting PRIOR at PRED closes a structural cycle of derived datatype instances."
  (declare (type expansion-frame prior)
           (type ty-predicate pred)
           (type ty-scheme key)
           (type list stack)
           (values boolean &optional))
  (flet ((derived-frame-p (frame)
           (ty-class-instance-derived-p (expansion-frame-instance frame))))
    (when (ty-scheme= key (expansion-frame-key prior))
      (let* ((cycle-frames (ldiff stack (cdr (member prior stack :test #'eq))))
             (last-derived (find-if #'derived-frame-p cycle-frames :from-end t))
             (initial-derived-pred (and last-derived
                                        (if (eq last-derived prior)
                                            pred
                                            (expansion-frame-pred last-derived)))))
        (when initial-derived-pred
          (loop :with target-derived-pred := initial-derived-pred
                :for frame :in cycle-frames
                :do (cond
                      ((derived-frame-p frame)
                       (setf target-derived-pred (expansion-frame-pred frame)))
                      ((not (predicate-arguments-proper-subterms-p
                             target-derived-pred
                             (expansion-frame-pred frame)))
                       (return nil)))
                :finally (return t)))))))

(defun expand-context (context env &optional root-instance)
  "Choose dictionary parameters for a new instance in its defining environment."
  (declare (type ty-predicate-list context)
           (type environment env)
           (type (or null ty-class-instance) root-instance)
           (values ty-predicate-list &optional))
  (unless context
    (return-from expand-context nil))

  ;; Derived instances on recursive datatypes synthesize field constraints that
  ;; cycle back to the derived heads. User-written circular or growing
  ;; prerequisites are not evidence and must remain obligations.
  (let* ((root-pred (and root-instance (ty-class-instance-predicate root-instance)))
         (fixed-vars (type-variables (if root-pred
                                         (cons root-pred context)
                                         context)))
         (sentinel (make-variable)))
    (labels ((find-matching-frame (instance constraint-index pred key stack)
               (or (find-if (lambda (frame)
                              (and (eq instance (expansion-frame-instance frame))
                                   (ty-scheme= key (expansion-frame-key frame))))
                            stack)
                   (find-if (lambda (frame)
                              (and (eq instance (expansion-frame-instance frame))
                                   (or (null (expansion-frame-constraint-index frame))
                                       (null constraint-index)
                                       (= constraint-index (expansion-frame-constraint-index frame)))
                                   (predicate-embeds-p (expansion-frame-pred frame) pred fixed-vars)))
                            stack)))
             (retain-at (prior depth stack pred)
               (let ((retain-depth (max 1 (expansion-frame-depth prior))))
                 (if (= depth retain-depth)
                     (list pred)
                     (throw (find retain-depth stack :key #'expansion-frame-depth :test #'=)
                            nil))))
             (expand (pred stack depth constraint-index)
               (let* ((key (predicate-key pred fixed-vars sentinel))
                      (instance (lookup-class-instance env pred :no-error t)))
                 (unless instance
                   (let ((root-prior (and (> depth 1)
                                          root-instance
                                          (predicate-subsumes-p root-pred pred)
                                          (find-matching-frame root-instance constraint-index pred key stack))))
                     (return-from expand
                       (if root-prior
                           (retain-at root-prior depth stack pred)
                           (list pred)))))
                 (let ((prior (find-matching-frame instance constraint-index pred key stack)))
                   (when prior
                     (when (valid-derived-cycle-p prior pred key stack)
                       (return-from expand nil))
                     (return-from expand (retain-at prior depth stack pred)))
                   (let* ((frame (make-expansion-frame
                                  :depth depth
                                  :pred pred
                                  :key key
                                  :instance instance
                                  :constraint-index constraint-index))
                          (next-stack (cons frame stack))
                          (required-preds (instantiate-instance-context instance pred))
                          (super-preds (instance-superclass-obligations env pred required-preds))
                          (given-supers (when super-preds
                                          (mapcan (lambda (p) (superclass-closure env p))
                                                  required-preds))))
                     (catch frame
                       ;; A candidate instance cannot discharge PRED if building its
                       ;; own superclass dictionaries cycles back into STACK.
                       (dolist (super super-preds)
                         (unless (member super given-supers :test #'type-predicate=)
                           (expand super next-stack (1+ depth) nil)))
                       (return-from expand
                         (loop :for required :in required-preds
                               :for child-index :from 0
                               :for next-index := (if (ty-class-instance-derived-p instance)
                                                      child-index
                                                      (or constraint-index child-index))
                               :append (expand required next-stack (1+ depth) next-index))))
                     (list pred))))))
      (let ((initial-stack
              (when root-instance
                (list (make-expansion-frame
                       :depth 0
                       :pred root-pred
                       :key (predicate-key root-pred fixed-vars sentinel)
                       :instance root-instance
                       :constraint-index nil)))))
        (remove-duplicates
         (loop :for pred :in context
               :for top-index :from 0
               :append (expand pred initial-stack 1 top-index))
         :test #'type-predicate=)))))

(defun finalize-instance-signatures (instances env)
  ;; Expand the whole batch before installing any signature, so forward
  ;; references and recursive derived instances do not depend on source order.
  (let ((contexts (loop :for instance :in instances
                        :collect (expand-context (ty-class-instance-constraints instance)
                                                 env
                                                 instance))))
    (loop :for instance :in instances
          :for context :in contexts
          :do (setf (ty-class-instance-dictionary-signature instance)
                    (cons (ty-class-instance-predicate instance) context)))))

(defmethod source:docstring ((self ty-class-instance))
  (ty-class-instance-docstring self))

(defmethod source:location ((self ty-class-instance))
  (ty-class-instance-location self))

(defmethod make-load-form ((self ty-class-instance) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type ty-class-instance))

(util:define-list-type ty-class-instance-list ty-class-instance-p)

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type ty-class-instance-list))

(defmethod apply-substitution (subst-list (instance ty-class-instance))
  (declare (type substitution-list subst-list)
           (values ty-class-instance &optional))
  (let ((signature (apply-substitution subst-list (instance-dictionary-signature instance))))
    (make-ty-class-instance
     :overlap-p (ty-class-instance-overlap-p instance)
     :derived-p (ty-class-instance-derived-p instance)
     :constraints (apply-substitution subst-list (ty-class-instance-constraints instance))
     :predicate (car signature)
     :dictionary-signature signature
     :codegen-sym (ty-class-instance-codegen-sym instance)
     :method-codegen-syms (ty-class-instance-method-codegen-syms instance)
     :method-codegen-inline-p (ty-class-instance-method-codegen-inline-p instance)
     :docstring (ty-class-instance-docstring instance)
     :location (ty-class-instance-location instance))))

(defstruct instance-environment
  ;; Compiled choices, indexed by class. Kept in the persistent environment so
  ;; failed compilation or FASL loading cannot partially install assumptions.
  (selections   (make-immutable-listmap) :type immutable-listmap :read-only t)
  (instances    (make-immutable-listmap) :type immutable-listmap :read-only t)
  (codegen-syms (make-immutable-map)     :type immutable-map     :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type instance-environment))

;;;
;;; Function environment
;;;
;;; A function's ABI layout tree describes how each logical Coalton parameter or
;;; return type is lowered into physical worker parameters or return values:
;;;
;;;   - ABI-LEAF:        Passed or returned as a single value without structural
;;;                      unpacking (e.g., scalars, functions, lists, type variables,
;;;                      or composite parameters that escape as whole values).
;;;   - ABI-PRODUCT:     A single-constructor product type (`define-struct` or
;;;                      single-constructor `define-type` such as `Tuple`) flattened
;;;                      into its constituent field layouts.
;;;   - ABI-SUM:         A non-recursive multi-constructor sum return type (such as
;;;                      `Optional` or `Result`) returned as `(values tag slot_0 ...)`
;;;                      where variants share payload slots of identical type.
;;;   - ABI-MULTI-VALUE: An explicit multi-output return pack (`A * B -> C * D` or
;;;                      `Void`) lowered to the concatenation of its component layouts.

(defstruct abi-leaf
  "An ABI layout node representing a single unsplit value of Coalton type TYPE."
  (type (util:required 'type) :type ty :read-only t))

(defmethod make-load-form ((self abi-leaf) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type abi-leaf))

(defstruct abi-product
  "An ABI layout node representing a single-constructor product type flattened
into its constituent field layouts."
  ;; The logical Coalton type of the product (e.g., `(Tuple U64 F64)`).
  (type        (util:required 'type)        :type ty     :read-only t)
  ;; The single constructor symbol that builds this product (e.g., `coalton:Tuple`).
  (constructor (util:required 'constructor) :type symbol :read-only t)
  ;; The type constructor name (e.g., `coalton:Tuple`).
  (type-name   (util:required 'type-name)   :type symbol :read-only t)
  ;; Ordered list of child ABI layout nodes, one per constructor field.
  (fields      (util:required 'fields)      :type list   :read-only t))

(defmethod make-load-form ((self abi-product) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type abi-product))

(defstruct abi-sum-variant
  "Metadata for one constructor variant within an unpacked ABI-SUM return layout."
  ;; The constructor symbol for this variant (e.g., `coalton:Some` or `coalton:None`).
  (constructor   (util:required 'constructor)   :type symbol :read-only t)
  ;; The runtime discriminant value emitted in the leading tag return slot:
  ;; `'coalton:True` / `'coalton:False` for 2-variant sums, or a 0-based `UFix`
  ;; integer index for 3-to-8-variant sums.
  (tag-value     (util:required 'tag-value)     :type t      :read-only t)
  ;; ABI layout nodes for the constructor's fields (each `abi-leaf` or `abi-product`).
  (field-layouts (util:required 'field-layouts) :type list   :read-only t)
  ;; 0-based indices into the parent `abi-sum-payload-types` vector indicating
  ;; which shared return payload slots hold this variant's flattened leaf values.
  (slot-indices  (util:required 'slot-indices)  :type list   :read-only t))

(defmethod make-load-form ((self abi-sum-variant) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type abi-sum-variant))

(defstruct abi-sum
  "An ABI layout node for a multi-constructor sum return type lowered to
`(cl:values tag payload_0 ... payload_{m-1})`."
  ;; The logical Coalton sum type (e.g., `(Optional U64)` or `(Result Err (Tuple A B))`).
  (type          (util:required 'type)          :type ty     :read-only t)
  ;; The type constructor name (e.g., `coalton:Optional`).
  (type-name     (util:required 'type-name)     :type symbol :read-only t)
  ;; Coalton type of the discriminant return value (`Boolean` for 2 variants, `UFix` for 3..8).
  (tag-type      (util:required 'tag-type)      :type ty     :read-only t)
  ;; Flat list of concrete leaf Coalton types for the shared payload return slots.
  ;; Variants with identical leaf types reuse the same slot index.
  (payload-types (util:required 'payload-types) :type list   :read-only t)
  ;; Ordered list of `abi-sum-variant` descriptors, one per constructor of `type-name`.
  (variants      (util:required 'variants)      :type list   :read-only t))

(defmethod make-load-form ((self abi-sum) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type abi-sum))

(defstruct abi-multi-value
  "An ABI layout node for an explicit multi-output function return (`result-ty`)."
  ;; The logical `result-ty` of the function.
  (type       (util:required 'type)       :type ty   :read-only t)
  ;; Ordered list of child ABI layout nodes, one per output in `result-ty-output-types`
  ;; (empty for `Void` / zero-value returns).
  (components (util:required 'components) :type list :read-only t))

(defmethod make-load-form ((self abi-multi-value) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type abi-multi-value))

(defstruct entrypoint-spec
  "Specification of one compiled worker entrypoint (e.g., `F%UNPACKED` or `F%UNBOXED`)
associated with a top-level Coalton function."
  ;; Calling convention keyword for this worker (e.g., `:unpacked` or `:unboxed`).
  (convention    (util:required 'convention)    :type keyword :read-only t)
  ;; The Common Lisp function symbol bound to this worker entrypoint.
  (symbol        (util:required 'symbol)        :type symbol  :read-only t)
  ;; Total number of flattened positional parameters accepted by `symbol`.
  (arity         (util:required 'arity)         :type fixnum  :read-only t)
  ;; List of ABI layout nodes corresponding 1-to-1 with the canonical function's
  ;; positional parameters (including leading typeclass dictionary parameters).
  (input-layouts (util:required 'input-layouts) :type list    :read-only t)
  ;; ABI layout node describing how the canonical return value is unmarshaled.
  (output-layout (util:required 'output-layout) :type t       :read-only t)
  ;; Physical `function-ty` of the worker after flattening inputs and outputs.
  (function-type (util:required 'function-type) :type ty      :read-only t))

(defmethod make-load-form ((self entrypoint-spec) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type entrypoint-spec))

(defstruct function-env-entry
  "Environment record for a compiled top-level Coalton function or instance method."
  ;; Canonical function symbol (always bound to the `:lisp` boxed entrypoint).
  (name               (util:required 'name)     :type symbol  :read-only t)
  ;; Positional arity of the canonical `:lisp` entrypoint (including dictionaries).
  (arity              (util:required 'arity)    :type fixnum  :read-only t)
  ;; True when the function was declared `(inline)`.
  (inline-p           (util:required 'inline-p) :type boolean :read-only t)
  ;; Highest-priority calling convention available for this function
  ;; (`:lisp`, `:unpacked`, or `:unboxed`).
  (primary-convention ':lisp                    :type keyword :read-only t)
  ;; List of specialized `entrypoint-spec`s generated for this function, ordered
  ;; from lowest to highest priority (excluding the canonical `:lisp` entrypoint).
  (entrypoints        nil                       :type list    :read-only t))

(defmethod make-load-form ((self function-env-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type function-env-entry))

(util:define-list-type function-env-entry-list function-env-entry-p)

(defstruct (function-environment (:include immutable-map)))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type function-environment))

;;;
;;; Name environment
;;;

(defstruct name-entry
  (name      (util:required 'name)      :type symbol                               :read-only t)
  (type      (util:required 'type)      :type (member :value :method :constructor) :read-only t)
  (docstring (util:required 'docstring) :type (or null string)                     :read-only t)
  (location  (util:required 'location)  :type source:location                      :read-only t))

(defmethod source:location ((self name-entry))
  (name-entry-location self))

(defmethod source:docstring ((self name-entry))
  (name-entry-docstring self))

(defmethod make-load-form ((self name-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type name-entry))

(defstruct (name-environment (:include immutable-map)))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type name-environment))

;;;
;;; Method Inline environment
;;;

(defstruct (method-inline-environment (:include immutable-map)))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type method-inline-environment))

;;;
;;; Code environment
;;;

(defstruct (code-environment (:include immutable-map)))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type code-environment))

;;;
;;; Specialization Environment
;;;

(defstruct specialization-entry
  (from (util:required 'from)   :type symbol :read-only t)
  (to (util:required 'to)       :type symbol :read-only t)
  (to-ty (util:required 'to-ty) :type ty     :read-only t))

(defmethod make-load-form ((self specialization-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

(util:define-list-type specialization-entry-list specialization-entry-p)

(defstruct (specialization-environment (:include immutable-listmap)))

;;;
;;; Source name environment
;;;
;; maps the names of user-defined functions to a list of their user-supplied parameter names, for
;; documentation generation.

(defstruct (source-name-environment (:include immutable-map)))

;;;
;;; Environment
;;;

(defmacro define-environment (&rest sub-environments)
  "Define the environment structure, default constructor, and copy-with updater.
Each sub-environment names its slot and type, optionally followed by a default
constructor instead of MAKE-<name>."
  (let ((slots
          (loop :for specification :in sub-environments
                :for (name constructor) := (alexandria:ensure-list specification)
                :collect (list name
                               (alexandria:make-keyword name)
                               (alexandria:format-symbol (symbol-package name) "ENVIRONMENT-~A" name)
                               (or constructor
                                   (alexandria:format-symbol (symbol-package name) "MAKE-~A" name))))))
    `(progn
       (defstruct environment
         ,@(loop :for (name) :in slots
                 :collect `(,name (util:required ',name) :type ,name :read-only t)))
       (defun make-default-environment ()
         (declare (values environment))
         (make-environment
          ,@(loop :for (name keyword accessor constructor) :in slots
                  :append `(,keyword (,constructor)))))
       (defun update-environment
           (env &key
                  ,@(loop :for (name keyword accessor) :in slots
                          :collect `(,name (,accessor env))))
         (declare (type environment env)
                  ,@(loop :for (name) :in slots
                          :collect `(type ,name ,name))
                  (values environment))
         (make-environment
          ,@(loop :for (name keyword) :in slots
                  :append `(,keyword ,name)))))))

(define-environment
  value-environment
  (type-environment make-default-type-environment)
  (constructor-environment make-default-constructor-environment)
  type-alias-environment
  struct-environment
  class-environment
  fundep-environment
  instance-environment
  function-environment
  name-environment
  method-inline-environment
  code-environment
  specialization-environment
  source-name-environment)

(defmethod print-object ((env environment) stream)
  (declare (type stream stream)
           (type environment env))
  (print-unreadable-object (env stream :type t :identity t)))


(defmethod make-load-form ((self environment) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type environment))

;;;
;;; Methods
;;;

(defmethod apply-substitution (subst-list (env environment))
  (declare (type substitution-list subst-list)
           (type environment env)
           (values environment &optional))
  (update-environment env
                      :value-environment
                      (apply-substitution
                       subst-list
                       (environment-value-environment env))))

(defmethod type-variables ((env environment))
  (type-variables (environment-value-environment env)))

(defun synchronize-type-variable-counter (env)
  "Advance `*next-variable-id*` past any tyvar IDs stored in ENV.

Compiled environment updates can reload typechecker structures whose tyvar IDs
were chosen in a previous image. When the fresh-variable counter is reset in a
new image, those persisted IDs can collide with newly inferred tyvars and cause
unrelated substitutions to alias each other. Shared type structures are scanned
only once."
  (declare (type environment env)
           (values null &optional))
  (let ((max-id -1)
        (visited (make-hash-table :test #'eq)))
    (labels ((scan (object)
               (when (gethash object visited)
                 (return-from scan nil))
               (setf (gethash object visited) t)
               (typecase object
                 (null nil)
                 (tyvar
                  (setf max-id (max max-id (tyvar-id object)))
                  (scan (ty-alias object)))
                 (tycon
                  (scan (ty-alias object)))
                 (tgen
                  (scan (ty-alias object)))
                 (tapp
                  (scan (ty-alias object))
                  (scan (tapp-from object))
                  (scan (tapp-to object)))
                 (keyword-ty-entry
                  (scan (keyword-ty-entry-type object)))
                 (function-ty
                  (scan (ty-alias object))
                  (scan (function-ty-positional-input-types object))
                  (scan (function-ty-keyword-input-types object))
                  (scan (function-ty-origin object))
                  (scan (function-ty-output-types object)))
                 (result-ty
                  (scan (ty-alias object))
                  (scan (result-ty-output-types object)))
                 (forall-ty
                  (scan (ty-alias object))
                  (scan (forall-ty-binders object))
                  (scan (forall-ty-type object)))
                 (qualified-ty
                  (scan (qualified-ty-predicates object))
                  (scan (qualified-ty-type object)))
                 (ty-predicate
                  (scan (ty-predicate-types object))
                  (scan (ty-predicate-superclass-evidence object)))
                 (ty-scheme
                  (scan (ty-scheme-type object)))
                 (type-entry
                  (scan (type-entry-runtime-type object))
                  (scan (type-entry-type object))
                  (scan (type-entry-tyvars object)))
                 (type-alias-entry
                  (scan (type-alias-entry-type object))
                  (scan (type-alias-entry-tyvars object)))
                 (struct-field
                  (scan (struct-field-type object)))
                 (struct-entry
                  (scan (struct-entry-fields object)))
                 (ty-class-method
                  (scan (ty-class-method-type object))
                  (scan (ty-class-method-outer-tvars object))
                  (scan (ty-class-method-explicit-tvars object)))
                 (ty-class
                  (scan (ty-class-predicate object))
                  (scan (ty-class-superclasses object))
                  (scan (ty-class-unqualified-methods object))
                  (scan (ty-class-superclass-dict object)))
                 (ty-class-instance
                  (scan (ty-class-instance-constraints object))
                  (scan (instance-dictionary-signature object)))
                 (instance-environment
                  (scan (instance-environment-instances object))
                  (scan (instance-environment-selections object)))
                 (specialization-entry
                  (scan (specialization-entry-to-ty object)))
                 (abi-leaf
                  (scan (abi-leaf-type object)))
                 (abi-product
                  (scan (abi-product-type object))
                  (scan (abi-product-fields object)))
                 (abi-sum-variant
                  (scan (abi-sum-variant-field-layouts object)))
                 (abi-sum
                  (scan (abi-sum-type object))
                  (scan (abi-sum-tag-type object))
                  (scan (abi-sum-payload-types object))
                  (scan (abi-sum-variants object)))
                 (abi-multi-value
                  (scan (abi-multi-value-type object))
                  (scan (abi-multi-value-components object)))
                 (entrypoint-spec
                  (scan (entrypoint-spec-input-layouts object))
                  (scan (entrypoint-spec-output-layout object))
                  (scan (entrypoint-spec-function-type object)))
                 (function-env-entry
                  (scan (function-env-entry-entrypoints object)))
                 (immutable-map
                  (do-immutable-map (_ value object)
                    (declare (ignore _))
                    (scan value)))
                 (immutable-listmap
                  (avl:do-avl (_ value (immutable-listmap-data object))
                    (declare (ignore _))
                    (scan value)))
                 (environment
                  (scan (environment-value-environment object))
                  (scan (environment-type-environment object))
                  (scan (environment-constructor-environment object))
                  (scan (environment-type-alias-environment object))
                  (scan (environment-struct-environment object))
                  (scan (environment-class-environment object))
                  (scan (environment-instance-environment object))
                  (scan (environment-function-environment object))
                  (scan (environment-specialization-environment object)))
                 (cons
                  (scan (car object))
                  (scan (cdr object)))
                 (list
                  (dolist (entry object)
                    (scan entry)))
                 (t nil))))
      (scan env)
      (ensure-next-variable-id-at-least max-id))
    nil))

;;;
;;; Functions
;;;

(declaim (ftype (function (environment symbol &key (:no-error t)) (or ty-scheme null)) lookup-value-type))
(defun lookup-value-type (env symbol &key no-error)
  "Look up the type scheme for a value binding (function or variable) in the environment.

ENV is the type checking environment to search.
SYMBOL is the name of the binding to look up.
NO-ERROR, if true, returns NIL instead of signaling an error when the binding is not found.

Returns the type scheme (ty-scheme) associated with the binding, or NIL if not found and NO-ERROR is true.
Type schemes include quantification information (∀ variables) and constraints.

This is the primary interface for retrieving function and variable types during type checking.
The returned scheme can be instantiated with fresh type variables to get a concrete type for
use in type inference.

Examples:
  (lookup-value-type env 'my-function)     ; Returns scheme like ∀ a. a -> a -> Boolean  
  (lookup-value-type env 'undefined :no-error t) ; Returns NIL if 'undefined not defined

Signals a coalton-bug error if the binding is not found and NO-ERROR is false (the default)."
  (declare (type environment env)
           (type symbol symbol))
  (or (immutable-map-lookup (environment-value-environment env) symbol)
      (unless no-error
        (util:coalton-bug "Unknown binding ~S" symbol))))

(define-env-updater set-value-type (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type ty-scheme value))

  ;; Schemes stored in the environment are not allowed to have any free variables.
  (when (type-variables value)
    (util:coalton-bug "Unable to add type with free variables to environment ~S" value))

  (update-environment
   env
   :value-environment (immutable-map-set
                       (environment-value-environment env)
                       symbol
                       value
                       #'make-value-environment)))

(define-env-updater unset-value-type (env symbol)
  (declare (type environment env)
           (type symbol symbol))

  (update-environment
   env
   :value-environment (immutable-map-remove
                       (environment-value-environment env)
                       symbol
                       #'make-value-environment)))

(declaim (ftype (function (environment symbol &key (:no-error t)) (or type-entry null)) lookup-type))
(defun lookup-type (env symbol &key no-error)
  "Look up a type definition (algebraic data type, struct, etc.) in the environment.

ENV is the type checking environment to search.
SYMBOL is the name of the type to look up.
NO-ERROR, if true, returns NIL instead of signaling an error when the type is not found.

Returns a type-entry containing:
- The type constructor and its kind
- Information about data constructors and their arities
- Runtime representation details (structs, enums, newtypes)
- Whether the type is an exception or resumption type

This is used during type checking to resolve type constructor references and
validate that types are properly defined before use.

Examples:
  (lookup-type env 'Maybe)     ; Returns type-entry for Maybe type constructor
  (lookup-type env 'NoSuchType :no-error t) ; Returns NIL if not defined

Signals a coalton-bug error if the type is not found and NO-ERROR is false (the default)."
  (declare (type environment env)
           (type symbol symbol))
  (or (immutable-map-lookup (environment-type-environment env) symbol)
      (unless no-error
        (util:coalton-bug "Unknown type ~S" symbol))))

(define-env-updater set-type (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type type-entry value))
  (update-environment
   env
   :type-environment (immutable-map-set
                      (environment-type-environment env)
                      symbol
                      value
                      #'make-type-environment)))

(defun lookup-constructor (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (immutable-map-lookup (environment-constructor-environment env) symbol)
      (unless no-error
        (util:coalton-bug "Unknown constructor ~S." symbol))))

(define-env-updater set-constructor (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type constructor-entry value))
  (update-environment
   env
   :constructor-environment (immutable-map-set
                             (environment-constructor-environment env)
                             symbol
                             value
                             #'make-constructor-environment)))

(define-env-updater unset-constructor (env symbol)
  (declare (type environment env)
           (type symbol symbol))
  (update-environment
   env
   :constructor-environment (immutable-map-remove
                             (environment-constructor-environment env)
                             symbol
                             #'make-constructor-environment)))

(defun lookup-type-alias (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (immutable-map-lookup (environment-type-alias-environment env) symbol)
      (unless no-error
        (util:coalton-bug "Unknown type-alias ~S" symbol))))

(define-env-updater set-type-alias (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type type-alias-entry value))
  (update-environment
   env
   :type-alias-environment (immutable-map-set
                            (environment-type-alias-environment env)
                            symbol
                            value
                            #'make-type-alias-environment)))

(define-env-updater unset-type-alias (env symbol)
  (declare (type environment env)
           (type symbol symbol))
  (update-environment
   env
   :type-alias-environment (immutable-map-remove
                            (environment-type-alias-environment env)
                            symbol
                            #'make-type-alias-environment)))

(defun lookup-struct (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (immutable-map-lookup (environment-struct-environment env) symbol)
      (unless no-error
        (util:coalton-bug "Unknown struct ~S" symbol))))

(define-env-updater set-struct (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type struct-entry value))
  (update-environment
   env
   :struct-environment (immutable-map-set
                        (environment-struct-environment env)
                        symbol
                        value
                        #'make-struct-environment)))

(define-env-updater unset-struct (env symbol)
  (declare (type environment env)
           (type symbol symbol))
  (update-environment
   env
   :struct-environment (immutable-map-remove
                        (environment-struct-environment env)
                        symbol
                        #'make-struct-environment)))

(defun lookup-class (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (immutable-map-lookup (environment-class-environment env) symbol)
      (unless no-error
        (util:coalton-bug "Unknown class ~S." symbol))))

(define-env-updater set-class (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type ty-class value))
  (update-environment
   env
   :class-environment (immutable-map-set
                       (environment-class-environment env)
                       symbol
                       value
                       #'make-class-environment)))

(defun lookup-function (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (immutable-map-lookup (environment-function-environment env) symbol)
      (unless no-error
        (util:coalton-bug "Unknown function ~S." symbol))))

(define-env-updater set-function (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type function-env-entry value))
  (update-environment
   env
   :function-environment (immutable-map-set
                          (environment-function-environment env)
                          symbol
                          value
                          #'make-function-environment)))

(define-env-updater unset-function (env symbol)
  (declare (type environment env)
           (type symbol symbol))
  (update-environment
   env
   :function-environment (immutable-map-remove
                          (environment-function-environment env)
                          symbol
                          #'make-function-environment)))

(defun lookup-name (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (immutable-map-lookup (environment-name-environment env) symbol)
      (unless no-error
        (error "Unknown name ~S." symbol))))

(define-env-updater set-name (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type name-entry value))
  (let ((old-value (lookup-name env symbol :no-error t)))
    (when (and old-value (not (equalp (name-entry-type old-value) (name-entry-type value))))
      (error "Unable to change the type of name ~S from ~A to ~A."
             symbol
             (name-entry-type old-value)
             (name-entry-type value)))
    (if old-value
        env
        (update-environment
         env
         :name-environment (immutable-map-set
                            (environment-name-environment env)
                            symbol
                            value
                            #'make-name-environment)))))

(define-env-updater unset-name (env symbol)
  (declare (type environment env)
           (type symbol symbol))
  (update-environment
   env
   :name-environment (immutable-map-remove
                      (environment-name-environment env)
                      symbol
                      #'make-name-environment)))

(defun lookup-class-instances (env class &key no-error)
  (declare (type environment env)
           (type symbol class)
           (values list &optional))
  (immutable-listmap-lookup (instance-environment-instances (environment-instance-environment env)) class :no-error no-error))

(defun synthesized-class-instance-constraints (pred)
  "Return compiler-synthesized constraints for PRED when applicable.

This currently covers structural `RuntimeRepr` instances for function types
that cannot be expressed as ordinary source instances.

Returns two values:
1. A list of prerequisite predicates.
2. A boolean indicating whether a synthesized instance exists."
  (declare (type ty-predicate pred)
           (values ty-predicate-list boolean &optional))
  (let ((types-package (cl:find-package "COALTON/TYPES")))
    (unless (and types-package
                 (= 1 (length (ty-predicate-types pred))))
      (return-from synthesized-class-instance-constraints (values nil nil)))
    (let* ((runtime-repr (cl:find-symbol "RUNTIMEREPR" types-package))
           (head-type (first (ty-predicate-types pred)))
           (location (source:location pred)))
      (declare (ignore location))
      (unless (typep head-type 'function-ty)
        (return-from synthesized-class-instance-constraints (values nil nil)))
      (if (eq (ty-predicate-class pred) runtime-repr)
          (values nil t)
          (values nil nil)))))

(defvar *instance-selection-hook* nil
  "NIL or a function collecting assumptions made during instance selection.
Called with (PREDICATE INSTANCE) when a stable instance is selected, or with
(:CLOSED-CLASS CLASS-NAME) when an optimizer relies on the absence of marked
instances in a class and its superclasses. Bind around compilation to collect
these assumptions for registration in the resulting environment. Validation
binds this to NIL so that checking an assumption does not record another one.")

(defvar *defer-instance-validation* nil
  "When true, ADD-INSTANCE postpones validation of recorded instance choices.
This allows a compilation unit's instance updates to be checked together.
Overlap permissions and functional dependencies are still checked immediately.
The caller must bind *CHANGED-INSTANCE-CLASSES* to NIL and validate its classes
with VALIDATE-INSTANCE-SELECTIONS before publishing the completed environment.")

(defvar *changed-instance-classes* nil
  "Class names modified by ADD-INSTANCE while validation is deferred.
ADD-INSTANCE accumulates names with PUSHNEW. Bind to NIL for each batch of
updates; use the final list to validate recorded choices in the resulting
environment before installing it. See *DEFER-INSTANCE-VALIDATION*.")

(defun predicate-subsumes-p (general specific)
  (and (not (predicate-match-impossible-p general specific))
       (handler-case
           (progn (predicate-match (fresh-pred general) (fresh-pred specific)) t)
         (predicate-unification-error () nil))))

(defun same-instance-head-p (left right)
  (and (predicate-subsumes-p left right)
       (predicate-subsumes-p right left)))

(defun instance-more-specific-p (left right)
  (let ((left (ty-class-instance-predicate left))
        (right (ty-class-instance-predicate right)))
    (and (predicate-subsumes-p right left)
         (not (predicate-subsumes-p left right)))))

(defun lookup-class-instance-by-head (env pred &key no-error)
  "Find a definition, rather than select evidence for a wanted constraint."
  (or (find pred (lookup-class-instances env (ty-predicate-class pred) :no-error t)
            :key #'ty-class-instance-predicate :test #'same-instance-head-p)
      (unless no-error (error "Unknown instance definition ~S" pred))))

(defun class-has-overlap-p (env class &optional seen)
  "Whether CLASS or any of its superclasses permits overlapping evidence."
  (unless (member class seen)
    (let ((overlap-p
            (or (some #'ty-class-instance-overlap-p
                      (lookup-class-instances env class :no-error t))
                (some (lambda (pred)
                        (class-has-overlap-p env (ty-predicate-class pred) (cons class seen)))
                      (ty-class-superclasses (lookup-class env class))))))
      ;; Optimizers depend on this negative fact too. Marking the class later
      ;; must invalidate code that was allowed to discard its dictionaries.
      (when (and (not overlap-p) *instance-selection-hook*)
        (funcall *instance-selection-hook* :closed-class class))
      overlap-p)))

(defun lookup-class-instance-in-list (pred instances &key no-error)
  "Select the unique most-specific matching head, independently of context.
Marked instances are open to future specialization: never discharge a wanted
predicate containing inference variables through such an instance."
  (let* ((matches
           (loop :for instance :in instances
                 :when (predicate-subsumes-p (ty-class-instance-predicate instance) pred)
                   :collect instance))
         (winners
           (remove-if (lambda (instance)
                        (some (lambda (other) (instance-more-specific-p other instance)) matches))
                      matches)))
    (when (and (cdr winners) (null (type-variables pred)))
      (error 'ambiguous-instance-error :pred pred
             :instances (mapcar #'ty-class-instance-predicate winners)))
    (when (and (= 1 (length winners))
               (not (and (type-variables pred)
                         (ty-class-instance-overlap-p (first winners)))))
      (let* ((instance (first winners))
             (subs (predicate-match (ty-class-instance-predicate instance) pred)))
        (when *instance-selection-hook*
          (funcall *instance-selection-hook* pred instance))
        (return-from lookup-class-instance-in-list (values instance subs))))
    (unless no-error
      (error "No stable instance for predicate ~S; retain its class constraint" pred))))

(defun lookup-class-instance (env pred &key no-error)
  (declare (type environment env))
  (let* ((pred-class (ty-predicate-class pred))
         (instances (lookup-class-instances env pred-class :no-error no-error)))
    (lookup-class-instance-in-list pred instances :no-error no-error)))

(defun lookup-instance-by-codegen-sym (env codegen-sym &key no-error)
  (declare (type environment env)
           (type symbol codegen-sym))

  (or (immutable-map-lookup (instance-environment-codegen-syms (environment-instance-environment env)) codegen-sym)
   (unless no-error
     (error "Unknown instance with codegen-sym ~A" codegen-sym))))

(defun lookup-function-source-parameter-names (env function-name)
  (declare (type environment env)
           (type symbol function-name)
           (values parser:pattern-list &optional))
  (values (immutable-map-lookup (environment-source-name-environment env) function-name)))

(define-env-updater set-function-source-parameter-names (env function-name source-parameter-names)
  (declare (type environment env)
           (type symbol function-name)
           (type parser:pattern-list source-parameter-names))
  (update-environment
   env
   :source-name-environment (immutable-map-set (environment-source-name-environment env)
                                               function-name
                                               source-parameter-names
                                               #'make-source-name-environment)))

(define-env-updater unset-function-source-parameter-names (env function-name)
  (declare (type environment env)
           (type symbol function-name))
  (update-environment
   env
   :source-name-environment (immutable-map-remove (environment-source-name-environment env)
                                                  function-name
                                                  #'make-source-name-environment)))


(defun constructor-arguments (name env)
  (declare (type symbol name)
           (type environment env)
           (values ty-list &optional))
  (function-type-arguments (lookup-value-type env name)))

(defun instance-signature= (left right)
  "Compare the ordered dictionary ABI, including variables shared with the head."
  (let ((left-preds (instance-dictionary-signature left))
        (right-preds (instance-dictionary-signature right)))
    (and (eq (ty-class-instance-overlap-p left) (ty-class-instance-overlap-p right))
         (eq (ty-class-instance-derived-p left) (ty-class-instance-derived-p right))
         (eq (ty-class-instance-codegen-sym left) (ty-class-instance-codegen-sym right))
         (= (length left-preds) (length right-preds))
         (every (lambda (a b)
                  (and (eq (ty-predicate-class a) (ty-predicate-class b))
                       (= (length (ty-predicate-types a)) (length (ty-predicate-types b)))))
                left-preds right-preds)
         (flet ((signature (preds)
                  (let ((ty (make-function-ty
                             :positional-input-types (mapcan (lambda (p) (copy-list (ty-predicate-types p))) preds)
                             :output-types nil)))
                    (quantify (type-variables ty) (qualify nil ty)))))
           (ty-scheme= (signature left-preds) (signature right-preds))))))

(defun validate-instance-selection (env selection)
  (destructuring-bind (pred previous origin) selection
    (when (eq pred :closed-class)
      (let ((*instance-selection-hook* nil))
        (when (class-has-overlap-p env previous)
          (error 'stale-instance-selection-error :pred previous :origin origin)))
      (return-from validate-instance-selection))
    (let* ((*instance-selection-hook* nil)
           (current (handler-case (lookup-class-instance env pred :no-error t)
                      (ambiguous-instance-error () nil))))
      (unless (and current (instance-signature= current previous))
        (error 'stale-instance-selection-error :pred pred :origin origin)))))

(defun validate-instance-selections (env &optional class)
  (let ((selections (instance-environment-selections (environment-instance-environment env))))
    (if class
        (dolist (selection (immutable-listmap-lookup selections class :no-error t))
          (validate-instance-selection env selection))
        (immutable-listmap-foreach
         (lambda (class selections)
           (declare (ignore class))
           (dolist (selection selections) (validate-instance-selection env selection)))
         selections)))
  env)

(define-env-updater register-instance-selections (env selections)
  (let* ((instances (environment-instance-environment env))
         (recorded (instance-environment-selections instances)))
    (dolist (selection selections)
      (let ((class (if (eq (first selection) :closed-class)
                       (second selection)
                       (ty-predicate-class (first selection)))))
        (unless (find selection (immutable-listmap-lookup recorded class :no-error t)
                      :test (lambda (a b)
                              (if (or (eq (first a) :closed-class) (eq (first b) :closed-class))
                                  (and (eq (first a) (first b)) (eq (second a) (second b)))
                                  (and (same-instance-head-p (first a) (first b))
                                       (instance-signature= (second a) (second b))))))
          (validate-instance-selection env selection)
          (setf recorded (immutable-listmap-push recorded class selection)))))
    (update-environment env :instance-environment
                        (make-instance-environment
                         :instances (instance-environment-instances instances)
                         :codegen-syms (instance-environment-codegen-syms instances)
                         :selections recorded))))

(define-env-updater add-instance (env class value)
  (declare (type environment env)
           (type symbol class)
           (type ty-class-instance value))
  ;; Do not guess the ABI of a loaded instance from its source constraints.
  (instance-dictionary-signature value)
  (lookup-class env class)
  (let ((replacement nil)
        (instances (environment-instance-environment env)))
    ;; Check every neighbor, even when replacing an existing definition: removing
    ;; an overlap marker must not silently leave an unmarked overlapping pair.
    (loop :for inst :in (lookup-class-instances env class :no-error t)
          :for index :from 0
          :do (cond
                ((same-instance-head-p (ty-class-instance-predicate value)
                                       (ty-class-instance-predicate inst))
                 (setf replacement index))
                ((handler-case
                     (progn (predicate-mgu (fresh-pred (ty-class-instance-predicate value))
                                           (fresh-pred (ty-class-instance-predicate inst))) t)
                   (predicate-unification-error () nil))
                 (unless (and (ty-class-instance-overlap-p value)
                              (ty-class-instance-overlap-p inst))
                   (error 'overlapping-instance-error
                          :inst1 (ty-class-instance-predicate value)
                          :inst2 (ty-class-instance-predicate inst))))))
    (let ((new-env
            (update-environment
             env :instance-environment
             (make-instance-environment
              :selections (instance-environment-selections instances)
              :instances (if replacement
                             (immutable-listmap-replace (instance-environment-instances instances)
                                                        class replacement value)
                             (immutable-listmap-push (instance-environment-instances instances) class value))
              :codegen-syms (immutable-map-set (instance-environment-codegen-syms instances)
                                             (ty-class-instance-codegen-sym value) value)))))
      (if *defer-instance-validation*
          (pushnew class *changed-instance-classes*)
          (validate-instance-selections new-env class))
      new-env)))

(define-env-updater set-method-inline (env method instance codegen-sym)
  (declare (type environment env)
           (type symbol method instance codegen-sym))
  (update-environment
   env
   :method-inline-environment
   (immutable-map-set
    (environment-method-inline-environment env)
    (cons method instance)
    codegen-sym
    #'make-method-inline-environment)))

(defun lookup-method-inline (env method instance &key no-error)
  (declare (type environment env)
           (type symbol method instance)
           (values symbol))
  (or
   (immutable-map-lookup
    (environment-method-inline-environment env)
    (cons method instance))
   (unless no-error
     (error "Unable to find inline method for method ~A on instance ~S." method instance))))

(define-env-updater set-code (env name code)
  (declare (type environment env)
           (type symbol name)
           (type t code))
  (update-environment
   env
   :code-environment
   (immutable-map-set
    (environment-code-environment env)
    name
    code
    #'make-code-environment)))

(defun lookup-code (env name &key no-error)
  (declare (type environment env)
           (type symbol name)
           (values t))
  (or
   (immutable-map-lookup
    (environment-code-environment env)
    name)
   (unless no-error
     (error "Unable to find code for function ~A." name))))

(define-env-updater add-specialization (env entry)
  (declare (type environment env)
           (type specialization-entry entry))

  (let* ((from (specialization-entry-from entry))
         (to (specialization-entry-to entry))
         (to-ty (specialization-entry-to-ty entry))

         (to-scheme (quantify (type-variables to-ty)
                              (qualify nil to-ty))))

    (loop :for elem :in (immutable-listmap-lookup (environment-specialization-environment env) from :no-error t)
          :for index :from 0
          :do
      (let* ((type (specialization-entry-to-ty elem))
             (scheme (quantify (type-variables type)
                               (qualify nil type))))

        (when (ty-scheme= to-scheme scheme)
          (return-from add-specialization
            (update-environment env
                                :specialization-environment
                                (immutable-listmap-replace
                                 (environment-specialization-environment env)
                                 from
                                 index
                                 entry
                                 #'make-specialization-environment))))

        (when (handler-case
                  (progn
                    (unify nil to-ty (qualified-ty-type (fresh-inst scheme)))
                    t)
                (coalton-internal-type-error () nil))
          (error 'overlapping-specialization-error
                 :new to
                 :existing (specialization-entry-to elem)))))

    (update-environment env
                        :specialization-environment
                        (immutable-listmap-push
                         (environment-specialization-environment env)
                         from
                         entry
                         #'make-specialization-environment))))

(defun lookup-specialization (env from to &key (no-error nil))
  (declare (type environment env)
           (type symbol from)
           (type symbol to)
           (values (or null specialization-entry) &optional))
  (dolist (elem (immutable-listmap-lookup (environment-specialization-environment env) from :no-error no-error))
    (when (eq to (specialization-entry-to elem))
      (return-from lookup-specialization elem)))

  (unless no-error
    (error "Unable to find specialization from ~A to ~A" from to)))

(defun lookup-specialization-by-type (env from ty &key (no-error nil))
  (declare (type environment env)
           (type symbol from)
           (type ty ty)
           (values (or null specialization-entry) &optional))
  (dolist (elem (immutable-listmap-lookup (environment-specialization-environment env) from :no-error no-error))
    (handler-case
        (progn
          (match (specialization-entry-to-ty elem) ty)
          (return-from lookup-specialization-by-type elem))
      (coalton-internal-type-error (e)
        (declare (ignore e))))))

(defun lookup-fundep-environment (env class &key (no-error nil))
  (declare (type environment env)
           (type symbol class)
           (values (or null immutable-listmap) &optional))
  (let ((result (immutable-map-lookup (environment-fundep-environment env) class)))
    (when (and (not result) (not no-error))
      (util:coalton-bug "Unable to find fundep environment for class ~A" class))

    result))

(define-env-updater initialize-fundep-environment (env class)
  (declare (type environment env)
           (type symbol class))
  (let ((result (lookup-fundep-environment env class :no-error t)))
    (when result
      (return-from initialize-fundep-environment env))
    (update-environment
     env
     :fundep-environment (immutable-map-set
                          (environment-fundep-environment env)
                          class
                          (make-immutable-listmap)
                          #'make-fundep-environment))))

(defstruct fundep-entry
  (from      (util:required 'from) :type ty-list :read-only t)
  (to        (util:required 'to)   :type ty-list :read-only t))

(defmethod make-load-form ((self fundep-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun insert-fundep-entry% (env class fundep entry)
  (declare (type environment env)
           (type symbol class)
           (type fixnum fundep)
           (type fundep-entry entry))

  (let ((result (lookup-fundep-environment env class)))
    (update-environment
     env
     :fundep-environment
     (immutable-map-set
      (environment-fundep-environment env)
      class
      (immutable-listmap-push
       result
       fundep
       entry)
      #'make-fundep-environment))))

(defun instantiate-superclasses (class pred)
  "Instantiate CLASS's superclass context for one occurrence of PRED.
Variables absent from the class head are shared within this context,
but not with any other occurrence of the class."
  (declare (type ty-class class)
           (type ty-predicate pred)
           (values ty-predicate-list &optional))
  (when (ty-class-superclasses class)
    (destructuring-bind (head . supers)
        (fresh-preds (cons (ty-class-predicate class) (ty-class-superclasses class)))
      (let ((subs (predicate-match head pred)))
        (loop :for super :in supers
              :collect (make-ty-predicate
                        :class (ty-predicate-class super)
                        :types (apply-substitution subs (ty-predicate-types super))
                        :location (source:location pred)))))))

(defun given-superclass-expansions (env context)
  "Instantiate the fields of each dictionary reachable from CONTEXT once."
  (let ((expansions nil))
    (labels ((visit (pred)
               (unless (assoc pred expansions :test #'type-predicate=)
                 (let ((supers (instantiate-superclasses (lookup-class env (ty-predicate-class pred)) pred)))
                   (when supers
                     (push (cons pred supers) expansions)
                     (mapc #'visit supers))))))
      (mapc #'visit context))
    expansions))

(defvar *given-superclass-expansions* nil
  "Instantiated fields of the dictionaries in scope while checking or translating an instance.")

(defun superclass-evidence (pred expansions)
  (or (ty-predicate-superclass-evidence pred)
      (cdr (assoc pred expansions :test #'type-predicate=))))

(defun context-superclasses (class pred)
  "Look up the superclass fields of a given dictionary in declaration order."
  (or (superclass-evidence pred *given-superclass-expansions*)
      (apply-substitution (predicate-match (ty-class-predicate class) pred)
                          (ty-class-superclasses class))))

(defun instantiate-context-superclasses (env preds)
  "Give each hidden superclass type a witness local to its dictionary.
Improvement, entailment and dictionary selection must all see the same fields."
  (declare (type environment env) (type ty-predicate-list preds))
  (labels ((visit (pred)
             (let* ((class (lookup-class env (ty-predicate-class pred)))
                    (supers (ty-class-superclasses class)))
               (when (or (null supers) (ty-predicate-superclass-evidence pred))
                 (return-from visit pred))
               (let* ((hidden (not (subsetp (type-variables supers)
                                            (ty-predicate-types (ty-class-predicate class))
                                            :test #'ty=)))
                      (fields (mapcar #'visit
                                      (if hidden
                                          (or (superclass-evidence pred *given-superclass-expansions*)
                                              (instantiate-superclasses class pred))
                                          (context-superclasses class pred)))))
                 ;; Ordinary superclass arguments can be reconstructed from the head.
                 (if (or hidden (some #'ty-predicate-superclass-evidence fields))
                     (make-ty-predicate :class (ty-predicate-class pred)
                                        :types (ty-predicate-types pred)
                                        :location (source:location pred)
                                        :superclass-evidence fields)
                     pred)))))
    (mapcar #'visit preds)))

(defun collect-fundeps (env preds &optional (scope (make-hash-table :test #'eq)))
  "Collect the functional dependencies associated with PREDS as CONSes of
LISTs of TYs, recursively expanding to the superclass predicates.
SCOPE keeps the hidden variables of equal predicates identical when
comparing collections of dependencies in the same context.

For example, with the definitions

  (define-class (C :a :b (:a -> :b)))
  (define-class (C :a :b => D :a :b))

and the single predicate 

  D (List #T1) #T2

This function will return the single functional dependency

  (List #T1 . #T2)"
  (declare (type environment env)
           (type ty-predicate-list preds)
           (type hash-table scope))
  (loop :for pred :in preds
        :for pred-tys := (ty-predicate-types pred)
        :for class := (lookup-class env (ty-predicate-class pred))
        :for class-vars := (ty-class-class-variables class)
        :for supers := (or (cdr (assoc pred (gethash (ty-class-name class) scope)
                                       :test #'type-predicate=))
                           (let ((supers (instantiate-superclasses class pred)))
                             (when supers
                               (push (cons pred supers) (gethash (ty-class-name class) scope)))
                             supers))
        :nconc (collect-fundeps env supers scope)
        :nconc (loop :for fundep :in (ty-class-fundeps class)
                     :collect (cons (project-elements
                                     (fundep-from fundep)
                                     class-vars
                                     pred-tys)
                                    (project-elements
                                     (fundep-to fundep)
                                     class-vars
                                     pred-tys)))))

(defun collect-fundep-vars (env preds &optional (scope (make-hash-table :test #'eq)))
  "Collect the type variable functional dependencies associated with
PREDs as CONSes of LISTs of TYVARs, recursively expanding to the
superclass predicates.

For example, with the definitions

  (define-class (C :a :b (:a -> :b)))
  (define-class (C :a :b => D :a :b))

and the single predicate 

  D (List #T1) #T2

This function will return the single functional dependency

  (#T1 . #T2)"
  (declare (type environment env)
           (type ty-predicate-list preds)
           (type hash-table scope))
  (loop :for (from . to) :in (collect-fundeps env preds scope)
        :collect (cons (type-variables from) (type-variables to))))

(defun context-fundep-substitutions (env preds)
  "Derive equalities from PREDS and their superclasses, without selecting instances."
  (declare (type environment env)
           (type ty-predicate-list preds)
           (values substitution-list &optional))
  (labels ((expand (pred)
             (let ((class (lookup-class env (ty-predicate-class pred))))
               (cons pred (mapcan #'expand (instantiate-superclasses class pred))))))
    ;; Pairwise improvement only unifies existing variables.
    (loop :with expanded := (mapcan #'expand preds)
          :for subs := nil :then new-subs
          :for new-subs := (improve-predicate-fundeps env expanded subs)
          :when (equalp subs new-subs) :return subs)))

(defun check-fundep-agreement (env class-vars fundep new old)
  "Check one dependency of two instances, freshened apart as (head . context)."
  (declare (type environment env)
           (type util:symbol-list class-vars)
           (type fundep fundep)
           (type ty-predicate-list new old))
  (flet ((project (keys instance)
           (project-elements keys class-vars (ty-predicate-types (car instance)))))
    (let* ((new-from (project (fundep-from fundep) new))
           (old-from (project (fundep-from fundep) old))
           (det-subs (handler-case (unify-list nil new-from old-from)
                       (coalton-internal-type-error () (return-from check-fundep-agreement))))
           (new-to (apply-substitution det-subs (project (fundep-to fundep) new)))
           (old-to (apply-substitution det-subs (project (fundep-to fundep) old))))
      (when (every #'ty= new-to old-to)
        (return-from check-fundep-agreement))
      (let* ((inputs (apply-substitution det-subs new-from))
             (new-row (append inputs new-to))
             (old-row (append inputs old-to)))
        (unless (and ;; Improvement uses these rows without their contexts. Neither
                     ;; row may promise a more specific result than the other.
                     (match-list-p new-row old-row)
                     (match-list-p old-row new-row)
                     ;; Only the prerequisites are givens. Including the heads here
                     ;; would assume the very dependency we're trying to establish.
                     (handler-case
                         (let* ((givens (apply-substitution det-subs (append (cdr new) (cdr old))))
                                (subs (context-fundep-substitutions env givens)))
                           (every #'ty= (apply-substitution subs new-to)
                                        (apply-substitution subs old-to)))
                       (context-fundep-conflict () nil)))
          (error 'fundep-conflict :new-pred (car new) :old-pred (car old) :fundep fundep))))))

(define-env-updater update-instance-fundeps (env pred context)
  ;; This function is essential for cases like the following.
  ;;
  ;;   (define-class (C :a :b (:a -> :b)))
  ;;   (define-instance (C UFix Integer))
  ;;
  ;; This function ensures that when the compile encounters the
  ;; predicate C UFix :t, it knows that :t must be the type Integer.
  (declare (type environment env)
           (type ty-predicate pred)
           (type ty-predicate-list context))

  (let* ((class (lookup-class env (ty-predicate-class pred)))
         (fundep-env
           (lookup-fundep-environment env (ty-predicate-class pred)))
         (class-variables (ty-class-class-variables class))
         (pred-tys (ty-predicate-types pred))
         (context-fundeps (collect-fundep-vars env context))
         (new (fresh-preds (cons pred context)))
         ;; The improvement index can coalesce identical rows even when the
         ;; instances have different prerequisites. Check the actual definitions.
         (others (loop :for inst :in (lookup-class-instances env (ty-class-name class) :no-error t)
                       :for head := (ty-class-instance-predicate inst)
                       :unless (same-instance-head-p pred head) ; being replaced, not a neighbor
                         :collect (fresh-preds (cons head (ty-class-instance-constraints inst))))))

    (loop :for fundep :in (ty-class-fundeps class)
          :for i :from 0
          ;; Lookup the state for the ith fundep
          :for state := (immutable-listmap-lookup fundep-env i :no-error t)
          :for from-tys := (project-elements
                            (fundep-from fundep)
                            class-variables
                            pred-tys)
          :for to-tys := (project-elements
                          (fundep-to fundep)
                          class-variables
                          pred-tys)
          ;; Coverage (Jones, §6.1) applies to the relation this instance adds.
          ;; The context supplies dependencies, including inherited ones. The
          ;; head's superclasses are checked separately by requiring dictionaries.
          :unless (subsetp (type-variables to-tys)
                           (generic-closure (type-variables from-tys) context-fundeps :test #'ty=)
                           :test #'ty=)
            :do (error 'fundep-ambiguity)
          :do (dolist (other others)
                (check-fundep-agreement env class-variables fundep new other))
          :unless (loop :with row := (append from-tys to-tys)
                        :for s :in state
                        :for fresh := (fresh-fundep-entry s)
                        :thereis (match-list-p row (append (fundep-entry-from fresh) (fundep-entry-to fresh))))
            :do (setf env (insert-fundep-entry% env (ty-class-name class) i
                                                (make-fundep-entry :from from-tys :to to-tys)))))

  env)

(defun check-instance-fundep-consistency (env pred context)
  "Signal CONTEXT-FUNDEP-CONFLICT if the head PRED and the CONTEXT of an
instance, including their superclasses, conflict with functional
dependencies.

An instance only applies when its context holds, so its head and its
context must hold together. For example, with the class

  (define-class (C :a :b (:a -> :b)))

the instance (C :a :b => C :a (List :b)) requires C :a :b whenever it
provides C :a (List :b), but :a cannot determine both :b and (List :b).
Its context covers the dependent :b, so the coverage check in
UPDATE-INSTANCE-FUNDEPS accepts it, yet improving with it never
terminates: every C :a :x determines :x to be (List :y) for some
C :a :y."
  (declare (type environment env)
           (type ty-predicate pred)
           (type ty-predicate-list context)
           (values null &optional))
  (context-fundep-substitutions env (cons pred context))
  nil)

;;; Instance-head improvement. A unique non-linear instance head, such
;;; as `Applicative (Box :r :r)`, can force equalities in a wanted
;;; predicate without implying that a lone concrete instance should
;;; become a default.

(defun type-variable-occurrences (type)
  (declare (type (or ty ty-predicate keyword-ty-entry list) type)
           (values tyvar-list &optional))
  (typecase type
    (ty-predicate
     (type-variable-occurrences (ty-predicate-types type)))
    (tyvar
     (list type))
    (tapp
     (append (type-variable-occurrences (tapp-from type))
             (type-variable-occurrences (tapp-to type))))
    (keyword-ty-entry
     (type-variable-occurrences (keyword-ty-entry-type type)))
    (function-ty
     (append
      (type-variable-occurrences (function-ty-positional-input-types type))
      (type-variable-occurrences (function-ty-keyword-input-types type))
      (type-variable-occurrences (function-ty-output-types type))))
    (result-ty
     (type-variable-occurrences (result-ty-output-types type)))
    (list
     (mapcan #'type-variable-occurrences type))
    (ty
     nil)))

(defun repeated-type-variables (type)
  (declare (type (or ty ty-predicate list) type)
           (values tyvar-list &optional))
  (let ((counts nil))
    (dolist (var (type-variable-occurrences type))
      (let ((entry (assoc var counts :test #'ty=)))
        (if entry
            (incf (cdr entry))
            (push (cons var 1) counts))))
    (loop :for (var . count) :in counts
          :when (> count 1)
            :collect var)))

;; Fundep solving repeatedly asks whether a wanted predicate can match or unify
;; with an instance head. Classes with many concrete instances, such as `Num`,
;; make a linear scan expensive for large generated predicate sets. Instance
;; indexes bucket instances by the head constructor of the first predicate
;; argument, while preserving original instance order by storing each instance's
;; original position. A concrete wanted head only needs its matching concrete
;; bucket plus variable-headed instances. Variable, function, result, and
;; unknown wanted heads conservatively fall back to the full instance list
;; whenever a first-head test would risk dropping a possible match.
(defconstant +instance-variable-head-key+ ':coalton-instance-variable-head)
(defconstant +instance-nullary-head-key+ ':coalton-instance-nullary-head)
(defconstant +instance-function-head-key+ ':coalton-instance-function-head)
(defconstant +instance-result-head-key+ ':coalton-instance-result-head)
(defconstant +instance-unknown-head-key+ ':coalton-instance-unknown-head)

(defstruct instance-index
  (instances nil :type list :read-only t)
  (buckets (util:required 'buckets) :type hash-table :read-only t)
  (nonlinear-head-p nil :type boolean :read-only t))

(defun type-head-key (type)
  (declare (type ty type)
           (values symbol &optional))
  (typecase type
    ((or tyvar tgen)
     +instance-variable-head-key+)
    (tycon
     (tycon-name type))
    (tapp
     (type-head-key (tapp-from type)))
    (function-ty
     +instance-function-head-key+)
    (result-ty
     +instance-result-head-key+)
    (t
     +instance-unknown-head-key+)))

(defun predicate-head-key (pred)
  (declare (type ty-predicate pred)
           (values symbol &optional))
  (let ((types (ty-predicate-types pred)))
    (if (endp types)
        +instance-nullary-head-key+
        (type-head-key (first types)))))

(defun broad-instance-head-key-p (key)
  (declare (type symbol key)
           (values boolean &optional))
  (or (eq key +instance-function-head-key+)
      (eq key +instance-result-head-key+)
      (eq key +instance-unknown-head-key+)))

(defun build-instance-index (instances)
  (declare (type list instances)
           (values instance-index &optional))
  (let ((buckets (make-hash-table :test #'eq))
        (nonlinear-head-p nil))
    (loop :for instance :in instances
          :for index :from 0
          :for pred := (ty-class-instance-predicate instance)
          :do
             (push (cons index instance)
                   (gethash (predicate-head-key pred) buckets))
             (when (repeated-type-variables pred)
               (setf nonlinear-head-p t)))
    (maphash (lambda (key entries)
               (setf (gethash key buckets) (nreverse entries)))
             buckets)
    (make-instance-index
     :instances instances
     :buckets buckets
     :nonlinear-head-p nonlinear-head-p)))

(defun merge-indexed-instance-lists (left right)
  (declare (type list left right)
           (values list &optional))
  (cond
    ((endp left)
     right)
    ((endp right)
     left)
    ((< (caar left) (caar right))
     (cons (car left)
           (merge-indexed-instance-lists (cdr left) right)))
    (t
     (cons (car right)
           (merge-indexed-instance-lists left (cdr right))))))

(defun indexed-instance-candidates-for-match (index pred)
  "Return instances that can match PRED with `predicate-match`.

This is an over-approximation based on the first predicate type. It preserves
instance order while avoiding impossible concrete-head matches such as
`Num UFix` against `Num #T`."
  (declare (type instance-index index)
           (type ty-predicate pred)
           (values list &optional))
  (let* ((buckets (instance-index-buckets index))
         (key (predicate-head-key pred))
         (variable-instances (gethash +instance-variable-head-key+ buckets)))
    (cond
      ((eq key +instance-variable-head-key+)
       (mapcar #'cdr variable-instances))
      ((broad-instance-head-key-p key)
       (instance-index-instances index))
      (t
       (mapcar #'cdr
               (merge-indexed-instance-lists
                (gethash key buckets)
                variable-instances))))))

(defun indexed-instance-candidates-for-unification (index pred)
  "Return instances that can unify with PRED with `predicate-mgu`.

When PRED has an unknown head, every concrete instance head may still determine
that variable, so this falls back to the full instance list."
  (declare (type instance-index index)
           (type ty-predicate pred)
           (values list &optional))
  (let* ((buckets (instance-index-buckets index))
         (key (predicate-head-key pred)))
    (if (or (eq key +instance-variable-head-key+)
            (broad-instance-head-key-p key))
        (instance-index-instances index)
        (mapcar #'cdr
                (merge-indexed-instance-lists
                 (gethash key buckets)
                 (gethash +instance-variable-head-key+ buckets))))))

(defun linearize-type (type)
  "Replace each type variable occurrence in TYPE with a distinct fresh variable."
  (declare (type (or ty keyword-ty-entry list) type))
  (typecase type
    (tyvar
     (make-variable :kind (kind-of type)
                    :allow-result-p (tyvar-allow-result-p type)
                    :source-name (tyvar-source-name type)))
    (tapp
     (make-tapp
      :alias (mapcar #'linearize-type (ty-alias type))
      :from (linearize-type (tapp-from type))
      :to (linearize-type (tapp-to type))))
    (keyword-ty-entry
     (make-keyword-ty-entry
      :keyword (keyword-ty-entry-keyword type)
      :type (linearize-type (keyword-ty-entry-type type))))
    (function-ty
     (make-function-ty
      :alias (mapcar #'linearize-type (ty-alias type))
      :positional-input-types (linearize-type (function-ty-positional-input-types type))
      :keyword-input-types (linearize-type (function-ty-keyword-input-types type))
      :keyword-open-p (function-ty-keyword-open-p type)
      :output-types (linearize-type (function-ty-output-types type))))
    (result-ty
     (make-result-ty
      :alias (mapcar #'linearize-type (ty-alias type))
      :output-types (linearize-type (result-ty-output-types type))))
    (list
     (mapcar #'linearize-type type))
    (ty
     type)))

(defun linearize-pred (pred)
  (declare (type ty-predicate pred)
           (values ty-predicate &optional))
  (make-ty-predicate
   :class (ty-predicate-class pred)
   :types (linearize-type (ty-predicate-types pred))
   :location (source:location pred)))

(defun single-unifying-instance (pred instances)
  (declare (type ty-predicate pred)
           (type list instances)
           (values (or null ty-class-instance) boolean &optional))
  (let ((match nil))
    (dolist (instance instances)
      (handler-case
          (progn
            (predicate-mgu (fresh-pred (ty-class-instance-predicate instance)) pred)
            (when match
              (return-from single-unifying-instance (values nil nil)))
            (setf match instance))
        (predicate-unification-error () nil)))
    (values match (and match t))))

(defun improve-predicate-from-instance-head (env pred subs &key (instances nil instances-supplied-p))
  (declare (type environment env)
           (type ty-predicate pred)
           (type substitution-list subs)
           (type list instances)
           (values substitution-list boolean &optional))
  (multiple-value-bind (instance foundp)
      (single-unifying-instance
       pred
       (if instances-supplied-p
           instances
           (lookup-class-instances env (ty-predicate-class pred) :no-error t)))
    (unless (and foundp (not (ty-class-instance-overlap-p instance)))
      (return-from improve-predicate-from-instance-head (values subs nil)))
    (let* (;; Repeated variables inside a structural argument can refine that
           ;; argument. Relationships between separate class parameters require
           ;; functional dependencies: e.g. Into :a :a must not specialize a
           ;; declared Into :b String constraint merely because it is the only
           ;; instance loaded so far.
           (instance-pred
             (make-ty-predicate
              :class (ty-predicate-class pred)
              :types (loop :for type :in (ty-predicate-types (ty-class-instance-predicate instance))
                           :collect (first (ty-predicate-types
                                            (fresh-pred (make-ty-predicate
                                                         :class (ty-predicate-class pred)
                                                         :types (list type))))))))
           (instance-vars (type-variables instance-pred))
           (wanted-pred (apply-substitution subs pred))
           (wanted-vars (type-variables wanted-pred)))
      (unless (repeated-type-variables instance-pred)
        (return-from improve-predicate-from-instance-head (values subs nil)))
      (handler-case
          (let ((full-subs (predicate-mgu instance-pred wanted-pred))
                ;; The wanted type must already have the instance's shape.
                ;; Unification here would invent that shape merely because
                ;; there is currently only one applicable instance.
                (linear-subs (predicate-match (linearize-pred instance-pred) wanted-pred)))
            ;; Only keep wanted substitutions that come from repeated
            ;; instance variables. Substitutions also produced by the
            ;; linearized head are ordinary instance matching, not
            ;; instance-head improvement.
            (loop :with new-subs := subs
                  :with improvedp := nil
                  :for var :in wanted-vars
                  :for full-type := (apply-substitution full-subs var)
                  :for linear-type := (apply-substitution linear-subs var)
                  :when (and (not (ty= var full-type))
                             (not (ty= full-type linear-type))
                             (not (intersection (type-variables full-type)
                                                instance-vars
                                                :test #'ty=)))
                    :do (let ((previous-subs new-subs))
                          (setf new-subs (unify new-subs var full-type))
                          (unless (equalp previous-subs new-subs)
                            (setf improvedp t)))
                  :finally (return (values new-subs improvedp))))
        (coalton-internal-type-error ()
          (values subs nil))))))

(defun improve-predicate-fundeps (env preds subs)
  "Relate every pair of predicates whose determinants are equal under SUBS."
  (loop :for remaining :on preds
        :for pred := (first remaining)
        :for class-name := (ty-predicate-class pred)
        :for class := (lookup-class env class-name)
        :for class-vars := (ty-class-class-variables class)
        :when (ty-class-fundeps class)
          :do (dolist (other (rest remaining))
                (when (eq class-name (ty-predicate-class other))
                  (handler-case
                      (dolist (fundep (ty-class-fundeps class))
                        (let ((types (apply-substitution subs (ty-predicate-types pred)))
                              (other-types (apply-substitution subs (ty-predicate-types other))))
                          (when (every #'ty=
                                       (project-elements (fundep-from fundep) class-vars types)
                                       (project-elements (fundep-from fundep) class-vars other-types))
                            (setf subs
                                  (unify-list subs
                                              (project-elements (fundep-to fundep) class-vars types)
                                              (project-elements (fundep-to fundep) class-vars other-types))))))
                    (coalton-internal-type-error ()
                      (error 'context-fundep-conflict
                             :first-pred (apply-substitution subs pred)
                             :second-pred (apply-substitution subs other)))))))
  subs)

(defun solve-fundeps (env preds subs &key given-expansions)
  "First, this function creates and applies substitutions to preds based
on functional dependencies that constrain them with respect to one
another, and then this function creates and applies substitutions to
preds based on functional dependencies that constrain them with
respect to defined type class instances. The values returned are the
predicates with all substitutions applied and the new substitutions.
GIVEN-EXPANSIONS, if supplied, fixes the identities of the hidden
variables in dictionaries already in scope.

Signal FUNDEP-NONTERMINATION if improvement keeps specializing PREDS
without reaching a fixed point within +FUNDEP-MAX-DEPTH+ rounds."
  (declare (type environment env)
           (type ty-predicate-list preds)
           (type substitution-list subs)
           (values ty-predicate-list substitution-list &optional))

  (let ((input-preds preds)
        (input-subs subs)
        (class-cache (make-hash-table :test #'eq))
        (fundepsp-cache (make-hash-table :test #'eq))
        (instance-index-cache (make-hash-table :test #'eq)))
    (labels ((class-for (class-name)
               (multiple-value-bind (class foundp)
                   (gethash class-name class-cache)
                 (if foundp
                     class
                     (setf (gethash class-name class-cache)
                           (lookup-class env class-name)))))
             (class-fundepsp (class-name)
               "Does CLASS-NAME or any superclass have functional dependencies?"
               (multiple-value-bind (fundepsp foundp)
                   (gethash class-name fundepsp-cache)
                 (if foundp
                     fundepsp
                     (let ((class (class-for class-name)))
                       (setf (gethash class-name fundepsp-cache)
                             (or (consp (ty-class-fundeps class))
                                 (loop :for superclass :in (ty-class-superclasses class)
                                       :thereis (class-fundepsp
                                                 (ty-predicate-class superclass)))))))))
             (fundepsp (preds)
               "Are any of PREDS constrained by functional dependencies?"
               (loop :for pred :in preds
                     :thereis (class-fundepsp (ty-predicate-class pred))))
             (instance-index-for (class-name)
               (multiple-value-bind (index foundp)
                   (gethash class-name instance-index-cache)
                 (if foundp
                     index
                     (setf (gethash class-name instance-index-cache)
                           (build-instance-index
                            (lookup-class-instances env class-name :no-error t))))))
             (improve-predicate-with-index (pred subs index)
               (if (not (instance-index-nonlinear-head-p index))
                   (values subs nil)
                   (let ((pred (apply-substitution subs pred)))
                     (improve-predicate-from-instance-head
                      env
                      pred
                      subs
                      :instances (indexed-instance-candidates-for-unification
                                  index
                                  pred)))))
             (instance-improvementp (preds)
               "Can any predicate be improved from a non-linear instance head?"
               (loop :for pred :in preds
                     :for index := (instance-index-for (ty-predicate-class pred))
                     :thereis (nth-value
                                1
                                (improve-predicate-with-index pred subs index)))))

      ;; If no predicates have fundeps or instance-head improvements,
      ;; then exit early.
      (unless (or (fundepsp preds) (instance-improvementp preds))
        (return-from solve-fundeps (values preds subs)))

      ;; Expand PREDS into the superclasses.
      (setf preds
            (loop :for remaining-preds := (copy-list preds)
                    :then (rest remaining-preds)
                  :until (endp remaining-preds)
                  :for pred := (first remaining-preds)
                  :for class := (class-for (ty-predicate-class pred))
                  :for applied-pred := (apply-substitution subs pred)
                  :do (alexandria:nconcf
                       remaining-preds
                       (or (copy-list (superclass-evidence applied-pred given-expansions))
                           (instantiate-superclasses class applied-pred)))
                  :collect pred))

      ;; This block is meant to simplify PREDS if instances exist in the
      ;; environment which constrain them by functional dependencies or by
      ;; repeated variables in an instance head.
      (let ((pred-histories (make-hash-table :test #'eq)))
        (labels ((substitute-preds (preds subs)
                   (loop :for pred :in preds
                         :for applied := (apply-substitution subs pred)
                         :do (unless (eq applied pred)
                               (setf (gethash applied pred-histories)
                                     (gethash pred pred-histories)))
                         :collect applied)))
          (loop :with new-subs := nil
                :with preds-generated := nil
                ;; The caller's predicates under SUBS at the start of each of
                ;; the last two quarters of the rounds, most recent first.
                :with snapshots := nil
                :for i :below +fundep-max-depth+
                :do
                   (when (or (= i (floor +fundep-max-depth+ 2))
                             (= i (floor (* 3 +fundep-max-depth+) 4)))
                     (push (apply-substitution subs input-preds) snapshots))

                   ;; Repeat pairwise improvement after instance improvement too:
                   ;; either may expose equal determinants for the next pass.
                   (setf new-subs (improve-predicate-fundeps env preds subs))
                   (setf preds (substitute-preds preds new-subs))

                   (loop :with applied-inputs := (apply-substitution new-subs input-preds)
                         :with fixed-vars := (type-variables applied-inputs)
                         :with height-cache := (make-hash-table :test #'eq)
                         :with max-given-height := (loop :for p :in applied-inputs
                                                         :maximize (predicate-height p height-cache))
                         :for pred :in preds
                         :for class-name := (ty-predicate-class pred)
                         :for class := (class-for class-name)
                         :for index := (instance-index-for class-name)
                         :for instance-candidates := (indexed-instance-candidates-for-match
                                                      index
                                                      pred)
                         ;; If there are super-predicates then add those
                         ;; predicates into the list of preds and remove
                         ;; this predicate from the list since it will give
                         ;; us no new type information. The new predicates
                         ;; are examined in the next round, so every round
                         ;; reduces each predicate that an instance matches
                         ;; and rounds measure the depth of the reductions.
                         :for instance := (lookup-class-instance-in-list
                                           pred
                                           instance-candidates
                                           :no-error t)
                         :for applied-pred := (apply-substitution new-subs pred)
                         :for history := (gethash pred pred-histories)
                         :for substituted-history
                           := (when (and instance
                                         (find instance history :key #'car :test #'eq))
                                (loop :for (prev-inst . prev-pred) :in history
                                      :collect (cons prev-inst
                                                     (apply-substitution new-subs prev-pred))))
                         :for blocked-p := (and instance
                                                (instance-recursion-blocked-p
                                                 env
                                                 instance
                                                 applied-pred
                                                 substituted-history
                                                 fixed-vars
                                                 max-given-height
                                                 height-cache))

                         :if (and instance (not blocked-p))
                           ;; Since we allow for type variables in the
                           ;; constraints which do not appear in the
                           ;; predicate, we need to create a full fresh set of
                           ;; predicates, rather than just matching the head.
                           :do (let* ((next-history (acons instance applied-pred history))
                                      (fresh-instance-preds
                                        (fresh-preds (instance-dictionary-signature instance)))
                                      (instance-head (car fresh-instance-preds))
                                      (instance-context (cdr fresh-instance-preds))

                                      (instance-subs (predicate-match
                                                      instance-head
                                                      applied-pred)))
                                 (loop :for new-pred :in instance-context
                                       :for child := (make-ty-predicate
                                                      :class (ty-predicate-class new-pred)
                                                      :types (apply-substitution instance-subs (ty-predicate-types new-pred))
                                                      :location (source:location pred))
                                       :do (setf (gethash child pred-histories) next-history)
                                           (push child preds))

                                 (setf preds (remove pred preds :test #'eq))
                                 (setf preds-generated t))

                         :else
                           :do (multiple-value-bind (improved-subs improvedp)
                                   (improve-predicate-with-index pred new-subs index)
                                 (when improvedp
                                   (setf new-subs improved-subs)
                                   (return)))
                               (when (ty-class-fundeps class)
                                 (setf new-subs (generate-fundep-subs% env applied-pred new-subs))))
                   (if (and (not preds-generated)
                            (or (equalp new-subs subs)
                                (null preds)))
                       (return-from solve-fundeps (values preds subs))
                       (setf subs new-subs))
                   (setf preds-generated nil)
                   (setf preds (substitute-preds preds subs))
                :finally
                   ;; Running out of rounds alone does not show that improvement
                   ;; diverged: reductions through instances can recur without
                   ;; improving anything, and improvement can rename variables
                   ;; without constraining them. Improvement that does not
                   ;; terminate keeps specializing the caller's predicates, which
                   ;; no finite solution allows.
                   (let ((growing
                           (loop :for input-pred :in input-preds
                                 :for earlier :in (second snapshots)
                                 :for later :in (first snapshots)
                                 :for final :in (apply-substitution subs input-preds)
                                 :unless (or (predicate-subsumes-p later earlier)
                                             (predicate-subsumes-p final later))
                                   :collect (apply-substitution input-subs input-pred))))
                     (unless growing
                       (util:coalton-bug "Fundep solving failed to fixpoint"))
                     (error 'fundep-nontermination
                            :preds (remove-duplicates growing
                                                      :test #'type-predicate=
                                                      :from-end t)))))))))


(defun generate-fundep-subs% (env pred subs)
  (declare (type environment env)
           (type ty-predicate pred)
           (type substitution-list subs))
  (let* ((class-name (ty-predicate-class pred))

         (class (lookup-class env class-name))

         (class-variables (ty-class-class-variables class))

         (fundep-env (lookup-fundep-environment env class-name)))

    (loop :for fundep :in (ty-class-fundeps class)
          :for i :from 0

          :for state := (immutable-listmap-lookup fundep-env i :no-error t)

          :when state
            :do (setf subs (generate-fundep-subs-for-pred% pred state class-variables fundep subs)))

    subs))

(defun generate-fundep-subs-for-pred% (pred state class-variables fundep subs)
  (declare (type ty-predicate pred)
           (type list state)
           (type util:symbol-list class-variables)
           (type fundep fundep)
           (type substitution-list subs)
           (values substitution-list &optional))

  (let* ((from-tys (util:project-elements
                    (fundep-from fundep)
                    class-variables
                    (ty-predicate-types pred)))
         (to-tys (util:project-elements
                    (fundep-to fundep)
                    class-variables
                    (ty-predicate-types pred))))

    (dolist (entry state)
      (handler-case
          (let* ((fresh-entry (fresh-fundep-entry entry))
                 (left-subs (match-list from-tys (fundep-entry-from fresh-entry)))
                 (right-side (apply-substitution left-subs (fundep-entry-to fresh-entry)))
                 (new-subs (unify-list subs to-tys right-side)))
            ;; The dependents of an instance can contain variables that only
            ;; its context determines. Unifying them with dependents that are
            ;; already as specific merely binds the entry's fresh variables,
            ;; which would change SUBS in every round without determining
            ;; anything, so only keep entries that specialize PRED.
            (unless (predicate-subsumes-p (apply-substitution new-subs pred)
                                          (apply-substitution subs pred))
              (return-from generate-fundep-subs-for-pred% new-subs)))
        ;; The entry does not apply: PRED's determinant is not an instance of
        ;; the entry's, for example because it differs where the entry repeats
        ;; a variable, or PRED's dependents conflict with the entry's.
        (coalton-internal-type-error () nil))))

  subs)

(defun fresh-fundep-entry (entry)
  (let* ((from-pred (make-ty-predicate
                     :class nil
                     :types (fundep-entry-from entry)))
         (to-pred (make-ty-predicate
                   :class nil
                   :types (fundep-entry-to entry)))
         (fresh-preds (fresh-preds (list from-pred to-pred))))
    (make-fundep-entry
     :from (ty-predicate-types (first fresh-preds))
     :to (ty-predicate-types (second fresh-preds)))))
