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
   (#:util #:coalton-impl/util)
   (#:source #:coalton-impl/source)
   (#:parser #:coalton-impl/parser))
  (:export
   #:*update-hook*                          ; VARIABLE
   #:value-environment                      ; STRUCT
   #:explicit-repr                          ; TYPE
   #:type-entry                             ; STRUCT
   #:make-type-entry                        ; CONSTRUCTOR
   #:type-entry-name                        ; ACCESSOR
   #:type-entry-runtime-type                ; ACCESSOR
   #:type-entry-type                        ; ACCESSOR
   #:type-entry-tyvars                      ; ACCESSOR
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
   #:constructor-entry-arity                ; ACCESSOR
   #:constructor-entry-constructs           ; ACCESSOR
   #:constructor-entry-classname            ; ACCESSOR
   #:constructor-entry-compressed-repr      ; ACCESSOR
   #:constructor-entry-list                 ; TYPE
   #:constructor-environment                ; STRUCT
   #:type-alias-entry                       ; STRUCT
   #:make-type-alias-entry                  ; CONSTRUCTOR
   #:type-alias-entry-name                  ; ACCESSOR
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
   #:struct-entry                           ; STRUCT
   #:make-struct-entry                      ; CONSTRUCTOR
   #:struct-entry-name                      ; ACCESSOR
   #:struct-entry-fields                    ; ACCESSOR
   #:struct-entry-list                      ; TYPE
   #:struct-environment                     ; STRUCT
   #:get-field                              ; FUNCTION
   #:ty-class-method                        ; STRUCT
   #:make-ty-class-method                   ; CONSTRUCTOR
   #:ty-class-method-name                   ; ACCESSOR
   #:ty-class-method-type                   ; ACCESSOR
   #:ty-class                               ; STRUCT
   #:make-ty-class                          ; CONSTRUCTOR
   #:ty-class-name                          ; ACCESSOR
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
   #:ty-class-instance-method-codegen-syms  ; ACCESSOR
   #:ty-class-instance-method-codegen-inline-p ; ACCESSOR
   #:ty-class-instance-constraints-expanded ; FUNCTION
   #:ty-class-instance-list                 ; TYPE
   #:instance-environment                   ; STRUCT
   #:instance-environment-instances         ; ACCESSOR
   #:function-env-entry                     ; STRUCT
   #:make-function-env-entry                ; CONSTRUCTOR
   #:function-env-entry-name                ; ACCESSOR
   #:function-env-entry-arity               ; ACCESSOR
   #:function-env-entry-inline-p            ; ACCESSOR
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
   #:update-instance-fundeps                ; FUNCTION
   #:solve-fundeps                          ; FUNCTION
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
  (make-value-environment :data (fset:image (lambda (key value)
                  (values key (apply-substitution subst-list value)))
                (immutable-map-data env))))

(defmethod type-variables ((env value-environment))
  (let ((out nil))
    (fset:do-map (name type (immutable-map-data env))
      (declare (ignore name))
      (setf out (append (type-variables type) out)))
    (remove-duplicates out :test #'equalp)))

;;;
;;; Type environments
;;;

(deftype explicit-repr ()
  '(or null
    (member :enum :lisp :transparent)
    (cons (eql :native) (cons t null))))

(defstruct type-entry
  (name          (util:required 'name)          :type symbol                    :read-only t)
  (runtime-type  (util:required 'runtime-type)  :type t                         :read-only t)
  (type          (util:required 'type)          :type ty                        :read-only t)
  (tyvars        (util:required 'tyvars)        :type tyvar-list                :read-only t)
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
   :data (fset:map
          ;; Early Types
          ('coalton:Boolean
           (make-type-entry
            :name 'coalton:Boolean
            :runtime-type 'cl:boolean
            :type *boolean-type*
            :tyvars nil
            :constructors '(coalton:True coalton:False)
            :explicit-repr '(:native cl:boolean)
            :enum-repr t
            :newtype nil
            :docstring "Either true or false, internally represented by `cl:t` and `cl:nil` respectively."))

          ('coalton:Unit
           (make-type-entry
            :name 'coalton:Unit
            :runtime-type 'coalton-impl/constants:lisp-type-of-unit
            :type *unit-type*
            :tyvars nil
            :constructors '(coalton:Unit)
            :explicit-repr :enum
            :enum-repr t
            :newtype nil
            :docstring "The \"unit\" type whose only member is the value `Unit`."))

          ('coalton:Char
           (make-type-entry
            :name 'coalton:Char
            :runtime-type 'cl:character
            :type *char-type*
            :tyvars nil
            :constructors nil
            :explicit-repr '(:native cl:character)
            :enum-repr nil
            :newtype nil
            :docstring "A character represented by a Common Lisp `cl:character`."))

          ('coalton:Integer
           (make-type-entry
            :name 'coalton:Integer
            :runtime-type 'cl:integer
            :type *integer-type*
            :tyvars nil
            :constructors nil
            :explicit-repr '(:native cl:integer)
            :enum-repr nil
            :newtype nil
            :docstring "Integer of unbounded size. Represented by a Common Lisp `cl:integer`."))

          ('coalton:F32
           (make-type-entry
            :name 'coalton:F32
            :runtime-type 'cl:single-float
            :type *single-float-type*
            :tyvars nil
            :constructors nil
            :explicit-repr '(:native cl:single-float)
            :enum-repr nil
            :newtype nil
            :docstring "Single-precision floating point number (32 bits in size). Represented by a Common Lisp `cl:single-float`."))

          ('coalton:F64
           (make-type-entry
            :name 'coalton:F64
            :runtime-type 'cl:double-float
            :type *double-float-type*
            :tyvars nil
            :constructors nil
            :explicit-repr '(:native cl:double-float)
            :enum-repr nil
            :newtype nil
            :docstring "Double-precision floating point number (64 bits in size). Represented by a Common Lisp `cl:double-float`."))

          ('coalton:String
           (make-type-entry
            :name 'coalton:String
            :runtime-type 'cl:string
            :type *string-type*
            :tyvars nil
            :constructors nil
            :explicit-repr '(:native cl:string)
            :enum-repr nil
            :newtype nil
            :docstring "String of characters. Represented by Common Lisp `cl:string`."))

          ('coalton:Fraction
           (make-type-entry
            :name 'coalton:Fraction
            :runtime-type 'cl:rational
            :type *fraction-type*
            :tyvars nil
            :constructors nil
            :explicit-repr '(:native cl:rational)
            :enum-repr nil
            :newtype nil
            :docstring "A ratio of integers always in reduced form. Represented by a Common Lisp `cl:rational`."))

          ('coalton:Arrow
           (make-type-entry
            :name 'coalton:Arrow
            :runtime-type nil
            :type *arrow-type*
            :tyvars nil
            :constructors nil
            :explicit-repr nil
            :enum-repr nil
            :newtype nil
            :docstring "A named constructor for function types. `Arrow :a :b` is equivalent to `:a -> :b`."))

          ('coalton:List
           (make-type-entry
            :name 'coalton:List
            :runtime-type 'cl:list
            :type *list-type*
            :tyvars (list (make-variable))
            :constructors '(coalton:Cons coalton:Nil)
            :explicit-repr '(:native cl:list)
            :enum-repr nil
            :newtype nil
            :docstring "Homogeneous list of objects. Represented as a typical Common Lisp chain of `cl:cons` (or `cl:nil`)."))

          ('coalton:Optional
           (make-type-entry
            :name 'coalton:Optional
            :runtime-type 'cl:t
            :type *optional-type*
            :tyvars (list (make-variable))
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

(defun constructor-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'constructor-entry-p x)))

(deftype constructor-entry-list ()
  '(satisfies constructor-entry-list-p))

(defstruct (constructor-environment (:include immutable-map)))

(defun make-default-constructor-environment ()
  "Create a TYPE-ENVIRONMENT containing early constructors"
  (make-constructor-environment
   :data (fset:map
          ;; Early Constructors
          ('coalton:True
           (make-constructor-entry
            :name 'coalton:True
            :arity 0
            :constructs 'coalton:Boolean
            :classname 'coalton::Boolean/True
            :docstring "Boolean `True`"
            :compressed-repr 't))

          ('coalton:False
           (make-constructor-entry
            :name 'coalton:False
            :arity 0
            :constructs 'coalton:Boolean
            :classname 'coalton::Boolean/False
            :docstring "Boolean `False`"
            :compressed-repr 'nil))

          ('coalton:Unit
           (make-constructor-entry
            :name 'coalton:Unit
            :arity 0
            :constructs 'coalton:Unit
            :classname 'coalton::Unit/Unit
            :docstring "`Unit` represents nullary parameters and return types."
            :compressed-repr coalton-impl/constants:+value-of-unit+))

          ('coalton:Cons
           (make-constructor-entry
            :name 'coalton:Cons
            :arity 2
            :constructs 'coalton:List
            :classname nil
            :docstring "`Cons` represents a `List` containing a first element (`car`) and a nested `Cons` (`cdr`)."
            :compressed-repr 'nil))

          ('coalton:Nil
           (make-constructor-entry
            :name 'coalton:Nil
            :arity 0
            :constructs 'coalton:List
            :classname nil
            :docstring "`Nil` represents an empty `List`."
            :compressed-repr 'nil))

          ('coalton:Some
           (make-constructor-entry
            :name 'coalton:Some
            :arity 1
            :constructs 'coalton:Optional
            :classname nil
            :docstring "`Some` expresses the presence of a meaningful value."
            :compressed-repr 'nil))

          ('coalton:None
           (make-constructor-entry
            :name 'coalton:None
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
  (tyvars    (util:required 'tyvars)    :type tyvar-list       :read-only t)
  (type      (util:required 'type)      :type ty               :read-only t)
  (docstring (util:required 'docstring) :type (or null string) :read-only t))

(defmethod source:docstring ((self type-alias-entry))
  (type-alias-entry-docstring self))

(defmethod make-load-form ((self type-alias-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type type-alias-entry))

(defun type-alias-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'type-alias-entry-p x)))

(deftype type-alias-entry-list ()
  '(satisfies type-alias-entry-list-p))

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

(defun struct-field-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'struct-field-p x)))

(deftype struct-field-list ()
  '(satisfies struct-field-list-p))

(defstruct struct-entry
  (name      (util:required 'name)      :type symbol            :read-only t)
  (fields    (util:required 'fields)    :type struct-field-list :read-only t)
  (docstring (util:required 'docstring) :type (or null string)  :read-only t))

(defmethod source:docstring ((self struct-entry))
  (struct-entry-docstring self))

(defmethod make-load-form ((self struct-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun struct-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'struct-entry-p x)))

(deftype struct-entry-list ()
  '(satisfies struct-entry-list-p))

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
  (docstring (util:required 'docstring) :type (or null string) :read-only t))

(defmethod source:docstring ((self ty-class-method))
  (ty-class-method-docstring self))

(defmethod make-load-form ((self ty-class-method) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun ty-class-method-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-class-method-p x)))

(deftype ty-class-method-list ()
  '(satisfies ty-class-method-list-p))

(defstruct ty-class
  (name                (util:required 'name)                :type symbol              :read-only t)
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

(defun ty-class-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-class-p x)))

(deftype ty-class-list ()
  '(satisfies ty-class-list-p))

(defmethod apply-substitution (subst-list (class ty-class))
  (declare (type substitution-list subst-list)
           (values ty-class &optional))
  (make-ty-class
   :name (ty-class-name class)
   :predicate (apply-substitution subst-list (ty-class-predicate class))
   :superclasses (apply-substitution subst-list (ty-class-superclasses class))
   :class-variables (ty-class-class-variables class)
   :fundeps (ty-class-fundeps class)
   :unqualified-methods (mapcar (lambda (method)
                                  (make-ty-class-method :name (ty-class-method-name method)
                                                        :type (apply-substitution subst-list (ty-class-method-type method))
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
  (constraints             (util:required 'constraints)             :type ty-predicate-list :read-only t)
  (predicate               (util:required 'predicate)               :type ty-predicate      :read-only t)
  (codegen-sym             (util:required 'codegen-sym)             :type symbol            :read-only t)
  (method-codegen-syms     (util:required 'method-codegen-syms)     :type util:symbol-list  :read-only t)
  (method-codegen-inline-p (util:required 'method-codegen-inline-p) :type list              :read-only t)
  (docstring               (util:required 'docstring)               :type (or null string)  :read-only t))

(defun expand-context (context env)
  "Traverse constraint predicates by looking up those entailed by
the base constraint by instances in the environment.  Eliminate
recursion by comparing these to the base constraint and return a list
of constraint predicates."
  (declare (type ty-predicate-list context)
           (type environment env)
           (values ty-predicate-list &optional))

  ;; This was implemented as a hack to make `derive' work on recursive
  ;; types.  Allows you to write an instance with signature
  ;; `(Eq A => Eq A)'.
  (flet ((expand-constraint (base-constraint)
           (labels ((f (constraint stack)
                      (multiple-value-bind (inst subs)
                          (lookup-class-instance env constraint :no-error t)
                        (if (null inst)
                            (list constraint)
                            (mapcan
                             (lambda (pred)
                               (let ((pred (apply-substitution subs pred)))
                                 (f pred (cons pred stack))))
                             (set-difference
                              (ty-class-instance-constraints inst)
                              stack
                              :test #'type-predicate=))))))
             (f base-constraint (list base-constraint)))))

    (remove-duplicates
     (alexandria:mappend #'expand-constraint context)
     :test #'type-predicate=)))

(defun ty-class-instance-constraints-expanded (inst env)
  (expand-context (ty-class-instance-constraints inst) env))

(defmethod source:docstring ((self ty-class-instance))
  (ty-class-instance-docstring self))

(defmethod make-load-form ((self ty-class-instance) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type ty-class-instance))

(defun ty-class-instance-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-class-instance-p x)))

(deftype ty-class-instance-list ()
  `(satisfies ty-class-instance-list-p))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type ty-class-instance-list))

(defmethod apply-substitution (subst-list (instance ty-class-instance))
  (declare (type substitution-list subst-list)
           (values ty-class-instance &optional))
  (make-ty-class-instance
   :constraints (apply-substitution subst-list (ty-class-instance-constraints instance))
   :predicate (apply-substitution subst-list (ty-class-instance-predicate instance))
   :codegen-sym (ty-class-instance-codegen-sym instance)
   :method-codegen-syms (ty-class-instance-method-codegen-syms instance)
   :method-codegen-inline-p (ty-class-instance-method-codegen-inline-p instance)
   :docstring (ty-class-instance-docstring instance)))

(defstruct instance-environment
  (instances    (make-immutable-listmap) :type immutable-listmap :read-only t)
  (codegen-syms (make-immutable-map)     :type immutable-map     :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type instance-environment))

;;;
;;; Function environment
;;;

(defstruct function-env-entry
  (name     (util:required 'name)     :type symbol  :read-only t)
  (arity    (util:required 'arity)    :type fixnum  :read-only t)
  (inline-p (util:required 'inline-p) :type boolean :read-only t))

(defmethod make-load-form ((self function-env-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type function-env-entry))

(defun function-env-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'function-env-entry-p x)))

(deftype function-env-entry-list ()
  `(satisfies function-env-entry-list-p))

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

(defun specialization-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'specialization-entry-p x)))

(deftype specialization-entry-list ()
  '(satisfies specialization-entry-list-p))

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

(defstruct environment
  (value-environment          (util:required 'value-environment)          :type value-environment          :read-only t)
  (type-environment           (util:required 'type-environment)           :type type-environment           :read-only t)
  (constructor-environment    (util:required 'constructor-environment)    :type constructor-environment    :read-only t)
  (type-alias-environment     (util:required 'type-alias-environment)     :type type-alias-environment     :read-only t)
  (struct-environment         (util:required 'struct-environment)         :type struct-environment         :read-only t)
  (class-environment          (util:required 'class-environment)          :type class-environment          :read-only t)
  (fundep-environment         (util:required 'fundep-environment)         :type fundep-environment         :read-only t)
  (instance-environment       (util:required 'instance-environment)       :type instance-environment       :read-only t)
  (function-environment       (util:required 'function-environment)       :type function-environment       :read-only t)
  (name-environment           (util:required 'name-environment)           :type name-environment           :read-only t)
  (method-inline-environment  (util:required 'method-inline-environment)  :type method-inline-environment  :read-only t)
  (code-environment           (util:required 'code-environment)           :type code-environment           :read-only t)
  (specialization-environment (util:required 'specialization-environment) :type specialization-environment :read-only t)
  (source-name-environment    (util:required 'source-name-environment)    :type source-name-environment    :read-only t))

(defmethod print-object ((env environment) stream)
  (declare (type stream stream)
           (type environment env))
  (print-unreadable-object (env stream :type t :identity t)))


(defmethod make-load-form ((self environment) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type environment))

(defun make-default-environment ()
  (declare (values environment))
  (make-environment
   :value-environment (make-value-environment)
   :type-environment (make-default-type-environment)
   :type-alias-environment (make-type-alias-environment)
   :struct-environment (make-struct-environment)
   :constructor-environment (make-default-constructor-environment)
   :class-environment (make-class-environment)
   :fundep-environment (make-fundep-environment)
   :instance-environment (make-instance-environment)
   :function-environment (make-function-environment)
   :name-environment (make-name-environment)
   :method-inline-environment (make-method-inline-environment)
   :code-environment (make-code-environment)
   :specialization-environment (make-specialization-environment)
   :source-name-environment (make-source-name-environment)))

(defun update-environment (env
                           &key
                             (value-environment (environment-value-environment env))
                             (type-environment (environment-type-environment env))
                             (type-alias-environment (environment-type-alias-environment env))
                             (constructor-environment (environment-constructor-environment env))
                             (struct-environment (environment-struct-environment env))
                             (class-environment (environment-class-environment env))
                             (fundep-environment (environment-fundep-environment env))
                             (instance-environment (environment-instance-environment env))
                             (function-environment (environment-function-environment env))
                             (name-environment (environment-name-environment env))
                             (method-inline-environment (environment-method-inline-environment env))
                             (code-environment (environment-code-environment env))
                             (specialization-environment (environment-specialization-environment env))
                             (source-name-environment (environment-source-name-environment env)))
  (declare (type environment env)
           (type value-environment value-environment)
           (type type-environment type-environment)
           (type constructor-environment constructor-environment)
           (type type-alias-environment type-alias-environment)
           (type struct-environment struct-environment)
           (type class-environment class-environment)
           (type fundep-environment fundep-environment)
           (type instance-environment instance-environment)
           (type function-environment function-environment)
           (type name-environment name-environment)
           (type method-inline-environment method-inline-environment)
           (type code-environment code-environment)
           (type specialization-environment specialization-environment)
           (type source-name-environment source-name-environment)
           (values environment))
  (make-environment
   :value-environment value-environment
   :type-environment type-environment
   :constructor-environment constructor-environment
   :type-alias-environment type-alias-environment
   :struct-environment struct-environment
   :class-environment class-environment
   :fundep-environment fundep-environment
   :instance-environment instance-environment
   :function-environment function-environment
   :name-environment name-environment
   :method-inline-environment method-inline-environment
   :code-environment code-environment
   :specialization-environment specialization-environment
   :source-name-environment source-name-environment))

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
           (values fset:seq &optional))
  (immutable-listmap-lookup (instance-environment-instances (environment-instance-environment env)) class :no-error no-error))

(defun lookup-class-instance (env pred &key no-error)
  (declare (type environment env))
  (let* ((pred-class (ty-predicate-class pred))
         (instances (lookup-class-instances env pred-class :no-error no-error)))
    (fset:do-seq (instance instances)
      (handler-case
          (let ((subs (predicate-match (ty-class-instance-predicate instance) pred)))
            (return-from lookup-class-instance (values instance subs)))
        (predicate-unification-error () nil)))
    (unless no-error
      (error "Unknown instance for predicate ~S" pred))))

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

(define-env-updater add-instance (env class value)
  (declare (type environment env)
           (type symbol class)
           (type ty-class-instance value))
  ;; Ensure the class is defined
  (unless (lookup-class env class)
    (error "Class ~S does not exist." class))

  (fset:do-seq (inst (lookup-class-instances env class :no-error t) :index index)
    (when (handler-case (or (predicate-mgu (ty-class-instance-predicate value)
                                           (ty-class-instance-predicate inst))
                            t)
            (predicate-unification-error () nil))

      ;; If we have the same instance then simply overwrite the old one
      (handler-case
          (progn
            (predicate-match (ty-class-instance-predicate value) (ty-class-instance-predicate inst))
            (predicate-match (ty-class-instance-predicate inst) (ty-class-instance-predicate value))

            (return-from add-instance
              (update-environment
               env
               :instance-environment (make-instance-environment
                                      :instances (immutable-listmap-replace
                                                  (instance-environment-instances (environment-instance-environment env))
                                                  class
                                                  index
                                                  value)
                                      :codegen-syms (immutable-map-set
                                                     (instance-environment-codegen-syms (environment-instance-environment env))
                                                     (ty-class-instance-codegen-sym value)
                                                     value))))
            )
        (predicate-unification-error ()
          (error 'overlapping-instance-error
                 :inst1 (ty-class-instance-predicate value)
                 :inst2 (ty-class-instance-predicate inst))))))

  (update-environment
   env
   :instance-environment (make-instance-environment
                          :instances (immutable-listmap-push
                                      (instance-environment-instances (environment-instance-environment env))
                                      class
                                      value)
                          :codegen-syms (immutable-map-set
                                         (instance-environment-codegen-syms (environment-instance-environment env))
                                         (ty-class-instance-codegen-sym value)
                                         value))))

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

    (fset:do-seq (elem (immutable-listmap-lookup (environment-specialization-environment env) from :no-error t) :index index)
      (let* ((type (specialization-entry-to-ty elem))
             (scheme (quantify (type-variables type)
                               (qualify nil type))))

        (when (equalp to-scheme scheme)
          (return-from add-specialization
            (update-environment env
                                :specialization-environment
                                (immutable-listmap-replace
                                 (environment-specialization-environment env)
                                 from
                                 index
                                 entry
                                 #'make-specialization-environment)))))

      (handler-case
          (progn
            (unify nil to-ty (specialization-entry-to-ty elem))

            (error 'overlapping-specialization-error
                   :new to
                   :existing (specialization-entry-to elem)))
        (unification-error ())))

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
  (fset:do-seq (elem (immutable-listmap-lookup (environment-specialization-environment env) from :no-error no-error))
    (when (eq to (specialization-entry-to elem))
      (return-from lookup-specialization elem)))

  (unless no-error
    (error "Unable to find specialization from ~A to ~A" from to)))

(defun lookup-specialization-by-type (env from ty &key (no-error nil))
  (declare (type environment env)
           (type symbol from)
           (type ty ty)
           (values (or null specialization-entry) &optional))
  (fset:do-seq (elem (immutable-listmap-lookup (environment-specialization-environment env) from :no-error no-error))
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

(defun collect-fundeps (env preds)
  "Collect the functional dependencies associated with PREDS as CONSes of
LISTs of TYs, recursively expanding to the superclass predicates.

For example, with the definitions

  (define-class (C :a :b (:a -> :b)))
  (define-class (C :a :b => D :a :b))

and the single predicate 

  D (List #T1) #T2

This function will return the single functional dependency

  (List #T1 . #T2)"
  (declare (type environment env)
           (type ty-predicate-list preds))
  (loop :for pred :in preds
        :for pred-tys := (ty-predicate-types pred)
        :for class := (lookup-class env (ty-predicate-class pred))
        :for class-vars := (ty-class-class-variables class)
        :for subs := (predicate-match (ty-class-predicate class) pred)
        :for supers := (mapcar (alexandria:curry #'apply-substitution subs)
                               (ty-class-superclasses class))
        :nconc (collect-fundeps env supers)
        :nconc (loop :for fundep :in (ty-class-fundeps class)
                     :collect (cons (project-elements
                                     (fundep-from fundep)
                                     class-vars
                                     pred-tys)
                                    (project-elements
                                     (fundep-to fundep)
                                     class-vars
                                     pred-tys)))))

(defun collect-fundep-vars (env preds)
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
           (type ty-predicate-list preds))
  (loop :for (from . to) :in (collect-fundeps env preds)
        :collect (cons (type-variables from) (type-variables to))))

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

  ;; Ensure dependent types do not contain type variables that
  ;; are not present in the corresponding determinant types,
  ;; unless they are otherwise determined by the context.
  ;; See "Type Class with Functional Dependencies" §6.1 (Jones)
  (loop :with superfundeps := (collect-fundep-vars env context)
        :for (from-vars . to-vars) :in (collect-fundep-vars env (list pred))
        :for known-to-vars
          := (generic-closure from-vars superfundeps :test #'ty=)
        :for unknown-to-vars
          := (set-difference to-vars known-to-vars :test #'ty=)
        :unless (subsetp unknown-to-vars from-vars :test #'ty=)
          :do (error 'fundep-ambiguity))

  (let* ((class (lookup-class env (ty-predicate-class pred)))
         (fundep-env
           (lookup-fundep-environment env (ty-predicate-class pred)))
         (class-variables (ty-class-class-variables class))
         (pred-tys (ty-predicate-types pred)))

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
          :do (block update-block
                ;; Try to find a matching relation for the current fundep
                (fset:do-seq (s state)
                  ;; If the left side matches checking either direction
                  (when (or (handler-case
                                (progn
                                  (match-list (fundep-entry-from s) from-tys)
                                  t)
                              (unification-error ()
                                nil))
                            (handler-case
                                (progn
                                  (match-list from-tys (fundep-entry-from s))
                                  t)
                              (unification-error ()
                                nil)))
                    (handler-case
                        (progn
                          (match-list (fundep-entry-to s) to-tys)
                          ;; Exit upon finding a match
                          (return-from update-block))

                      ;; If the right side does not match
                      ;; signal an error
                      (unification-error ()
                        (error-fundep-conflict
                         env
                         class
                         pred
                         fundep
                         (fundep-entry-from s)
                         from-tys
                         (fundep-entry-to s)
                         to-tys)))))

                ;; Insert a new relation if there wasn't a match
                (setf env
                      (insert-fundep-entry%
                       env
                       (ty-class-name class)
                       i
                       (make-fundep-entry
                        :from from-tys
                        :to to-tys))))))

  env)

(defun error-fundep-conflict (env class pred fundep old-from-tys new-from-tys old-to-tys new-to-tys)
  "Finds a conflicting instance and signals an error"
  (declare (type environment env)
           (type ty-class class)
           (type ty-predicate pred)
           (type fundep fundep)
           (type ty-list old-from-tys new-from-tys old-to-tys new-to-tys))
  (let* ((class-name (ty-class-name class))

         (from-tys_ (copy-list new-from-tys))

         (vars
           (loop :for v :in (ty-class-class-variables class)
                 :if (find v (fundep-from fundep))
                   :collect (prog1
                              (car from-tys_)
                              (setf from-tys_ (cdr from-tys_)))
                 :else
                   :collect (make-variable)))

         (new-pred (make-ty-predicate :class class-name :types vars :location (source:location pred))))

    (fset:do-seq (inst (lookup-class-instances env class-name))
      (handler-case
          (progn
            (predicate-mgu new-pred (ty-class-instance-predicate inst))
            (error 'fundep-conflict
                   :new-pred pred
                   :old-pred (ty-class-instance-predicate inst)
                   :fundep fundep
                   :class class-name
                   :class-vars (ty-class-class-variables class)
                   :class-fundeps (ty-class-fundeps class)
                   :old-from-tys old-from-tys
                   :new-from-tys new-from-tys
                   :old-to-tys old-to-tys
                   :new-to-tys new-to-tys))
        (predicate-unification-error () nil)))

    ;; If there was a fundep conflict, one of the instances should have matched
    (util:unreachable)))

(defun solve-fundeps (env preds subs)
  "First, this function creates and applies substitutions to preds based
on functional dependencies that constrain them with respect to one
another, and then this function creates and applies substitutions to
preds based on functional dependencies that constrain them with
respect to defined type class instances. The values returned are the
predicates with all substitutions applied and the new substitutions."
  (declare (type environment env)
           (type ty-predicate-list preds)
           (type substitution-list subs)
           (values ty-predicate-list substitution-list &optional))

  ;; We memoize this function to support the type-checking of
  ;; constructors of collections, e.g., MAKE-LIST.
  (let ((fundepsp-cache (make-hash-table :test #'eq)))
    (labels ((fundepsp (preds)
               "Are any of PREDS constrained by functional dependencies?"
               (when (endp preds)
                 (return-from fundepsp nil))
               (let* ((pred (first preds))
                      (class-name (ty-predicate-class pred)))
                 (multiple-value-bind (fundepsp foundp)
                     (gethash class-name fundepsp-cache)
                   (cond
                     (foundp
                      (or fundepsp (fundepsp (rest preds))))
                     (t
                      (let ((class (lookup-class env class-name)))
                        (or (setf (gethash class-name fundepsp-cache)
                                  (or (consp (ty-class-fundeps class))
                                      (fundepsp (ty-class-superclasses class))))
                            (fundepsp (rest preds))))))))))

      ;; If no predicates have fundeps, then exit early
      (unless (fundepsp preds)
        (return-from solve-fundeps (values preds subs)))))

  ;; Expand PREDS into the superclasses
  (setf preds
        (loop :for remaining-preds := (copy-list preds)
                :then (rest remaining-preds)
              :until (endp remaining-preds)
              :for pred := (first remaining-preds)
              :for class := (lookup-class env (ty-predicate-class pred))
              :for _subs := (predicate-match (ty-class-predicate class) pred subs)
              :do (alexandria:nconcf
                   remaining-preds
                   (mapcar (alexandria:curry #'apply-substitution _subs)
                           (ty-class-superclasses class)))
              :collect pred))

  ;; The purpose of this block is to create a substitution list that
  ;; unifies the expanded predicates from the previous block based
  ;; on the functional dependencies that constrain them.
  (loop :for remaining-preds := preds :then (rest remaining-preds)
        :until (endp remaining-preds)
        :for pred := (first remaining-preds)
        :for class-name := (ty-predicate-class pred)
        :for class := (lookup-class env class-name)
        :for fundeps := (ty-class-fundeps class)
        :for other-pred := (find class-name (rest preds)
                                 :key #'ty-predicate-class
                                 :test #'eq)
        :when (and (consp fundeps) (not (null other-pred)))
          :do (handler-case
                  (let ((class-vars (ty-class-class-variables class))
                        (pred-tys (ty-predicate-types pred))
                        (other-pred-tys (ty-predicate-types other-pred)))
                    (dolist (fundep fundeps)
                      (let* ((from (fundep-from fundep))
                             (to (fundep-to fundep))
                             (pred-from
                               (project-elements from
                                                 class-vars
                                                 pred-tys))
                             (other-pred-from
                               (project-elements from
                                                 class-vars
                                                 other-pred-tys)))
                        (when (every #'ty= pred-from other-pred-from)
                          (let ((pred-to
                                  (project-elements to
                                                    class-vars
                                                    pred-tys))
                                (other-pred-to
                                  (project-elements to
                                                    class-vars
                                                    other-pred-tys)))
                            (setf subs (unify-list subs
                                                   pred-to
                                                   other-pred-to)))))))
                (unification-error ()
                  (error 'context-fundep-conflict
                         :first-pred pred
                         :second-pred other-pred)))
        :finally (setf preds (apply-substitution subs preds)))

  ;; This block is meant to simplify PREDS if instances exist in the
  ;; environment which constrain them by functional dependencies.
  (loop :with new-subs := nil
        :with preds-generated := nil
        :for i :below +fundep-max-depth+
        :do
           (setf new-subs subs)

           (loop :for pred :in preds
                 :for class-name := (ty-predicate-class pred)
                 :for class := (lookup-class env class-name)
                 ;; If there are super-predicates then add those
                 ;; predicates into the list of preds and remove
                 ;; this predicate from the list since it will give
                 ;; us no new type information. Additionally,
                 ;; restart the current check to avoid terminating
                 ;; early when no subs are generated.
                 :for instance := (lookup-class-instance env pred :no-error t)

                 :when instance
                   ;; Since we allow for type variables in the
                   ;; constraints which do not appear in the
                   ;; predicate, we need to create a full fresh set of
                   ;; predicates, rather than just matching the head.
                   :do (let* ((fresh-instance-preds
                                (fresh-preds
                                 (cons (ty-class-instance-predicate instance)
                                       (ty-class-instance-constraints-expanded instance env))))
                              (instance-head (car fresh-instance-preds))
                              (instance-context (cdr fresh-instance-preds))

                              (instance-subs (predicate-match instance-head pred new-subs)))
                         (loop :for new-pred :in instance-context :do
                           (push (make-ty-predicate
                                  :class (ty-predicate-class new-pred)
                                  :types (apply-substitution instance-subs (ty-predicate-types new-pred)))
                                 preds))

                         (setf preds (remove pred preds :test #'eq))
                         (setf preds-generated t)
                         (return))

                 :when (ty-class-fundeps class)
                   :do (setf new-subs (generate-fundep-subs% env (apply-substitution new-subs pred) new-subs)))
           (if (and (not preds-generated)
                    (or (equalp new-subs subs)
                        (null preds)))
               (return-from solve-fundeps (values preds subs))
               (setf subs new-subs))
           (setf preds-generated nil)
           (setf preds (apply-substitution subs preds))
        :finally (util:coalton-bug "Fundep solving failed to fixpoint")))


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
           (type fset:seq state)
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

    (fset:do-seq (entry state)
      (handler-case
          (let* ((fresh-entry (fresh-fundep-entry entry))
                 (left-subs (match-list from-tys (fundep-entry-from fresh-entry)))
                 (right-side (apply-substitution left-subs (fundep-entry-to fresh-entry))))
            (return-from generate-fundep-subs-for-pred% (unify-list subs to-tys right-side)))
        (unification-error () nil))))

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
