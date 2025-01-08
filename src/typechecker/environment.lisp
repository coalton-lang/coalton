(defpackage #:coalton-impl/typechecker/environment
  (:use
   #:cl
   #:coalton-impl/algorithm
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/map
   #:coalton-impl/typechecker/type-errors
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/predicate
   #:coalton-impl/typechecker/scheme
   #:coalton-impl/typechecker/unify)
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
   #:+fundep-max-depth+)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:source #:coalton-impl/source)
   (#:parser #:coalton-impl/parser)
   (#:map #:coalton-impl/algorithm/hamt))
  (:export
   #:env-apply-substitution
   #:*update-hook*                          ; VARIABLE
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
   #:type-environment                       ; STRUCT
   #:constructor-entry                      ; STRUCT
   #:make-constructor-entry                 ; ACCESSOR
   #:constructor-entry-name                 ; ACCESSOR
   #:constructor-entry-arity                ; ACCESSOR
   #:constructor-entry-constructs           ; ACCESSOR
   #:constructor-entry-classname            ; ACCESSOR
   #:constructor-entry-compressed-repr      ; ACCESSOR
   #:constructor-entry-list                 ; TYPE
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
   #:ty-class-class-variable-map            ; ACCESSOR
   #:ty-class-fundeps                       ; ACCESSOR
   #:ty-class-unqualified-methods           ; ACCESSOR
   #:ty-class-codegen-sym                   ; ACCESSOR
   #:ty-class-superclass-dict               ; ACCESSOR
   #:ty-class-superclass-map                ; ACCESSOR
   #:ty-class-list                          ; TYPE
   #:ty-class-instance                      ; STRUCT
   #:make-ty-class-instance                 ; CONSTRUCTOR
   #:ty-class-instance-constraints          ; ACCESSOR
   #:ty-class-instance-predicate            ; ACCESSOR
   #:ty-class-instance-codegen-sym          ; ACCESSOR
   #:ty-class-instance-method-codegen-syms  ; ACCESSOR
   #:ty-class-instance-list                 ; TYPE
   #:instance-environment                   ; STRUCT
   #:function-env-entry                     ; STRUCT
   #:make-function-env-entry                ; CONSTRUCTOR
   #:function-env-entry-name                ; ACCESSOR
   #:function-env-entry-arity               ; ACCESSOR
   #:name-entry                             ; STRUCT
   #:make-name-entry                        ; CONSTRUCTOR
   #:name-entry-name                        ; ACCESSOR
   #:name-entry-type                        ; ACCESSOR
   #:name-environment                       ; STRUCT
   #:specialization-entry                   ; STRUCT
   #:make-specialization-entry              ; CONSTRUCTOR
   #:specialization-entry-from              ; ACCESSOR
   #:specialization-entry-to                ; ACCESSOR
   #:specialization-entry-to-ty             ; ACCESSOR
   #:specialization-entry-list              ; TYPE
   #:environment                            ; STRUCT
   #:make-environment                       ; FUNCTION
   #:environment-value-environment          ; ACCESSOR
   #:environment-type-environment           ; ACCESSOR
   #:environment-class-environment          ; ACCESSOR
   #:environment-fundep-environment         ; ACCESSOR
   #:environment-instance-environment       ; ACCESSOR
   #:environment-function-environment       ; ACCESSOR
   #:environment-specialization-environment ; ACCESSOR
   #:environment-name-environment           ; ACCESSOR
   #:lookup-value-type                      ; FUNCTION
   #:set-value-type                         ; FUNCTION
   #:unset-value-type                       ; FUNCTION
   #:lookup-type                            ; FUNCTION
   #:set-type                               ; FUNCTION
   #:lookup-constructor                     ; FUNCTION
   #:set-constructor                        ; FUNCTION
   #:unset-constructor                      ; FUNCTION
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
   #:update-instance-fundeps                ; FUNCTION
   #:solve-fundeps                          ; FUNCTION
   #:environment-map                        ; STRUCT
   #:make-map                               ; FUNCTION
   #:get-value                              ; FUNCTION
   #:get-table                              ; FUNCTION
   ))

;;; Coalton environment management
;;;
;;; An environment contains all information about Coalton types,
;;; functions, instances, specializations, and so on.
;;;
;;; Environments are immutable: all update functions defined below
;;; return a new environment.
;;;
;;; The global environment root is:
;;;
;;;   coalton-impl/entry:*global-environment*
;;;
;;; An update hook mechanism is provided so that the compiler can
;;; record updates that occur while compiling Coalton source. When
;;; compilation is finished, an update log is prepended to generated
;;; lisp code, to allow replaying the environment updates when the
;;; compiled fasl is loaded.

(in-package #:coalton-impl/typechecker/environment)

;; *update-hook* may be bound to a function that is called whenever
;; an environment is updated.
;;
;; The bound function must accept 2 arguments:
;;
;; - the symbol naming the environment update function
;; - that function's arg list
;;
;; The environment itself, which is always the first argument to an
;; update function, is not provided in the arg list: just the update
;; values.

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

(defun value-apply-substitution (m subst-list)
  (map:map m (lambda (k v)
               (declare (ignore k))
               (apply-substitution subst-list v))))

(defun value-type-variables (m)
  (remove-duplicates (loop :for type :in (map:vals m)
                           :append (type-variables type))
                     :test #'equalp))

;;;
;;; Type environments
;;;

(deftype explicit-repr ()
  '(or null
    (member :enum :lisp :transparent)
    (cons (eql :native) (cons t null))))

(defstruct type-entry
  (name         (util:required 'name)         :type symbol           :read-only t)
  (runtime-type (util:required 'runtime-type) :type t                :read-only t)
  (type         (util:required 'type)         :type ty               :read-only t)
  (tyvars       (util:required 'tyvars)       :type tyvar-list       :read-only t)
  (constructors (util:required 'constructors) :type util:symbol-list :read-only t)

  ;; An explicit repr defined in the source, or nil if none was supplied. Computed repr will be reflected in
  ;; ENUM-REPR, NEWTYPE, and/or RUNTIME-TYPE.
  (explicit-repr (util:required 'explicit-repr) :type explicit-repr  :read-only t)

  ;; If this is true then the type is compiled to a more effecient
  ;; enum representation at runtime
  (enum-repr (util:required 'enum-repr)       :type boolean :read-only t)

  ;; If this is true then the type does not exist at runtime
  ;; See https://wiki.haskell.org/Newtype
  ;;
  ;; A type cannot be both enum repr and a newtype
  ;;
  ;; A type that is a newtype has another Coalton type as its
  ;; runtime-type instead of a lisp type. This is to avoid issues with
  ;; recursive newtypes.
  (newtype (util:required 'newtype)     :type boolean :read-only t)

  (docstring (util:required 'docstring) :type (or null string)   :read-only t)
  (location  nil                        :type (or null source:location) :read-only t))

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

(defun make-default-type-environment ()
  "Create a TYPE-ENVIRONMENT containing early types"
  (map:make-map 'coalton:Boolean
                (make-type-entry
                 :name 'coalton:Boolean
                 :runtime-type 'cl:boolean
                 :type *boolean-type*
                 :tyvars nil
                 :constructors '(coalton:True coalton:False)
                 :explicit-repr '(:native cl:boolean)
                 :enum-repr t
                 :newtype nil
                 :docstring "Either true or false represented by `t` and `nil` respectively.")

                'coalton:Unit
                (make-type-entry
                 :name 'coalton:Unit
                 :runtime-type '(member coalton::Unit/Unit)
                 :type *unit-type*
                 :tyvars nil
                 :constructors '(coalton:Unit)
                 :explicit-repr :enum
                 :enum-repr t
                 :newtype nil
                 :docstring "")

                'coalton:Char
                (make-type-entry
                 :name 'coalton:Char
                 :runtime-type 'cl:character
                 :type *char-type*
                 :tyvars nil
                 :constructors nil
                 :explicit-repr '(:native cl:character)
                 :enum-repr nil
                 :newtype nil
                 :docstring "A single character represented as a `character` type.")
                
                'coalton:Integer
                (make-type-entry
                 :name 'coalton:Integer
                 :runtime-type 'cl:integer
                 :type *integer-type*
                 :tyvars nil
                 :constructors nil
                 :explicit-repr '(:native cl:integer)
                 :enum-repr nil
                 :newtype nil
                 :docstring "Unbound integer. Uses `integer`.")

                'coalton:Single-Float
                (make-type-entry
                 :name 'coalton:Single-Float
                 :runtime-type 'cl:single-float
                 :type *single-float-type*
                 :tyvars nil
                 :constructors nil
                 :explicit-repr '(:native cl:single-float)
                 :enum-repr nil
                 :newtype nil
                 :docstring "Single precision floating point number. Uses `single-float`.")

                'coalton:Double-Float
                (make-type-entry
                 :name 'coalton:Double-Float
                 :runtime-type 'cl:double-float
                 :type *double-float-type*
                 :tyvars nil
                 :constructors nil
                 :explicit-repr '(:native cl:double-float)
                 :enum-repr nil
                 :newtype nil
                 :docstring "Double precision floating point number. Uses `double-float`.")

                'coalton:String
                (make-type-entry
                 :name 'coalton:String
                 :runtime-type 'cl:string
                 :type *string-type*
                 :tyvars nil
                 :constructors nil
                 :explicit-repr '(:native cl:string)
                 :enum-repr nil
                 :newtype nil
                 :docstring "String of characters represented by Common Lisp `string`.")

                'coalton:Fraction
                (make-type-entry
                 :name 'coalton:Fraction
                 :runtime-type 'cl:rational
                 :type *fraction-type*
                 :tyvars nil
                 :constructors nil
                 :explicit-repr '(:native cl:rational)
                 :enum-repr nil
                 :newtype nil
                 :docstring "A ratio of integers always in reduced form.")

                'coalton:Arrow
                (make-type-entry
                 :name 'coalton:Arrow
                 :runtime-type nil
                 :type *arrow-type*
                 :tyvars nil
                 :constructors nil
                 :explicit-repr nil
                 :enum-repr nil
                 :newtype nil
                 :docstring "Type constructor for function types.")

                'coalton:List
                (make-type-entry
                 :name 'coalton:List
                 :runtime-type 'cl:list
                 :type *list-type*
                 :tyvars (list (make-variable))
                 :constructors '(coalton:Cons coalton:Nil)
                 :explicit-repr '(:native cl:list)
                 :enum-repr nil
                 :newtype nil
                 :docstring "Homogeneous list of objects represented as a Common Lisp `list`.")))

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

(defun make-default-constructor-environment ()
  "Create a TYPE-ENVIRONMENT containing early constructors"
  (map:make-map 'coalton:True
                (make-constructor-entry
                 :name 'coalton:True
                 :arity 0
                 :constructs 'coalton:Boolean
                 :classname 'coalton::Boolean/True
                 :docstring "Boolean `True`"
                 :compressed-repr 't)

                'coalton:False
                (make-constructor-entry
                 :name 'coalton:False
                 :arity 0
                 :constructs 'coalton:Boolean
                 :classname 'coalton::Boolean/False
                 :docstring "Boolean `False`"
                 :compressed-repr 'nil)

                'coalton:Unit
                (make-constructor-entry
                 :name 'coalton:Unit
                 :arity 0
                 :constructs 'coalton:Unit
                 :classname 'coalton::Unit/Unit
                 :docstring "`Unit` represents nullary parameters and return types."
                 :compressed-repr 'coalton::Unit/Unit)

                'coalton:Cons
                (make-constructor-entry
                 :name 'coalton:Cons
                 :arity 2
                 :constructs 'coalton:List
                 :classname nil
                 :docstring "`Cons` represents a `List` containing a first element (`car`) and a nested `Cons` (`cdr`)."
                 :compressed-repr 'nil)

                'coalton:Nil
                (make-constructor-entry
                 :name 'coalton:Nil
                 :arity 0
                 :constructs 'coalton:List
                 :classname nil
                 :docstring "`Nil` represents an empty `List`."
                 :compressed-repr 'nil)))

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

  ;; Hash table mapping variable symbols to their index in the predicate
  (class-variable-map  (util:required 'class-variable-map)  :type environment-map     :read-only t)
  (fundeps             (util:required 'fundeps)             :type fundep-list         :read-only t)

  ;; Methods of the class containing the same tyvars in PREDICATE for
  ;; use in pretty printing
  (unqualified-methods (util:required 'unqualified-methods) :type ty-class-method-list :read-only t)
  (codegen-sym         (util:required 'codegen-sym)         :type symbol               :read-only t)
  (superclass-dict     (util:required 'superclass-dict)     :type list                 :read-only t)
  (superclass-map      (util:required 'superclass-map)      :type environment-map      :read-only t)
  (docstring           (util:required 'docstring)           :type (or null string)     :read-only t)
  (location            (util:required 'location)            :type source:location             :read-only t))

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
   :class-variable-map (ty-class-class-variable-map class)
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

;;;
;;; Instance environment
;;;

(defstruct ty-class-instance
  (constraints         (util:required 'constraints)         :type ty-predicate-list :read-only t)
  (predicate           (util:required 'predicate)           :type ty-predicate      :read-only t)
  (codegen-sym         (util:required 'codegen-sym)         :type symbol            :read-only t)
  (method-codegen-syms (util:required 'method-codegen-syms) :type environment-map   :read-only t)
  (docstring           (util:required 'docstring)           :type (or null string)  :read-only t))

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
   :docstring (ty-class-instance-docstring instance)))

;;;
;;; Function environment
;;;

(defstruct function-env-entry
  (name  (util:required 'name)  :type symbol :read-only t)
  (arity (util:required 'arity) :type fixnum :read-only t))

(defmethod make-load-form ((self function-env-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type function-env-entry))

(defun function-env-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'function-env-entry-p x)))

(deftype function-env-entry-list ()
  `(satisfies function-env-entry-list-p))

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

;;;
;;; Environment
;;;

(defstruct environment
  (value-environment          map:+empty+ :type map:immutable-map :read-only t)
  (type-environment           (make-default-type-environment)
   :type map:immutable-map :read-only t)
  (constructor-environment    (make-default-constructor-environment)
   :type map:immutable-map :read-only t)
  (struct-environment         map:+empty+ :type map:immutable-map :read-only t)
  (class-environment          map:+empty+ :type map:immutable-map :read-only t)
  (fundep-environment         map:+empty+ :type map:immutable-map :read-only t)
  (instance-environment       map:+empty+ :type map:immutable-map :read-only t)
  (instance-sym-environment   map:+empty+ :type map:immutable-map :read-only t)
  (function-environment       map:+empty+ :type map:immutable-map :read-only t)
  (name-environment           map:+empty+ :type map:immutable-map :read-only t)
  (method-inline-environment  map:+empty+ :type map:immutable-map :read-only t)
  (code-environment           map:+empty+ :type map:immutable-map :read-only t)
  (specialization-environment map:+empty+ :type map:immutable-map :read-only t)
  ;; maps the names of user-defined functions to a list of their
  ;; user-supplied parameter names, for documentation generation
  (source-name-environment    map:+empty+ :type map:immutable-map :read-only t))

(defmethod print-object ((env environment) stream)
  (declare (type stream stream)
           (type environment env))
  (print-unreadable-object (env stream :type t :identity t)))

(defmethod make-load-form ((self environment) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type environment))

(defun update-environment (env
                           &key
                             (value-environment (environment-value-environment env))
                             (type-environment (environment-type-environment env))
                             (constructor-environment (environment-constructor-environment env))
                             (struct-environment (environment-struct-environment env))
                             (class-environment (environment-class-environment env))
                             (fundep-environment (environment-fundep-environment env))
                             (instance-environment (environment-instance-environment env))
                             (instance-sym-environment (environment-instance-sym-environment env))
                             (function-environment (environment-function-environment env))
                             (name-environment (environment-name-environment env))
                             (method-inline-environment (environment-method-inline-environment env))
                             (code-environment (environment-code-environment env))
                             (specialization-environment (environment-specialization-environment env))
                             (source-name-environment (environment-source-name-environment env)))
  (declare (type environment env)
           (type map:immutable-map value-environment)
           (type map:immutable-map type-environment)
           (type map:immutable-map constructor-environment)
           (type map:immutable-map struct-environment)
           (type map:immutable-map class-environment)
           (type map:immutable-map fundep-environment)
           (type map:immutable-map instance-environment)
           (type map:immutable-map instance-sym-environment)
           (type map:immutable-map function-environment)
           (type map:immutable-map name-environment)
           (type map:immutable-map method-inline-environment)
           (type map:immutable-map code-environment)
           (type map:immutable-map specialization-environment)
           (type map:immutable-map source-name-environment)
           (values environment))
  (make-environment
   :value-environment value-environment
   :type-environment type-environment
   :constructor-environment constructor-environment
   :struct-environment struct-environment
   :class-environment class-environment
   :fundep-environment fundep-environment
   :instance-environment instance-environment
   :instance-sym-environment instance-sym-environment
   :function-environment function-environment
   :name-environment name-environment
   :method-inline-environment method-inline-environment
   :code-environment code-environment
   :specialization-environment specialization-environment
   :source-name-environment source-name-environment))

;;;
;;; Methods
;;;

(defun env-apply-substitution (subst-list env)
  (declare (type substitution-list subst-list)
           (type environment env)
           (values environment &optional))
  (update-environment env
                      :value-environment
                      (value-apply-substitution
                       (environment-value-environment env)
                       subst-list)))

;;;
;;; Functions
;;;

(defun lookup-value-type (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (map:get (environment-value-environment env) symbol)
      (unless no-error
        (util:coalton-bug "Unknown binding ~S" symbol))))

(define-env-updater set-value-type (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type ty-scheme value))

  ;; Schemes stored in the environment are not allowed to have any free variables.
  (when (type-variables value)
    (util:coalton-bug "Unable to add type with free variables to environment ~S" value))

  (update-environment env
                      :value-environment (map:assoc (environment-value-environment env)
                                                    symbol
                                                    value)))

(define-env-updater unset-value-type (env symbol)
  (declare (type environment env)
           (type symbol symbol))

  (update-environment env
                      :value-environment (map:dissoc (environment-value-environment env)
                                                     symbol)))

(defun lookup-type (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (map:get (environment-type-environment env) symbol)
      (unless no-error
        (util:coalton-bug "Unknown type ~S" symbol))))

(define-env-updater set-type (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type type-entry value))
  (update-environment
   env
   :type-environment (map:assoc
                      (environment-type-environment env)
                      symbol
                      value)))

(defun lookup-constructor (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (map:get (environment-constructor-environment env) symbol)
      (unless no-error
        (util:coalton-bug "Unknown constructor ~S." symbol))))

(define-env-updater set-constructor (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type constructor-entry value))
  (update-environment env
                      :constructor-environment (map:assoc (environment-constructor-environment env)
                                                          symbol
                                                          value)))

(define-env-updater unset-constructor (env symbol)
  (declare (type environment env)
           (type symbol symbol))
  (update-environment env
                      :constructor-environment
                      (map:dissoc (environment-constructor-environment env)
                                  symbol)))

(defun lookup-struct (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (map:get (environment-struct-environment env) symbol)
      (unless no-error
        (util:coalton-bug "Unknown struct ~S" symbol))))

(define-env-updater set-struct (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type struct-entry value))
  (update-environment env
                      :struct-environment (map:assoc (environment-struct-environment env)
                                                     symbol
                                                     value)))

(define-env-updater unset-struct (env symbol)
  (declare (type environment env)
           (type symbol symbol))
  (update-environment
   env
   :struct-environment (map:dissoc (environment-struct-environment env) symbol)))

(defun lookup-class (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (map:get (environment-class-environment env) symbol)
      (unless no-error
        (util:coalton-bug "Unknown class ~S." symbol))))

(define-env-updater set-class (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type ty-class value))
  (update-environment
   env
   :class-environment (map:assoc (environment-class-environment env)
                                 symbol
                                 value)))

(defun lookup-function (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (map:get (environment-function-environment env) symbol)
      (unless no-error
        (util:coalton-bug "Unknown function ~S." symbol))))

(define-env-updater set-function (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type function-env-entry value))
  (update-environment
   env
   :function-environment (map:assoc
                          (environment-function-environment env)
                          symbol
                          value)))

(define-env-updater unset-function (env symbol)
  (declare (type environment env)
           (type symbol symbol))
  (update-environment
   env
   :function-environment (map:dissoc
                          (environment-function-environment env)
                          symbol)))

(defun lookup-name (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (map:get (environment-name-environment env) symbol)
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
         :name-environment (map:assoc
                            (environment-name-environment env)
                            symbol
                            value)))))

(define-env-updater unset-name (env symbol)
  (declare (type environment env)
           (type symbol symbol))
  (update-environment env
                      :name-environment
                      (map:dissoc (environment-name-environment env) symbol)))

(defun lookup-class-instances (env class)
  (declare (type environment env)
           (type symbol class))
  (map:get (environment-instance-environment env) class))

(defun lookup-class-instance (env pred &key no-error)
  (declare (type environment env))
  (let* ((pred-class (ty-predicate-class pred))
         (instances (lookup-class-instances env pred-class)))
    (dolist (instance instances)
      (handler-case
          (let ((subs (predicate-match (ty-class-instance-predicate instance) pred)))
            (return-from lookup-class-instance (values instance subs)))
        (predicate-unification-error () nil)))
    (unless no-error
      (error "Unknown instance for predicate ~S" pred))))

(defun lookup-instance-by-codegen-sym (env codegen-sym &key no-error)
  (declare (type environment env)
           (type symbol codegen-sym))

  (or (map:get (environment-instance-sym-environment env) codegen-sym)
   (unless no-error
     (error "Unknown instance with codegen-sym ~A" codegen-sym))))

(defun lookup-function-source-parameter-names (env function-name)
  (declare (type environment env)
           (type symbol function-name)
           (values parser:pattern-list &optional))
  (values (map:get (environment-source-name-environment env) function-name)))

(define-env-updater set-function-source-parameter-names (env function-name source-parameter-names)
  (declare (type environment env)
           (type symbol function-name)
           (type parser:pattern-list source-parameter-names))
  (update-environment
   env
   :source-name-environment (map:assoc (environment-source-name-environment env)
                                       function-name
                                       source-parameter-names)))

(define-env-updater unset-function-source-parameter-names (env function-name)
  (declare (type environment env)
           (type symbol function-name))
  (update-environment env
                      :source-name-environment (map:dissoc (environment-source-name-environment env)
                                                           function-name)))

(defun constructor-arguments (name env)
  (declare (type symbol name)
           (type environment env)
           (values ty-list &optional))
  (lookup-constructor env name)
  (function-type-arguments (lookup-value-type env name)))

(define-env-updater add-instance (env class value)
  (declare (type environment env)
           (type symbol class)
           (type ty-class-instance value))
  ;; Ensure the class is defined
  (unless (lookup-class env class)
    (error "Class ~S does not exist." class))

  (let ((instances (lookup-class-instances env class)))
    (loop :for inst :in instances
          :for index :from 0
          :do (when (handler-case (or (predicate-mgu (ty-class-instance-predicate value)
                                                     (ty-class-instance-predicate inst))
                                      t)
                      (predicate-unification-error () nil))
                ;; If we have the same instance then simply overwrite the old one
                (handler-case
                    (progn
                      (predicate-match (ty-class-instance-predicate value)
                                       (ty-class-instance-predicate inst))
                      (predicate-match (ty-class-instance-predicate inst)
                                       (ty-class-instance-predicate value))
                      (return-from add-instance
                        (update-environment
                         env
                         :instance-environment (map:assoc
                                                (environment-instance-environment env)
                                                class
                                                (replace-at-index instances index value))
                         :instance-sym-environment (map:assoc
                                                    (environment-instance-sym-environment env)
                                                    (ty-class-instance-codegen-sym value)
                                                    value))))
                  (predicate-unification-error ()
                    (error 'overlapping-instance-error
                           :inst1 (ty-class-instance-predicate value)
                           :inst2 (ty-class-instance-predicate inst))))))
    (update-environment
     env
     :instance-environment (map:assoc
                            (environment-instance-environment env)
                            class
                            (cons value instances))
     :instance-sym-environment (map:assoc
                                (environment-instance-sym-environment env)
                                (ty-class-instance-codegen-sym value)
                                value))))

(define-env-updater set-method-inline (env method instance codegen-sym)
  (declare (type environment env)
           (type symbol method instance codegen-sym))
  (update-environment
   env
   :method-inline-environment
   (map:assoc
    (environment-method-inline-environment env)
    (cons method instance)
    codegen-sym)))

(defun lookup-method-inline (env method instance &key no-error)
  (declare (type environment env)
           (type symbol method instance)
           (values symbol))
  (or
   (map:get
    (environment-method-inline-environment env)
    (cons method instance))
   (unless no-error
     (error "Unable to find inline method for method ~A on instance ~S." method instance))))

(define-env-updater set-code (env name code)
  (declare (type environment env)
           (type symbol name)
           (type t code))
  (update-environment env
                      :code-environment (map:assoc (environment-code-environment env)
                                                   name
                                                   code)))

(defun lookup-code (env name &key no-error)
  (declare (type environment env)
           (type symbol name)
           (values t))
  (or (map:get (environment-code-environment env) name)
      (unless no-error
        (error "Unable to find code for function ~A." name))))

(defun replace-at-index (list index element)
  (append (subseq list 0 index)
          (list element)
          (subseq list (1+ index))))

(define-env-updater add-specialization (env entry)
  (declare (type environment env)
           (type specialization-entry entry))
  (let* ((from (specialization-entry-from entry))
         (to (specialization-entry-to entry))
         (to-ty (specialization-entry-to-ty entry))
         (to-scheme (quantify (type-variables to-ty)
                              (qualify nil to-ty)))
         (spec-env (environment-specialization-environment env))
         (specializations (map:get spec-env from))
         (index (position to-scheme specializations
                          :key (lambda (elem)
                                 (let ((type (specialization-entry-to-ty elem)))
                                   (quantify (type-variables type)
                                             (qualify nil type)))))))
    (when index
      (return-from add-specialization
        (update-environment env
                            :specialization-environment
                            (map:assoc spec-env from
                                       (replace-at-index specializations index entry)))))

    #++ (dolist (elem specializations)   ; FIXME -- think
      (handler-case
          (progn
            (unify nil to-ty (specialization-entry-to-ty elem))
            (error 'overlapping-specialization-error
                   :new to
                   :existing (specialization-entry-to elem)))
        (unification-error ())))

    (update-environment env
                        :specialization-environment
                        (map:assoc spec-env from
                                   (cons entry specializations)))))

(defun lookup-specialization (env from to &key (no-error nil))
  (declare (type environment env)
           (type symbol from)
           (type symbol to)
           (values (or null specialization-entry) &optional))
  (dolist (elem (or (map:get (environment-specialization-environment env) from)
                    (unless no-error
                      (error "Unable to find specialization from ~A to ~A" from to))))
    (when (eq to (specialization-entry-to elem))
      (return-from lookup-specialization elem)))
  (unless no-error
    (error "Unable to find specialization from ~A to ~A" from to)))

(defun lookup-specialization-by-type (env from ty &key (no-error nil))
  (declare (type environment env)
           (type symbol from)
           (type ty ty)
           (values (or null specialization-entry) &optional))
  (dolist (elem (or (map:get (environment-specialization-environment env) from)
                    (unless no-error
                      (error "Unable to find specialization from ~A of type ~A" from ty))))
    (handler-case
        (progn
          (match (specialization-entry-to-ty elem) ty)
          (return-from lookup-specialization-by-type elem))
      (coalton-internal-type-error (e)
        (declare (ignore e)))))
  (unless no-error
    (error "Unable to find specialization from ~A, of type ~A" from ty)))
  

(defun lookup-fundep-environment (env class)
  (declare (type environment env)
           (type symbol class))
  (map:get (environment-fundep-environment env) class))

(defun set-fundep-environment (env class fundeps)
  (update-environment env
                      :fundep-environment (map:assoc
                                           (environment-fundep-environment env)
                                           class
                                           fundeps)))

(defun init-fundep-env (env class)
  (set-fundep-environment env class map:+empty+))

(defun get-fundep-entries (fundep-env fundep)
  (map:get fundep-env fundep))

(defun push-fundep-entry (env class fundep entry)
  (declare (type environment env)
           (type symbol class)
           (type fixnum fundep)
           (type fundep-entry entry))
  (let* ((fundep-env (lookup-fundep-environment env class))
         (entries (cons entry (get-fundep-entries fundep-env fundep)))
         (fundep-env (map:assoc fundep-env fundep entries)))
    (set-fundep-environment env class fundep-env)))

(define-env-updater initialize-fundep-environment (env class)
  (declare (type environment env)
           (type symbol class))
  (when (lookup-fundep-environment env class)
    (return-from initialize-fundep-environment env))
  (init-fundep-env env class))

(defstruct fundep-entry
  (from      (util:required 'from) :type ty-list :read-only t)
  (to        (util:required 'to)   :type ty-list :read-only t))

(defmethod make-load-form ((self fundep-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

(define-env-updater update-instance-fundeps (env pred)
  (declare (type environment env)
           (type ty-predicate pred))
  (let* ((class (lookup-class env (ty-predicate-class pred)))
         (fundep-env (lookup-fundep-environment env (ty-predicate-class pred)))
         (class-variable-map (ty-class-class-variable-map class)))
    (loop :for fundep :in (ty-class-fundeps class)
          :for i :from 0
          ;; Lookup the state for the ith fundep
          :for state := (get-fundep-entries fundep-env i)
          :for from-tys
            := (mapcar
                (lambda (var)
                  (nth (get-value class-variable-map var)
                       (ty-predicate-types pred)))
                (fundep-from fundep))
          :for to-tys
            := (mapcar
                (lambda (var)
                  (nth (get-value class-variable-map var)
                       (ty-predicate-types pred)))
                (fundep-to fundep))
          :do (block update-block
                ;; Try to find a matching relation for the current fundep
                (dolist (s state)
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
                      (push-fundep-entry env
                                         (ty-class-name class)
                                         i
                                         (make-fundep-entry :from from-tys
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
    (dolist (inst (lookup-class-instances env class-name))
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
  (declare (type environment env)
           (type ty-predicate-list preds)
           (type substitution-list subs)
           (values ty-predicate-list substitution-list &optional))
  ;; If no predicates have fundeps, then exit early
  (unless (loop :for pred :in preds
                :for class-name := (ty-predicate-class pred)
                :for class := (lookup-class env class-name)
                :when (ty-class-fundeps class)
                  :collect class)
    (return-from solve-fundeps (values preds subs)))
  
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
                                       (ty-class-instance-constraints instance))))
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
         (class-variable-map (ty-class-class-variable-map class))
         (fundep-env (lookup-fundep-environment env class-name)))
    (loop :for fundep :in (ty-class-fundeps class)
          :for i :from 0
          :do (let ((state (get-fundep-entries fundep-env i)))
                (setf subs (generate-fundep-subs-for-pred% pred state class-variable-map fundep subs))))
    subs))


(defun generate-fundep-subs-for-pred% (pred state class-variable-map fundep subs)
  (declare (type ty-predicate pred)
           (type environment-map class-variable-map)
           (type fundep fundep)
           (type substitution-list subs)
           (values substitution-list &optional))
  (let* ((from-tys
           (mapcar
            (lambda (var)
              (nth (get-value class-variable-map var) (ty-predicate-types pred)))
            (fundep-from fundep)))
         (to-tys
           (mapcar
            (lambda (var)
              (nth (get-value class-variable-map var) (ty-predicate-types pred)))
            (fundep-to fundep))))
    (dolist (entry state)
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
