;;;;
;;;; Central environment managment. Coalton stores all persistent
;;;; state in a single immutable struct. State is partitioned into a
;;;; series of sub-environments. Each sub-environment is analogous to
;;;; a database table, and holds multiple entries which function like
;;;; database rows. The process of compiling Coalton code will require
;;;; many "writes" to the environment. Each write is an immutable
;;;; update which returns a new environment. The writes preformed when
;;;; compiling a single coalton-toplevel form are tracked in a log.
;;;; After compilation is finished the write log is added to the
;;;; generated lisp code. This allows replaying the environment
;;;; updates when the compiled fasl is loaded.
;;;;

(defpackage #:coalton-impl/typechecker/environment
  (:use
   #:cl
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
  (:import-from
   #:coalton-impl/environment
   #:environment
   #:keys)
  (:local-nicknames
   (#:a #:alexandria)
   (#:env #:coalton-impl/environment)
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:parser #:coalton-impl/parser))
  (:export
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
   #:type-entry-docstring                   ; ACCESSOR
   #:type-entry-location                    ; ACCESSOR
   #:constructor-entry                      ; STRUCT
   #:make-constructor-entry                 ; ACCESSOR
   #:constructor-entry-name                 ; ACCESSOR
   #:constructor-entry-arity                ; ACCESSOR
   #:constructor-entry-constructs           ; ACCESSOR
   #:constructor-entry-classname            ; ACCESSOR
   #:constructor-entry-compressed-repr      ; ACCESSOR
   #:constructor-entry-list                 ; TYPE
   #:struct-entry                           ; STRUCT
   #:make-struct-entry                      ; CONSTRUCTOR
   #:struct-entry-name                      ; ACCESSOR
   #:struct-entry-fields                    ; ACCESSOR
   #:struct-entry-field-tys                 ; ACCESSOR
   #:struct-entry-field-idx                 ; ACCESSOR`
   #:struct-entry-list                      ; TYPE
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
   #:ty-class-docstring                     ; ACCESSOR
   #:ty-class-location                      ; ACCESSOR
   #:ty-class-list                          ; TYPE
   #:ty-class-instance                      ; STRUCT
   #:make-ty-class-instance                 ; CONSTRUCTOR
   #:ty-class-instance-constraints          ; ACCESSOR
   #:ty-class-instance-predicate            ; ACCESSOR
   #:ty-class-instance-codegen-sym          ; ACCESSOR
   #:ty-class-instance-method-codegen-syms  ; ACCESSOR
   #:ty-class-instance-docstring            ; ACCESSOR
   #:ty-class-instance-list                 ; TYPE
   #:function-env-entry                     ; STRUCT
   #:make-function-env-entry                ; CONSTRUCTOR
   #:function-env-entry-name                ; ACCESSOR
   #:function-env-entry-arity               ; ACCESSOR
   #:name-entry                             ; STRUCT
   #:make-name-entry                        ; CONSTRUCTOR
   #:name-entry-name                        ; ACCESSOR
   #:name-entry-type                        ; ACCESSOR
   #:name-entry-docstring                   ; ACCESSOR
   #:name-entry-location                    ; ACCESSOR
   #:specialization-entry                   ; STRUCT
   #:make-specialization-entry              ; CONSTRUCTOR
   #:specialization-entry-from              ; ACCESSOR
   #:specialization-entry-to                ; ACCESSOR
   #:specialization-entry-to-ty             ; ACCESSOR
   #:specialization-entry-list              ; TYPE
   #:environment                            ; STRUCT
   #:make-default-environment               ; FUNCTION
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
   #:update-instance-fundeps                ; FUNCTION
   #:solve-fundeps                          ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/environment)

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

  ;; If this is true then the type is compiled to a more efficient
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
  (newtype (util:required 'newtype)           :type boolean :read-only t)

  (docstring (util:required 'docstring)       :type (or null string) :read-only t)
  (location  (util:required 'location)        :type t                :read-only t))

(defmethod make-load-form ((self type-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

(defmethod kind-of ((entry type-entry))
  (kind-of (type-entry-type entry)))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type type-entry))

(defun add-early-types (env)
  "Add early types to an environment."
  (env:set* env :type
            'coalton:Boolean      (make-type-entry
                                   :name 'coalton:Boolean
                                   :runtime-type 'cl:boolean
                                   :type *boolean-type*
                                   :tyvars nil
                                   :constructors '(coalton:True coalton:False)
                                   :explicit-repr '(:native cl:boolean)
                                   :enum-repr t
                                   :newtype nil
                                   :docstring "Either true or false represented by `t` and `nil` respectively."
                                   :location "")
            'coalton:Unit         (make-type-entry
                                   :name 'coalton:Unit
                                   :runtime-type '(member coalton::Unit/Unit)
                                   :type *unit-type*
                                   :tyvars nil
                                   :constructors '(coalton:Unit)
                                   :explicit-repr :enum
                                   :enum-repr t
                                   :newtype nil
                                   :docstring ""
                                   :location "")
            'coalton:Char         (make-type-entry
                                   :name 'coalton:Char
                                   :runtime-type 'cl:character
                                   :type *char-type*
                                   :tyvars nil
                                   :constructors nil
                                   :explicit-repr '(:native cl:character)
                                   :enum-repr nil
                                   :newtype nil
                                   :docstring "A single character represented as a `character` type."
                                   :location "")
            'coalton:Integer      (make-type-entry
                                   :name 'coalton:Integer
                                   :runtime-type 'cl:integer
                                   :type *integer-type*
                                   :tyvars nil
                                   :constructors nil
                                   :explicit-repr '(:native cl:integer)
                                   :enum-repr nil
                                   :newtype nil
                                   :docstring "Unbound integer. Uses `integer`."
                                   :location "")
            'coalton:Single-Float (make-type-entry
                                   :name 'coalton:Single-Float
                                   :runtime-type 'cl:single-float
                                   :type *single-float-type*
                                   :tyvars nil
                                   :constructors nil
                                   :explicit-repr '(:native cl:single-float)
                                   :enum-repr nil
                                   :newtype nil
                                   :docstring "Single precision floating point number. Uses `single-float`."
                                   :location "")
            'coalton:Double-Float (make-type-entry
                                   :name 'coalton:Double-Float
                                   :runtime-type 'cl:double-float
                                   :type *double-float-type*
                                   :tyvars nil
                                   :constructors nil
                                   :explicit-repr '(:native cl:double-float)
                                   :enum-repr nil
                                   :newtype nil
                                   :docstring "Double precision floating point number. Uses `double-float`."
                                   :location "")
            'coalton:String       (make-type-entry
                                   :name 'coalton:String
                                   :runtime-type 'cl:string
                                   :type *string-type*
                                   :tyvars nil
                                   :constructors nil
                                   :explicit-repr '(:native cl:string)
                                   :enum-repr nil
                                   :newtype nil
                                   :docstring "String of characters represented by Common Lisp `string`."
                                   :location "")
            'coalton:Fraction     (make-type-entry
                                   :name 'coalton:Fraction
                                   :runtime-type 'cl:rational
                                   :type *fraction-type*
                                   :tyvars nil
                                   :constructors nil
                                   :explicit-repr '(:native cl:rational)
                                   :enum-repr nil
                                   :newtype nil
                                   :docstring "A ratio of integers always in reduced form."
                                   :location "")
            'coalton:Arrow        (make-type-entry
                                   :name 'coalton:Arrow
                                   :runtime-type nil
                                   :type *arrow-type*
                                   :tyvars nil
                                   :constructors nil
                                   :explicit-repr nil
                                   :enum-repr nil
                                   :newtype nil
                                   :docstring "Type constructor for function types."
                                   :location "")
            'coalton:List         (make-type-entry
                                   :name 'coalton:List
                                   :runtime-type 'cl:list
                                   :type *list-type*
                                   :tyvars (list (make-variable))
                                   :constructors '(coalton:Cons coalton:Nil)
                                   :explicit-repr '(:native cl:list)
                                   :enum-repr nil
                                   :newtype nil
                                   :docstring "Homogeneous list of objects represented as a Common Lisp `list`."
                                   :location "")))

;;;
;;; Constructor environment
;;;

(defstruct constructor-entry
  (name            (util:required 'name)            :type symbol                         :read-only t)
  (arity           (util:required 'arity)           :type alexandria:non-negative-fixnum :read-only t)
  (constructs      (util:required 'constructs)      :type symbol                         :read-only t)
  (classname       (util:required 'classname)       :type symbol                         :read-only t)

  ;; If this constructor constructs a compressed-repr type then
  ;; compressed-repr is the runtime value of this nullary constructor
  (compressed-repr (util:required 'compressed-repr) :type t                              :read-only t))

(defmethod make-load-form ((self constructor-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type constructor-entry))

(defun constructor-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'constructor-entry-p x)))

(deftype constructor-entry-list ()
  '(satisfies constructor-entry-list-p))

(defun add-early-constructors (env)
  "Add early constructors to an environment."
  (env:set* env :constructor
            ;; Early Constructors
            'coalton:True  (make-constructor-entry
                            :name 'coalton:True
                            :arity 0
                            :constructs 'coalton:Boolean
                            :classname 'coalton::Boolean/True
                            :compressed-repr 't)
            'coalton:False (make-constructor-entry
                            :name 'coalton:False
                            :arity 0
                            :constructs 'coalton:Boolean
                            :classname 'coalton::Boolean/False
                            :compressed-repr 'nil)
            'coalton:Unit  (make-constructor-entry
                            :name 'coalton:Unit
                            :arity 0
                            :constructs 'coalton:Unit
                            :classname 'coalton::Unit/Unit
                            :compressed-repr 'coalton::Unit/Unit)
            'coalton:Cons  (make-constructor-entry
                            :name 'coalton:Cons
                            :arity 2
                            :constructs 'coalton:List
                            :classname nil
                            :compressed-repr 'nil)
            'coalton:Nil   (make-constructor-entry
                            :name 'coalton:Nil
                            :arity 0
                            :constructs 'coalton:List
                            :classname nil
                            :compressed-repr 'nil)))

;;;
;;; Class environment
;;;

(defstruct ty-class
  (name                (util:required 'name)                :type symbol              :read-only t)
  (predicate           (util:required 'predicate)           :type ty-predicate        :read-only t)
  (superclasses        (util:required 'superclasses)        :type ty-predicate-list   :read-only t)
  (class-variables     (util:required 'class-variables)     :type util:symbol-list    :read-only t)

  ;; Hash table mapping variable symbols to their index in the predicate
  (class-variable-map  (util:required 'class-variable-map)  :type hash-table          :read-only t)
  (fundeps             (util:required 'fundeps)             :type fundep-list         :read-only t)

  ;; Methods of the class containing the same tyvars in PREDICATE for
  ;; use in pretty printing
  (unqualified-methods (util:required 'unqualified-methods) :type scheme-binding-list :read-only t)
  (codegen-sym         (util:required 'codegen-sym)         :type symbol              :read-only t)
  (superclass-dict     (util:required 'superclass-dict)     :type list                :read-only t)
  (superclass-map      (util:required 'superclass-map)      :type hash-table          :read-only t)
  (docstring           (util:required 'docstring)           :type (or null string)    :read-only t)
  (location            (util:required 'location)            :type t                   :read-only t))

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
  (make-ty-class :name (ty-class-name class)
                 :predicate (apply-substitution subst-list (ty-class-predicate class))
                 :superclasses (apply-substitution subst-list (ty-class-superclasses class))
                 :class-variables (ty-class-class-variables class)
                 :class-variable-map (ty-class-class-variable-map class)
                 :fundeps (ty-class-fundeps class)
                 :unqualified-methods (mapcar (lambda (entry)
                                                (cons (car entry)
                                                      (apply-substitution subst-list (cdr entry))))
                                              (ty-class-unqualified-methods class))
                 :codegen-sym (ty-class-codegen-sym class)
                 :superclass-dict (mapcar (lambda (entry)
                                            (cons (apply-substitution subst-list (car entry))
                                                  (cdr entry)))
                                          (ty-class-superclass-dict class))
                 :superclass-map (ty-class-superclass-map class)
                 :docstring (ty-class-docstring class)
                 :location (ty-class-location class)))

;;;
;;; Instance environment
;;;

(defstruct ty-class-instance
  (constraints         (util:required 'constraints)         :type ty-predicate-list :read-only t)
  (predicate           (util:required 'predicate)           :type ty-predicate      :read-only t)
  (codegen-sym         (util:required 'codegen-sym)         :type symbol            :read-only t)
  (method-codegen-syms (util:required 'method-codegen-syms) :type hash-table        :read-only t)
  (docstring           (util:required 'docstring)           :type (or null string)  :read-only t))

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
  (make-ty-class-instance :constraints (apply-substitution subst-list (ty-class-instance-constraints instance))
                          :predicate (apply-substitution subst-list (ty-class-instance-predicate instance))
                          :codegen-sym (ty-class-instance-codegen-sym instance)
                          :method-codegen-syms (ty-class-instance-method-codegen-syms instance)
                          :docstring (ty-class-instance-docstring instance)))

;;;
;;; Environment
;;;

(defun make-default-environment ()
  (add-early-types (add-early-constructors (env:empty))))

(defmethod apply-substitution (subst-list (env environment))
  (env:update-entries env :value (lambda (v)
                                   (apply-substitution subst-list v))))

(defmethod type-variables ((env environment))
  (let ((out nil))
    (env:do-environment (name type env :value)
      (declare (ignore name))
      (setf out (append (type-variables type) out)))
    (remove-duplicates out :test #'equalp)))

;; Utility

(declaim (inline %lookup))
(defun %lookup (env ns symbol error-fmt no-error)
  (or (env:get env ns symbol)
      (unless no-error
        (util:coalton-bug error-fmt symbol))))

(defun acons-list (key datum alist)
  (if (assoc key alist)
      (mapcar (lambda (e)
                (if (eql (car e) key)
                    (cons (car e) (cons datum (cdr e)))
                    e))
              alist)
      (cons (list key datum) alist)))

(defun replace-or-append (list element predicate)
  (let ((item (find-if predicate list)))
    (if item
        (substitute element item list)
        (cons element list))))

(defun env-update (env ns k f)
  (env:set env ns k (funcall f (env:get env ns k))))

;;;
;;; Values
;;;

(defun lookup-value-type (env symbol &key no-error)
  (%lookup env :value symbol "Unknown binding ~S" no-error))

(defun set-value-type (env symbol value)
  (when (type-variables value)
    ;; Schemes stored in the environment are not allowed to have any free variables.
    (util:coalton-bug "Unable to add type with free variables to environment ~S" value))
  (env:set env :value symbol value))

(defun unset-value-type (env symbol)
  (env:unset env :value symbol))

;;;
;;; Types
;;;

(defun lookup-type (env symbol &key no-error)
  (%lookup env :type symbol "Unknown type ~S" no-error))

(defun set-type (env symbol value)
  (env:set env :type symbol value))

;;;
;;; Constructors
;;;

(defun lookup-constructor (env symbol &key no-error)
  (%lookup env :constructor symbol "Unknown constructor ~S" no-error))

(defun set-constructor (env symbol value)
  (env:set env :constructor symbol value))

(defun unset-constructor (env symbol)
  (env:unset env :constructor symbol))

;;;
;;; Struct environment
;;;

(defstruct struct-entry
  (name      (util:required 'name)      :type symbol           :read-only t)
  (fields    (util:required 'fields)    :type util:string-list :read-only t)

  ;; Mapping of "field name" -> "field type"
  ;; Type variables are the same as in `type-entry-type'
  (field-tys (util:required 'field-tys) :type hash-table       :read-only t)

  ;; Mapping of "field name" -> "field index"
  (field-idx (util:required 'field-idx) :type hash-table       :read-only t)h)

(defmethod make-load-form ((self struct-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun struct-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'struct-entry-p x)))

(deftype struct-entry-list ()
  '(satisfies struct-entry-list-p))

(defun lookup-struct (env symbol &key no-error)
  (%lookup env :struct symbol "Unknown struct ~S" no-error))

(defun set-struct (env symbol value)
  (env:set env :struct symbol value))

(defun unset-struct (env symbol)
  (env:unset env :struct symbol))

;;;

(defun lookup-class (env symbol &key no-error)
  (%lookup env :class symbol "Unknown class ~S" no-error))

(defun set-class (env symbol value)
  (env:set env :class symbol value))

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

(defun lookup-function (env symbol &key no-error)
  (%lookup env :function symbol "Unknown function ~S" no-error))

(defun set-function (env symbol value)
  (env:set env :function symbol value))

(defun unset-function (env symbol)
  (env:unset env :function symbol))

;;;
;;; Name environment
;;;

(defstruct name-entry
  (name      (util:required 'name)      :type symbol                               :read-only t)
  (type      (util:required 'type)      :type (member :value :method :constructor) :read-only t)
  (docstring (util:required 'docstring) :type (or null string)                     :read-only t)
  (location  (util:required 'location)  :type t                                    :read-only t))

(defmethod make-load-form ((self name-entry) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type name-entry))

(defun lookup-name (env symbol &key no-error)
  (%lookup env :name symbol "Unknown name ~S" no-error))

(defun set-name (env symbol value)
  (let ((old-value (lookup-name env symbol :no-error t)))
    (when (and old-value (not (equalp (name-entry-type old-value) (name-entry-type value))))
      (error "Unable to change the type of name ~S from ~A to ~A."
             symbol
             (name-entry-type old-value)
             (name-entry-type value)))
    (if old-value
        env
        (env:set env :name symbol value))))

(defun unset-name (env symbol)
  (env:unset env :name symbol))

;;;

(defun lookup-class-instances (env class &key no-error)
  (declare (ignore no-error))
  (env:get env :instance class))

(defun lookup-class-instance (env pred &key no-error)
  (let* ((pred-class (ty-predicate-class pred))
         (instances (lookup-class-instances env pred-class :no-error no-error)))
    (dolist (instance instances)
      (handler-case
          (let ((subs (predicate-match (ty-class-instance-predicate instance) pred)))
            (return-from lookup-class-instance (values instance subs)))
        (predicate-unification-error () nil)))
    (unless no-error
      (error "Unknown instance for predicate ~S" pred))))

(defun lookup-instance-by-codegen-sym (env codegen-sym &key no-error)
  (%lookup env :codegen-sym codegen-sym "Unknown instance with codegen-sym ~A" no-error))

(defun lookup-function-source-parameter-names (env function-name)
  (env:get env :source-name function-name))

(defun set-function-source-parameter-names (env function-name source-parameter-names)
  (env:set env :source-name function-name source-parameter-names))

(defun unset-function-source-parameter-names (env function-name)
  (env:unset env :source-name function-name))

(defun constructor-arguments (name env)
  (lookup-constructor env name)
  (function-type-arguments (lookup-value-type env name)))

(define-env-updater add-instance (env class value)
  (declare (type environment env)
           (type symbol class)
           (type ty-class-instance value))
  ;; Ensure the class is defined
  (unless (lookup-class env class)
    (error "Class ~S does not exist." class))
  (let ((instances (lookup-class-instances env class :no-error t)))
    (loop :for inst :in instances
          :for index :from 0
          :do (when (handler-case (or (predicate-mgu (ty-class-instance-predicate value)
                                                     (ty-class-instance-predicate inst))
                                      t)
                      (predicate-unification-error () nil))
                ;; If we have the same instance then overwrite the old one
                (handler-case
                    (progn
                      (predicate-match (ty-class-instance-predicate value) (ty-class-instance-predicate inst))
                      (predicate-match (ty-class-instance-predicate inst) (ty-class-instance-predicate value))
                      (setf (nth index instances) value)
                      (return-from add-instance
                        (env:set
                         (env:set env :instance class instances)
                         :codegen-sym (ty-class-instance-codegen-sym value) value)))
                  (predicate-unification-error ()
                    (error 'overlapping-instance-error
                           :inst1 (ty-class-instance-predicate value)
                           :inst2 (ty-class-instance-predicate inst))))))
    (env:set
     (env:set env :instance class (cons value instances))
     :codegen-sym (ty-class-instance-codegen-sym value) value)))

(defun set-method-inline (env method instance codegen-sym)
  (env:set env :method-inline (cons method instance) codegen-sym))

(defun lookup-method-inline (env method instance &key no-error)
  (declare (type environment env)
           (type symbol method instance)
           (values symbol &optional))
  (%lookup env :method-inline (cons method instance)
           "Unable to find inline method for method ~A on instance ~S." no-error))

(defun set-code (env name code)
  (env:set env :code name code))

(defun lookup-code (env name &key no-error)
  (%lookup env :code name "Unable to find code for function ~A." no-error))

;;;
;;; Specialization environment
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

(defun add-specialization (env entry)
  (declare (type environment env)
           (type specialization-entry entry))
  (let* ((from (specialization-entry-from entry))
         (to (specialization-entry-to entry))
         (to-ty (specialization-entry-to-ty entry))
         (to-scheme (quantify (type-variables to-ty)
                              (qualify nil to-ty))))
    (env-update env :specialization from
                (lambda (entries)
                  (replace-or-append
                   entries
                   entry
                   (lambda (elem)
                     (let* ((type (specialization-entry-to-ty elem))
                            (scheme (quantify (type-variables type)
                                              (qualify nil type))))
                       (cond ((equalp to-scheme scheme)
                              t)
                             (t
                              (handler-case
                                  (progn
                                    (unify nil to-ty (specialization-entry-to-ty elem))
                                    (error 'overlapping-specialization-error
                                           :new to
                                           :existing (specialization-entry-to elem)))
                                (unification-error ()))
                              nil)))))))))

(defun lookup-specialization (env from to &key (no-error nil))
  (declare (type environment env)
           (type symbol from)
           (type symbol to)
           (values (or null specialization-entry) &optional))
  (dolist (elem (env:get env :specialization from))
    (when (eq to (specialization-entry-to elem))
      (return-from lookup-specialization elem)))
  (unless no-error
    (error "Unable to find specialization from ~A to ~A" from to)))

(defun lookup-specialization-by-type (env from ty &key (no-error nil))
  (declare (type environment env)
           (type symbol from)
           (type ty ty)
           (values (or null specialization-entry) &optional))
  (dolist (elem (env:get env :specialization from))
    (handler-case
        (progn
          (match (specialization-entry-to-ty elem) ty)
          (return-from lookup-specialization-by-type elem))
      (error:coalton-internal-type-error (e)
        (declare (ignore e)))))
  (unless no-error
    (error "Unable to find specialization for type ~A" ty)))

;;;
;;; Functional dependency environment
;;;

(defun lookup-fundep-environment (env class)
  (declare (type environment env)
           (type symbol class)
           (values (or null cons) &optional))
  (env:get env :fundep class))

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
  (env-update env :fundep class (lambda (fundeps)
                                  (acons-list fundep entry fundeps))))

(defun update-instance-fundeps (env pred)
  (declare (type environment env)
           (type ty-predicate pred))
  (let* ((class (lookup-class env (ty-predicate-class pred)))
         (fundep-env (lookup-fundep-environment env (ty-predicate-class pred)))
         (class-variable-map (ty-class-class-variable-map class)))
    (loop :for fundep :in (ty-class-fundeps class)
          :for i :from 0
          ;; Lookup the state for the ith fundep
          :for state := (cdr (assoc i fundep-env))
          :for from-tys
            := (mapcar
                (lambda (var)
                  (nth (gethash var class-variable-map) (ty-predicate-types pred)))
                (fundep-from fundep))
          :for to-tys
            := (mapcar
                (lambda (var)
                  (nth (gethash var class-variable-map) (ty-predicate-types pred)))
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
         (new-pred (make-ty-predicate :class class-name :types vars :source (ty-predicate-source pred))))
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
           (type substitution-list subs))
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
           (type ty-predicate pred))
  (let* ((class-name (ty-predicate-class pred))

         (class (lookup-class env class-name))

         (class-variable-map (ty-class-class-variable-map class))

         (fundep-env (lookup-fundep-environment env class-name)))
    (loop :for fundep :in (ty-class-fundeps class)
          :for i :from 0
          :for state := (cdr (assoc i fundep-env))
          :when state
            :do (setf subs (generate-fundep-subs-for-pred% pred state class-variable-map fundep subs)))
    subs))

(defun generate-fundep-subs-for-pred% (pred state class-variable-map fundep subs)
  (declare (type ty-predicate pred)
           (type (or cons null) state)
           (type hash-table class-variable-map)
           (type fundep fundep))

  (let* ((from-tys
           (mapcar
            (lambda (var)
              (nth (gethash var class-variable-map) (ty-predicate-types pred)))
            (fundep-from fundep)))

         (to-tys
           (mapcar
            (lambda (var)
              (nth (gethash var class-variable-map) (ty-predicate-types pred)))
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
