(in-package #:coalton-impl/typechecker)

;;;
;;; Value type environments
;;;

(defstruct (value-environment (:include immutable-map)))

#+sbcl
(declaim (sb-ext:freeze-type value-environment))

(defmethod apply-substitution (subst-list (env value-environment))
  (make-value-environment :data (fset:image (lambda (key value)
                  (values key (apply-substitution subst-list value)))
                (immutable-map-data env))))

(defmethod type-variables ((env value-environment))
  (remove-duplicates (mapcan #'type-variables (fset:convert 'list (fset:range (immutable-map-data env)))) :test #'equalp))

;;;
;;; Type environments
;;;

(defstruct (type-entry (:constructor type-entry))
  (name         (required 'name)         :type symbol  :read-only t)
  (runtime-type (required 'runtime-type) :type t       :read-only t)
  (type         (required 'type)         :type ty      :read-only t)

  ;; If this is true then the type is compiled to a more effecient
  ;; enum representation at runtime
  (enum-repr (required 'enum-repr)       :type boolean :read-only t)

  ;; If this is true then the type does not exist at runtime
  ;; See https://wiki.haskell.org/Newtype
  ;;
  ;; Because Haskell is a lazy language there is an observable
  ;; difference between a boxed wrapper type and a newtype. Because
  ;; Coalton is strict there is no observable difference in Coalton
  ;; code between the two.
  ;;
  ;; A type cannot be both enum repr and a newtype
  ;;
  ;; A type that is a newtype has another Coalton type as it's
  ;; runtime-type instead of a lisp type. This is to avoid issues with
  ;; recursive newtypes.
  (newtype (required 'newtype)           :type boolean :read-only t))

(defmethod make-load-form ((self type-entry) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names '(name runtime-type type enum-repr newtype)
   :environment env))

(defmethod coalton-impl/typechecker::kind-of ((entry type-entry))
  (coalton-impl/typechecker::kind-of (type-entry-type entry)))

#+sbcl
(declaim (sb-ext:freeze-type type-entry))

(defstruct (type-environment (:include immutable-map)))

#+sbcl
(declaim (sb-ext:freeze-type type-environment))


(defun make-default-type-environment ()
  "Create a TYPE-ENVIRONMENT containing early types"
  (make-type-environment
   :data (fset:map
          ;; Early Types
          ('coalton:Boolean
           (type-entry
            :name 'coalton:Boolean
            :runtime-type 'cl:boolean
            :type tBoolean
            :enum-repr t
            :newtype nil))

          ('coalton:Char
           (type-entry
            :name 'coalton:Char
            :runtime-type 'cl:character
            :type tChar
            :enum-repr nil
            :newtype nil))

          ('coalton:I32
           (type-entry
            :name 'coalton:I32
            :runtime-type '(cl:signed-byte 32)
            :type tI32
            :enum-repr nil
            :newtype nil))

          ('coalton:I64
           (type-entry
            :name 'coalton:I64
            :runtime-type '(cl:signed-byte 64)
            :type tI64
            :enum-repr nil
            :newtype nil))

          ('coalton:U8
           (type-entry
            :name 'coalton:U8
            :runtime-type '(cl:unsigned-byte 8)
            :type tU8
            :enum-repr nil
            :newtype nil))

          ('coalton:U32
           (type-entry
            :name 'coalton:U32
            :runtime-type '(cl:unsigned-byte 32)
            :type tU32
            :enum-repr nil
            :newtype nil))

          ('coalton:U64
           (type-entry
            :name 'coalton:U64
            :runtime-type '(cl:unsigned-byte 64)
            :type tU64
            :enum-repr nil
            :newtype nil))

          ('coalton:Integer
           (type-entry
            :name 'coalton:Integer
            :runtime-type 'cl:integer
            :type tInteger
            :enum-repr nil
            :newtype nil))

          ('coalton:Single-Float
           (type-entry
            :name 'coalton:Single-Float
            :runtime-type 'cl:single-float
            :type tSingle-Float
            :enum-repr nil
            :newtype nil))

          ('coalton:Double-Float
           (type-entry
            :name 'coalton:Double-Float
            :runtime-type 'cl:double-float
            :type tDouble-Float
            :enum-repr nil
            :newtype nil))

          ('coalton:String
           (type-entry
            :name 'coalton:String
            :runtime-type 'cl:string
            :type tString
            :enum-repr nil
            :newtype nil))

          ('coalton:Lisp-Object
           (type-entry
            :name 'coalton:Lisp-Object
            :runtime-type 't
            :type tLisp-Object
            :enum-repr nil
            :newtype nil))

          ('coalton:Arrow
           (type-entry
            :name 'coalton:Arrow
            :runtime-type nil
            :type tArrow
            :enum-repr nil
            :newtype nil))

          ('coalton:List
           (type-entry
            :name 'coalton:List
            :runtime-type 'cl:list
            :type tList
            :enum-repr nil
            :newtype nil)))))

;;;
;;; Constructor environment
;;;

(defstruct constructor-entry
  (name            (required 'name)            :type symbol                         :read-only t)
  (arity           (required 'arity)           :type alexandria:non-negative-fixnum :read-only t)
  (constructs      (required 'constructs)      :type symbol                         :read-only t)
  (scheme          (required 'scheme)          :type ty-scheme                      :read-only t)
  (arguments       (required 'arguments)       :type scheme-list                    :read-only t)
  (classname       (required 'classname)       :type symbol                         :read-only t)

  ;; If this constructor constructs a compressed-repr type then
  ;; compressed-repr is the runtime value of this nullary constructor
  (compressed-repr (required 'compressed-repr) :type t                              :read-only t))

(defmethod make-load-form ((self constructor-entry) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names '(name arity constructs scheme arguments classname compressed-repr)
   :environment env))

#+sbcl
(declaim (sb-ext:freeze-type constructor-entry))

(defun constructor-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'constructor-entry-p x)))

(deftype constructor-entry-list ()
  '(satisfies constructor-entry-list-p))

#+sbcl
(declaim (sb-ext:freeze-type constructor-entry-list))


(defstruct (constructor-environment (:include immutable-map)))

(defun make-default-constructor-environment ()
  "Create a TYPE-ENVIRONMENT containing early constructors"
  (let* ((tvar (make-variable))
         (list-scheme (quantify (list tvar) (qualify nil (%make-tapp tList tvar))))
         (var-scheme (quantify (list tvar) (qualify nil tvar))))
    (make-constructor-environment
     :data (fset:map
            ;; Early Constructors
            ('coalton:True
             (make-constructor-entry
              :name 'coalton:True
              :arity 0
              :constructs 'coalton:Boolean
              :scheme (to-scheme (qualify nil tBoolean))
              :arguments nil
              :classname 'coalton::Boolean/True
              :compressed-repr 't))

            ('coalton:False
             (make-constructor-entry
              :name 'coalton:False
              :arity 0
              :constructs 'coalton:Boolean
              :scheme (to-scheme (qualify nil tBoolean))
              :arguments nil
              :classname 'coalton::Boolean/False
              :compressed-repr 'nil))

            ('coalton:Cons
             (make-constructor-entry
              :name 'coalton:Cons
              :arity 2
              :constructs 'coalton:List
              :scheme list-scheme
              :arguments (list var-scheme list-scheme)
              :classname nil
              :compressed-repr 'nil))

            ('coalton:Nil
             (make-constructor-entry
              :name 'coalton:Nil
              :arity 0
              :constructs 'coalton:List
              :scheme list-scheme
              :arguments nil
              :classname nil
              :compressed-repr 'nil))))))

#+sbcl
(declaim (sb-ext:freeze-type constructor-environment))

;;;
;;; Class environment
;;;

(defstruct
    (ty-class
     (:constructor ty-class (name predicate superclasses unqualified-methods codegen-sym superclass-dict docstring location)))
  (name                (required 'name)                :type symbol              :read-only t)
  (predicate           (required 'predicate)           :type ty-predicate        :read-only t)
  (superclasses        (requried 'superclasses)        :type ty-predicate-list   :read-only t)
  ;; Methods of the class containing the same tyvars in PREDICATE for
  ;; use in pretty printing
  (unqualified-methods (required 'unqualified-methods) :type scheme-binding-list :read-only t)
  (codegen-sym         (required 'codegen-sym)         :type symbol              :read-only t)
  (superclass-dict     (required 'superclass-dict)     :type list                :read-only t)
  (docstring           (required 'docstring)           :type (or null string)    :read-only t)
  (location            (required 'location)            :type t                   :read-only t))

(defmethod make-load-form ((self ty-class) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names '(name predicate superclasses unqualified-methods codegen-sym superclass-dict docstring location)
   :environment env))

#+sbcl
(declaim (sb-ext:freeze-type ty-class))

(defun ty-class-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-class-p x)))

(deftype ty-class-list ()
  '(satisfies ty-class-list-p))

#+sbcl
(declaim (sb-ext:freeze-type ty-class-list))

(defmethod apply-substitution (subst-list (class ty-class))
  (declare (type substitution-list subst-list)
           (values ty-class &optional))
  (ty-class (ty-class-name class)
            (apply-substitution subst-list (ty-class-predicate class))
            (apply-substitution subst-list (ty-class-superclasses class))
            (mapcar (lambda (entry)
                      (cons (car entry)
                            (apply-substitution subst-list (cdr entry))))
                    (ty-class-unqualified-methods class))
            (ty-class-codegen-sym class)
            (mapcar (lambda (entry)
                      (cons (apply-substitution subst-list (car entry))
                            (cdr entry)))
                    (ty-class-superclass-dict class))
            (ty-class-docstring class)
            (ty-class-location class)))

(defstruct (class-environment (:include immutable-map)))

#+sbcl
(declaim (sb-ext:freeze-type class-environment))

;;;
;;; Instance environment
;;;

(defstruct
    (ty-class-instance
     (:constructor ty-class-instance (constraints predicate codegen-sym)))
  (constraints (required 'constraints) :type ty-predicate-list :read-only t)
  (predicate  (required 'predicate)    :type ty-predicate      :read-only t)
  (codegen-sym (required 'codegen-sym) :type symbol            :read-only t))

(defmethod make-load-form ((self ty-class-instance) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names '(constraints predicate codegen-sym)
   :environment env))

#+sbcl
(declaim (sb-ext:freeze-type ty-class-instance))

(defun ty-class-instance-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-class-instance-p x)))

(deftype ty-class-instance-list ()
  `(satisfies ty-class-instance-list-p))

#+sbcl
(declaim (sb-ext:freeze-type ty-class-instance-list))

(defmethod apply-substitution (subst-list (instance ty-class-instance))
  (declare (type substitution-list subst-list)
           (values ty-class-instance &optional))
  (ty-class-instance
   (apply-substitution subst-list (ty-class-instance-constraints instance))
   (apply-substitution subst-list (ty-class-instance-predicate instance))
   (ty-class-instance-codegen-sym instance)))

(defstruct (instance-environment (:include immutable-listmap)))

#+sbcl
(declaim (sb-ext:freeze-type instance-environment))

;;;
;;; Function environment
;;;

(defstruct function-env-entry
  (name  (required 'name)  :type symbol :read-only t)
  (arity (required 'arity) :type fixnum :read-only t))

(defmethod make-load-form ((self function-env-entry) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names '(name arity)
   :environment env))

#+sbcl
(declaim (sb-ext:freeze-type function-env-entry))

(defun function-env-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'function-env-entry-p x)))

(deftype function-env-entry-list ()
  `(satisfies function-env-entry-list-p))

#+sbcl
(declaim (sb-ext:freeze-type function-env-entry-list))

(defstruct (function-environment (:include immutable-map)))

#+sbcl
(declaim (sb-ext:freeze-type function-environment))

;;;
;;; Name environment
;;;

(defstruct name-entry
  (name      (required 'name)      :type symbol                               :read-only t)
  (type      (required 'type)      :type (member :value :method :constructor) :read-only t)
  (docstring (required 'docstring) :type (or null string)                     :read-only t)
  (location  (required 'location)  :type t                                    :read-only t))

(defmethod make-load-form ((self name-entry) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names '(name type docstring location)
   :environment env))

#+sbcl
(declaim (sb-ext:freeze-type name-entry))

(defstruct (name-environment (:include immutable-map)))

#+sbcl
(declaim (sb-ext:freeze-type name-environment))

;;;
;;; Environment
;;;

(defstruct
    (environment
     (:constructor make-environment
         (value-environment
          type-environment
          constructor-environment
          class-environment
          instance-environment
          function-environment
          name-environment)))
  (value-environment       (required 'value-environment)       :type value-environment       :read-only t)
  (type-environment        (required 'type-environment)        :type type-environment        :read-only t)
  (constructor-environment (requried 'constructor-environment) :type constructor-environment :read-only t)
  (class-environment       (required 'class-environment)       :type class-environment       :read-only t)
  (instance-environment    (required 'instance-environment)    :type instance-environment    :read-only t)
  (function-environment    (required 'function-environment)    :type function-environment    :read-only t)
  (name-environment        (required 'name-environment)        :type name-environment       :read-only t))

(defmethod make-load-form ((self environment) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names '(value-environment type-environment constructor-environment class-environment instance-environment function-environment name-environment)
   :environment env))

#+sbcl
(declaim (sb-ext:freeze-type environment))

(defun make-default-environment ()
  (make-environment
   (make-value-environment)
   (make-default-type-environment)
   (make-default-constructor-environment)
   (make-class-environment)
   (make-instance-environment)
   (make-function-environment)
   (make-name-environment)))

(defun update-environment (env
                           &key
                             (value-environment (environment-value-environment env))
                             (type-environment (environment-type-environment env))
                             (constructor-environment (environment-constructor-environment env))
                             (class-environment (environment-class-environment env))
                             (instance-environment (environment-instance-environment env))
                             (function-environment (environment-function-environment env))
                             (name-environment (environment-name-environment env)))
  (declare (type environment env)
           (type value-environment value-environment)
           (type constructor-environment constructor-environment)
           (type class-environment class-environment)
           (type instance-environment instance-environment)
           (type function-environment function-environment)
           (type name-environment name-environment)
           (values environment))
  (make-environment
   value-environment
   type-environment
   constructor-environment
   class-environment
   instance-environment
   function-environment
   name-environment))

(defun environment-diff (env old-env)
  (declare (type environment env)
           (type environment old-env)
           (values environment))
  (make-environment
   (immutable-map-diff (environment-value-environment env)
                      (environment-value-environment old-env)
                      #'make-value-environment)
   (immutable-map-diff (environment-type-environment env)
                      (environment-type-environment old-env)
                      #'make-type-environment)
   (immutable-map-diff (environment-constructor-environment env)
                      (environment-constructor-environment old-env)
                      #'make-constructor-environment)
   (immutable-map-diff (environment-class-environment env)
                      (environment-class-environment old-env)
                      #'make-class-environment)
   (immutable-listmap-diff (environment-instance-environment env)
                     (environment-instance-environment old-env)
                     #'make-instance-environment)
   (immutable-map-diff (environment-function-environment env)
                      (environment-function-environment old-env)
                      #'make-function-environment)
   (immutable-map-diff (environment-name-environment env)
                      (environment-name-environment old-env)
                      #'make-name-environment)))

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

(defun lookup-value-type (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (immutable-map-lookup (environment-value-environment env) symbol)
      (unless no-error
        (let* ((sym-name (symbol-name symbol))
               (valid-bindings (coalton-impl/algorithm::immutable-map-keys (environment-value-environment env)))
               (matches (remove-if-not (lambda (s) (string= (symbol-name s) sym-name)) valid-bindings)))
          (error 'unknown-binding-error :symbol symbol :alternatives matches)))))

(defun set-value-type (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type ty-scheme value)
           (values environment &optional))
  (update-environment
   env
   :value-environment (immutable-map-set
                       (environment-value-environment env)
                       symbol
                       value
                       #'make-value-environment)))

(defun lookup-type (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (immutable-map-lookup (environment-type-environment env) symbol)
      (unless no-error
        (let* ((sym-name (symbol-name symbol))
               (valid-types (coalton-impl/algorithm::immutable-map-keys (environment-type-environment env)))
               (matches (remove-if-not (lambda (s) (string= (symbol-name s) sym-name)) valid-types)))
          (cond
            ((= 1 (length sym-name))
             (error "Unknown type ~S. Did you mean the type variable ~S?" symbol (intern sym-name 'keyword)))
            (matches
             (error "Unknown type ~S. Did you mean ~{~S~^, ~}?" symbol matches))
            (t
             (error "Unknown type ~S" symbol)))))))

(defun set-type (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type type-entry value)
           (values environment &optional))
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
        (error "Unknown constructor ~S." symbol))))

(defun set-constructor (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type constructor-entry value)
           (values environment &optional))
  (update-environment
   env
   :constructor-environment (immutable-map-set
                             (environment-constructor-environment env)
                             symbol
                             value
                             #'make-constructor-environment)))

(defun lookup-class (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (immutable-map-lookup (environment-class-environment env) symbol)
      (unless no-error
        (error "Unknown class ~S." symbol))))

(defun set-class (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type ty-class value)
           (values environment &optional))
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
        (error "Unknown function ~S." symbol))))

(defun set-function (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type function-env-entry value)
           (values environment &optional))
  (update-environment
   env
   :function-environment (immutable-map-set
                          (environment-function-environment env)
                          symbol
                          value
                          #'make-function-environment)))

(defun unset-function (env symbol)
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

(defun set-name (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type name-entry value)
           (values environment &optional))
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

(defun lookup-class-instances (env class &key no-error)
  (declare (type environment env)
           (type symbol class)
           (values fset:seq &optional))
  (immutable-listmap-lookup (environment-instance-environment env) class :no-error no-error))

(defun lookup-class-instance (env pred &key no-error)
  (declare (type environment env))
  (let* ((pred-class (ty-predicate-class pred))
         (instances (lookup-class-instances env pred-class :no-error no-error)))
    (fset:do-seq (instance instances :index index)
      (declare (ignore index))
      (handler-case
          (let ((subs (predicate-match (ty-class-instance-predicate instance) pred)))
            (return-from lookup-class-instance (values instance subs)))
        (predicate-unification-error () nil)))
    (unless no-error
      (error "Unknown instance for predicate ~A" pred))))


(defun push-value-environment (env value-types)
  (declare (type environment env)
           (type scheme-binding-list value-types)
           (values environment &optional))
  (update-environment
   env
   :value-environment (immutable-map-set-multiple
                       (environment-value-environment env)
                       value-types
                       #'make-value-environment)))

(defun push-type-environment (env types)
  (declare (type environment env)
           (type list types)
           (values environment &optional))
  (update-environment
   env
   :type-environment (immutable-map-set-multiple
                      (environment-type-environment env)
                      types
                      #'make-type-environment)))

(defun push-constructor-environment (env constructors)
  (declare (type environment env)
           (type constructor-entry-list constructors)
           (values environment &optional))
  (update-environment
   env
   :constructor-environment (immutable-map-set-multiple
                             (environment-constructor-environment env)
                             constructors
                             #'make-constructor-environment)))

(defun push-function-environment (env functions)
  (declare (type environment env)
           (type function-env-entry-list functions)
           (values environment &optional))
  (update-environment
   env
   :function-environment (immutable-map-set-multiple
                          (environment-function-environment env)
                          functions
                          #'make-function-environment)))

(defun add-class (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type ty-class value)
           (values environment &optional))
  ;; Ensure this class does not already exist
  (when (lookup-class env symbol :no-error t)
    (error "Class ~S already exists." symbol))
  ;; Ensure all super classes exist
  (dolist (sc (ty-class-superclasses value))
    (unless (lookup-class env (ty-predicate-class sc) :no-error t)
      (error "Superclass ~S is not defined." sc)))
  (set-class env symbol value))

(defun add-instance (env class value)
  (declare (type environment env)
           (type symbol class)
           (type ty-class-instance value)
           (values environment &optional))
  ;; Ensure the class is defined
  (unless (lookup-class env class)
    (error "Class ~S does not exist." class))

  (fset:do-seq (inst (lookup-class-instances env class :no-error t) :index index)
    (when (handler-case (or (predicate-mgu (ty-class-instance-predicate value)
                                           (ty-class-instance-predicate inst))
                            t)
            (predicate-unification-error () nil))

      ;; If we have the same instance then simply overwrite the old one
      (if (type-predicate= (ty-class-instance-predicate value)
                           (ty-class-instance-predicate inst))
          (return-from add-instance
            (update-environment
             env
             :instance-environment (immutable-listmap-replace
                                    (environment-instance-environment env)
                                    class
                                    index
                                    value
                                    #'make-instance-environment)))
          (error 'overlapping-instance-error
                 :inst1 (ty-class-instance-predicate value)
                 :inst2 (ty-class-instance-predicate inst)))))

  (update-environment
   env
   :instance-environment (immutable-listmap-push
                          (environment-instance-environment env)
                          class
                          value
                          #'make-instance-environment)))
;;;
;;; Directly applicable functions
;;;

(defun directly-applicable-functions (env)
  (mapcar
   (lambda (f) (cons (function-env-entry-name f) (function-env-entry-arity f)))
   (fset:convert 'list (fset:range (immutable-map-data (environment-function-environment env))))))

;;;
;;; Generating environment update code
;;;

(defun generate-environment-update (env-diff env)
  (let ((type-table (immutable-map-data (environment-type-environment env-diff)))
        (value-table (immutable-map-data (environment-value-environment env-diff)))
        (constructor-table (immutable-map-data (environment-constructor-environment env-diff)))
        (class-table (immutable-map-data (environment-class-environment env-diff)))
        (function-table (immutable-map-data (environment-function-environment env-diff)))
        (instance-table (immutable-listmap-data (environment-instance-environment env-diff)))
        (name-table (immutable-map-data (environment-name-environment env-diff)))
        (forms nil))

    ;; Tell the world about our types
    (fset:do-map (k v type-table)
      (push `(setf ,env (set-type ,env ',k ,v)) forms))
    ;; Store all the values
    (fset:do-map (k v value-table)
      (push `(setf ,env (set-value-type ,env ',k ,v)) forms))
    ;; Constructor
    (fset:do-map (k v constructor-table)
      (push `(setf ,env (set-constructor ,env ',k ,v)) forms))
    ;; Classes
    (fset:do-map (k v class-table)
      (push `(setf ,env (set-class ,env ',k ,v)) forms))
    ;; Functions
    (fset:do-map (k v function-table)
      (push `(setf ,env (set-function ,env ',k ,v)) forms))
    ;; Instance
    (fset:do-map (k v instance-table)
      (fset:do-seq (inst v)
        (push `(setf ,env (add-instance ,env ',k ,inst)) forms)))
    ;; Names
    (fset:do-map (k v name-table)
      (push `(setf ,env (set-name ,env ',k ,v)) forms))

    `(eval-when (:load-toplevel)
       ,@(reverse forms))))

;;;
;;; Pretty printing
;;;

;; Print the environment as an undreadable object because otherwise
;; it is too big
(defun pprint-env (stream env &optional colon-p at-sign-p)
  (declare (type stream stream)
           (type environment env)
           (ignore colon-p)
           (ignore at-sign-p)
           (values environment))
  (print-unreadable-object (env stream :type t :identity t))
  env)

(set-pprint-dispatch 'environment 'pprint-env)
