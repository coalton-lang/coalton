(in-package #:coalton-impl/typechecker)

;;;
;;; Value type environments
;;;

(serapeum:defstruct-read-only (value-environment (:include shadow-realm)))

#+sbcl
(declaim (sb-ext:freeze-type value-environment))

(defmethod apply-substitution (subst-list (env value-environment))
  (make-value-environment :data (fset:image (lambda (key value)
                  (values key (apply-substitution subst-list value)))
                (shadow-realm-data env))))

(defmethod type-variables ((env value-environment))
  (remove-duplicates (mapcan #'type-variables (fset:convert 'list (fset:range (shadow-realm-data env)))) :test #'equalp))

;;;
;;; Type environments
;;;

(serapeum:defstruct-read-only (type-environment (:include shadow-realm)))

#+sbcl
(declaim (sb-ext:freeze-type type-environment))


(defun make-default-type-environment ()
  "Create a TYPE-ENVIRONMENT containing early types"
  (make-type-environment
   :data (fset:map
          ;; Early Types
          ('coalton-library:Unit    tUnit)
          ('coalton-library:Boolean tBoolean)
          ('coalton:Char tChar)

          ('coalton:I32 tI32)
          ('coalton:I64 tI64)
          ('coalton:U8  tU8)
          ('coalton:U32 tU32)
          ('coalton:U64 tU64)
          ('coalton:Integer tInteger)
          ('coalton:Single-Float tSingle-Float)
          ('coalton:Double-Float tDouble-Float)

          ('coalton:String tString)

          ('coalton:Lisp-Object tLisp-Object)
          ('coalton:Arrow tArrow))))

;;;
;;; Constructor environment
;;;

(serapeum:defstruct-read-only constructor-entry
  (name :type symbol)
  (arity :type alexandria:non-negative-fixnum)
  (constructs  :type symbol)
  (scheme :type ty-scheme)
  (arguments :type scheme-list)
  ;; CLASSNAME is either a structure class name (a symbol), or the
  ;; list (:TYPE <cl-type>) for a given Common Lisp type <cl-type>.
  ;;
  ;; The :TYPE form is only valid if ARITY = 0.
  (classname :type (or symbol (cons (member :type)))))

#+sbcl
(declaim (sb-ext:freeze-type constructor-entry))

(defun constructor-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'constructor-entry-p x)))

(deftype constructor-entry-list ()
  '(satisfies constructor-entry-list-p))

#+sbcl
(declaim (sb-ext:freeze-type constructor-entry-list))


(serapeum:defstruct-read-only (constructor-environment (:include shadow-realm)))

#+sbcl
(declaim (sb-ext:freeze-type constructor-environment))

(defun make-default-constructor-environment ()
  (make-constructor-environment
   :data (fset:map
          ('coalton-library:Unit (make-constructor-entry
                                  :name 'coalton-library:Unit
                                  :arity 0
                                  :constructs 'coalton-library:Unit
                                  :scheme (to-scheme
                                           (qualify '() tUnit))
                                  :arguments '()
                                  :classname '(:type (member coalton-library:Unit))))
          ('coalton-library:False (make-constructor-entry
                                   :name 'coalton-library:False
                                   :arity 0
                                   :constructs 'coalton-library:Boolean
                                   :scheme (to-scheme
                                            (qualify '() tBoolean))
                                   :arguments '()
                                   :classname '(:type (member cl:nil))))
          ('coalton-library:True (make-constructor-entry
                                  :name 'coalton-library:True
                                  :arity 0
                                  :constructs 'coalton-library:Boolean
                                  :scheme (to-scheme
                                           (qualify '() tBoolean))
                                  :arguments '()
                                  :classname '(:type (member cl:t)))))))

;;;
;;; Class environment
;;;

(serapeum:defstruct-read-only
    (ty-class
     (:constructor ty-class (name predicate superclasses unqualified-methods codegen-sym superclass-dict docstring location)))
  (name :type symbol)
  (predicate :type ty-predicate)
  (superclasses :type ty-predicate-list)
  ;; Methods of the class containing the same tyvars in PREDICATE for
  ;; use in pretty printing
  (unqualified-methods :type scheme-binding-list)
  (codegen-sym :type symbol)
  (superclass-dict :type list)
  (docstring :type (or null string))
  (location :type t))

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

(serapeum:defstruct-read-only (class-environment (:include shadow-realm)))

#+sbcl
(declaim (sb-ext:freeze-type class-environment))

;;;
;;; Instance environment
;;;

(serapeum:defstruct-read-only
    (ty-class-instance
     (:constructor ty-class-instance (constraints predicate codegen-sym)))
  (constraints :type ty-predicate-list)
  (predicate :type ty-predicate)
  (codegen-sym :type symbol))

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

(serapeum:defstruct-read-only (instance-environment (:include shadow-list)))

#+sbcl
(declaim (sb-ext:freeze-type instance-environment))

;;;
;;; Function environment
;;;

(serapeum:defstruct-read-only function-env-entry
  (name :type symbol)
  (arity :type fixnum))

#+sbcl
(declaim (sb-ext:freeze-type function-env-entry))

(defun function-env-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'function-env-entry-p x)))

(deftype function-env-entry-list ()
  `(satisfies function-env-entry-list-p))

#+sbcl
(declaim (sb-ext:freeze-type function-env-entry-list))

(serapeum:defstruct-read-only (function-environment (:include shadow-realm)))

#+sbcl
(declaim (sb-ext:freeze-type function-environment))

;;;
;;; Name environment
;;;

(serapeum:defstruct-read-only name-entry
  (name :type symbol)
  (type :type (member :value :method :constructor))
  (docstring :type (or null string))
  (location :type t))

#+sbcl
(declaim (sb-ext:freeze-type name-entry))

(serapeum:defstruct-read-only (name-environment (:include shadow-realm)))

#+sbcl
(declaim (sb-ext:freeze-type name-environment))

;;;
;;; Environment
;;;

(serapeum:defstruct-read-only
    (environment
     (:constructor make-environment
         (value-environment
          type-environment
          constructor-environment
          class-environment
          instance-environment
          function-environment
          name-environment)))
  (value-environment       :type value-environment)
  (type-environment        :type type-environment)
  (constructor-environment :type constructor-environment)
  (class-environment       :type class-environment)
  (instance-environment    :type instance-environment)
  (function-environment    :type function-environment)
  (name-environment        :type name-environment))

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
   (shadow-realm-diff (environment-value-environment env)
                      (environment-value-environment old-env)
                      #'make-value-environment)
   (shadow-realm-diff (environment-type-environment env)
                      (environment-type-environment old-env)
                      #'make-type-environment)
   (shadow-realm-diff (environment-constructor-environment env)
                      (environment-constructor-environment old-env)
                      #'make-constructor-environment)
   (shadow-realm-diff (environment-class-environment env)
                      (environment-class-environment old-env)
                      #'make-class-environment)
   (shadow-list-diff (environment-instance-environment env)
                     (environment-instance-environment old-env)
                     #'make-instance-environment)
   (shadow-realm-diff (environment-function-environment env)
                      (environment-function-environment old-env)
                      #'make-function-environment)
   (shadow-realm-diff (environment-name-environment env)
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
  (or (shadow-realm-lookup (environment-value-environment env) symbol)
      (unless no-error
        (let* ((sym-name (symbol-name symbol))
               (valid-bindings (coalton-impl/algorithm::shadow-realm-keys (environment-value-environment env)))
               (matches (remove-if-not (lambda (s) (string= (symbol-name s) sym-name)) valid-bindings)))
          (error 'unknown-binding-error :symbol symbol :alternatives matches)))))

(defun set-value-type (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type ty-scheme value)
           (values environment &optional))
  (update-environment
   env
   :value-environment (shadow-realm-set
                       (environment-value-environment env)
                       symbol
                       value
                       #'make-value-environment)))

(defun lookup-type (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (shadow-realm-lookup (environment-type-environment env) symbol)
      (unless no-error
        (let* ((sym-name (symbol-name symbol))
               (valid-types (coalton-impl/algorithm::shadow-realm-keys (environment-type-environment env)))
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
           (type ty value)
           (values environment &optional))
  (update-environment
   env
   :type-environment (shadow-realm-set
                       (environment-type-environment env)
                       symbol
                       value
                       #'make-type-environment)))

(defun lookup-constructor (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (shadow-realm-lookup (environment-constructor-environment env) symbol)
      (unless no-error
        (error "Unknown constructor ~S." symbol))))

(defun set-constructor (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type constructor-entry value)
           (values environment &optional))
  (update-environment
   env
   :constructor-environment (shadow-realm-set
                             (environment-constructor-environment env)
                             symbol
                             value
                             #'make-constructor-environment)))

(defun lookup-class (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (shadow-realm-lookup (environment-class-environment env) symbol)
      (unless no-error
        (error "Unknown class ~S." symbol))))

(defun set-class (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type ty-class value)
           (values environment &optional))
  (update-environment
   env
   :class-environment (shadow-realm-set
                       (environment-class-environment env)
                       symbol
                       value
                       #'make-class-environment)))

(defun lookup-function (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (shadow-realm-lookup (environment-function-environment env) symbol)
      (unless no-error
        (error "Unknown function ~S." symbol))))

(defun set-function (env symbol value)
  (declare (type environment env)
           (type symbol symbol)
           (type function-env-entry value)
           (values environment &optional))
  (update-environment
   env
   :function-environment (shadow-realm-set
                          (environment-function-environment env)
                          symbol
                          value
                          #'make-function-environment)))

(defun unset-function (env symbol)
  (declare (type environment env)
           (type symbol symbol))
  (update-environment
   env
   :function-environment (shadow-realm-remove
                          (environment-function-environment env)
                          symbol
                          #'make-function-environment)))

(defun lookup-name (env symbol &key no-error)
  (declare (type environment env)
           (type symbol symbol))
  (or (shadow-realm-lookup (environment-name-environment env) symbol)
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
         :name-environment (shadow-realm-set
                            (environment-name-environment env)
                            symbol
                            value
                            #'make-name-environment)))))

(defun lookup-class-instances (env class &key no-error)
  (declare (type environment env)
           (type symbol class)
           (values fset:seq &optional))
  (shadow-list-lookup (environment-instance-environment env) class :no-error no-error))

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
   :value-environment (shadow-realm-push-frame
                       (environment-value-environment env)
                       value-types
                       #'make-value-environment)))

(defun push-type-environment (env types)
  (declare (type environment env)
           (type list types)
           (values environment &optional))
  (update-environment
   env
   :type-environment (shadow-realm-push-frame
                      (environment-type-environment env)
                      types
                      #'make-type-environment)))

(defun push-constructor-environment (env constructors)
  (declare (type environment env)
           (type constructor-entry-list constructors)
           (values environment &optional))
  (update-environment
   env
   :constructor-environment (shadow-realm-push-frame
                             (environment-constructor-environment env)
                             constructors
                             #'make-constructor-environment)))

(defun push-function-environment (env functions)
  (declare (type environment env)
           (type function-env-entry-list functions)
           (values environment &optional))
  (update-environment
   env
   :function-environment (shadow-realm-push-frame
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
             :instance-environment (shadow-list-replace
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
   :instance-environment (shadow-list-push
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
   (fset:convert 'list (fset:range (shadow-realm-data (environment-function-environment env))))))

;;;
;;; Generating environment update code
;;;

(defun generate-environment-update (env-diff env)
  (let ((type-table (shadow-realm-data (environment-type-environment env-diff)))
        (value-table (shadow-realm-data (environment-value-environment env-diff)))
        (constructor-table (shadow-realm-data (environment-constructor-environment env-diff)))
        (class-table (shadow-realm-data (environment-class-environment env-diff)))
        (function-table (shadow-realm-data (environment-function-environment env-diff)))
        (instance-table (shadow-list-data (environment-instance-environment env-diff)))
        (name-table (shadow-realm-data (environment-name-environment env-diff)))
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
