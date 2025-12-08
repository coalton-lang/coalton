(defpackage #:coalton-impl/typechecker/types
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/kinds)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:settings #:coalton-impl/settings))
  (:export
   #:ty                                 ; STRUCT
   #:ty-alias                           ; ACCESSOR
   #:ty-list                            ; TYPE
   #:tyvar                              ; STRUCT
   #:make-tyvar                         ; CONSTRUCTOR
   #:tyvar-id                           ; ACCESSOR
   #:tyvar-kind                         ; ACCESSOR
   #:tyvar-p                            ; FUNCTION
   #:tyvar-list                         ; TYPE
   #:tycon                              ; STRUCT
   #:make-tycon                         ; CONSTRUCTOR
   #:tycon-name                         ; ACCESSOR
   #:tycon-kind                         ; ACCESSOR
   #:tycon-p                            ; FUNCTION
   #:tapp                               ; STRUCT
   #:make-tapp                          ; CONSTRUCTOR
   #:tapp-from                          ; ACCESSOR
   #:tapp-to                            ; ACCESSOR
   #:tapp-p                             ; FUNCTION
   #:tgen                               ; STRUCT
   #:make-tgen                          ; CONSTRUCTOR
   #:tgen-id                            ; ACCESSOR
   #:tgen-p                             ; FUNCTION
   #:make-variable                      ; FUNCTION
   #:fresh-type-renamer                 ; FUNCTION
   #:instantiate                        ; FUNCTION
   #:kind-of                            ; FUNCTION
   #:type-constructors                  ; FUNCTION
   #:ty=                                ; FUNCTION
   #:*boolean-type*                     ; VARIABLE
   #:*unit-type*                        ; VARIABLE
   #:*char-type*                        ; VARIABLE
   #:*integer-type*                     ; VARIABLE
   #:*ifix-type*                        ; VARIABLE
   #:*ufix-type*                        ; VARIABLE
   #:*single-float-type*                ; VARIABLE
   #:*double-float-type*                ; VARIABLE
   #:*string-type*                      ; VARIABLE
   #:*fraction-type*                    ; VARIABLE
   #:*arrow-type*                       ; VARIABLE
   #:*list-type*                        ; VARIABLE
   #:*optional-type*                    ; VARIABLE
   #:push-type-alias                    ; FUNCTION
   #:flatten-type                       ; FUNCTION
   #:apply-type-argument                ; FUNCTION
   #:apply-type-argument-list           ; FUNCTION
   #:make-function-type                 ; FUNCTION
   #:make-function-type*                ; FUNCTION
   #:function-type-p                    ; FUNCTION
   #:function-type-from                 ; FUNCTION
   #:function-type-to                   ; FUNCTION
   #:function-type-arity                ; FUNCTION
   #:function-type-arguments            ; FUNCTION
   #:function-return-type               ; FUNCTION
   #:function-remove-arguments          ; FUNCTION
   #:type-variables                     ; FUNCTION
   #:next-pprint-variable               ; FUNCTION
   #:next-pprint-variable-as-tvar       ; FUNCTION
   #:pprint-tvar                        ; FUNCTION
   #:pprint-ty                          ; FUNCTION
   #:ty->string                         ; FUNCTION
   #:type-application-error             ; CONDITION
   ))

(in-package #:coalton-impl/typechecker/types)

;;;;
;;;; Type System Core
;;;;
;;;; This module defines the type structures used by the Coalton
;;;; type checker. The type system is based on Hindley-Milner type
;;;; inference, with extensions for type classes and higher-kinded
;;;; types.
;;;;
;;;; Types are represented as a hierarchy of structures, all inheriting from `ty`:
;;;;
;;;; - Tyvar: Type variables (e.g., :a, :b) with associated kinds
;;;;   Used during type inference to represent unknown types that will be unified
;;;;
;;;; - Tycon: Type constructors (e.g., Integer, String, Maybe)
;;;;   Concrete types that can be applied to arguments
;;;;
;;;; - Tapp: Type applications (e.g., Maybe Integer, List String)
;;;;   Represents applying a type constructor to arguments
;;;;
;;;; - Tgen: Generic type variables (e.g., in polymorphic schemes)
;;;;   Used in type schemes to represent universally quantified variables
;;;;
;;;; All type structures include an `alias` field that tracks the
;;;; chain of type aliases used to reach the current type. This
;;;; preserves user-written type names in error messages and
;;;; documentation while working with the underlying representation.
;;;;
;;;; Every type has an associated kind that describes its "type of types":
;;;;
;;;; - * (star): Concrete types like Integer, String
;;;; - * -> *: Type constructors taking one argument like Maybe, List
;;;; - (* -> *) -> * -> *: Higher-order constructors like Monad
;;;;

;;;
;;; Types
;;;

(defstruct (ty (:constructor nil))
  ;; When this field is not null, it comprises a head which is the
  ;; explicit type-alias used, and a tail which consists of the
  ;; type-aliases used to define the explicit alias.
  ;; for example:
  ;;   (define-type-alias T1 T)
  ;;   (define-type-alias T2 T1)
  ;;   (declare x T2)
  ;;   (define x ...)
  ;; the type of x will be T, with the alias field
  ;; populated with (Cons T2 (Cons T1 Nil)).
  ;;
  ;; Could be replaced by a weak hash table.
  (alias nil :type (or null ty-list) :read-only nil))

(defmethod make-load-form ((self ty) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun ty-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-p x)))

(deftype ty-list ()
  '(satisfies ty-list-p))

(defstruct (tyvar (:include ty))
  (id   (util:required 'id)   :type fixnum :read-only t)
  (kind (util:required 'kind) :type kind   :read-only t))

(defun tyvar-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'tyvar-p x)))

(deftype tyvar-list ()
  '(satisfies tyvar-list-p))

(defstruct (tycon (:include ty))
  (name (util:required 'name) :type symbol :read-only t)
  (kind (util:required 'kind) :type kind   :read-only t))

(defstruct (tapp (:include ty))
  (from (util:required 'from) :type ty :read-only t)
  (to   (util:required 'to)   :type ty :read-only t))

(defstruct (tgen (:include ty))
  (id (util:required 'id) :type fixnum :read-only t))

(defmethod make-load-form ((self tgen) &optional env)
  (make-load-form-saving-slots self :environment env))

;;;
;;; Type Variables
;;;

(defparameter *next-variable-id* 0)

#+sbcl
(declaim (sb-ext:always-bound *next-variable-id*))

(declaim (ftype (function (&optional kind) tyvar) make-variable))
(declaim (inline make-variable))
(defun make-variable (&optional (kind +kstar+))
  "Create a fresh type variable with the specified kind.

KIND defaults to +kstar+ (* kind) for concrete types. The returned type variable
has a globally unique ID that distinguishes it from all other variables created
during compilation.

This function is the primary way to generate type variables during type inference.
Each call returns a distinct variable, even if called with the same kind.

Usage in type inference:
- Generate unknowns to be unified with concrete types
- Create placeholder types for function parameters
- Represent polymorphic variables during scheme instantiation

Examples:
  (make-variable)          ; Creates :a with kind *
  (make-variable +karrow+) ; Creates :b with kind * -> *"
  (prog1 (make-tyvar :id *next-variable-id* :kind kind)
    (incf *next-variable-id*)))

(declaim (ftype (function (tyvar) tyvar) fresh-type-renamer))
(defun fresh-type-renamer (tyvar)
  "Create a fresh type variable with the same kind as the given type variable.

TYVAR must be a type variable (tyvar-p must return true). This function is used
during type scheme instantiation to create fresh copies of bound type variables,
ensuring that each use of a polymorphic function gets distinct type variables.

Without fresh renaming, different uses of the same polymorphic function would
share type variables, leading to incorrect unification and overly restrictive types.

Example usage in scheme instantiation:
  Original scheme: ∀ a. a -> a
  First instantiation: :b -> :b (where :b is fresh)
  Second instantiation: :c -> :c (where :c is fresh, distinct from :b)

The function preserves the kind of the original variable, so if TYVAR has kind
* -> *, the returned variable will also have kind * -> *."
  (make-variable (kind-of tyvar)))

;;;
;;; Methods
;;;

(defgeneric instantiate (types type)
  (:documentation "Instantiate a type scheme by replacing generic type variables (tgen) with concrete types.

TYPES is a list of types to substitute for generic variables, where the position
in the list corresponds to the tgen-id of the generic variable being replaced.

TYPE is the type or type structure to instantiate. Generic variables (tgen instances)
are replaced with the corresponding type from TYPES, while other type structures
are recursively instantiated.

This is used during type scheme instantiation to create concrete instances of
polymorphic types. For example, when using a function with type ∀ a b. a -> b -> a,
we instantiate it with fresh type variables to get something like :c -> :d -> :c.

Returns a new type structure with all tgen instances replaced by their corresponding
types from the TYPES list.")
  (:method (types (type tapp))
    (make-tapp
     :alias (mapcar (lambda (alias) (instantiate types alias)) (ty-alias type))
     :from (instantiate types (tapp-from type))
     :to (instantiate types (tapp-to type))))
  (:method (types (type tgen))
    (nth (tgen-id type) types))
  (:method (types (type ty))
    type)
  (:method (types (type list))
    (mapcar (lambda (type) (instantiate types type)) type)))

(defgeneric kind-of (type)
  (:documentation "Get the kind of the given type.

Kinds classify types in the same way that types classify values. The most common kinds are:
- * (star): The kind of concrete types like Integer, String, Boolean
- * -> *: The kind of type constructors taking one argument, like Maybe, List  
- * -> * -> *: The kind of type constructors taking two arguments, like Either, Map
- (* -> *) -> *: The kind of higher-order type constructors like Monad

TYPE can be any type structure (tyvar, tycon, tapp, tgen). For type applications (tapp),
the kind is computed by applying the kind function (treating kinds as types and following
the same application rules).

This function is used for kind checking during type definition
processing and ensuring that type applications are well-kinded.

Examples:
  (kind-of *integer-type*)     => +kstar+
  (kind-of *list-type*)        => +karrow+ (i.e., * -> *)
  (kind-of (make-variable))    => +kstar+ (by default)

Throws an error if applied to a malformed type application.")
  (:method ((type tyvar))
    (tyvar-kind type))
  (:method ((type tycon))
    (tycon-kind type))
  (:method ((type tapp))
    (let ((from-kind (kind-of (tapp-from type))))
      (if (kfun-p from-kind)
          (kfun-to from-kind)
          (util:coalton-bug "Malformed type application")))))

(defmethod apply-ksubstitution (subs (type tyvar))
  (make-tyvar
   :alias (mapcar (lambda (alias) (apply-ksubstitution subs alias)) (ty-alias type))
   :id (tyvar-id type)
   :kind (apply-ksubstitution subs (tyvar-kind type))))

(defmethod apply-ksubstitution (subs (type tycon))
  (make-tycon
   :alias (mapcar (lambda (alias) (apply-ksubstitution subs alias)) (ty-alias type))
   :name (tycon-name type)
   :kind (apply-ksubstitution subs (tycon-kind type))))

(defmethod apply-ksubstitution (subs (type tapp))
  (make-tapp
   :alias (mapcar (lambda (alias) (apply-ksubstitution subs alias)) (ty-alias type))
   :from (apply-ksubstitution subs (tapp-from type))
   :to (apply-ksubstitution subs (tapp-to type))))

(defmethod kind-variables-generic% ((type tyvar))
  (kind-variables-generic% (kind-of type)))

(defmethod kind-variables-generic% ((type tycon))
  (kind-variables-generic% (kind-of type)))

(defmethod kind-variables-generic% ((type tapp))
  (append
   (kind-variables-generic% (tapp-to type))
   (kind-variables-generic% (tapp-from type))))

(declaim (ftype (function (t) util:symbol-list) type-constructors))
(defun type-constructors (type)
  "Extract all type constructor names from a type expression.

TYPE can be any type structure (tyvar, tycon, tapp, or list of types).
Returns a list of symbols representing all type constructors used in the type,
with duplicates removed.

This is used for dependency analysis during type checking - knowing which
type constructors are referenced allows the compiler to determine processing
order and check that all referenced types are defined.

Examples:
  (type-constructors *integer-type*)           => (Integer)
  (type-constructors (make-tapp *maybe-type*   => (Maybe Integer)
                               *integer-type*))
  (type-constructors (make-tyvar 42 +kstar+)) => ()  ; variables have no constructors"
  (declare (values util:symbol-list &optional))
  (remove-duplicates (type-constructors-generic% type)))

(defgeneric type-constructors-generic% (type)
  (:method ((type tyvar))
    nil)

  (:method ((type tycon))
    (list (tycon-name type)))

  (:method ((type tapp))
    (append
     (type-constructors-generic% (tapp-from type))
     (type-constructors-generic% (tapp-to type))))

  (:method ((lst list))
    (mapcan #'type-constructors-generic% lst)))

(defgeneric ty= (type1 type2)
  (:documentation "Are TYPE1 to TYPE2 EQUALP, ignoring their aliases.")

  (:method ((type1 tyvar) (type2 tyvar))
    (and (equalp (tyvar-id type1)
                 (tyvar-id type2))
         (equalp (tyvar-kind type1)
                 (tyvar-kind type2))))

  (:method ((type1 tycon) (type2 tycon))
    (and (equalp (tycon-name type1)
                 (tycon-name type2))
         (equalp (tycon-kind type1)
                 (tycon-kind type2))))

  (:method ((type1 tapp) (type2 tapp))
    (and (ty= (tapp-from type1)
              (tapp-from type2))
         (ty= (tapp-to type1)
              (tapp-to type2))))

  (:method ((type1 tgen) (type2 tgen))
    (equalp (tgen-id type1)
            (tgen-id type2)))

  (:method (type1 type2)
    (declare (ignore type1 type2))
    nil))

;;;
;;; Early types
;;;

(defvar *boolean-type*      (make-tycon :name 'coalton:Boolean      :kind +kstar+))
(defvar *unit-type*         (make-tycon :name 'coalton:Unit         :kind +kstar+))
(defvar *char-type*         (make-tycon :name 'coalton:Char         :kind +kstar+))
(defvar *integer-type*      (make-tycon :name 'coalton:Integer      :kind +kstar+))
(defvar *ifix-type*         (make-tycon :name 'coalton:IFix         :kind +kstar+))
(defvar *ufix-type*         (make-tycon :name 'coalton:UFix         :kind +kstar+))
(defvar *single-float-type* (make-tycon :name 'coalton:F32          :kind +kstar+))
(defvar *double-float-type* (make-tycon :name 'coalton:F64          :kind +kstar+))
(defvar *string-type*       (make-tycon :name 'coalton:String       :kind +kstar+))
(defvar *fraction-type*     (make-tycon :name 'coalton:Fraction     :kind +kstar+))
(defvar *arrow-type*        (make-tycon :name 'coalton:Arrow        :kind (make-kfun :from +kstar+ :to (make-kfun :from +kstar+ :to +kstar+))))
(defvar *list-type*         (make-tycon :name 'coalton:List         :kind (make-kfun :from +kstar+ :to +kstar+)))
(defvar *optional-type*     (make-tycon :name 'coalton:Optional     :kind (make-kfun :from +kstar+ :to +kstar+)))

;;;
;;; Operations on Types
;;;

(defun push-type-alias (type alias)
  "Update the alias field of TYPE with ALIAS as the most high-level alias."
  (declare (type ty type)
           (type ty alias)
           (values ty &optional))
  (let ((new-type (copy-structure type)))
    (push alias (ty-alias new-type))
    new-type))

(defun flatten-type (type)
  "If TYPE is a TAPP of the form ((((T1 T2) T3) T4) ...), then return
the list (T1 T2 T3 T4 ...). Otherwise, return (LIST TYPE)."
  (declare (type ty type)
           (values ty-list &optional))
  (let ((flattened-type nil))
    (loop :for from := type :then (tapp-from from)
          :while (typep from 'tapp)
          :do (push (tapp-to from) flattened-type)
          :finally (push from flattened-type))
    flattened-type))

(defun apply-type-argument (tcon arg &key ksubs)
  (declare (type (or tycon tapp tyvar) tcon)
           (type ty arg)
           (values tapp ksubstitution-list &optional))
  (handler-case
      (let ((ksubs (kunify
                    (kind-of (apply-ksubstitution ksubs tcon))
                    (make-kfun :from (kind-of (apply-ksubstitution ksubs arg)) :to (make-kvariable))
                    ksubs)))
        (values
         (make-tapp :from tcon :to arg)
         ksubs))
    (kunify-error (e)
      (declare (ignore e))
      (error 'type-application-error :type tcon :argument arg))))

(defun apply-type-argument-list (tcon args &key ksubs)
  (declare (type ty tcon)
           (type ty-list args)
           (type ksubstitution-list ksubs)
           (values ty ksubstitution-list &optional))
  (labels ((%apply-type-argument-list (tcon args ksubs)
             (if args
                 (multiple-value-bind (tcon ksubs)
                     (%apply-type-argument-list tcon (cdr args) ksubs)
                   (apply-type-argument tcon (car args) :ksubs ksubs))
                 (values tcon ksubs))))
    (%apply-type-argument-list tcon (reverse args) ksubs)))

(defun make-function-type (from to)
  (declare (type ty from to)
           (values ty))
  (unless (kstar-p (kind-of from))
    (util:coalton-bug "Unable to construct function with type ~S of kind ~S" from (kind-of from)))
  (unless (kstar-p (kind-of to))
    (util:coalton-bug "Unable to construct function with type ~S of kind ~S" to (kind-of to)))
  (make-tapp :from (make-tapp :from *arrow-type* :to from) :to to))

(defun make-function-type* (args to)
  (declare (type ty-list args)
           (type ty to)
           (values ty &optional))
  (if (null args)
      to
      (make-function-type (car args)
                          (make-function-type* (cdr args) to))))

(defgeneric function-type-p (ty)
  (:method ((ty ty))
    (declare (type ty ty))
    (and (tapp-p ty)
         (tapp-p (tapp-from ty))
         (equalp *arrow-type* (tapp-from (tapp-from ty))))))

(defun function-type-from (ty)
  (declare (type tapp ty))
  (tapp-to (tapp-from ty)))

(defun function-type-to (ty)
  (declare (type tapp ty))
  (tapp-to ty))

(defun function-type-arity (ty)
  (if (function-type-p ty)
      (+ 1 (function-type-arity (function-type-to ty)))
      0))

(defgeneric function-type-arguments (ty)
  (:method ((ty ty))
    (if (function-type-p ty)
        (cons (function-type-from ty)
              (function-type-arguments
               (function-type-to ty)))
        nil)))

(defgeneric function-return-type (ty)
  (:method ((ty ty))
    (if (function-type-p ty)
        (function-return-type (tapp-to ty))
        ty)))

(defun function-remove-arguments (ty num)
  (declare (type ty ty)
           (type fixnum num))

  (assert (<= num (length (function-type-arguments ty))))

  (make-function-type*
   (subseq (function-type-arguments ty) num)
   (function-return-type ty)))

(defgeneric type-variables (type)
  (:documentation "Get a list containing the type variables in TYPE.")
  ;; For any type variable, simply return a list containing itself
  (:method ((type tyvar))
    (list type))
  ;; For a type application, return the union of the tyvars of all the contained types
  (:method ((type tapp))
    (remove-duplicates (append (type-variables (tapp-from type))
                               (type-variables (tapp-to type)))
                       :test #'equalp
                       :from-end t))
  ;; Otherwise, return nothing
  (:method ((type ty))
    nil)
  ;; Allow for calling on lists
  (:method ((type-list list))
    (remove-duplicates (mapcan #'type-variables type-list) :test #'equalp :from-end t)))

;;;
;;; Pretty printing
;;;
;;; (See typechecker/base.lisp for pretty printer control)
;;;

(defun next-pprint-variable ()
  "Get the next type variable symbol interned in the keyword package"
  (prog1
      (intern
       (if (= 0 *pprint-variable-symbol-suffix*)
           (format nil "~A" (code-char *pprint-variable-symbol-code*))
           (format nil "~A~A"
                   (code-char *pprint-variable-symbol-code*)
                   *pprint-variable-symbol-suffix*))
       'keyword)
    (incf *pprint-variable-symbol-code*)
    (when (< (char-code #\Z) *pprint-variable-symbol-code*)
      (setf *pprint-variable-symbol-code* (char-code #\A))
      (incf *pprint-variable-symbol-suffix*))))

(defun next-pprint-variable-as-tvar (&optional (kind +kstar+))
  "Get the next type variable as a TVAR"
  ;; This is an awful awful hack
  (make-tycon :name (next-pprint-variable) :kind kind))

(defun pprint-tvar (tvar)
  (unless (boundp '*pprint-tyvar-dict*)
    (util:coalton-bug "Unable to pretty print tvar outside pprint variable context"))
  (let ((value (gethash (tyvar-id tvar) *pprint-tyvar-dict*)))
    (or value
        (setf (gethash (tyvar-id tvar) *pprint-tyvar-dict*)
              (next-pprint-variable-as-tvar)))))

(defun pprint-ty (stream ty)
  (declare (type stream stream)
           (type ty ty)
           (values ty))

  ;; If the type printing mode is :ALIASES and TY is aliased, print the
  ;; most high-level alias that represents TY and return from PPRINT-TY.
  (when (and (eq *coalton-type-printing-mode* :aliases) (ty-alias ty))
    (format stream "~S" (first (ty-alias ty)))
    (return-from pprint-ty ty))

  ;; If the type printing mode is :TYPES-AND-ALIASES and TY is aliased,
  ;; print the stack of aliases that represent TY before printing TY.
  (when (and (eq *coalton-type-printing-mode* :types-and-aliases) (ty-alias ty))
    (format stream "[~{~S := ~}" (ty-alias ty)))
  
  (etypecase ty
    (tyvar
     (if *coalton-pretty-print-tyvars*
         ;; Print the tvar using the current printing context. Requires use of PPRINT-VARIABLE-CONTEXT
         (pprint-ty stream (pprint-tvar ty))
         (progn
           (write-string "#T" stream)
           (write (tyvar-id ty) :stream stream))))
    (tycon
     (write (tycon-name ty) :stream stream))
    (tapp
     (cond
       ((function-type-p ty) ;; Print function types
        (write-string "(" stream)
        (pprint-ty stream (tapp-to (tapp-from ty)))
        (write-string (if settings:*coalton-print-unicode*
                          " → "
                          " -> ")
                      stream)
        ;; Avoid printing extra parenthesis on curried functions
        (labels ((print-subfunction (to)
                   (cond
                     ((function-type-p to)
                      (pprint-ty stream (tapp-to (tapp-from to)))
                      (write-string (if settings:*coalton-print-unicode*
                                        " → "
                                        " -> ")
                                    stream)
                      (print-subfunction (tapp-to to)))
                     (t (pprint-ty stream to)))))
          (print-subfunction (tapp-to ty)))
        (write-string ")" stream))
       (t ;; Print type constructors
        (let* ((tcon ty)
               (tcon-args (loop :while (tapp-p tcon)
                                :collect (tapp-to tcon)
                                :do (setf tcon (tapp-from tcon)))))
          (cond
            ((and (tycon-p tcon)
                  (simple-kind-p (tycon-kind tcon))
                  (<= (length tcon-args)
                      (kind-arity (tycon-kind tcon))))
             (write-string "(" stream)
             (pprint-ty stream tcon)
             (dolist (arg (reverse tcon-args))
               (write-string " " stream)
               (pprint-ty stream arg))
             (write-string ")" stream))
            (t
             (write-string "(" stream)
             (pprint-ty stream (tapp-from ty))
             (write-string " " stream)
             (pprint-ty stream (tapp-to ty))
             (write-string ")" stream)))))))
    (tgen
     (write-string "#GEN" stream)
     (write (tgen-id ty) :stream stream)))

  ;; Close the braces in the case that the type printing mode is
  ;; :TYPES-AND-ALIASES and TY is aliased.
  (when (and (eq *coalton-type-printing-mode* :types-and-aliases) (ty-alias ty))
    (format stream "]"))
  
  ty)

(defmethod print-object ((ty ty) stream)
  (if *print-readably*
      (call-next-method)
      (pprint-ty stream ty)))

(defun ty->string (ty)
  "Convert TY to a string for debugging."
  (with-output-to-string (s)
    (with-pprint-variable-context ()
      (pprint-ty s ty))))

;;;
;;; Conditions
;;;

(define-condition type-application-error (coalton-internal-type-error)
  ((type :initarg :type
         :reader type-application-error-type)
   (argument :initarg :argument
             :reader type-application-error-argument))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           (*print-readably* nil)
           (*coalton-type-printing-mode* :types))
       (format s "Cannot apply ~S of kind ~S to ~S of kind ~S"
               (type-application-error-argument c)
               (kind-of (type-application-error-argument c))
               (type-application-error-type c)
               (kind-of (type-application-error-type c)))))))
