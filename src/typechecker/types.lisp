(defpackage #:coalton-impl/typechecker/types
  (:use
   #:cl
   #:coalton-impl/typechecker/kinds)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:settings #:coalton-impl/settings)
   (#:error #:coalton-impl/error))
  (:export
   #:ty                                 ; STRUCT
   #:ty-list                            ; TYPE
   #:ty-binding-list                    ; TYPE
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
   #:tget                               ; STRUCT
   #:make-tgen                          ; CONSTRUCTOR
   #:tgen-id                            ; ACCESOR
   #:tgen-p                             ; FUNCTION
   #:make-variable                      ; FUNCTION
   #:instantiate                        ; FUNCTION
   #:kind-of
   #:type-constructors                  ; FUNCTION
   #:*boolean-type*                     ; VARIABLE
   #:*unit-type*                        ; VARIABLE
   #:*char-type*                        ; VARIABLE
   #:*u8-type*                          ; VARIABLE
   #:*u16-type*                         ; VARIABLE
   #:*u32-type*                         ; VARIABLE
   #:*u64-type*                         ; VARIABLE
   #:*i8-type*                          ; VARIABLE
   #:*i16-type*                         ; VARIABLE
   #:*132-type*                         ; VARIABLE
   #:*164-type*                         ; VARIABLE
   #:*integer-type*                     ; VARIABLE
   #:*ifix-type*                        ; VARIABLE
   #:*ufix-type*                        ; VARIABLE
   #:*single-float-type*                ; VARIABLE
   #:*double-float-type*                ; VARIABLE
   #:*string-type*                      ; VARIABLE
   #:*fraction-type*                    ; VARIABLE
   #:*arrow-type*                       ; VARIABLE
   #:*list-type*                        ; VARIABLE
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
   #:type-variables                     ; FUNCTION
   #:*coalton-pretty-print-tyvars*      ; VARIABLE
   #:with-pprint-variable-scope         ; MACRO
   #:with-pprint-variable-context       ; MACRO
   #:next-pprint-variable               ; FUNCTION
   #:next-pprint-variable-as-tvar       ; FUNCTION
   #:pprint-tvar                        ; FUNCTION
   #:pprint-ty                          ; FUNCTION
   #:type-application-error             ; CONDITION
   ))

(in-package #:coalton-impl/typechecker/types)

;;;
;;; Types
;;;

(defstruct (ty (:constructor nil)))

(defmethod make-load-form ((self ty) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun ty-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-p x)))

(deftype ty-list ()
  '(satisfies ty-list-p))

(defun ty-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol ty))) x)))

(deftype ty-binding-list ()
  `(satisfies ty-binding-list-p))

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

(declaim (inline make-variable))
(defun make-variable (&optional (kind +kstar+))
  (prog1 (make-tyvar :id *next-variable-id* :kind kind)
    (incf *next-variable-id*)))

;;;
;;; Methods
;;;

(defgeneric instantiate (types type)
  (:method (types (type tapp))
    (make-tapp
     :from (instantiate types (tapp-from type))
     :to (instantiate types (tapp-to type))))
  (:method (types (type tgen))
    (nth (tgen-id type) types))
  (:method (types (type ty))
    type)
  (:method (types (type list))
    (mapcar (lambda (type) (instantiate types type)) type)))

(defgeneric kind-of (type)
  (:documentation "Get the kind of TYPE.")
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
   :id (tyvar-id type)
   :kind (apply-ksubstitution subs (tyvar-kind type))))

(defmethod apply-ksubstitution (subs (type tycon))
  (make-tycon
   :name (tycon-name type)
   :kind (apply-ksubstitution subs (tycon-kind type))))

(defmethod apply-ksubstitution (subs (type tapp))
  (make-tapp
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

(defun type-constructors (type)
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

;;;
;;; Early types
;;;

(defvar *boolean-type*      (make-tycon :name 'coalton:Boolean      :kind +kstar+))
(defvar *unit-type*         (make-tycon :name 'coalton:Unit         :kind +kstar+))
(defvar *char-type*         (make-tycon :name 'coalton:Char         :kind +kstar+))
(defvar *u8-type*           (make-tycon :name 'coalton:U8           :kind +kstar+))
(defvar *u16-type*          (make-tycon :name 'coalton:U16          :kind +kstar+))
(defvar *u32-type*          (make-tycon :name 'coalton:U32          :kind +kstar+))
(defvar *u64-type*          (make-tycon :name 'coalton:U64          :kind +kstar+))
(defvar *i8-type*           (make-tycon :name 'coalton:I8           :kind +kstar+))
(defvar *i16-type*          (make-tycon :name 'coalton:I16          :kind +kstar+))
(defvar *i32-type*          (make-tycon :name 'coalton:I32          :kind +kstar+))
(defvar *i64-type*          (make-tycon :name 'coalton:I64          :kind +kstar+))
(defvar *integer-type*      (make-tycon :name 'coalton:Integer      :kind +kstar+))
(defvar *ifix-type*         (make-tycon :name 'coalton:IFix         :kind +kstar+))
(defvar *ufix-type*         (make-tycon :name 'coalton:UFix         :kind +kstar+))
(defvar *single-float-type* (make-tycon :name 'coalton:Single-Float :kind +kstar+))
(defvar *double-float-type* (make-tycon :name 'coalton:Double-Float :kind +kstar+))
(defvar *string-type*       (make-tycon :name 'coalton:String       :kind +kstar+))
(defvar *fraction-type*     (make-tycon :name 'coalton:Fraction     :kind +kstar+))
(defvar *arrow-type*        (make-tycon :name 'coalton:Arrow        :kind (make-kfun :from +kstar+ :to (make-kfun :from +kstar+ :to +kstar+))))
(defvar *list-type*         (make-tycon :name 'coalton:List         :kind (make-kfun :from +kstar+ :to +kstar+)))

;;;
;;; Operations on Types
;;;

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
    (util:coalton-bug "Unable to construct function with type ~A of kind ~A" from (kind-of from)))
  (unless (kstar-p (kind-of to))
    (util:coalton-bug "Unable to construct function with type ~A of kind ~A" to (kind-of to)))
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

(defvar *pprint-variable-symbol-code*)
(defvar *pprint-variable-symbol-suffix*)

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

(defmacro with-pprint-variable-scope (() &body body)
  "If there is no pretty printing variable scope then create one for BODY"
  `(if (boundp '*pprint-variable-symbol-code*)
       (let ((*pprint-variable-symbol-code* *pprint-variable-symbol-code*)
             (*pprint-variable-symbol-suffix* *pprint-variable-symbol-suffix*))
         ,@body)
       (let ((*pprint-variable-symbol-code* (char-code #\A))
             (*pprint-variable-symbol-suffix* 0))
         ,@body)))

(defvar *pprint-tyvar-dict*)

(defun pprint-tvar (tvar)
  (unless (boundp '*pprint-tyvar-dict*)
    (util:coalton-bug "Unable to pretty print tvar outside pprint variable context"))
  (let ((value (gethash (tyvar-id tvar) *pprint-tyvar-dict*)))
    (or value
        (setf (gethash (tyvar-id tvar) *pprint-tyvar-dict*)
              (next-pprint-variable-as-tvar)))))

(defmacro with-pprint-variable-context (() &body body)
  "Create a variable context which can be used with PPRINT-TVAR"
  `(let ((*pprint-tyvar-dict* (make-hash-table :test #'equalp))
         (*coalton-pretty-print-tyvars* t))
     (with-pprint-variable-scope ()
       ,@body)))

(defvar *coalton-pretty-print-tyvars* nil
  "Whether to print all tyvars using type variable syntax

This requires a valid PPRINT-VARIABLE-CONTEXT")

(defun pprint-ty (stream ty)
  (declare (type stream stream)
           (type ty ty)
           (values ty))
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
  ty)

(defmethod print-object ((ty ty) stream)
  (if *print-readably*
      (call-next-method)
      (pprint-ty stream ty)))

;;;
;;; Conditions
;;;

(define-condition type-application-error (error:coalton-type-error)
  ((type :initarg :type
         :reader type-application-error-type)
   (argument :initarg :argument
             :reader type-application-error-argument))
  (:report
   (lambda (c s)
     (let ((*print-circle* nil) ; Prevent printing using reader macros
           )
       (format s "Cannot apply ~A of kind ~A to ~A of kind ~A"
               (type-application-error-argument c)
               (kind-of (type-application-error-argument c))
               (type-application-error-type c)
               (kind-of (type-application-error-type c)))))))
