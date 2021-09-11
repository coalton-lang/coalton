(in-package #:coalton-impl/typechecker)

;;;
;;; Types
;;;

(serapeum:defstruct-read-only (ty (:constructor nil)))

(defun ty-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-p x)))

(deftype ty-list ()
  '(satisfies ty-list-p))

#+sbcl
(declaim (sb-ext:freeze-type ty-list))

(defun ty-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol ty))) x)))

(deftype ty-binding-list ()
  `(satisfies ty-binding-list-p))

#+sbcl
(declaim (sb-ext:freeze-type ty-binding-list))

(serapeum:defstruct-read-only (tyvar (:constructor %make-tyvar))
  (id   :type fixnum)
  (kind :type kind))

#+sbcl
(declaim (sb-ext:freeze-type tyvar))

(defun tyvar-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'tyvar-p x)))

(deftype tyvar-list ()
  '(satisfies tyvar-list-p))

#+sbcl
(declaim (sb-ext:freeze-type tyvar-list))

(serapeum:defstruct-read-only (tvar (:include ty)
                 (:constructor %make-tvar (tyvar)))
  (tyvar (required 'tyvar) :type tyvar))

#+sbcl
(declaim (sb-ext:freeze-type tvar))

(defun tvar-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'tvar-p x)))

(deftype tvar-list ()
  '(satisfies tvar-list-p))

#+sbcl
(declaim (sb-ext:freeze-type tvar-list))

(serapeum:defstruct-read-only (tycon (:constructor %make-tycon))
  (name  :type symbol)
  (kind  :type kind))

#+sbcl
(declaim (sb-ext:freeze-type tycon))

(serapeum:defstruct-read-only (tcon (:include ty)
                 (:constructor %make-tcon (tycon)))
  (tycon :type tycon))

#+sbcl
(declaim (sb-ext:freeze-type tcon))

(serapeum:defstruct-read-only (tapp (:include ty)
                 (:constructor %make-tapp (from to)))
  (from  :type ty)
  (to    :type ty))

#+sbcl
(declaim (sb-ext:freeze-type tapp))

(serapeum:defstruct-read-only (tgen (:include ty)
                 (:constructor %make-tgen (id)))
  (id  :type fixnum))

#+sbcl
(declaim (sb-ext:freeze-type tgen))

#+sbcl
(declaim (sb-ext:freeze-type ty))


(defvar *next-variable-id* 0)

#+sbcl
(declaim (sb-ext:always-bound *next-variable-id*))

(declaim (inline make-variable))
(defun make-variable (&optional (kind kstar))
  (prog1 (%make-tvar (%make-tyvar :id *next-variable-id* :kind kind))
    (incf *next-variable-id*)))

;;;
;;; Methods
;;;

(defgeneric kind-of (type)
  (:documentation "Get the kind of TYPE.")
  (:method ((type tyvar))
    (tyvar-kind type))
  (:method ((type tycon))
    (tycon-kind type))
  (:method ((type tcon))
    (kind-of (tcon-tycon type)))
  (:method ((type tvar))
    (kind-of (tvar-tyvar type)))
  (:method ((type tapp))
    (let ((from-kind (kind-of (tapp-from type))))
      (if (kfun-p from-kind)
          (kfun-to from-kind)
          (error "Malformed type application")))))

;;;
;;; Early types
;;;

(defvar tUnit    (%make-tcon (%make-tycon :name 'coalton:Unit        :kind kstar)))
(defvar tChar    (%make-tcon (%make-tycon :name 'coalton:Char        :kind kstar)))
(defvar tI32     (%make-tcon (%make-tycon :name 'coalton:I32         :kind kstar)))
(defvar tI64     (%make-tcon (%make-tycon :name 'coalton:I64         :kind kstar)))
(defvar tU8      (%make-tcon (%make-tycon :name 'coalton:U8          :kind kstar)))
(defvar tU32     (%make-tcon (%make-tycon :name 'coalton:U32         :kind kstar)))
(defvar tU64     (%make-tcon (%make-tycon :name 'coalton:U64         :kind kstar)))
(defvar tInteger (%make-tcon (%make-tycon :name 'coalton:Integer     :kind kstar)))
(defvar tSingle-Float
  (%make-tcon (%make-tycon :name 'coalton:Single-Float :kind kstar)))
(defvar tDouble-Float
  (%make-tcon (%make-tycon :name 'coalton:Double-Float :kind kstar)))
(defvar tString  (%make-tcon (%make-tycon :name 'coalton:String      :kind kstar)))
(defvar tLisp-Object
  (%make-tcon (%make-tycon :name 'coalton:Lisp-Object :kind kstar)))

(defvar tArrow (%make-tcon (%make-tycon :name '-> :kind (kfun kstar (kfun kstar kstar)))))


(defun apply-type-argument (tcon arg)
  (declare (type (or tcon tapp tvar) tcon)
           (type ty arg)
           (values tapp))
  (unless (kfun-p (kind-of tcon))
    (error 'type-application-error :type tcon :argument arg))
  (unless (equalp (kfun-from (kind-of tcon)) (kind-of arg))
    (error 'type-application-error :type tcon :argument arg))
  (%make-tapp tcon arg))

(defun apply-type-argument-list (tcon args)
  (labels ((%apply-type-argument-list (tcon args)
             (if args
                 (apply-type-argument (%apply-type-argument-list tcon (cdr args))
                                      (car args))
                 tcon)))
    (%apply-type-argument-list tcon (reverse args))))

(defun make-function-type (from to)
  (declare (type ty from to)
           (values ty))
  (unless (kstar-p (kind-of from))
    (error "Unable to construct function with type ~A of kind ~A" from (kind-of from)))
  (unless (kstar-p (kind-of to))
    (error "Unable to construct function with type ~A of kind ~A" to (kind-of to)))
  (%make-tapp (%make-tapp tArrow from) to))

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
         (equalp tArrow (tapp-from (tapp-from ty))))))

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

;;;
;;; Pretty printing
;;;

(defvar *coalton-print-unicode* t
  "Whether to print coalton info using unicode symbols")

(defvar *coalton-pretty-print-tyvars* nil
  "Whether to print all tyvars using type variable syntax

This requires a valid PPRINT-VARIABLE-CONTEXT")

(defun pprint-ty (stream ty &optional colon-p at-sign-p)
  (declare (type stream stream)
           (type ty ty)
           (ignore colon-p)
           (ignore at-sign-p)
           (values ty))
  (etypecase ty
    (tvar
     (if *coalton-pretty-print-tyvars*
         ;; Print the tvar using the current printing context. Requires use of PPRINT-VARIABLE-CONTEXT
         (pprint-ty stream (pprint-tvar ty))
         (format stream "#T~A" (tyvar-id (tvar-tyvar ty)))))
    (tcon
     (format stream "~S" (tycon-name (tcon-tycon ty))))
    (tapp
     (cond
       ((function-type-p ty) ;; Print function types
        (write-string "(" stream)
        (pprint-ty stream (tapp-to (tapp-from ty)))
        (write-string (if *coalton-print-unicode*
                          " → "
                          " -> ")
                      stream)
        ;; Avoid printing extra parenthesis on curried functions
        (labels ((print-subfunction (to)
                   (cond
                     ((function-type-p to)
                      (pprint-ty stream (tapp-to (tapp-from to)))
                      (write-string (if *coalton-print-unicode*
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
            ((and (tcon-p tcon)
                  (simple-kind-p (tycon-kind (tcon-tycon tcon)))
                  (<= (length tcon-args)
                      (kind-arity (tycon-kind (tcon-tycon tcon)))))
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
     (format stream "#GEN~A" (tgen-id ty))))
  ty)

(set-pprint-dispatch 'ty 'pprint-ty)
