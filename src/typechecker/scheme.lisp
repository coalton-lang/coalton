(defpackage #:coalton-impl/typechecker/scheme
  (:use
   #:cl
   #:coalton-impl/typechecker/kinds
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/substitutions
   #:coalton-impl/typechecker/predicate)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:settings #:coalton-impl/settings))
  (:export
   #:ty-scheme                          ; STRUCT
   #:make-ty-scheme                     ; CONSTRUCTOR
   #:ty-scheme-kinds                    ; ACCESSOR
   #:ty-scheme-type                     ; ACCESSOR
   #:ty-scheme-p                        ; FUNCTION
   #:scheme-list                        ; TYPE
   #:scheme-binding-list                ; TYPE
   #:quantify                           ; FUNCTION
   #:to-scheme                          ; FUNCTION
   #:fresh-inst                         ; FUNCTION
   #:scheme-predicates                  ; FUNCTION
   #:fresh-pred                         ; FUNCTION
   #:fresh-preds                        ; FUNCTION
   #:quantify-using-tvar-order          ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/scheme)

;;;
;;; Type schemes
;;;

(defstruct ty-scheme 
  (kinds (util:required 'kinds) :type list         :read-only t)
  (type  (util:required 'type)  :type qualified-ty :read-only t))

(defmethod make-load-form ((self ty-scheme) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun scheme-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-scheme-p x)))

(deftype scheme-list ()
  '(satisfies scheme-list-p))

(defun scheme-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol ty-scheme))) x)))

(deftype scheme-binding-list ()
  `(satisfies scheme-binding-list-p))

;;;
;;; Operations on Schemes
;;;

(defun quantify (tyvars type)
  (declare (type tyvar-list tyvars)
           (type qualified-ty type)
           (values ty-scheme))
  (let* ((vars (remove-if
                (lambda (x) (not (find x tyvars :test #'equalp)))
                (type-variables type)))
         (kinds (mapcar #'kind-of vars))
         (subst (loop :for var :in vars
                      :for id :from 0
                      :collect (make-substitution :from var :to (make-tgen :id id)))))
    (make-ty-scheme
     :kinds kinds
     :type (apply-substitution subst type))))

(defgeneric to-scheme (ty)
  (:method ((ty qualified-ty))
    (make-ty-scheme
     :kinds nil
     :type ty))

  (:method ((ty ty))
    (to-scheme (qualify nil ty))))

(defun fresh-inst (ty-scheme)
  (declare (type ty-scheme ty-scheme)
           (values qualified-ty &optional))
  (let ((types (mapcar (lambda (k) (make-variable k))
                       (ty-scheme-kinds ty-scheme))))
    (instantiate types (ty-scheme-type ty-scheme))))

(defun scheme-predicates (ty-scheme)
  "Get freshly instantiated predicates of scheme TY-SCHEME"
  (qualified-ty-predicates (fresh-inst ty-scheme)))

(defun fresh-pred (pred)
  "Returns PRED with fresh type variables"
  (declare (type ty-predicate pred)
           (values ty-predicate))
  (let* ((var (make-variable))
         (qual-ty (make-qualified-ty :predicates (list pred) :type var))
         (scheme (quantify (type-variables (list pred var)) qual-ty)))
    (car (qualified-ty-predicates (fresh-inst scheme)))))

(defun fresh-preds (preds)
  "Returns PRED with fresh type variables"
  (declare (type ty-predicate-list preds)
           (values ty-predicate-list))
  (let* ((var (make-variable))
         (qual-ty (make-qualified-ty :predicates preds :type var))
         (scheme (quantify (type-variables (cons var preds)) qual-ty)))
    (qualified-ty-predicates (fresh-inst scheme))))

(defun quantify-using-tvar-order (tyvars type)
  (let* ((vars (remove-if
                (lambda (x) (not (find x (type-variables type) :test #'equalp)))
                tyvars))
         (kinds (mapcar #'kind-of vars))
         (subst (loop :for var :in vars
                      :for id :from 0
                      :collect (make-substitution :from var :to (make-tgen :id id)))))
    (make-ty-scheme
     :kinds kinds
     :type (apply-substitution subst type))))

;;;
;;; Methods
;;;

(defmethod apply-substitution (subst-list (type ty-scheme))
  (make-ty-scheme
   :kinds (ty-scheme-kinds type)
   :type (apply-substitution subst-list (ty-scheme-type type))))

(defmethod type-variables ((type ty-scheme))
  (type-variables (ty-scheme-type type)))

(defmethod kind-of ((type ty-scheme))
  (kind-of (fresh-inst type)))

(defmethod function-type-p ((type ty-scheme))
  (function-type-p (fresh-inst type)))

(defmethod function-return-type ((type ty-scheme))
  (to-scheme (function-return-type (fresh-inst type))))

(defmethod function-type-arguments ((type ty-scheme))
  (function-type-arguments (fresh-inst type)))

;;;
;;; Pretty printing
;;;

(defun pprint-scheme (stream scheme)
  (declare (type stream stream)
           (type ty-scheme scheme))
  (cond
    ((null (ty-scheme-kinds scheme))
     (write (ty-scheme-type scheme) :stream stream))
    (t
     (with-pprint-variable-scope ()
       (let* ((types (mapcar (lambda (k) (next-pprint-variable-as-tvar k))
                             (ty-scheme-kinds scheme)))
              (new-type (instantiate types (ty-scheme-type scheme))))
         (write-string (if settings:*coalton-print-unicode*
                           "∀"
                           "FORALL")
                       stream)
         (loop :for ty :in types
               :do (write-char #\space stream)
                   (write ty :stream stream))
         (write-string ". " stream)
         (write new-type :stream stream)))
     ))

  nil)

(defmethod print-object ((scheme ty-scheme) stream)
  (if *print-readably*
      (call-next-method)
      (pprint-scheme stream scheme)))
