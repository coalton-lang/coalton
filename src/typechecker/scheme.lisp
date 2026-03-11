(defpackage #:coalton-impl/typechecker/scheme
  (:use
   #:cl
   #:coalton-impl/typechecker/base
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
   #:ty-scheme=                         ; FUNCTION
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

(defun ty-scheme= (ty-scheme1 ty-scheme2)
  (and (equalp (ty-scheme-kinds ty-scheme1)
               (ty-scheme-kinds ty-scheme2))
       (qualified-ty= (ty-scheme-type ty-scheme1)
                      (ty-scheme-type ty-scheme2))))

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
                (lambda (x) (not (find x tyvars :test #'ty=)))
                (type-variables type)))
         (kinds (mapcar #'kind-of vars))
         (subst (loop :for var :in vars
                      :for id :from 0
                      :collect (make-substitution
                                :from var
                                :to (make-tgen :id id
                                               :source-name (tyvar-source-name var))))))
    (make-ty-scheme
     :kinds kinds
     :type (apply-substitution subst type))))

(defun ty-scheme-instantiation-types (ty-scheme)
  (declare (type ty-scheme ty-scheme)
           (values tyvar-list &optional))
  (let* ((source-names (make-array (length (ty-scheme-kinds ty-scheme))
                                   :initial-element nil))
         (scheme-type (ty-scheme-type ty-scheme)))
    (labels ((collect-source-names (object)
               (typecase object
                 (tgen
                  (when (< (tgen-id object) (length source-names))
                    (setf (aref source-names (tgen-id object))
                          (or (aref source-names (tgen-id object))
                              (tgen-source-name object)))))
                 (tapp
                  (collect-source-names (tapp-from object))
                  (collect-source-names (tapp-to object)))
                 (qualified-ty
                  (collect-source-names (qualified-ty-predicates object))
                  (collect-source-names (qualified-ty-type object)))
                 (ty-predicate
                  (collect-source-names (ty-predicate-types object)))
                 (list
                  (map nil #'collect-source-names object)))))
      (collect-source-names scheme-type)
      (loop :for kind :in (ty-scheme-kinds ty-scheme)
            :for i :from 0
            :collect (make-variable kind (aref source-names i))))))

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
  (let ((types (ty-scheme-instantiation-types ty-scheme)))
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
                (lambda (x) (not (find x (type-variables type) :test #'ty=)))
                tyvars))
         (kinds (mapcar #'kind-of vars))
         (subst (loop :for var :in vars
                      :for id :from 0
                      :collect (make-substitution
                                :from var
                                :to (make-tgen :id id
                                               :source-name (tyvar-source-name var))))))
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

(defmethod remove-source-info ((scheme ty-scheme))
  (make-ty-scheme
   :kinds (ty-scheme-kinds scheme)
   :type (remove-source-info (ty-scheme-type scheme))))

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
     (with-pprint-variable-context ()
       (let* ((types (ty-scheme-instantiation-types scheme))
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
