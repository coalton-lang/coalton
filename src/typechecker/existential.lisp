(defpackage #:coalton-impl/typechecker/existential
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/kinds
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/substitutions
   #:coalton-impl/typechecker/predicate)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:settings #:coalton-impl/settings))
  (:export
   #:existential-ty                     ; STRUCT
   #:make-existential-ty                ; CONSTRUCTOR
   #:existential-ty-p                   ; FUNCTION
   #:existential-ty-kinds               ; ACCESSOR
   #:existential-ty-type                ; ACCESSOR
   #:existential-ty-list                ; TYPE
   #:remove-source-info                 ; FUNCTION
   #:existentialize                     ; FUNCTION
   #:fresh-inst-ex                      ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/existential)

;;;
;;; Existential types
;;;

(defstruct existential-ty
  (kinds (util:required 'kinds) :type list         :read-only t)
  (type  (util:required 'type)  :type qualified-ty :read-only t))

(defmethod make-load-form ((self existential-ty) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type existential-ty))

(defun existential-ty-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'existential-ty-p x)))

(deftype existential-ty-list ()
  '(satisfies existential-ty-list-p))

(defmethod remove-source-info ((ex-ty existential-ty))
  (make-existential-ty
   :kinds (existential-ty-kinds ex-ty)
   :type (remove-source-info (existential-ty-type ex-ty))))

;;;
;;; Operations on existential types
;;;

(defun existentialize (tyvars type)
  (declare (type tyvar-list tyvars)
           (type qualified-ty type)
           (values existential-ty))
  (let* ((vars (remove-if
                (lambda (x) (not (find x tyvars :test #'equalp)))
                (type-variables type)))
         (kinds (mapcar #'kind-of vars))
         (subst (loop :for var :in vars
                      :for id :from 0
                      :collect (make-substitution :from var :to (make-tex :id id)))))
    (make-existential-ty
     :kinds kinds
     :type (apply-substitution subst type))))

(defun fresh-inst-ex (ex-ty &key skolemize-p)
  (declare (type existential-ty ex-ty)
           (values qualified-ty &optional))
  (let ((types (mapcar (if skolemize-p #'make-skolem #'make-variable)
                       (existential-ty-kinds ex-ty))))
    (instantiate-ex types (existential-ty-type ex-ty))))

;;;
;;; Methods
;;;

(defmethod apply-substitution (subst-list (type existential-ty))
  (declare (type substitution-list subst-list))
  (make-existential-ty
   :kinds (existential-ty-kinds type)
   :type (apply-substitution subst-list (existential-ty-type type))))

(defmethod apply-ksubstitution (subs (type existential-ty))
  (declare (type ksubstitution-list subs))
  (make-existential-ty
   :kinds (existential-ty-kinds type)
   :type (apply-ksubstitution subs (existential-ty-type type))))

(defmethod type-variables ((type existential-ty))
  (type-variables (existential-ty-type type)))

(defmethod kind-variables-generic% ((type existential-ty))
  (declare (values kyvar-list &optional))
  (kind-variables-generic% (existential-ty-type type)))

(defmethod instantiate (types (type existential-ty))
  (make-existential-ty
   :kinds (existential-ty-kinds type)
   :type (instantiate types (existential-ty-type type))))

(defmethod instantiate-ex (types (type existential-ty))
  (make-existential-ty
   :kinds (existential-ty-kinds type)
   :type (instantiate-ex types (existential-ty-type type))))

(defmethod kind-of ((type existential-ty))
  (kind-of (existential-ty-type type)))

(defmethod function-type-p ((type existential-ty))
  (function-type-p (existential-ty-type type)))

(defmethod function-return-type ((type existential-ty))
  (function-return-type (existential-ty-type type)))

(defmethod function-type-arguments ((type existential-ty))
  (function-type-arguments (existential-ty-type type)))

;;;
;;; Pretty printing
;;;

(defun pprint-existential-ty (stream existential-ty)
  (declare (type stream stream)
           (type existential-ty existential-ty))
  (cond
    ((null (existential-ty-kinds existential-ty))
     (write (existential-ty-type existential-ty) :stream stream))
    (t
     (with-pprint-variable-scope ()
       (let* ((types (mapcar (lambda (k) (next-pprint-variable-as-tvar k))
                             (existential-ty-kinds existential-ty)))
              (new-type (instantiate-ex types (existential-ty-type existential-ty))))
         (write-string (if settings:*coalton-print-unicode*
                           "∃"
                           "EXISTS")
                       stream)
         (loop :for ty :in types
               :do (write-char #\space stream)
                   (write ty :stream stream))
         (write-string ". " stream)
         (write new-type :stream stream)))
     ))

  nil)


(defmethod print-object ((existential-ty existential-ty) stream)
  (if *print-readably*
      (call-next-method)
      (pprint-existential-ty stream existential-ty)))
