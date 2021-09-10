(in-package #:coalton-impl/typechecker)

;;;
;;; Type schemes
;;;

(serapeum:defstruct-read-only (ty-scheme (:constructor %make-ty-scheme (kinds type)))
  (kinds :type list)
  (type  :type qualified-ty))

#+sbcl
(declaim (sb-ext:freeze-type ty-scheme))

(defun scheme-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-scheme-p x)))

(deftype scheme-list ()
  '(satisfies scheme-list-p))

#+sbcl
(declaim (sb-ext:freeze-type scheme-list))

(defun scheme-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol ty-scheme))) x)))

(deftype scheme-binding-list ()
  `(satisfies scheme-binding-list-p))

#+sbcl
(declaim (sb-ext:freeze-type scheme-binding-list))

(defun quantify (tyvars type)
  (let* ((vars (remove-if
                (lambda (x) (not (find x tyvars :test #'equalp)))
                (type-variables type)))
         (kinds (mapcar #'kind-of vars))
         (subst (loop :for var :in vars
                      :for id :from 0
                      :collect (%make-substitution var (%make-tgen id)))))
    (%make-ty-scheme kinds (apply-substitution subst type))))

(declaim (inline to-scheme))
(defun to-scheme (type)
  (%make-ty-scheme nil type))

(defgeneric instantiate (types type)
  (:method (types (type tapp))
    (%make-tapp (instantiate types (tapp-from type))
                (instantiate types (tapp-to type))))
  (:method (types (type tgen))
    (nth (tgen-id type) types))
  (:method (types (type ty))
    type)
  (:method (types (type list))
    (mapcar (lambda (type) (instantiate types type)) type)))

(defun fresh-inst (ty-scheme)
  (let ((types (mapcar (lambda (k) (make-variable k))
                       (ty-scheme-kinds ty-scheme))))
    (instantiate types (ty-scheme-type ty-scheme))))

(defun scheme-predicates (ty-scheme)
  "Get freshly instantiated predicates of scheme TY-SCHEME"
  (qualified-ty-predicates (fresh-inst ty-scheme)))

;;;
;;; Methods
;;;

(defmethod apply-substitution (subst-list (type ty-scheme))
  (%make-ty-scheme (ty-scheme-kinds type)
                   (apply-substitution subst-list (ty-scheme-type type))))

(defmethod type-variables ((type ty-scheme))
  (type-variables (ty-scheme-type type)))

(defmethod kind-of ((type ty-scheme))
  (kind-of (fresh-inst type)))

(defmethod function-type-p ((type ty-scheme))
  (function-type-p (fresh-inst type)))

(defmethod function-return-type ((type ty-scheme))
  (to-scheme (function-return-type (fresh-inst type))))

;;;
;;; Pretty printing
;;;

(defun pprint-scheme (stream scheme &optional colon-p at-sign-p)
  (declare (type stream stream)
           (type ty-scheme scheme)
           (ignore colon-p)
           (ignore at-sign-p)
           (values ty-scheme))
  (cond
    ((null (ty-scheme-kinds scheme))
     (format stream "~A" (ty-scheme-type scheme)))
    (t
     (with-pprint-variable-scope ()
       (let* ((types (mapcar (lambda (k) (next-pprint-variable-as-tvar k))
                             (ty-scheme-kinds scheme)))
              (new-type (instantiate types (ty-scheme-type scheme))))
         (format stream "~A~{ ~A~}. ~A"
                 (if *coalton-print-unicode*
                     "∀"
                     "FORALL")
                 types new-type)))
     ))
  scheme)

(set-pprint-dispatch 'ty-scheme 'pprint-scheme)
