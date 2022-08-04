(in-package #:coalton-impl/typechecker)

;;;
;;; Type schemes
;;;

(defstruct (ty-scheme (:constructor %make-ty-scheme (kinds type)))
  (kinds (required 'kinds) :type list         :read-only t)
  (type  (required 'type)  :type qualified-ty :read-only t))

(defmethod make-load-form ((self ty-scheme) &optional env)
  (make-load-form-saving-slots
   self
   :slot-names '(kinds type)
   :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type ty-scheme))

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

(defun fresh-inst (ty-scheme)
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
         (qual-ty (qualified-ty (list pred) var))
         (scheme (quantify (type-variables (list pred var)) qual-ty)))
    (car (qualified-ty-predicates (fresh-inst scheme)))))

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

(defmethod function-type-arguments ((type ty-scheme))
  (function-type-arguments (fresh-inst type)))

;;;
;;; Pretty printing
;;;

(defmethod print-object ((scheme ty-scheme) stream)
  (if *print-readably*
      (call-next-method)
      (cond
        ((null (ty-scheme-kinds scheme))
         (write (ty-scheme-type scheme) :stream stream))
        (t
         (with-pprint-variable-scope ()
           (let* ((types (mapcar (lambda (k) (next-pprint-variable-as-tvar k))
                                 (ty-scheme-kinds scheme)))
                  (new-type (instantiate types (ty-scheme-type scheme))))
             (write-string (if *coalton-print-unicode*
                               "∀"
                               "FORALL")
                           stream)
             (loop :for ty :in types
                   :do (write-char #\space stream)
                       (write ty :stream stream))
             (write-string ". " stream)
             (write new-type :stream stream)))
         )))
  scheme)
