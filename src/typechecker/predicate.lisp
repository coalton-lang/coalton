(defpackage #:coalton-impl/typechecker/predicate
  (:use
   #:cl
   #:coalton-impl/typechecker/kinds
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/substitutions)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:settings #:coalton-impl/settings))
  (:export
   #:ty-predicate                       ; STRUCT
   #:make-ty-predicate                  ; CONSTRUCTOR
   #:ty-predicate-class                 ; ACCESSOR
   #:ty-predicate-types                 ; ACCESSOR
   #:ty-predicate-superclass-evidence   ; ACCESSOR
   #:ty-predicate-p                     ; FUNCTION
   #:ty-predicate-list                  ; TYPE
   #:qualified-ty                       ; STRUCT
   #:make-qualified-ty                  ; CONSTRUCTOR
   #:qualified-ty-predicates            ; ACCESSOR
   #:qualified-ty-type                  ; ACCESSOR
   #:qualified-ty=                      ; FUNCTION
   #:qualified-ty-list                  ; TYPE
   #:forall-ty                          ; STRUCT
   #:make-forall-ty                     ; CONSTRUCTOR
   #:forall-ty-p                        ; FUNCTION
   #:forall-ty-explicit-p               ; ACCESSOR
   #:forall-ty-binders                  ; ACCESSOR
   #:forall-ty-type                     ; ACCESSOR
   #:forall-ty-predicates               ; FUNCTION
   #:forall-ty-body                     ; FUNCTION
   #:instantiate-forall-ty              ; FUNCTION
   #:contains-forall-ty-p               ; FUNCTION
   #:remove-source-info                 ; FUNCTION
   #:static-predicate-p                 ; FUNCTION
   #:type-predicate=                    ; FUNCTION
   #:qualify                            ; FUNCTION
   #:pprint-predicate                   ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/predicate)

;;;
;;; Type predicates
;;;

(defstruct ty-predicate
  "A type predicate indicating that TYPE is of the CLASS"
  (class    (util:required 'class) :type symbol                    :read-only t)
  (types    (util:required 'types) :type ty-list                   :read-only t)
  ;; Instantiated fields of a dictionary in scope, not part of the predicate's type.
  (superclass-evidence nil         :type list                      :read-only t)
  (location nil                    :type (or source:location null) :read-only t))

(defmethod make-load-form ((self ty-predicate) &optional env)
  (make-load-form-saving-slots self :environment env))

(defmethod source:location ((self ty-predicate))
  (ty-predicate-location self))

(defun ty-predicate-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ty-predicate-p x)))

(deftype ty-predicate-list ()
  "A list of type predicates"
  `(satisfies ty-predicate-list-p))

(defun static-predicate-p (pred)
  "Is PRED a static predicate (no type variables)"
  (endp (type-variables (ty-predicate-types pred))))

(defun type-predicate= (pred1 pred2)
  (and (eq (ty-predicate-class pred1) (ty-predicate-class pred2))
       (= (length (ty-predicate-types pred1))
          (length (ty-predicate-types pred2)))
       (every #'ty= (ty-predicate-types pred1) (ty-predicate-types pred2))))

;;;
;;; Qualified types
;;;

(defstruct qualified-ty
  (predicates (util:required 'predicates) :type ty-predicate-list :read-only t)
  (type       (util:required 'type)       :type ty                :read-only t))

(defun qualified-ty= (qualified-ty1 qualified-ty2)
  (and (= (length (qualified-ty-predicates qualified-ty1))
          (length (qualified-ty-predicates qualified-ty2)))
       (every #'type-predicate=
              (qualified-ty-predicates qualified-ty1)
              (qualified-ty-predicates qualified-ty2))
       (ty= (qualified-ty-type qualified-ty1)
            (qualified-ty-type qualified-ty2))))

(defmethod make-load-form ((self qualified-ty) &optional env)
  (make-load-form-saving-slots self :environment env))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type qualified-ty))

(util:define-list-type qualified-ty-list qualified-ty-p)

(defun qualify (predicates type)
  "Qualify TYPE with PREDICATES"
  (declare (type ty type)
           (type ty-predicate-list predicates)
           (values qualified-ty &optional))
  (make-qualified-ty
   :predicates predicates
   :type type))

;;;
;;; Universally quantified types (Rank-N)
;;;

(defstruct (forall-ty (:include ty))
  (explicit-p t                        :type boolean      :read-only t)
  (binders    (util:required 'binders) :type tyvar-list   :read-only t)
  (type       (util:required 'type)    :type qualified-ty :read-only t))

(defmethod make-load-form ((self forall-ty) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun forall-ty-predicates (forall-ty)
  (declare (type forall-ty forall-ty)
           (values ty-predicate-list &optional))
  (qualified-ty-predicates (forall-ty-type forall-ty)))

(defun forall-ty-body (forall-ty)
  (declare (type forall-ty forall-ty)
           (values ty &optional))
  (qualified-ty-type (forall-ty-type forall-ty)))

(defun instantiate-forall-ty (forall-ty &key (rigid-p nil))
  "Instantiate FORALL-TY's binders with fresh flexible or rigid (skolem) type variables."
  (declare (type forall-ty forall-ty)
           (type boolean rigid-p)
           (values qualified-ty tyvar-list &optional))
  (let* ((fresh-vars
           (loop :for binder :in (forall-ty-binders forall-ty)
                 :collect (make-variable :kind (tyvar-kind binder)
                                         :source-name (tyvar-source-name binder)
                                         :allow-result-p (tyvar-allow-result-p binder)
                                         :rigid-p rigid-p)))
         (substs
           (loop :for binder :in (forall-ty-binders forall-ty)
                 :for fresh :in fresh-vars
                 :collect (make-substitution :from binder :to fresh))))
    (values (apply-substitution substs (forall-ty-type forall-ty))
            fresh-vars)))

(defgeneric contains-forall-ty-p (type)
  (:method ((type forall-ty))
    t)
  (:method ((type tapp))
    (or (contains-forall-ty-p (tapp-from type))
        (contains-forall-ty-p (tapp-to type))))
  (:method ((entry keyword-ty-entry))
    (contains-forall-ty-p (keyword-ty-entry-type entry)))
  (:method ((type function-ty))
    (or (some #'contains-forall-ty-p (function-ty-positional-input-types type))
        (some #'contains-forall-ty-p (function-ty-keyword-input-types type))
        (some #'contains-forall-ty-p (function-ty-output-types type))))
  (:method ((type result-ty))
    (some #'contains-forall-ty-p (result-ty-output-types type)))
  (:method ((type qualified-ty))
    (or (some #'contains-forall-ty-p (qualified-ty-predicates type))
        (contains-forall-ty-p (qualified-ty-type type))))
  (:method ((pred ty-predicate))
    (some #'contains-forall-ty-p (ty-predicate-types pred)))
  (:method ((list list))
    (some #'contains-forall-ty-p list))
  (:method ((type t))
    nil))

(defgeneric remove-source-info (ty)
  (:method ((pred ty-predicate))
    (make-ty-predicate
     :class (ty-predicate-class pred)
     :types (mapcar #'remove-source-info (ty-predicate-types pred))
     :location nil))

  (:method ((qual-ty qualified-ty))
    (make-qualified-ty
     :predicates (mapcar #'remove-source-info (qualified-ty-predicates qual-ty))
     :type (remove-source-info (qualified-ty-type qual-ty))))

  (:method ((type forall-ty))
    (make-forall-ty
     :alias (mapcar #'remove-source-info (ty-alias type))
     :explicit-p (forall-ty-explicit-p type)
     :binders (forall-ty-binders type)
     :type (remove-source-info (forall-ty-type type))))

  (:method ((type tapp))
    (make-tapp
     :alias (mapcar #'remove-source-info (ty-alias type))
     :from (remove-source-info (tapp-from type))
     :to (remove-source-info (tapp-to type))))

  (:method ((entry keyword-ty-entry))
    (make-keyword-ty-entry
     :keyword (keyword-ty-entry-keyword entry)
     :type (remove-source-info (keyword-ty-entry-type entry))))

  (:method ((type function-ty))
    (make-function-ty
     :alias (mapcar #'remove-source-info (ty-alias type))
     :positional-input-types (mapcar #'remove-source-info (function-ty-positional-input-types type))
     :keyword-input-types (mapcar #'remove-source-info (function-ty-keyword-input-types type))
     :keyword-open-p (function-ty-keyword-open-p type)
     :origin (function-ty-origin type)
     :output-types (mapcar #'remove-source-info (function-ty-output-types type))))

  (:method ((type result-ty))
    (make-result-ty
     :alias (mapcar #'remove-source-info (ty-alias type))
     :output-types (mapcar #'remove-source-info (result-ty-output-types type))))

  (:method ((type ty))
    type))

;;;
;;; Methods
;;;

(defmethod apply-substitution (subst-list (type ty-predicate))
  (declare (type substitution-list subst-list)
           (values ty-predicate &optional))
  (rebuild-if-changed (subst-list type)
      ((types (ty-predicate-types type))
       (evidence (ty-predicate-superclass-evidence type)))
    (make-ty-predicate
     :class (ty-predicate-class type)
     :types types
     :superclass-evidence evidence
     :location (ty-predicate-location type))))

(defmethod apply-ksubstitution (subs (type ty-predicate))
  (declare (type ksubstitution-list subs))
  (make-ty-predicate
   :class (ty-predicate-class type)
   :types (apply-ksubstitution subs (ty-predicate-types type))))

(defmethod type-variables ((type ty-predicate))
  (type-variables (ty-predicate-types type)))

(defmethod kind-variables-generic% ((type ty-predicate))
  (declare (values kyvar-list &optional))
  (mapcan #'kind-variables (copy-list (ty-predicate-types type))))

(defmethod instantiate (types (type ty-predicate))
  ;; A new use of the scheme does not inherit the defining dictionary's witnesses.
  (make-ty-predicate
   :class (ty-predicate-class type)
   :types (instantiate types (ty-predicate-types type))))

(defmethod apply-substitution (subst-list (type qualified-ty))
  (declare (type substitution-list subst-list))
  (rebuild-if-changed (subst-list type)
      ((predicates (qualified-ty-predicates type))
       (new-type (qualified-ty-type type)))
    (make-qualified-ty :predicates predicates :type new-type)))

(defmethod apply-ksubstitution (subs (type qualified-ty))
  (declare (type ksubstitution-list subs))
  (make-qualified-ty
   :predicates (apply-ksubstitution subs (qualified-ty-predicates type))
   :type (apply-ksubstitution subs (qualified-ty-type type))))

(defmethod type-variables ((type qualified-ty))
  (remove-duplicates
   (append (type-variables (qualified-ty-predicates type))
           (type-variables (qualified-ty-type type)))
   :test #'ty=))

(defmethod kind-variables-generic% ((type qualified-ty))
  (declare (values kyvar-list &optional))
  (nconc
   (kind-variables-generic% (qualified-ty-type type))
   (mapcan #'kind-variables (copy-list (qualified-ty-predicates type)))))

(defmethod instantiate (types (type qualified-ty))
  (make-qualified-ty
   :predicates (instantiate types (qualified-ty-predicates type))
   :type (instantiate types (qualified-ty-type type))))

(defmethod kind-of ((type qualified-ty))
  (kind-of (qualified-ty-type type)))

(defmethod function-type-p ((type qualified-ty))
  (function-type-p (qualified-ty-type type)))

(defmethod function-return-type ((type qualified-ty))
  (qualify nil (function-return-type (qualified-ty-type type))))

(defmethod function-type-arguments ((type qualified-ty))
  (function-type-arguments (qualified-ty-type type)))

(defmethod apply-substitution (subst-list (type forall-ty))
  (declare (type substitution-list subst-list)
           (values forall-ty &optional))
  (let ((filtered-substs
          (remove-if (lambda (s)
                       (find (substitution-from s) (forall-ty-binders type) :test #'ty=))
                     subst-list)))
    (if (null filtered-substs)
        type
        (rebuild-if-changed (filtered-substs type)
            ((alias (ty-alias type))
             (qual-ty (forall-ty-type type)))
          (make-forall-ty
           :alias alias
           :explicit-p (forall-ty-explicit-p type)
           :binders (forall-ty-binders type)
           :type qual-ty)))))

(defmethod apply-ksubstitution (subs (type forall-ty))
  (declare (type ksubstitution-list subs)
           (values forall-ty &optional))
  (make-forall-ty
   :alias (mapcar (lambda (alias) (apply-ksubstitution subs alias)) (ty-alias type))
   :explicit-p (forall-ty-explicit-p type)
   :binders (mapcar (lambda (b) (apply-ksubstitution subs b)) (forall-ty-binders type))
   :type (apply-ksubstitution subs (forall-ty-type type))))

(defmethod type-variables ((type forall-ty))
  (remove-if (lambda (var)
               (find var (forall-ty-binders type) :test #'ty=))
             (type-variables (forall-ty-type type))))

(defmethod open-function-origins ((type forall-ty))
  (remove-if (lambda (var)
               (find var (forall-ty-binders type) :test #'ty=))
             (open-function-origins (forall-ty-type type))))

(defmethod kind-variables-generic% ((type forall-ty))
  (declare (values kyvar-list &optional))
  (nconc (mapcan #'kind-variables (copy-list (forall-ty-binders type)))
         (kind-variables-generic% (forall-ty-type type))))

(defmethod coalton-impl/typechecker/types::type-constructors-generic% ((type ty-predicate))
  (coalton-impl/typechecker/types::type-constructors-generic% (ty-predicate-types type)))

(defmethod coalton-impl/typechecker/types::type-constructors-generic% ((type qualified-ty))
  (append (coalton-impl/typechecker/types::type-constructors-generic% (qualified-ty-predicates type))
          (coalton-impl/typechecker/types::type-constructors-generic% (qualified-ty-type type))))

(defmethod coalton-impl/typechecker/types::type-constructors-generic% ((type forall-ty))
  (coalton-impl/typechecker/types::type-constructors-generic% (forall-ty-type type)))

(defmethod instantiate (types (type forall-ty))
  (let* ((fresh-binders
           (loop :for binder :in (forall-ty-binders type)
                 :collect (fresh-type-renamer binder)))
         (binder-substs
           (loop :for binder :in (forall-ty-binders type)
                 :for fresh :in fresh-binders
                 :collect (make-substitution :from binder :to fresh)))
         (inst-qual-ty
           (apply-substitution binder-substs
                               (instantiate types (forall-ty-type type)))))
    (make-forall-ty
     :alias (mapcar (lambda (alias) (instantiate types alias)) (ty-alias type))
     :explicit-p (forall-ty-explicit-p type)
     :binders fresh-binders
     :type inst-qual-ty)))

(defmethod kind-of ((type forall-ty))
  (declare (ignore type))
  +kstar+)

(defvar *forall-ty=-counter* -1)

(defmethod ty= ((type1 forall-ty) (type2 forall-ty))
  (and (= (length (forall-ty-binders type1))
          (length (forall-ty-binders type2)))
       (every (lambda (b1 b2)
                (and (equalp (tyvar-kind b1) (tyvar-kind b2))
                     (eq (tyvar-allow-result-p b1) (tyvar-allow-result-p b2))))
              (forall-ty-binders type1)
              (forall-ty-binders type2))
       (let* ((base-id *forall-ty=-counter*)
              (count (length (forall-ty-binders type1)))
              (*forall-ty=-counter* (- base-id count))
              (shared-vars
                (loop :for b1 :in (forall-ty-binders type1)
                      :for i :below count
                      :collect (make-tgen :id (- base-id i)
                                          :allow-result-p (tyvar-allow-result-p b1)
                                          :source-name (tyvar-source-name b1))))
              (subs1
                (loop :for b1 :in (forall-ty-binders type1)
                      :for sv :in shared-vars
                      :collect (make-substitution :from b1 :to sv)))
              (subs2
                (loop :for b2 :in (forall-ty-binders type2)
                      :for sv :in shared-vars
                      :collect (make-substitution :from b2 :to sv))))
         (qualified-ty= (apply-substitution subs1 (forall-ty-type type1))
                        (apply-substitution subs2 (forall-ty-type type2))))))
