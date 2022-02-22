(in-package #:coalton-impl/typechecker)

;;;
;;; Type substitutions
;;;

(defstruct (substitution (:constructor %make-substitution (from to)))
  (from (required 'from) :type tyvar :read-only t)
  (to   (required 'to)   :type ty    :read-only t))

(defun substitution-list-p (thing)
  (and (alexandria:proper-list-p thing)
       (every (lambda (x) (typep x 'substitution)) thing)))

(deftype substitution-list ()
  '(satisfies substitution-list-p))

(defun merge-substitution-lists (s1 s2)
  "Merge substitution lists S1 and S2 together, erroring on disagreeing entries."
  (let ((overlap (intersection s1 s2 :key #'substitution-from :test #'equalp)))
    (if (every (lambda (x)
                 (equalp (apply-substitution s1 (%make-tvar x)) (apply-substitution s2 (%make-tvar x))))
               (mapcar #'substitution-from overlap))
        (concatenate 'list s1 s2)
        (error 'coalton-type-error))))

(defun compose-substitution-lists (s1 s2)
  "Compose substitution lists S1 and S2 together, applying S1 to S2."
  (append
   (mapcar
    (lambda (s)
      (%make-substitution
       (substitution-from s)
       (apply-substitution s1 (substitution-to s))))
    s2)
   s1))

(defgeneric apply-substitution (subst-list type)
  (:documentation "Apply the substitutions defined in SUBST-LIST on TYPE.")
  ;; For a type variable, substitute if it is in SUBST-LIST, otherwise return the original type
  (:method (subst-list (type tvar))
    (let ((subst (find (tvar-tyvar type) subst-list :key #'substitution-from :test #'equalp)))
      (if subst
          (substitution-to subst)
          type)))
  ;; For a type application, recurse down into all the types
  (:method (subst-list (type tapp))
    (%make-tapp (apply-substitution subst-list (tapp-from type))
                (apply-substitution subst-list (tapp-to type))))
  ;; Otherwise, do nothing
  (:method (subst-list (type ty))
    type)
  ;; Allow for calling on lists
  (:method (subst-list (type-list list))
    (mapcar (lambda (x) (apply-substitution subst-list x)) type-list)))

(defgeneric type-variables (type)
  (:documentation "Get a list containing the type variables in TYPE.")
  ;; For any type variable, simply return a list containing itself
  (:method ((type tvar))
    (list (tvar-tyvar type)))
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

(defun pprint-substution (stream sub &optional colon-p at-sign-p)
  (declare (ignore colon-p)
           (ignore at-sign-p))
  (format stream "#T~a" (tyvar-id (substitution-from sub)))
  (format stream " +-> ")
  (format stream "~a" (substitution-to sub))
  nil)

(set-pprint-dispatch 'substitution 'pprint-substution)
