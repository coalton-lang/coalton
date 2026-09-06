(defpackage #:coalton-impl/typechecker/unify
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/type-errors
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/substitutions
   #:coalton-impl/typechecker/predicate)
  (:export
   #:unify                              ; FUNCTION
   #:match                              ; FUNCTION
   #:predicate-mgu                      ; FUNCTION
   #:predicate-match                    ; FUNCTION
   #:match-list                         ; FUNCTION
   #:match-list-p                       ; PREDICATE
   #:unify-list                         ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/unify)

;;;
;;; Type unification
;;;

(defun unify (substs type1 type2)
  "Unify TYPE1 and TYPE2 under given substitutions, returning an updated substitution list"
  (let ((new-substs (mgu (apply-substitution substs type1)
                         (apply-substitution substs type2))))
    (compose-substitution-lists new-substs substs)))

(defun function-keyword-entry (entries keyword)
  (declare (type keyword keyword)
           (values (or null keyword-ty-entry) &optional))
  (find keyword entries :key #'keyword-ty-entry-keyword :test #'eq))

(defun unary-function-type-as-arrow-type (type)
  "Return TYPE as a fully applied `Arrow` type when that encoding exists."
  (declare (type function-ty type)
           (values (or null tapp) &optional))
  (when (and (= 1 (length (function-ty-positional-input-types type)))
             (= 1 (length (function-ty-output-types type)))
             (null (function-ty-keyword-input-types type))
             (not (function-ty-keyword-open-p type)))
    (make-tapp
     :from (make-tapp
            :from *arrow-type*
            :to (first (function-ty-positional-input-types type)))
     :to (output-types-result-type (function-ty-output-types type)))))

(defun ensure-compatible-function-types (type1 type2 condition)
  "Verify that two function types have compatible structure for unification.

Checks that positional arity, keyword count, output arity, and keyword-open-p
all match exactly.  Also checks that every keyword in type1 exists in type2.
Because the keyword counts are equal, this subset check implies set equality:
same count + type1 ⊆ type2 ⟹ type1 = type2 (as keyword sets).

One result-capable output variable may stand for an entire result pack,
including zero outputs (Void). Ordinary value variables cannot do so."
  (flet ((fail ()
           (error condition :type1 type1 :type2 type2)))
    (unless (= (length (function-ty-positional-input-types type1))
               (length (function-ty-positional-input-types type2)))
      (fail))
    (unless (= (length (function-ty-keyword-input-types type1))
               (length (function-ty-keyword-input-types type2)))
      (fail))
    (unless (or (= (length (function-ty-output-types type1))
                   (length (function-ty-output-types type2)))
                (some (lambda (outputs)
                        (and (= 1 (length outputs))
                             (tyvar-p (first outputs))
                             (tyvar-allow-result-p (first outputs))))
                      (list (function-ty-output-types type1)
                            (function-ty-output-types type2))))
      (fail))
    (unless (eq (function-ty-keyword-open-p type1)
                (function-ty-keyword-open-p type2))
      (fail))
    (dolist (entry (function-ty-keyword-input-types type1))
      (unless (function-keyword-entry (function-ty-keyword-input-types type2)
                                      (keyword-ty-entry-keyword entry))
        (fail)))))

(defun mgu-keyword-function-types (type1 type2)
  (let ((subs nil))
    (loop :for from-type :in (function-ty-positional-input-types type1)
          :for to-type :in (function-ty-positional-input-types type2)
          :do (setf subs
                    (compose-substitution-lists
                     (mgu (apply-substitution subs from-type)
                          (apply-substitution subs to-type))
                     subs)))
    (dolist (entry1 (function-ty-keyword-input-types type1))
      (let ((entry2 (function-keyword-entry (function-ty-keyword-input-types type2)
                                            (keyword-ty-entry-keyword entry1))))
        (setf subs
              (compose-substitution-lists
               (mgu (apply-substitution subs (keyword-ty-entry-type entry1))
                    (apply-substitution subs (keyword-ty-entry-type entry2)))
               subs))))
    (unify subs
           (output-types-result-type (function-ty-output-types type1))
           (output-types-result-type (function-ty-output-types type2)))))

(defun match-keyword-function-types (type1 type2)
  ;; Match components independently and require their source bindings to agree.
  ;; Applying one component's solution to the next would make target variables
  ;; available for binding, turning one-way matching into unification.
  (reduce #'merge-substitution-lists
          (append
           (list (match-list (function-ty-positional-input-types type2)
                             (function-ty-positional-input-types type1)))
           (loop :for entry1 :in (function-ty-keyword-input-types type1)
                 :for entry2 := (function-keyword-entry (function-ty-keyword-input-types type2)
                                                        (keyword-ty-entry-keyword entry1))
                 :collect (match (keyword-ty-entry-type entry1) (keyword-ty-entry-type entry2)))
           (list (match (output-types-result-type (function-ty-output-types type1))
                        (output-types-result-type (function-ty-output-types type2)))))
          :initial-value nil))

(defun ensure-compatible-result-types (type1 type2 condition)
  (unless (= (length (result-ty-output-types type1))
             (length (result-ty-output-types type2)))
    (error condition :type1 type1 :type2 type2)))

(defun mgu-result-types (type1 type2)
  (let ((subs nil))
    (loop :for from-type :in (result-ty-output-types type1)
          :for to-type :in (result-ty-output-types type2)
          :do (setf subs
                    (compose-substitution-lists
                     (mgu (apply-substitution subs from-type)
                          (apply-substitution subs to-type))
                     subs)))
    subs))

(defun match-result-types (type1 type2)
  (match-list (result-ty-output-types type2) (result-ty-output-types type1)))

(defgeneric mgu (type1 type2)
  (:documentation "Returns a SUBSTITUTION-LIST of the most general substitutions required to unify TYPE1 and TYPE2.")
  (:method ((type1 tapp) (type2 tapp))
    (let* ((s1 (mgu (tapp-from type1)
                    (tapp-from type2)))
           (s2 (mgu (apply-substitution s1 (tapp-to type1))
                    (apply-substitution s1 (tapp-to type2)))))
      (compose-substitution-lists s2 s1)))
  (:method ((type1 tapp) (type2 function-ty))
    (let ((arrow-type2 (unary-function-type-as-arrow-type type2)))
      (if arrow-type2
          (mgu type1 arrow-type2)
          (error 'unification-error :type1 type1 :type2 type2))))
  (:method ((type1 function-ty) (type2 tapp))
    (let ((arrow-type1 (unary-function-type-as-arrow-type type1)))
      (if arrow-type1
          (mgu arrow-type1 type2)
          (error 'unification-error :type1 type1 :type2 type2))))
  (:method ((type1 function-ty) (type2 function-ty))
    (ensure-compatible-function-types type1 type2 'unification-error)
    (mgu-keyword-function-types type1 type2))
  (:method ((type1 result-ty) (type2 result-ty))
    (ensure-compatible-result-types type1 type2 'unification-error)
    (mgu-result-types type1 type2))
  (:method ((type1 tyvar) (type2 ty))
    (bind-variable type1 type2))
  (:method ((type1 ty) (type2 tyvar))
    (bind-variable type2 type1))
  (:method ((type1 tycon) (type2 tycon))
    (if (ty= type1 type2)
        nil
        (error 'unification-error :type1 type1 :type2 type2)))
  (:method ((type1 ty) (type2 ty))
    (error 'unification-error :type1 type1 :type2 type2)))

(defun bind-variable (tyvar type)
  (cond
    ((and (tyvar-p type)
          (ty= type tyvar))
     nil)
    ((and (typep type 'result-ty)
          (not (tyvar-allow-result-p tyvar)))
     (error 'unification-error :type1 tyvar :type2 type))
    ((tyvar-p type)
     (when (not (equalp (kind-of tyvar)
                        (kind-of type)))
       (error 'kind-mismatch-error
              :type tyvar
              :kind (kind-of type)))
     ;; Preserve the stricter representative when only one side may
     ;; unify with result packs, so ordinary value variables never
     ;; widen into result-pack variables through indirection.
     (cond
       ((and (not (tyvar-allow-result-p tyvar))
             (tyvar-allow-result-p type))
        (list (make-substitution :from type :to tyvar)))
       ((and (tyvar-allow-result-p tyvar)
             (not (tyvar-allow-result-p type)))
        (list (make-substitution :from tyvar :to type)))
       (t
        (list (make-substitution :from tyvar :to type)))))
    ((find tyvar (type-variables type) :test #'ty=)
     (error 'infinite-type-unification-error :type type))
    ((not (equalp (kind-of tyvar)
                  (kind-of type)))
     (error 'kind-mismatch-error
            :type tyvar
            :kind (kind-of type)))
    (t (list (make-substitution :from tyvar :to type)))))

(defgeneric match (type1 type2)
  (:documentation "Returns a SUBSTITUTION-LIST which unifies TYPE1 to TYPE2

apply s type1 == type2")
  (:method ((type1 tapp) (type2 tapp))
    (let ((s1 (match (tapp-from type1) (tapp-from type2)))
          (s2 (match (tapp-to type1) (tapp-to type2))))
      (merge-substitution-lists s1 s2)))
  (:method ((type1 tapp) (type2 function-ty))
    (let ((arrow-type2 (unary-function-type-as-arrow-type type2)))
      (if arrow-type2
          (match type1 arrow-type2)
          (error 'unification-error :type1 type1 :type2 type2))))
  (:method ((type1 function-ty) (type2 tapp))
    (let ((arrow-type1 (unary-function-type-as-arrow-type type1)))
      (if arrow-type1
          (match arrow-type1 type2)
          (error 'unification-error :type1 type1 :type2 type2))))
  (:method ((type1 function-ty) (type2 function-ty))
    (ensure-compatible-function-types type1 type2 'unification-error)
    (match-keyword-function-types type1 type2))
  (:method ((type1 result-ty) (type2 result-ty))
    (ensure-compatible-result-types type1 type2 'unification-error)
    (match-result-types type1 type2))
  (:method ((type1 tyvar) (type2 ty))
    (cond
      ((not (equalp (kind-of type1) (kind-of type2)))
       (error 'type-kind-mismatch-error :type1 type1 :type2 type2))
      ((and (not (tyvar-allow-result-p type1))
            (typep type2 'result-ty))
       (error 'unification-error :type1 type1 :type2 type2))
      ((and (not (tyvar-allow-result-p type1))
            (tyvar-p type2)
            (tyvar-allow-result-p type2))
       (error 'unification-error :type1 type1 :type2 type2))
      (t
       (list (make-substitution :from type1 :to type2)))))
  (:method ((type1 tycon) (type2 tycon))
    (if (ty= type1 type2)
        nil
        (error 'unification-error :type1 type1 :type2 type2)))
  (:method ((type1 ty) (type2 ty))
    (error 'unification-error :type1 type1 :type2 type2)))


;;;
;;; Predicate unification
;;;

(defun predicate-mgu (pred1 pred2)
  "Returns a SUBSTITUTION-LIST of the most general substitutions required to unify PRED1 and PRED2."
  (declare (type ty-predicate pred1 pred2))
  (unless (eql (ty-predicate-class pred1)
               (ty-predicate-class pred2))
    (error 'predicate-unification-error :pred1 pred1 :pred2 pred2))
  (handler-case
      (let ((subs nil))
        (loop :for pred-type1 :in (ty-predicate-types pred1)
              :for pred-type2 :in (ty-predicate-types pred2)
              :do (setf subs (unify subs pred-type1 pred-type2)))
        subs)
    (coalton-internal-type-error ()
      (error 'predicate-unification-error :pred1 pred1 :pred2 pred2))))

(defun predicate-match (pred1 pred2 &optional subs)
  "Returns a SUBSTITUTION-LIST of the most general substitutions required to unify PRED1 to PRED2."
  (declare (type ty-predicate pred1 pred2)
           (type substitution-list subs))
  (unless (eq (ty-predicate-class pred1)
               (ty-predicate-class pred2))
    (error 'predicate-unification-error :pred1 pred1 :pred2 pred2))
  (handler-case
      (compose-substitution-lists
       (match-list (apply-substitution subs (ty-predicate-types pred2))
                   (apply-substitution subs (ty-predicate-types pred1)))
       subs)
    (coalton-internal-type-error ()
      (error 'predicate-unification-error :pred1 pred1 :pred2 pred2))))

(defun match-list (list1 list2)
  "Returns true if all of the types in LIST2 match the types in LIST1 pairwise"
  (declare (type ty-list list1)
           (type ty-list list2))
  (reduce #'merge-substitution-lists
          (loop :for t1 :in list1
                :for t2 :in list2

                :collect (match t2 t1))
          :initial-value nil))

(defun match-list-p (list1 list2)
  (handler-case (progn (match-list list1 list2) t)
    (coalton-internal-type-error () nil)))

(defun unify-list (subs list1 list2)
  (declare (type substitution-list subs)
           (type ty-list list1)
           (type ty-list list2))
  (loop :for t1 :in list1
        :for t2 :in list2

        :do (setf subs (unify subs t1 t2)))

  subs)
