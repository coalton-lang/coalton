(defpackage #:coalton-impl/typechecker/context-reduction
  (:use
   #:cl
   #:coalton-impl/typechecker/type-errors
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/substitutions
   #:coalton-impl/typechecker/predicate
   #:coalton-impl/typechecker/unify
   #:coalton-impl/typechecker/environment)
  (:import-from
   #:coalton-impl/typechecker/substitutions
   #:apply-substitution
   #:substitution-list)
  (:import-from
   #:coalton-impl/typechecker/fundeps
   #:closure)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:entail                             ; FUNCTION
   #:fundep-entail                      ; FUNCTION
   #:reduce-context                     ; FUNCTION
   #:split-context                      ; FUNCTION
   #:default-preds                      ; FUNCTION
   #:default-subs                       ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/context-reduction)

;;
;; Context reduction
;;

(defun true (x)
  (if x
      t
      nil))

(defun by-super (env pred)
  "Recursively get all super classes of predicate

Requires acyclic superclasses"
  (declare (type environment env)
           (type ty-predicate pred)
           (values ty-predicate-list))
  (let* ((class (lookup-class env (ty-predicate-class pred)))
         (subs (predicate-match (ty-class-predicate class) pred)))
    (cons
     pred
     (loop
       :for super-class-pred :in (ty-class-superclasses class)
       :append (by-super
                env
                (apply-substitution subs super-class-pred))))))

(defun by-inst (env pred)
  "Find the first instance that matches PRED and return resulting predicate constraints

Returns (PREDS FOUNDP)"
  (declare (type environment env)
           (type ty-predicate pred)
           (values ty-predicate-list boolean))
  (fset:do-seq (inst (lookup-class-instances env (ty-predicate-class pred) :no-error t))
    (handler-case
        (let* ((subs (predicate-match (ty-class-instance-predicate inst) pred))
               (resulting-preds (mapcar (lambda (p) (apply-substitution subs p))
                                        (ty-class-instance-constraints-expanded inst env))))
          (return-from by-inst (values resulting-preds t)))
      (predicate-unification-error () nil)))
  (values nil nil))

(defun entail (env preds pred)
  "Does PRED hold if and only if all of PREDS hold?"
  (declare (type environment env)
           (type ty-predicate-list preds)
           (type ty-predicate pred)
           (values boolean))
  (let* ((super (mapcan (lambda (p) (by-super env p)) preds))
        (value
          (or (true (member pred super :test #'type-predicate=))
              (true (multiple-value-bind (inst-preds found)
                        (by-inst env pred)
                      (and found
                           (every (lambda (p) (entail env preds p)) inst-preds)))))))
    value))

(defun super-entail (env preds pred)
  "Does PRED hold if and only if all of PREDS hold, only checking superclass relations?"
  (declare (type environment env)
           (type ty-predicate-list preds)
           (type ty-predicate pred)
           (values boolean &optional))
  (true (member pred (mapcan (lambda (p) (by-super env p)) preds) :test #'type-predicate=)))

(defun simplify-context (f preds)
  "Simplify PREDS to head-normal form"
  (labels ((simp-loop (rs ps)
             (if (endp ps)
                 rs
                 (if (funcall f (append rs (rest ps)) (first ps))
                     (simp-loop rs (rest ps))
                     (simp-loop (append (list (first ps)) rs) (rest ps))))))
    (simp-loop nil preds)))

(defun reduce-context (env preds subs)
  (let ((env (apply-substitution subs env))
        (preds (apply-substitution subs preds)))
    (simplify-context
     (lambda (preds pred)
       (super-entail env preds pred))
     (loop :for pred :in (apply-substitution subs preds)
          :unless (entail env nil pred)
            :collect pred))))

(defun split-context (env environment-vars preds subs)
  "Split PREDS into retained predicates and deferred predicates

Returns (VALUES deferred-preds retained-preds defaultable-preds)"
  (declare (values ty-predicate-list ty-predicate-list))
  (let ((reduced-preds (reduce-context env preds subs)))

    (loop :for p :in reduced-preds
          :if (every (lambda (tv) (member tv environment-vars :test #'equalp))
                     (type-variables p))
            :collect p :into deferred
          :else
            :collect p :into retained
          :finally (return (values deferred retained)))))

(defstruct ambiguity
  (var   (util:required 'var)   :type tyvar             :read-only t)
  (preds (util:required 'preds) :type ty-predicate-list :read-only t))

(defun ambiguity-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'ambiguity-p x)))

(deftype ambiguity-list ()
  '(satisfies ambiguity-list-p))

(defun ambiguities (env tvars preds)
  (declare (type environment env)
           (type tyvar-list tvars)
           (type ty-predicate-list preds)
           (values ambiguity-list)
           (ignore env))
  (loop :for v :in (set-difference (type-variables preds) tvars :test #'equalp)
        :for preds_ := (remove-if-not
                        (lambda (pred)
                          (find v (type-variables pred) :test #'equalp))
                        preds)
        :collect (make-ambiguity :var v :preds preds_)))

(defun candidates (env ambig)
  (declare (type environment env)
           (type ambiguity ambig))
  (let* ((var (ambiguity-var ambig))     ; v
         (preds (ambiguity-preds ambig)) ; qs

         (pred-names (mapcar #'ty-predicate-class preds)) ; is
         (pred-heads (mapcar #'ty-predicate-types preds)) ; ts
         )
    (loop :for type :in (defaults env)

          :when (and
                 ;; Check that for the predicates containing VAR, VAR is their only type variable
                 ;;
                 ;; NOTE: Haskell has a much stricter check here. Haskell requires that the predicate
                 ;; is in the form "Pred [var]". Coalton will default the following other predicates
                 ;;
                 ;; * multiple variable classes "Pred [var var]" and "Pred [var String]"
                 ;; * more complex types "Pred [List var]"
                 (subsetp (type-variables pred-heads) (list var) :test #'equalp)

                 ;; Check that at least one predicate is a numeric class
                 (some (lambda (name)
                         (find name (num-classes) :test #'equalp))
                       pred-names)

                 ;; NOTE: Haskell checks that all predicates are stdlib classes here

                 ;; Check that the variable would be defaulted to a valid type
                 ;; for the given predicates
                 (every (lambda (name)
                          (entail env nil (make-ty-predicate :class name :types (list type))))
                        pred-names))

            :collect type)))

(defun defaults (env)
  (declare (type environment env)
           (ignore env)
           (values ty-list))
  (list *integer-type* *double-float-type* *single-float-type*))

(defun num-classes ()
  ;; Lookup class symbols if they exist. This allows defaulting to
  ;; work before the standard library is fully loaded
  (append (util:find-symbol? "NUM" "COALTON-LIBRARY/CLASSES")
          (util:find-symbol? "QUANTIZABLE" "COALTON-LIBRARY/MATH")
          (util:find-symbol? "RECIPROCABLE" "COALTON-LIBRARY/MATH")
          (util:find-symbol? "COMPLEX" "COALTON-LIBRARY/MATH")
          (util:find-symbol? "REMAINDER" "COALTON-LIBRARY/MATH")
          (util:find-symbol? "INTEGRAL" "COALTON-LIBRARY/MATH")))

(defun default-preds (env tvars preds)
  (declare (type environment env)
           (type tyvar-list tvars)
           (type ty-predicate-list preds)
           (values ty-predicate-list &optional))
  
  (loop :for ambig :in (ambiguities env tvars preds)
        :for candidates := (candidates env ambig)

        :unless candidates
          :do (error 'ambiguous-constraint :pred (first (ambiguity-preds ambig)))
        
        :when candidates
          :append (ambiguity-preds ambig)))

(defun default-subs (env tvars preds)
  (declare (type environment env)
           (type tyvar-list tvars)
           (type ty-predicate-list preds)
           (values substitution-list &optional))
  (loop :for ambig :in (ambiguities env tvars preds)
        :for candidates := (candidates env ambig)
        :when candidates
          :collect (make-substitution :from (ambiguity-var ambig) :to (first candidates))))

;;;
;;; When typechecking bindings with fundeps, there can be unambiguous
;;; type variables in predicates that do not appear in the bindings
;;; body.
;;;
;;; For a class defined like C:
;;;
;;;     class C :a :b :c (:a :b -> :c)
;;;       m :: :a -> :b
;;;
;;; The following is a valid definition:
;;;
;;;     f x = m x
;;;
;;; However if f had a type declaration like:
;;;
;;;     f :: C :a :b :c => :a -> :b
;;;
;;; Then its inferred predicates would need to be checked against its
;;; declared predicates. Because the class variable :c is not used
;;; anywhere, the declaration of f's type and the invocation of "m"
;;; would chose different type variables for :c. This would then error
;;; because "C :a :b :c" does not `entail' "C :a :b :d".
;;;
;;; The function `fundep-entail' takes a list of declared predicates,
;;; a list of inferred predicates, and a list of known type variables.
;;; It then finds inferred predicates that would match declared
;;; predicates if not for differing unknown types at identical
;;; indices.
;;;
;;; The other requirement is that unknown types must be within the
;;; "transitive `closure'" of the known types.

(defun fundep-entail (env expr-preds preds known-tyvars)
  "Produce a list of type substitutions that reduce the generality of
PREDS with respect to EXPR-PREDS based on functional dependencies.

For example, consider the following,

```
(define-class (C :a :b (:a -> :b)))
(define-class (C :a :b => D :a :b)
  (m :a))

(declare f (D :a :b => Unit -> :a))
(define (f) m)
```

For the example above, EXPR-PREDS includes only D #T1 #T2 and PREDS
includes only D #T1 #T3, and KNOWN-TYVARS includes only #T1. In the
absence of functional dependencies, the second type variable in the
predicate for class D is ambiguous, i.e., it cannot be determined.
But, functional dependencies allow us to determine that #T2 and #T3
must in fact be the same type, so FUNDEP-ENTAIL will return the
substitution #T3 +-> #T2. Note that in the absence of functional
dependencies, the class definition for D itself would not have passed
through the typechecker, because the method M does not contain all of
the type variables of the class D.
"
  (flet ((expand (context)
           (alexandria:mappend (alexandria:curry #'by-super env) context)))
    (loop :with expr-preds := (expand expr-preds)
          :with preds      := (expand preds)
          :with subs       := nil
          :for pred :in preds
          :for new-subs :=
            (if (entail env expr-preds pred)
                '()
                (fundep-entail% env expr-preds pred known-tyvars))
          :do (setf subs (compose-substitution-lists subs new-subs))
          :finally (return subs))))

(defun fundep-entail% (env expr-preds pred known-tyvars)
  "A helper function for FUNDEP-ENTAIL to be applied iteratively, binding
PRED to each element in a list of predicates. Note that we do not
consider superclass functional dependencies here. That is because the
predicates were expanded into their superclasses prior to the
applications of this function such that any relevant superclass
functional dependencies will be considered directly when the
respective superclass predicate is passed to this function."
  (let* ((class-name (ty-predicate-class pred))
         (class (lookup-class env class-name))
         (class-vars (ty-class-class-variables class))
         (fundeps (ty-class-fundeps class))
         (pred-tys (ty-predicate-types pred)))

    ;; If there are no fundeps, then there is no entailment to be had.
    (when (endp fundeps)
      (return-from fundep-entail% nil))

    (flet ((known-indices (pred)
             ;; This function collects the type indices that are considered
             ;; "known" given KNOWN-TYVARS.
             (loop :for ty :in (ty-predicate-types pred)
                   :for tyvars := (type-variables ty)
                   :for i :from 0
                   :when (consp (intersection tyvars known-tyvars :test #'ty=))
                     :collect i)))

      (let* ((known-indices (known-indices pred))
             (known-class-vars (util:project-indices known-indices class-vars))

             ;; We expand the list of known variables by computing the
             ;; closure of the known class variables over the class's
             ;; functional dependencies. See the docstring for CLOSURE
             ;; for more details.
             (closure (closure known-class-vars fundeps))

             ;; We collect the newly-determined vars and determine
             ;; their indices.
             (determined-class-vars
               (set-difference closure known-class-vars :test #'eq))
             (determined-indices
               (mapcar
                #'(lambda (determined-class-var)
                    (position determined-class-var class-vars :test #'eq))
                determined-class-vars)))

        ;; If there are no determined vars, then there is no
        ;; entailment to be had.
        (when (endp determined-class-vars)
          (return-from fundep-entail% nil))

        (flet ((known (tys)
                 (util:project-indices known-indices tys))
               (determined (tys)
                 (util:project-indices determined-indices tys)))
          (let* ((expr-pred
                   (find-if
                    #'(lambda (expr-pred)
                        ;; We find the expression predicate that is
                        ;; associated with PRED based on KNOWN-TYVARS.
                        (and (eq class-name (ty-predicate-class expr-pred))
                             (subsetp known-indices (known-indices expr-pred))
                             (every #'ty=
                                    (known pred-tys)
                                    (known (ty-predicate-types expr-pred)))))
                    expr-preds)))
            ;; There should always be an associated expression
            ;; predicate.
            (when (null expr-pred)
              (util:coalton-bug
                (format nil "No expression predicate matches ~a." pred)))
            ;; So, we create substitutions based on the
            ;; newly-determined types.
            (loop :with expr-pred-tys := (ty-predicate-types expr-pred)
                  :with new-subs := nil
                  :for from-ty :in (determined pred-tys)
                  :for to-ty :in (determined expr-pred-tys)
                  :do (setf new-subs
                            (compose-substitution-lists
                             (match from-ty to-ty)
                             new-subs))
                  :finally (return new-subs))))))))

