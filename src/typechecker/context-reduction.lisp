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
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:entail                             ; FUNCTION
   #:reduce-context                     ; FUNCTION
   #:split-context                      ; FUNCTION
   #:default-preds                      ; FUNCTION
   #:default-subs                       ; FUNCTION
   ))

(in-package #:coalton-impl/typechecker/context-reduction)

;;
;; Context reduction
;;

(defun predicate= (pred1 pred2)
  (declare (type ty-predicate pred1)
           (type ty-predicate pred2)
           (values boolean))
  (and (equalp (ty-predicate-class pred1) (ty-predicate-class pred2))
       (equalp (ty-predicate-types pred1) (ty-predicate-types pred2))))

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
                                        (ty-class-instance-constraints inst))))
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
          (or (true (member pred super :test #'predicate=))
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
  (true (member pred (mapcan (lambda (p) (by-super env p)) preds) :test #'predicate=)))

(defun simplify-context (f preds)
  "Simplify PREDS to head-normal form"
  (labels ((simp-loop (rs ps)
             (if (endp ps)
                 rs
                 (if (funcall f (append rs (rest ps)) (first ps))
                     (simp-loop rs (rest ps))
                     (simp-loop (append (list (first ps)) rs) (rest ps))))))
    (simp-loop nil preds)))

;; TODO: Remove unused param
(defun reduce-context (env preds subs &key (allow-deferred-predicates t))
  (declare (ignore allow-deferred-predicates))
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
                 ;; Check that for the predicates containg VAR, VAR is their only type variable
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
          :do (error 'ambigious-constraint :pred (first (ambiguity-preds ambig)))
        
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
