(in-package #:coalton-impl/typechecker)

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
          (or (true (member pred super :test #'equalp))
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
           (values boolean))
  (true (member pred (mapcan (lambda (p) (by-super env p)) preds) :test #'equalp)))

(defun hnf-p (pred)
  "Is PRED in head-normal form?"
  (labels ((hnf (ty)
             (etypecase ty
               (tvar t)
               (tcon nil)
               (tapp (hnf (tapp-from ty))))))
    (every (lambda (type)
             (hnf type))
           (ty-predicate-types pred))))

(defun to-hnf (env pred)
  "Simplify PRED to a list of head-normal predicates"
  (if (hnf-p pred)
      (list pred)
      (multiple-value-bind (inst-preds found)
          (by-inst env pred)
        (unless found
          (error 'context-reduction-failure :pred pred))
        (mapcan (lambda (p) (to-hnf env p)) inst-preds))))

(defun simplify-context (env preds)
  "Simplify PREDS to head-normal form"
  (labels ((simp-loop (rs ps)
             (if (endp ps)
                 rs
                 (if (entail env (append rs (rest ps)) (first ps))
                     (simp-loop rs (rest ps))
                     (simp-loop (append (list (first ps)) rs) (rest ps))))))
    (simp-loop nil preds)))

(defun reduce-context (env preds)
  "Reduce predicate context PREDS in ENV"
  (simplify-context env
                    (mapcan (lambda (p)
                              (to-hnf env p))
                            preds)))

(defun split-context (env fixed-vars preds)
  "Split PREDS into retained predicates and deferred predicates

Returns (VALUES deferred-preds retained-preds)"
  (declare (values ty-predicate-list ty-predicate-list))
  (let* ((reduced-preds (reduce-context env preds)))
    (loop :for p :in reduced-preds
          :if (every (lambda (tv) (member tv fixed-vars :test #'equalp))
                     (type-variables p))
            :collect p :into deferred
          :else
            :collect p :into retained
          :finally (return (values deferred retained)))))
