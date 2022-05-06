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
           (values boolean &optional))
  (true (member pred (mapcan (lambda (p) (by-super env p)) preds) :test #'equalp)))

(defun simplify-context (f preds)
  "Simplify PREDS to head-normal form"
  (labels ((simp-loop (rs ps)
             (if (endp ps)
                 rs
                 (if (funcall f (append rs (rest ps)) (first ps))
                     (simp-loop rs (rest ps))
                     (simp-loop (append (list (first ps)) rs) (rest ps))))))
    (simp-loop nil preds)))

(defun reduce-context (env preds subs &key (allow-deferred-predicates t))
  (declare (ignore allow-deferred-predicates))
  (let ((env (apply-substitution subs env)))
    (simplify-context
     (lambda (preds pred)
       (super-entail env preds pred))
     (loop :for pred :in (apply-substitution subs preds)
          :unless (entail env nil pred)
            :collect pred))))

(defun split-context (env fixed-vars preds subs)
  "Split PREDS into retained predicates and deferred predicates

Returns (VALUES deferred-preds retained-preds)"
  (declare (values ty-predicate-list ty-predicate-list))
  (let* ((reduced-preds (reduce-context env preds subs)))
    (loop :for p :in reduced-preds
          :if (every (lambda (tv) (member tv fixed-vars :test #'equalp))
                     (type-variables p))
            :collect p :into deferred
          :else
            :collect p :into retained
          :finally (return (values deferred retained)))))
