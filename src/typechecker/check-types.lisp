(in-package #:coalton-impl/typechecker)

(defun check-binding-node-types (bindings env)
  (dolist (binding bindings)
    (unless (type-scheme= (check-node-type (cdr binding) env)
                          (lookup-value-type env (car binding)))
      (error 'invalid-typed-node-type
             :node (cdr binding)
             :inferred-type (lookup-value-type env (car binding))))))

(defgeneric check-node-type (node env)
  (:documentation "Check the type of a typed node erroring on invalid node types")
  
  (:method ((node typed-node-literal) env)
    (let ((literal-value (typed-node-literal-value node)))
      (multiple-value-bind (type preds)
          (derive-literal-type literal-value)
        (let ((scheme (to-scheme (qualify preds type))))
          (unless (type-scheme=
                   scheme
                   (typed-node-type node))
            (error 'invalid-typed-node-type
                   :node node
                   :inferred-type type))
          scheme))))

  (:method ((node typed-node-variable) env)
    (let ((env-type (fresh-inst (lookup-value-type env (typed-node-variable-name node))))
          (node-type (fresh-inst (typed-node-type node))))
      ;; TODO: Match predicates
      (let ((subs (match (qualified-ty-type env-type)
                    (qualified-ty-type node-type))))
        (loop :for env-pred :in (qualified-ty-predicates env-type)
              :for node-pred :in (qualified-ty-predicates node-type) :do
                (unless (type-predicate=
                         (apply-substitution subs env-pred)
                         (apply-substitution subs node-pred))
                  (error 'invalid-typed-node-type
                         :node node
                         :inferred-type (lookup-value-type env (typed-node-variable-name node)))))))
    (typed-node-type node))

  (:method ((node typed-node-lisp) env)
    (typed-node-type node))

  (:method ((node typed-node-application) env)
    (check-application-node-type
     (typed-node-application-rator node)
     (typed-node-application-rands node)
     node env))

  (:method ((node typed-node-direct-application) env)
    ;; TODO: something else here
    (dolist (rand (typed-node-direct-application-rands node))
      (check-node-type rand env))
    (typed-node-type node))

  (:method ((node typed-node-abstraction) env)
    (let ((env (push-value-environment env (typed-node-abstraction-vars node))))
      (check-node-type (typed-node-abstraction-subexpr node) env))

    (let* ((arg-qual-types (mapcar (lambda (b)
                                     (fresh-inst (cdr b)))
                                   (typed-node-abstraction-vars node)))
           (arg-types (mapcar #'qualified-ty-type arg-qual-types))

           (ret-qual-type (fresh-inst (typed-node-type (typed-node-abstraction-subexpr node))))
           (ret-type (qualified-ty-type ret-qual-type))
           
           (function-type (make-function-type* arg-types ret-type))

           (local-vars (type-variables function-type))
           (expected-type (quantify local-vars
                                    (qualify (remove-if-not (lambda (x) (member x local-vars))
                                                            (collect-type-predicates node))
                                             function-type))))
      (unless (sub-ty-scheme-p (typed-node-type node)
                               expected-type
                               env)
        (error 'invalid-typed-node-type
               :node node
               :inferred-type expected-type)))
    (typed-node-type node))

  (:method ((node typed-node-let) env)
    (let ((env (push-value-environment env
                                       (mapcar (lambda (b)
                                                 (cons (car b) (typed-node-type (cdr b))))
                                               (typed-node-let-bindings node)))))
      (dolist (binding (typed-node-let-bindings node))
        (check-node-type (cdr binding) env))
      (check-node-type (typed-node-let-subexpr node) env))

    (typed-node-type node))

  (:method ((node typed-node-match) env)
    (dolist (branch (typed-node-match-branches node))
      (let* ((env (push-value-environment env (typed-match-branch-bindings branch)))
             (branch-type (check-node-type (typed-match-branch-subexpr branch) env)))
        (unless (sub-ty-scheme-p
                 branch-type
                 (typed-node-type node)
                 env)
          (error 'invalid-typed-node-type
                 :node (typed-node-type node)
                 :inferred-type branch-type))))
    (typed-node-type node))

  (:method ((node typed-node-seq) env)
    (let ((last-node (car (last (typed-node-seq-subnodes node)))))
      (unless (sub-ty-scheme-p (typed-node-type node)
			       (typed-node-type last-node)
			       env)
	(error 'invalid-typed-node-type
	       :node (typed-node-type node)
	       :inferred-type (typed-node-type last-node)))

      (dolist (sub-node (typed-node-seq-subnodes node))
	(check-node-type sub-node env)))))

(defun check-application-node-type (rator rands node env)
  (declare (type typed-node rator node)
	   (type typed-node-list rands)
	   (type environment env))
  (let* ((rator-qual-type (fresh-inst (typed-node-type rator)))
         (rator-type (qualified-ty-type rator-qual-type))
         (rator-preds (qualified-ty-predicates rator-qual-type))
         
         (rands-qual-types (mapcar (lambda (r) (fresh-inst (typed-node-type r))) rands))
         (rands-types (mapcar (lambda (r) (qualified-ty-type r)) rands-qual-types))
         (rands-preds (mapcan (lambda (r) (qualified-ty-predicates r)) rands-qual-types))

         (preds (append rands-preds rator-preds)))
    (loop :for rand-type :in rands-types :do
      (unless (function-type-p rator-type)
        (coalton-impl::coalton-bug "Typed node application rator is not of type function"))
        
      (let* ((param-type (function-type-from rator-type))
             (subs (unify nil rand-type param-type)))
        (setf rator-type (apply-substitution subs (function-type-to rator-type))
              preds (apply-substitution subs preds))))

    (let* ((inferred-type (quantify (type-variables rator-type)
                                    (qualify nil rator-type))))
      (unless (sub-ty-scheme-p (typed-node-type node)
                               inferred-type
                               env)
        (error 'invalid-typed-node-type
               :node node
               :inferred-type inferred-type)))

    ;; Recurse down to subnodes
    (check-node-type rator env)
    (dolist (rand rands)
      (check-node-type rand env))

    ;; Ensure that if there are predicates then the rator must be a variable
    #+ignore
    (unless (null (qualified-ty-predicates
                   (fresh-inst (typed-node-type node))))
      (unless (typed-node-variable-p rator)
        (coalton-impl::coalton-bug "Predicate appears in non-variable application")))

    ;; And we have the same number of predicates
    ;; NOTE: This should be updated to perform a unificaiton on predicates
    (when (typed-node-variable-p rator)
      (unless (= (length (qualified-ty-predicates
                          (fresh-inst (lookup-value-type
                                       env
                                       (typed-node-variable-name
                                        (typed-node-application-rator node))))))
                 (length (qualified-ty-predicates
                          
                          (fresh-inst (typed-node-type (typed-node-application-rator node))))))
        (coalton-impl::coalton-bug "Number of predicates not equal in application of variable")))
      
    (typed-node-type node)))

(defun sub-ty-scheme-p (scheme1 scheme2 env)
  "Is SCHEME1 a sub-scheme of SCHEME2?"
  (let* ((qual1 (fresh-inst scheme1))
         (qual2 (fresh-inst scheme2))

         (type1 (qualified-ty-type qual1))
         (type2 (qualified-ty-type qual2))

         (subs (match type2 type1)))
    (loop :for pred :in (qualified-ty-predicates qual2) :do
        (unless (entail env
                        (qualified-ty-predicates qual1)
                        (apply-substitution subs pred))
          (return-from sub-ty-scheme-p nil)))
    t))
