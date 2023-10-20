(defpackage #:coalton-impl/codegen/optimizer
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/typecheck-node
   #:typecheck-node)
  (:import-from
   #:coalton-impl/codegen/resolve-instance
   #:pred-type
   #:resolve-static-dict)
  (:import-from
   #:coalton-impl/codegen/hoister
   #:hoister
   #:hoist-definition
   #:make-hoister
   #:pop-hoist-point
   #:push-hoist-point
   #:pop-final-hoist-point)
  (:import-from
   #:coalton-impl/codegen/monomorphize
   #:monomorphize
   #:make-candidate-manager)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:traverse-bindings
   #:traverse
   #:update-function-env
   #:make-function-table)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:optimize-bindings
   #:optimize-node))

(in-package #:coalton-impl/codegen/optimizer)

(defun optimize-bindings (bindings monomorphize-table package env)
  (declare (type binding-list bindings)
           (type hash-table monomorphize-table)
           (type package package)
           (type tc:environment env)
           (values binding-list tc:environment))


  (let ((bindings (optimize-bindings-initial bindings package env)))

    ;; Make code and environment data avaliable to the monomorphizer
    (loop :for (name . node) :in bindings
          :do (setf env (tc:set-code env name node)))

    (let* ((manager (make-candidate-manager))

           (resolve-table (alexandria:alist-hash-table bindings))

           (bindings
             (loop :with function-table := (make-function-table env)
                   :for (name . node) :in bindings
                   :collect (cons name (pointfree node function-table env)))))

      (loop :for (name . node) :in bindings
            :do (typecheck-node node env)
            :do (setf env (tc:set-code env name node)))

      ;; Run the monomorphizer
      (setf bindings
            (loop :for (name . node) :in bindings

                  :if (gethash name monomorphize-table)
                    :append (optimize-bindings-initial
                             (monomorphize
                              name
                              manager
                              package
                              resolve-table
                              (lambda (node env)
                                (optimize-node node env))
                              env)
                             package env)
                  :else
                    :collect (cons name node)))


      ;; Update function env
      (setf env (update-function-env bindings env))


      (let ((function-table (make-function-table env)))

        (setf bindings
              (loop :for (name . node) :in bindings
                    :collect (cons name (direct-application node function-table))))

        ;; Update code db
        (loop :for (name . node) :in bindings
              :do (setf env (tc:set-code env name node)))

        (loop :for (name . node) :in bindings
              :do (typecheck-node node env))

        (values bindings env)))))

(defun direct-application (node table)
  (declare (type node node)
           (type hash-table table)
           (values node &optional))
  (labels ((rewrite-direct-application (node &rest rest)
             (declare (ignore rest)
                      (dynamic-extent rest))
             (when (node-variable-p (node-application-rator node))
               (let ((name (node-variable-value (node-application-rator node))))
                 (when (and (gethash name table)
                            (equalp (gethash name table)
                                    (length (node-application-rands node))))
                   (return-from rewrite-direct-application
                     (make-node-direct-application
                      :type (node-type node)
                      :rator-type (node-type (node-application-rator node))
                      :rator name
                      :rands (node-application-rands node)))))))

           (add-local-funs (node &rest rest)
             (declare (ignore rest)
                      (dynamic-extent rest))
             (loop :for (name . node) :in (node-let-bindings node)
                   :when (node-abstraction-p node) :do
                     (setf (gethash name table) (length (node-abstraction-vars node)))))

           (add-bind-fun (node &rest rest)
             (declare (ignore rest)
                      (dynamic-extent rest))
             (when (node-abstraction-p (node-bind-expr node))
               (setf (gethash (node-bind-name node) table) (length (node-abstraction-vars (node-bind-expr node)))))
             nil))

    (traverse
     node
     (list
      (cons :application #'rewrite-direct-application)
      (cons :before-let #'add-local-funs)
      (cons :before-bind #'add-bind-fun))
     nil)))

(defun optimize-bindings-initial (bindings package env)
  (declare (type binding-list bindings)
           (type package package)
           (type tc:environment env)
           (values binding-list))
  (let ((hoister (make-hoister)))
    (append
     (loop :for (name . node) :in bindings
           :collect (cons name (static-dict-lift (optimize-node node env) name hoister package env)))
     (pop-final-hoist-point hoister))))

(defun optimize-node (node env)
  (declare (type node node)
           (type tc:environment env)
           (values node &optional))
  (alexandria-2:line-up-first
   node

   canonicalize

   (match-dynamic-extent-lift env)

   (apply-specializations env)

   (resolve-static-superclass env)

   (inline-methods env)))

(defun pointfree (node table env)
  (declare (type node node)
           (type hash-table table)
           (type tc:environment env)
           (values node))

  (let (function args num-params)
    (cond
      ;; Node must be a variable of type function
      ((node-variable-p node)
       (unless (tc:function-type-p (node-type node))
         (return-from pointfree node))
       (setf function node)
       (setf num-params (length (tc:function-type-arguments (node-type node)))))

      ;; Or an application on a function
      ((node-application-p node)
       (unless (node-variable-p (node-application-rator node))
         (return-from pointfree node))

       ;; The application must be on a known function
       (unless (gethash (node-variable-value (node-application-rator node)) table)
         (return-from pointfree node))

       ;; The function must take more arguments than it is currently applied with
       (unless (> (gethash (node-variable-value (node-application-rator node)) table)
                  (length (node-application-rands node)))
         (return-from pointfree node))

       (labels ((valid-pointfree-argument (node)
                  (declare (type node node)
                           (values boolean))
                  (cond
                    ;; Valid pointfree arguments are instances dictionaries
                    ((node-variable-p node)
                     (and (tc:lookup-instance-by-codegen-sym env (node-variable-value node) :no-error t) t))

                    ;; And expressions that build instance dictionaries
                    ((node-application-p node)
                     (and (valid-pointfree-argument (node-application-rator node))
                          (reduce
                           (lambda (old new)
                             (and old (valid-pointfree-argument new)))
                           (node-application-rands node)
                           :initial-value t)
                          t))

                    (t
                     nil))))

         ;; The applications arguments must all be valid
         (unless (reduce (lambda (old new)
                           (and old (valid-pointfree-argument new)))
                         (node-application-rands node)
                         :initial-value t)
           (return-from pointfree node))

         (setf function (node-application-rator node))
         (setf args (node-application-rands node))
         (setf num-params (- (gethash (node-variable-value (node-application-rator node)) table)
                             (length (node-application-rands node))))))

      (t
       (return-from pointfree node)))

    (let* ((orig-param-names (tc:lookup-function-source-parameter-names env (node-variable-value function)))

           (param-names (loop :for i :from 0 :below num-params
                              :if orig-param-names
                                :collect (let ((param (nth (+ i (length args)) orig-param-names)))
                                           (if (parser:pattern-var-p param)
                                               (gentemp (concatenate 'string
                                                                     (symbol-name (parser:pattern-var-name param))
                                                                     "-"))
                                               (gentemp)))
                              :else
                                :collect (gentemp)))

           (param-types (subseq (tc:function-type-arguments (node-type function)) (length args)))

           (param-nodes (loop :for name :in param-names
                              :for ty :in param-types
                              :collect (make-node-variable
                                        :type ty
                                        :value name)))

           (new-args (append args param-nodes)))

      (make-node-abstraction
       :type (node-type node)
       :vars param-names
       :subexpr (make-node-application
                 :type (tc:make-function-type*
                        (subseq (tc:function-type-arguments (node-type function)) (length new-args))
                        (tc:function-return-type (node-type node)))
                 :rator function
                 :rands new-args)
       ;; FIXME: What should INLINE-P be?
       :inline-p ()))))

(defun canonicalize (node)
  (declare (type node node)
           (values node &optional))
  (labels ((rewrite-application (node &rest rest)
             (declare (ignore rest)
                      (dynamic-extent rest))
             (let ((rator (node-application-rator node))
                   (rands (node-application-rands node)))
               (when (node-application-p rator)
                 (make-node-application
                  :type (node-type node)
                  :rator (node-application-rator rator)
                  :rands (append
                          (node-application-rands rator)
                          rands))))))
    (traverse
     node
     (list
      (cons :application #'rewrite-application))
     nil)))


(defun inline-methods (node env)
  (declare (type node node)
           (type tc:environment env)
           (values node &optional))
  (labels ((inline-method (node &rest rest)
             (declare (ignore rest)
                      (dynamic-extent rest))
             (let ((rator (node-application-rator node))
                   (rands (node-application-rands node)))
               (when (node-variable-p rator)
                 (let (dict rands_)
                   (cond
                     ((node-variable-p (first rands))
                      (setf dict (node-variable-value (first rands)))
                      (setf rands_ (cdr rands)))

                     ((and (node-application-p (first rands))
                           (node-variable-p (node-application-rator (first rands))))
                      (setf dict (node-variable-value (node-application-rator (first rands))))
                      (setf rands_ (append (node-application-rands (first rands)) (cdr rands))))

                     (t
                      (return-from inline-method nil)))

                   (let* ((method-name (node-variable-value rator))
                          (inline-method-name (tc:lookup-method-inline env method-name dict :no-error t)))

                     (when inline-method-name
                       (if (null rands_)
                           (make-node-variable
                            :type (node-type node)
                            :value inline-method-name)

                           (make-node-application
                            :type (node-type node)
                            :rator (make-node-variable
                                    :type (tc:make-function-type*
                                           (mapcar #'node-type rands_)
                                           (node-type node))
                                    :value inline-method-name)
                            :rands rands_))))))))

           (inline-direct-method (node &rest rest)
             (declare (ignore rest)
                      (dynamic-extent rest))
             (let ((rands (node-direct-application-rands node)))
               (let (dict rands_)
                 (cond
                   ((node-variable-p (first rands))
                    (setf dict (node-variable-value (first rands)))
                    (setf rands_ (cdr rands)))

                   ((and (node-application-p (first rands))
                         (node-variable-p (node-application-rator (first rands))))
                    (setf dict (node-variable-value (node-application-rator (first rands))))
                    (setf rands_ (append (node-application-rands (first rands)) (cdr rands))))

                   (t
                    (return-from inline-direct-method nil)))

                 (let* ((method-name (node-direct-application-rator node))
                        (inline-method-name (tc:lookup-method-inline env method-name dict :no-error t)))
                   (when inline-method-name
                     (if (null rands_)
                         (make-node-variable
                          :type (node-type node)
                          :value inline-method-name)

                         (make-node-application
                          :type (node-type node)
                          :rator (make-node-variable
                                  :type (tc:make-function-type*
                                         (mapcar #'node-type rands_)
                                         (node-type node))
                                  :value inline-method-name)
                          :rands rands_)))))))) 

    (traverse
     node
     (list
      (cons :application #'inline-method)
      (cons :direct-application #'inline-direct-method))
     nil)))

(defun static-dict-lift (node name hoister package env)
  (declare (type node node)
           (type symbol name)
           (type hoister hoister)
           (type package package)
           (type tc:environment env)
           (values node &optional))
  (labels ((lift-static-dict (node &rest rest)
             (declare (ignore rest)
                      (dynamic-extent rest))
             (unless (and
                      ;; The function is a variable
                      (node-variable-p (node-application-rator node))

                      ;; The function constructs a typeclass dictionary
                      (tc:lookup-instance-by-codegen-sym
                       env
                       (node-variable-value (node-application-rator node))
                       :no-error t))
               (return-from lift-static-dict nil))

             ;; Don't lift a function in itself
             (when (eq name (node-variable-value (node-application-rator node)))
               (return-from lift-static-dict nil))

             (hoist-definition node package hoister))

           (handle-push-hoist-point (node &rest rest)
             (declare (ignore rest)
                      (dynamic-extent rest))
             (push-hoist-point (node-abstraction-vars node) hoister)
             nil)

           (handle-pop-hoist-point (node &rest rest)
             (declare (ignore rest)
                      (dynamic-extent rest))
             ;; If definitions were hoisted to this lambda
             ;; then add them to the ast
             (let ((hoisted (pop-hoist-point hoister)))
               (if hoisted
                   (make-node-abstraction
                    :type (node-type node)
                    :vars (node-abstraction-vars node)
                    :subexpr (make-node-let
                              :type (node-type (node-abstraction-subexpr node))
                              :bindings hoisted
                              :subexpr (node-abstraction-subexpr node))
                    ;; FIXME: What should INLINE-P be?
                    :inline-p ())

                   node))))

    (traverse
     node
     (list
      (cons :application #'lift-static-dict)
      (cons :before-abstraction #'handle-push-hoist-point)
      (cons :abstraction #'handle-pop-hoist-point))
     nil)))

(defun resolve-compount-superclass (node env)
  (declare (type (or node-application node-direct-application node-variable) node)
           (type tc:environment env))

  (when (node-variable-p node)
    (let* ((instance (tc:lookup-instance-by-codegen-sym env (node-variable-value node))))
      (return-from resolve-compount-superclass (tc:fresh-pred (tc:ty-class-instance-predicate instance)))))

  (let ((rator (node-rator-name node)))
    (unless rator
      (util:coalton-bug "Expected rator to be a symbol."))

    (let* (;; Lookup the instance
           (instance (tc:lookup-instance-by-codegen-sym env rator))

           ;; Generate a fresh predicate to avoid type variable colisions
           (pred (tc:fresh-pred (tc:ty-class-instance-predicate instance)))

           ;; Generate subs to match the class's superclasses to PRED
           (subs (tc:predicate-match (tc:ty-class-instance-predicate instance) pred))

           ;; Find the superclasses of PRED
           (constraints (tc:apply-substitution subs (tc:ty-class-instance-constraints instance)))

           (args
             (loop :for arg :in (node-rands node)
                   :collect (resolve-compount-superclass arg env))))

      (unless (= (length constraints)
                 (length args))
        (util:coalton-bug "Expected the number of arguments (~D) to match the number of constraints (~D)."
                     (length args)
                     (length constraints)))

      ;; Match the instances constraints against the dictionary's arguments
      (loop :for arg :in args
            :for constraint :in constraints
            :do (setf subs (tc:predicate-match constraint arg subs)))

      (tc:apply-substitution subs pred))))

(defun resolve-static-superclass (node env)
  (declare (type node node)
           (type tc:environment env)
           (values node &optional))
  (labels ((handle-static-superclass (node &key bound-variables &allow-other-keys)
             (declare (type util:symbol-list bound-variables))

             (unless (or (node-variable-p (node-field-dict node))
                         (node-application-p (node-field-dict node))
                         (node-direct-application-p (node-field-dict node)))
               (return-from handle-static-superclass))

             (unless (node-free-p (node-field-dict node) bound-variables)
               (return-from handle-static-superclass))

             (let* ( ;; Resolve the predicate
                    (pred (resolve-compount-superclass (node-field-dict node) env))

                    ;; Lookup the predicate's class
                    (class (tc:lookup-class env (tc:ty-predicate-class pred)))

                    ;; Find the class's superclasses
                    (superclass-dict (tc:ty-class-superclass-dict class))

                    ;; Map the accessor to a named superclass
                    (superclass-name (gethash (node-field-name node) (tc:ty-class-superclass-map class)))

                    ;; Find the predicate of the accessed superclass
                    (superclass-pred (car (find superclass-name superclass-dict :key #'cdr)))

                    ;; Match the class's predicate against the instance
                    (subs (tc:predicate-match (tc:ty-class-predicate class) pred))

                    (superclass-pred (tc:apply-substitution subs superclass-pred)))

               ;; Re-resolve the dictionary
               (resolve-static-dict superclass-pred nil env))))

    (traverse
     node
     (list
      (cons :field #'handle-static-superclass))
     nil)))

(defun apply-specializations (node env)
  (declare (type node node)
           (type tc:environment env)
           (values node &optional))

  (when settings:*coalton-disable-specialization*
    (return-from apply-specializations node))

  (labels ((apply-specialization (node &rest rest)
             (declare (dynamic-extent rest)
                      (ignore rest))
             (let ((rator-name (node-rator-name node)))
               (unless rator-name
                 (return-from apply-specialization))

               (let ((from-ty (tc:lookup-value-type env rator-name :no-error t)))
                 (unless from-ty
                   (return-from apply-specialization))

                 (let* ((from-ty (tc:fresh-inst from-ty))

                        (preds (tc:qualified-ty-predicates from-ty))

                        (num-preds (length preds))

                        (rator-type
                          (tc:make-function-type*
                           (subseq (tc:function-type-arguments (node-rator-type node)) num-preds)
                           (tc:function-return-type (node-rator-type node))))

                        (specialization (tc:lookup-specialization-by-type env rator-name rator-type :no-error t)))
                   (unless specialization
                     (return-from apply-specialization))

                   (unless (>= (length (node-rands node)) num-preds)
                     (util:coalton-bug "Expected function ~A to have at least ~A args when applying specialization." rator-name (length preds)))

                   (cond
                     ((= num-preds (length (node-rands node)))
                      (make-node-variable
                       :type rator-type
                       :value (tc:specialization-entry-to specialization)))

                     ((< num-preds (length (node-rands node)))
                      (make-node-application
                       :type (node-type node)
                       :rator (make-node-variable
                               :type rator-type
                               :value (tc:specialization-entry-to specialization))
                       :rands (subseq (node-rands node) num-preds)))

                     (t
                      (util:coalton-bug "Invalid specialization ~A~%" specialization))))))))
    (traverse
     node 
     (list
      (cons :application #'apply-specialization)
      (cons :direct-application #'apply-specialization))
     nil)))

(defun match-dynamic-extent-lift (node env)
  "Stack allocates uncaptured ADTs constructed in the head of a match expression" 
  (declare (type node node)
           (type tc:environment env)
           (values node &optional))

  ;; CLOS classes generated in development mode cannot be stack
  ;; allocated
  (unless (settings:coalton-release-p)
    (return-from match-dynamic-extent-lift node))

  (labels ((apply-lift (node &rest rest)
             (declare (dynamic-extent rest)
                      (ignore rest))

             ;; If the constructed value is captured by a variable
             ;; pattern, then it can escape the match branches scope,
             ;; and thus cannot be safely stack allocated.
             (loop :for branch :in (node-match-branches node)
                   :when (typep (match-branch-pattern branch) 'pattern-var) :do
                     (return-from apply-lift nil))

             (let ((expr (node-match-expr node)))
               (unless (or (node-direct-application-p expr)
                           (node-application-p expr))
                 (return-from apply-lift nil))

               (let ((expr-name (node-rator-name expr)))
                 (unless expr-name
                   (return-from apply-lift nil))

                 (let ((expr-ctor (tc:lookup-constructor env expr-name :no-error t)))
                   (unless expr-ctor
                     (return-from apply-lift nil))


                   (let ((ty (tc:lookup-type env (tc:constructor-entry-constructs expr-ctor))))
                     (when (tc:type-entry-newtype ty)
                       (return-from apply-lift nil))

                     (let ((name (gensym)))
                       (make-node-dynamic-extent
                        :type (node-type node)
                        :name name
                        :node (node-match-expr node)
                        :body (make-node-match
                               :type (node-type node)
                               :expr (make-node-variable :type (node-type (node-match-expr node)) :value name)
                               :branches (node-match-branches node))))))))))

    (traverse
     node
     (list
      (cons :match #'apply-lift))
     nil)))
