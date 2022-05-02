;;;; src/codegen/monomorphize.lisp
;;;;
;;;; Recompile specialized versions of type class generic functions across a call graph.
;;;;

(defpackage #:coalton-impl/codegen/monomorphize
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:traverse
   #:traverse-bindings)
  (:import-from
   #:coalton-impl/codegen/ast-substitutions
   #:ast-substitution
   #:ast-substitution-from
   #:ast-substitution-to
   #:apply-ast-substitution)
  (:import-from
   #:coalton-impl/codegen/typecheck-node
   #:typecheck-node)
  (:import-from
   #:coalton-impl/codegen/hoister
   #:make-hoister
   #:pop-final-hoist-point)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:monomorphize))

(in-package #:coalton-impl/codegen/monomorphize)

(deftype argument ()
  `(or (member @@unpropagated) node))

(defun argument-p (x)
  (typep x 'argument))

(defun argument-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'argument-p x)))

(deftype argument-list ()
  '(satisfies argument-list-p))

(defstruct compile-candidate
  (name      (required 'name) :type symbol        :read-only t)
  (args      (required 'args) :type argument-list :read-only t))

(defun compile-candidate-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'compile-candidate-p x)))

(deftype compile-candidate-list ()
  '(satisfies compile-candidate-list-p))

(defun propagatable-p (x)
  (not (eq x '@@unpropagated)))

(defun node-can-be-propidated (node env)
  "Returns true if a given node should be const propidated to a callee.
Currently only typeclass dictionaries will be propidated, although
constant values could be."
  (cond
    ((and
      (node-variable-p node)
      (tc:lookup-instance-by-codegen-sym env (node-variable-value node) :no-error t))
     t)

    ((and
      (node-direct-application-p node)
      (tc:lookup-instance-by-codegen-sym env (node-direct-application-rator node) :no-error t))
     t) 

    ((and
      (node-application-p node)
      (node-variable-p (node-application-rator node))
      (tc:lookup-instance-by-codegen-sym env (node-variable-value (node-application-rator node)) :no-error t))
     t)

    (t
     nil)))

(defstruct (candidate-manager (:constructor candidate-manager))
  (map   (make-hash-table :test #'equalp) :type hash-table            :read-only t)
  (stack nil                              :type compile-candidate-list            ))

(defun candidate-manager-get (candidate-manager candidate)
  "Returns name the given to a recompiled compile-candidate"
  (declare (type candidate-manager candidate-manager)
           (type compile-candidate candidate)
           (values symbol))
  (let ((name (gethash candidate (candidate-manager-map candidate-manager))))
    name))

(defun candidate-manager-push (candidate-manager candidate package)
  "Push a new candidate to be compiled"
  (declare (type candidate-manager candidate-manager)
           (type compile-candidate candidate)
           (values))

  (when (candidate-manager-get candidate-manager candidate)
    (return-from candidate-manager-push))

  (let ((new-name
          (alexandria:ensure-symbol
           (gensym (concatenate 'string (symbol-name (compile-candidate-name candidate)) "#"))
           package)))

    (setf (gethash candidate (candidate-manager-map candidate-manager)) new-name))

  (push candidate (candidate-manager-stack candidate-manager))

  nil)

(defun candidate-manager-pop (candidate-manager)
  "Returns a candidate that has been queued for compilation"
  (declare (type candidate-manager candidate-manager)
           (values (or null compile-candidate)))
  (pop (candidate-manager-stack candidate-manager)))

(defun valid-candidate-p (name args bound-variables env)
  "Returns the candidate for a given function and arguments. Returns nil if a given application cannot be a candidate."
  (declare (type symbol name)
           (type node-list args)
           (type symbol-list bound-variables)
           (values (or compile-candidate null) &optional))

  ;; Only functions with known code are valid candidates
  (unless (tc:lookup-code env name :no-error t)
    (return-from valid-candidate-p))

  (let ((new-args (loop :for node :in args
                    :if (and (node-can-be-propidated node env) (node-free-p node bound-variables))
                      :collect node
                    :else
                      :collect '@@unpropagated)))

    ;; Candidates must have at least one propagatable argument
    (unless (some #'propagatable-p new-args)
      (return-from valid-candidate-p))

    ;; Fully propigating arguments could change the ordering of side effects
    ;; Exit early to avoid this
    (when (every #'propagatable-p new-args)
      (return-from valid-candidate-p))

    (make-compile-candidate
     :name name
     :args new-args)))

(defun compile-candidate (candidate node env)
  (declare (type compile-candidate candidate)
           (type node-abstraction node)
           (type tc:environment env)
           (values node &optional))

  (let* ((ast-subs nil)

         (new-type (node-type node))

         (arg-tys nil)

         (subs nil)

         (new-vars (loop :for var :in (node-abstraction-vars node)
                         :for arg :in (compile-candidate-args candidate)

                         :if (eq arg '@@unpropagated)
                           :collect (progn
                                      (push (tc:function-type-from new-type) arg-tys)
                                      var)
                         :else
                           :do (progn
                                 (setf subs (tc:unify subs (tc:function-type-from new-type) (node-type arg)))
                                 (push (ast-substitution var arg) ast-subs))

                         :do (setf new-type (tc:function-type-to new-type))))

         (new-node
           (tc:apply-substitution
            subs
            (node-abstraction
             (tc:make-function-type*
              (reverse arg-tys)
              new-type)
             new-vars
             (apply-ast-substitution ast-subs (node-abstraction-subexpr node))))))

    (typecheck-node new-node env)

    new-node))

(defun resolve-var (node resolve-table)
  (declare (type node node)
           (type hash-table resolve-table)
           (values node))

  (unless (node-variable-p node)
    (return-from resolve-var node))

  (let ((resolved (gethash (node-variable-value node) resolve-table)))
    (if resolved
        resolved
        node)))

(defun candidate-selection (node candidate-manager resolve-table package env)
  (declare (type node node)
           (type candidate-manager candidate-manager)
           (type hash-table resolve-table)
           (type package package)
           (type tc:environment env))
  (labels ((validate-candidate (node &key bound-variables &allow-other-keys)
             (let ((name (node-rator-name node))
                   (rands (node-rands node)))

               (unless name
                 (return-from validate-candidate))

               (let ((candidate
                       (valid-candidate-p
                        name (loop :for rand :in rands
                              :collect (resolve-var rand resolve-table))
                        bound-variables
                        env)))

                 (unless candidate
                   (return-from validate-candidate))

                 (candidate-manager-push candidate-manager candidate package)

                 nil))))

    (traverse
     node
     (list
      (cons :direct-application #'validate-candidate)
      (cons :application #'validate-candidate))
     nil))
  (values))


(defun rewrite-callsites (node candidate-manager resolve-table env)
  (declare (type node node)
           (type candidate-manager candidate-manager)
           (type hash-table resolve-table)
           (type tc:environment env)
           (values node &optional))

  (labels ((apply-candidate (node &key bound-variables &allow-other-keys)
             (let ((name (node-rator-name node))
                   (rands (node-rands node)))

               (unless name
                 (return-from apply-candidate))

               (let ((candidate (valid-candidate-p
                                 name
                                 (loop :for rand :in rands
                                       :collect (resolve-var rand resolve-table))
                                 bound-variables
                                 env)))

                 (unless candidate
                   (return-from apply-candidate))

                 (let* ((function-name (candidate-manager-get candidate-manager candidate))

                        (new-type (node-rator-type node))

                        (arg-tys nil) 

                        (args (loop
                                :for arg :in (node-rands node)
                                :for candidate-arg :in (compile-candidate-args candidate)

                                :when (eq candidate-arg '@@unpropagated)
                                  :collect (progn
                                             (push (node-type arg) arg-tys)
                                             arg)

                                :do (setf new-type (tc:function-type-to new-type)))))

                   (node-application
                    (node-type node)
                    (node-variable
                     (tc:make-function-type*
                      (reverse arg-tys)
                      new-type)
                     function-name)
                    args))))))

    (traverse
     node
     (list
      (cons :application #'apply-candidate)
      (cons :direct-application #'apply-candidate))
     nil)))

(defun monomorphize (name manager package resolve-table optimize-node env)
  (declare (type symbol name)
           (type candidate-manager manager)
           (type package package)
           (type hash-table resolve-table)
           (type function optimize-node)
           (type tc:environment env)
           (values binding-list &optional))

  (let* ((initial-code (tc:lookup-code env name))

         (binding-group nil))

    (candidate-selection initial-code manager resolve-table package env)
    (push (cons name (rewrite-callsites initial-code manager resolve-table env)) binding-group)

    (loop :for candidate := (candidate-manager-pop manager)
          :while candidate

          :for name := (compile-candidate-name candidate)
          :for code := (tc:lookup-code env name)
          :for new-code := (funcall optimize-node (compile-candidate candidate code env) env)

          :for new-code_ := (progn
                              (candidate-selection new-code manager resolve-table package env)
                              (rewrite-callsites new-code manager resolve-table env))

          :for new-code__ := (funcall optimize-node new-code_ env)

          :do (candidate-selection new-code__ manager resolve-table package env)
          :do (push (cons (candidate-manager-get manager candidate) (rewrite-callsites new-code__ manager resolve-table env))
                    binding-group))

    binding-group))
