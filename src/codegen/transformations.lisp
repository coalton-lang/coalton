(defpackage #:coalton-impl/codegen/transformations
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/algorithm
   #:immutable-map-data)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:traverse
   #:traverse-with-binding-list
   #:update-function-env
   #:make-function-t
   #:substitute-fresh-type-variables))

(in-package #:coalton-impl/codegen/transformations)

(defun call-if (key funs args node)
  (declare (type symbol key)
           (type list   funs)
           (type list   args)
           (type node   node)
           (values node &optional))
  (or
   (alexandria:when-let ((f (cdr (assoc key funs))))
     (apply f (cons node args)))
   node))

(defmacro traverse-if (key funs args before-node else-expr)
  (let ((before-key
          (alexandria:make-keyword (concatenate 'string "BEFORE-" (symbol-name key))))
        (traverse-key
          (alexandria:make-keyword (concatenate 'string "TRAVERSE-" (symbol-name key))))
        (after-key
          (alexandria:make-keyword (concatenate 'string "AFTER-" (symbol-name key)))))
    `(let* ((node
              (call-if ,before-key ,funs ,args ,before-node))
            (after-node
              (alexandria:if-let ((f (cdr (assoc ,traverse-key ,funs))))
                (apply f (cons node (cons ,funs ,args)))
                ,else-expr)))
       (call-if ,after-key ,funs ,args after-node))))

(defun traverse (node funs &optional (args nil))
  "Wrap `in-traverse` with a pre-operation and a post-operation"
  (declare (type node node)
           (type list funs)
           (type list args)
           (values node &optional))
  (call-if ':after-each funs args
           (in-traverse (call-if ':before-each funs args node)
                        funs
                        args)))

(defgeneric in-traverse (node funs args)
  (:method ((node node-literal) funs args)
    (call-if ':literal funs args node))

  (:method ((node node-variable) funs args)
    (call-if ':variable funs args node))

  (:method ((node node-application) funs args)
    (traverse-if :application funs args node
                 (make-node-application
                  :type (node-type node)
                  :rator (traverse (node-application-rator node) funs args)
                  :rands (mapcar
                          (lambda (node)
                            (traverse node funs args))
                          (node-application-rands node)))))

  (:method ((node node-direct-application) funs args)
    (traverse-if :direct-application funs args node
                 (make-node-direct-application
                  :type (node-type node)
                  :rator-type (node-direct-application-rator-type node)
                  :rator (node-direct-application-rator node)
                  :rands (mapcar
                          (lambda (node)
                            (traverse node funs args))
                          (node-direct-application-rands node)))))

  (:method ((node node-abstraction) funs args)
    (traverse-if :abstraction funs args node
                 (make-node-abstraction
                  :type (node-type node)
                  :vars (node-abstraction-vars node)
                  :subexpr (traverse
                            (node-abstraction-subexpr node)
                            funs
                            args))))

  (:method ((node node-let) funs args)
    (traverse-if :let funs args node
                 (make-node-let
                  :type (node-type node)
                  :bindings (loop :for (name . node) :in (node-let-bindings node)
                                  :collect (cons name (traverse node funs args)))
                  :subexpr (traverse (node-let-subexpr node) funs args))))

  (:method ((node node-lisp) funs args)
    (call-if ':lisp funs args node))

  (:method ((node node-match) funs args)
    (traverse-if :match funs args node
                 (make-node-match
                  :type (node-type node)
                  :expr (traverse (node-match-expr node) funs args)
                  :branches (mapcar
                             (lambda (branch)
                               (make-match-branch
                                :pattern (match-branch-pattern branch)
                                :body (traverse
                                       (match-branch-body branch)
                                       funs
                                       args)))
                             (node-match-branches node)))))

  (:method ((node node-while) funs args)
    (traverse-if :while funs args node
                 (make-node-while
                  :type (node-type node)
                  :label (node-while-label node)
                  :expr (traverse (node-while-expr node) funs args)
                  :body (traverse (node-while-body node) funs args))))

  (:method ((node node-while-let) funs args)
    (traverse-if :while-let funs args node
                 (make-node-while-let
                  :type (node-type node)
                  :label (node-while-let-label node)
                  :pattern (node-while-let-pattern node)
                  :expr (traverse (node-while-let-expr node) funs args)
                  :body (traverse (node-while-let-body node) funs args))))

  (:method ((node node-loop) funs args)
    (traverse-if :loop funs args node
                 (make-node-loop
                  :type (node-type node)
                  :label (node-loop-label node)
                  :body (traverse (node-loop-body node) funs args))))

  (:method ((node node-break) funs args)
    (call-if :break funs args node))

  (:method ((node node-continue) funs args)
    (call-if ':continue funs args node))

  (:method ((node node-seq) funs args)
    (traverse-if :seq funs args node
                 (make-node-seq
                  :type (node-type node)
                  :nodes (mapcar
                          (lambda (node)
                            (traverse node funs args))
                          (node-seq-nodes node)))))

  (:method ((node node-return) funs args)
    (traverse-if :return funs args node
                 (make-node-return
                  :type (node-type node)
                  :expr (traverse (node-return-expr node) funs args))))

  (:method ((node node-field) funs args)
    (traverse-if :field funs args node
                 (make-node-field
                  :type (node-type node)
                  :name (node-field-name node)
                  :dict (traverse (node-field-dict node) funs args))))

  (:method ((node node-dynamic-extent) funs args)
    (traverse-if :dynamic-extent funs args node
                 (make-node-dynamic-extent
                  :type (node-type node)
                  :name (node-dynamic-extent-name node)
                  :node (traverse (node-dynamic-extent-node node) funs args)
                  :body (traverse (node-dynamic-extent-body node) funs args))))

  (:method ((node node-bind) funs args)
    (traverse-if :bind funs args node
                 (make-node-bind
                  :type (node-type node)
                  :name (node-bind-name node)
                  :expr (traverse (node-bind-expr node) funs args)
                  :body (traverse (node-bind-body node) funs args)))))

(defun split-binding-definitions (bindings)
  (let ((functions nil)
        (variables nil))
    (loop :for (name . node) :in bindings
          :do (if (node-abstraction-p node)
                  (push (cons name (length (node-abstraction-vars node))) functions)
                  (push name variables)))
    (values functions variables)))

(defun update-function-env (bindings env)
  (declare (type binding-list bindings)
           (type tc:environment env)
           (values tc:environment))
  (multiple-value-bind (toplevel-functions toplevel-values)
      (split-binding-definitions bindings)
    (loop :for (name . arity) :in toplevel-functions
          :do
             (setf env
                   (tc:set-function
                    env
                    name
                    (tc:make-function-env-entry
                     :name name
                     :arity arity))))
    (dolist (name toplevel-values)
      (when (tc:lookup-function env name :no-error t)
        (setf env (tc:unset-function env name)))))
  env)

(defun make-function-table (env)
  (declare (type tc:environment env)
           (values hash-table))
  (let ((table (make-hash-table)))
    (fset:do-map (name entry (immutable-map-data (tc:environment-function-environment env)))
      (setf (gethash name table) (tc:function-env-entry-arity entry)))
    table))

(defun make-binding-list-traversals ()
  (load-time-value
   (list
    (cons ':traverse-abstraction
          (lambda (node funs &key bound-variables)
            (declare (type node-abstraction node)
                     (type list funs)
                     (type util:symbol-list bound-variables)
                     (values node-abstraction &optional))
            (make-node-abstraction
             :type (node-type node)
             :vars (node-abstraction-vars node)
             :subexpr (traverse
                       (node-abstraction-subexpr node)
                       funs
                       (list ':bound-variables
                             (append (node-abstraction-vars node) bound-variables))))))
    (cons ':traverse-let
          (lambda (node funs &key bound-variables)
            (declare (type node-let node)
                     (type list funs)
                     (type util:symbol-list bound-variables)
                     (values node-let &optional))
            (let ((new-args (list ':bound-variables
                                  (append
                                   (mapcar #'car (node-let-bindings node))
                                   bound-variables))))
              (make-node-let
               :type (node-type node)
               :bindings (loop :for (name . node) :in (node-let-bindings node)
                               :collect (cons name (traverse node funs new-args)))
               :subexpr (traverse (node-let-subexpr node) funs new-args)))))
    (cons ':traverse-match
          (lambda (node funs &key bound-variables)
            (declare (type node-match node)
                     (type list funs)
                     (type util:symbol-list bound-variables)
                     (values node-match &optional))
            (make-node-match
             :type (node-type node)
             :expr (traverse (node-match-expr node) funs (list ':bound-variables bound-variables))
             :branches (mapcar
                        (lambda (branch)
                          (make-match-branch
                           :pattern (match-branch-pattern branch)
                           :body (traverse
                                  (match-branch-body branch)
                                  funs
                                  (list ':bound-variables
                                        (append (pattern-variables (match-branch-pattern branch))
                                                bound-variables)))))
                        (node-match-branches node)))))
    (cons ':traverse-dynamic-extent
          (lambda (node funs &key bound-variables)
            (declare (type node-dynamic-extent node)
                     (type list funs)
                     (type util:symbol-list bound-variables)
                     (values node-dynamic-extent &optional))
            (let ((new-args (list ':bound-variables
                                  (cons (node-dynamic-extent-name node) bound-variables))))
              (make-node-dynamic-extent
               :type (node-type node)
               :name (node-dynamic-extent-name node)
               :node (traverse (node-dynamic-extent-node node) funs new-args)
               :body (traverse (node-dynamic-extent-body node) funs new-args)))))
    (cons ':traverse-bind
          (lambda (node funs &key bound-variables)
            (declare (type node-bind node)
                     (type list funs)
                     (type util:symbol-list bound-variables)
                     (values node-bind &optional))
            (let ((new-args (list ':bound-variables
                                  (cons (node-bind-name node) bound-variables))))
              (make-node-bind
               :type (node-type node)
               :name (node-bind-name node)
               :expr (traverse (node-bind-expr node) funs new-args)
               :body (traverse (node-bind-body node) funs new-args))))))
   t))

(defun traverse-with-binding-list (node funs)
  (declare (type node node)
           (type list funs)
           (values node &optional))
  (traverse node
            (append funs (make-binding-list-traversals))
            (list ':bound-variables nil)))

(defmethod tc:apply-substitution (subs (node node))
  (declare (type tc:substitution-list subs)
           (values node &optional))
  (traverse
   node
   (list
    (cons ':after-each
          (lambda (node)
            (declare (type node node)
                     (values node &optional))
            (copy-node node
                       (tc:apply-substitution subs (node-type node)))))
    (cons ':after-direct-application
          (lambda (node)
            (declare (type node-direct-application node)
                     (values node-direct-application &optional))
            (make-node-direct-application
             :type (node-type node)
             :rator-type (tc:apply-substitution subs (node-direct-application-rator-type node))
             :rator (node-direct-application-rator node)
             :rands (node-direct-application-rands node))))
    (cons ':after-match
          (lambda (node)
            (declare (type node-match node)
                     (values node-match &optional))
            (make-node-match
             :type (node-type node)
             :expr (node-match-expr node)
             :branches (mapcar
                        (lambda (branch)
                          (make-match-branch
                           :pattern (tc:apply-substitution subs (match-branch-pattern branch))
                           :body (match-branch-body branch)))
                        (node-match-branches node)))))
    (cons ':after-while-let
          (lambda (node)
            (declare (type node-while-let node)
                     (values node-while-let &optional))
            (make-node-while-let
             :type (node-type node)
             :label (node-while-let-label node)
             :pattern (tc:apply-substitution subs (node-while-let-pattern node))
             :expr (node-while-let-expr node)
             :body (node-while-let-body node)))))))

(defmethod tc:type-variables ((node node))
  (declare (values tc:tyvar-list &optional))
  (let ((tyvars nil))
    (traverse
     node
     (list
      (cons ':after-each
            (lambda (node)
              (declare (type node node)
                       (values))
              (alexandria:unionf tyvars
                                 (tc:type-variables (node-type node)))
              (values)))
      (cons ':after-direct-application
            (lambda (node)
              (declare (type node-direct-application node)
                       (values))
              (alexandria:unionf tyvars
                                 (tc:type-variables (node-direct-application-rator-type node)))
              (values)))
      (cons ':after-match
            (lambda (node)
              (declare (type node-match node)
                       (values))
              (mapcar
               (lambda (branch)
                 (alexandria:unionf tyvars
                                    (tc:type-variables (match-branch-pattern branch))))
               (node-match-branches node))
              (values)))
      (cons ':after-while-let
            (lambda (node)
              (declare (type node-while-let node)
                       (values))
              (alexandria:unionf tyvars
                                 (tc:type-variables (node-while-let-pattern node)))
              (values)))))
    tyvars))

(defun substitute-fresh-type-variables (node)
  (declare (type node node)
           (values node &optional))
  (alexandria:if-let ((old-type-variables (tc:type-variables node)))
    (tc:apply-substitution
     (mapcar (lambda (tyvar)
               (tc:make-substitution
                :from tyvar
                :to (tc:make-variable (tc:kind-of tyvar))))
             old-type-variables)
     node)
    node))
