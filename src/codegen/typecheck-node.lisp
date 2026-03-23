(defpackage #:coalton-impl/codegen/typecheck-node
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:explicit-nullary-callable-p ; FUNCTION
   #:typecheck-node ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/typecheck-node)

(defun explicit-nullary-callable-p (type)
  (declare (type tc:ty type)
           (values boolean &optional))
  (and (typep type 'tc:function-ty)
       (zerop (length (tc:function-ty-positional-input-types type)))
       (null (tc:function-ty-keyword-input-types type))
       (not (tc:function-ty-keyword-open-p type))))

(defun abstraction-subexpr-type (expr)
  (declare (type node-abstraction expr)
           (values tc:ty &optional))
  (let ((type (node-type expr)))
    (when (and (null (node-abstraction-vars expr))
               (explicit-nullary-callable-p type))
      (setf type (tc:function-return-type type)))
    (loop :for _ :in (node-abstraction-vars expr) :do
      (setf type (tc:function-type-to type)))
    (when (node-abstraction-keyword-params expr)
      (setf type (tc:function-return-type type)))
    type))

(defun application-callee-type (rator-type positional-rands keyword-rands)
  (declare (type tc:ty rator-type)
           (type list positional-rands keyword-rands)
           (values tc:ty &optional))
  (let ((type rator-type))
    (unless (tc:function-type-p type)
      (return-from application-callee-type type))
    (typecase type
      (tc:function-ty
       (let ((remaining (nthcdr (length positional-rands)
                                (tc:function-ty-positional-input-types type))))
         (if (or remaining
                 keyword-rands
                 (tc:function-ty-keyword-input-types type)
                 (tc:function-ty-keyword-open-p type))
             (tc:make-function-ty
              :alias (tc:ty-alias type)
              :positional-input-types remaining
              :keyword-input-types (tc:function-ty-keyword-input-types type)
              :keyword-open-p (tc:function-ty-keyword-open-p type)
              :output-types (tc:function-ty-output-types type))
             (tc:output-types-result-type (tc:function-ty-output-types type)))))
      (t
       (loop :repeat (length positional-rands)
             :do (setf type (tc:function-type-to type)))
       (if (and (null positional-rands)
                (null keyword-rands)
                (zerop (tc:function-input-arity type)))
           (tc:function-type-to type)
           type)))))

(defun check-keyword-application (callee-type keyword-rands env site)
  (declare (type tc:ty callee-type)
           (type list keyword-rands)
           (type tc:environment env)
           (values tc:substitution-list &optional))
  (let ((subs nil))
    (dolist (arg keyword-rands)
      (unless (tc:function-type-p callee-type)
        (util:coalton-bug "Keyword application to non-function type ~S at site ~S"
                          callee-type
                          site))
      (let ((entry (find (node-application-keyword-arg-keyword arg)
                         (tc:function-ty-keyword-input-types callee-type)
                         :key #'tc:keyword-ty-entry-keyword
                         :test #'eq)))
        (unless entry
          (util:coalton-bug "Keyword ~S not found in function type ~S at site ~S"
                            (node-application-keyword-arg-keyword arg)
                            callee-type
                            site))
        (let ((arg-ty (typecheck-node (node-application-keyword-arg-value arg) env)))
          (setf subs (tc:unify subs (tc:keyword-ty-entry-type entry) arg-ty)))))
    subs))

(defun check-positional-argument-type (subs expected-type actual-type)
  (declare (type tc:substitution-list subs)
           (type tc:ty expected-type actual-type)
           (values tc:substitution-list &optional))
  (cond
    ((and (typep expected-type 'tc:function-ty)
          (typep actual-type 'tc:function-ty))
     (nth-value
      0
      (coalton-impl/typechecker/define::coerce-function-value-type
       actual-type
       expected-type
       subs)))
    (t
     (tc:unify subs expected-type actual-type))))

(defgeneric typecheck-node (expr env)
  (:documentation "Check that EXPR is valid. Currently only verifies
  that applied functions match their arguments.")
  (:method ((expr node-literal) env)
    (declare (type tc:environment env)
             (ignore env)
             (values tc:ty))
    (node-type expr))

  (:method ((expr node-variable) env)
    (declare (type tc:environment env)
             (ignore env)
             (values tc:ty))
    (node-type expr))

  (:method ((expr node-application) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (let* ((rator-type (typecheck-node (node-application-rator expr) env))
           (type rator-type)
           (subs nil))
      (loop :for arg :in (node-application-rands expr)
            :for arg-ty := (typecheck-node arg env) :do
              (progn
                (setf subs
                      (check-positional-argument-type
                       subs
                       (tc:function-type-from type)
                       arg-ty))
                (setf type (tc:function-type-to type))))
      (setf subs
            (tc:compose-substitution-lists
             (check-keyword-application
              (application-callee-type
               rator-type
               (node-application-rands expr)
               (node-application-keyword-rands expr))
              (node-application-keyword-rands expr)
              env
              expr)
             subs))
      (node-type expr)))

  (:method ((expr node-direct-application) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (let ((type (node-direct-application-rator-type expr))
          (subs nil))
      (loop :for arg :in (node-direct-application-rands expr)
            :for arg-ty := (typecheck-node arg env) :do
              (progn
                (setf subs
                      (check-positional-argument-type
                       subs
                       (tc:function-type-from type)
                       arg-ty))
                (setf type (tc:function-type-to type))))
      (setf subs
            (tc:compose-substitution-lists
             (check-keyword-application
              (application-callee-type
               (node-direct-application-rator-type expr)
               (node-direct-application-rands expr)
               (node-direct-application-keyword-rands expr))
              (node-direct-application-keyword-rands expr)
              env
              expr)
             subs))
      (node-type expr)))

  (:method ((expr node-abstraction) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (let ((type (abstraction-subexpr-type expr)))
      (let ((subexpr-ty (typecheck-node (node-abstraction-subexpr expr) env)))
        (tc:unify nil type subexpr-ty)
        (node-type expr))))

  (:method ((expr node-let) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (loop :for (name . node) :in (node-let-bindings expr) :do
      (typecheck-node node env))
    (let ((subexpr-ty (typecheck-node (node-let-subexpr expr) env)))
      (tc:unify nil subexpr-ty (node-type expr))
      subexpr-ty))

  (:method ((expr node-dynamic-let) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (loop :for binding :in (node-dynamic-let-bindings expr) :do
      (typecheck-node (node-dynamic-binding-value binding) env))
    (let ((subexpr-ty (typecheck-node (node-dynamic-let-subexpr expr) env)))
      (tc:unify nil subexpr-ty (node-type expr))
      subexpr-ty))

  (:method ((expr node-locally) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (let ((subexpr-ty (typecheck-node (node-locally-subexpr expr) env)))
      (tc:unify nil subexpr-ty (node-type expr))
      subexpr-ty))

  (:method ((expr node-lisp) env)
    (declare (type tc:environment env)
             (ignore env)
             (values tc:ty))
    (node-type expr))

  (:method ((expr match-branch) env)
    (declare (type tc:environment env)
             (values tc:ty &optional))
    (typecheck-node (match-branch-body expr) env))

  (:method ((expr node-match) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (let ((type (node-type expr)))
      (loop :for branch :in (node-match-branches expr)
            :for subexpr-ty := (typecheck-node branch env) :do
              (tc:unify nil type subexpr-ty))
      type))

  (:method ((expr catch-branch) env)
    (declare (type tc:environment env)
             (values tc:ty &optional))
    (typecheck-node (catch-branch-body expr) env))

  (:method ((expr node-catch) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (let ((type (node-type expr)))
      (loop :for branch :in (node-catch-branches expr)
            :for subexpr-ty := (typecheck-node branch env) :do
              (tc:unify nil type subexpr-ty))
      type))

  (:method ((expr node-resumable) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (let ((type (node-type expr)))
      (loop :for branch :in (node-resumable-branches expr)
            :for subexpr-ty := (typecheck-node branch env) :do
              (tc:unify nil type subexpr-ty))
      type))

  (:method ((expr resumable-branch) env)
    (declare (type tc:environment env)
             (values tc:ty &optional))
    (typecheck-node (resumable-branch-body expr) env))

  (:method ((expr node-for) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (loop :for binding :in (node-for-bindings expr) :do
      (typecheck-node (node-for-binding-init binding) env)
      (when (node-for-binding-step binding)
        (typecheck-node (node-for-binding-step binding) env)))
    (when (node-for-returns expr)
      (typecheck-node (node-for-returns expr) env))
    (when (node-for-termination-expr expr)
      (typecheck-node (node-for-termination-expr expr) env))
    (typecheck-node (node-for-body expr) env)
    (node-type expr))

  (:method ((expr node-break) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (node-type expr))

  (:method ((expr node-continue) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (node-type expr))

  (:method ((expr node-seq) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (assert (not (null (node-seq-nodes expr))))
    (loop :for node :in (node-seq-nodes expr) :do
      (typecheck-node node env))
    (let ((last-node (car (last (node-seq-nodes expr)))))
      (tc:unify nil (node-type expr) (node-type last-node))
      (node-type last-node)))

  (:method ((expr node-return-from) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (typecheck-node (node-return-from-expr expr) env)
    (node-type expr))

  (:method ((node node-throw) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (typecheck-node (node-throw-expr node) env)
    (node-type node))

  (:method ((node node-resume-to) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (typecheck-node (node-resume-to-expr node) env)
    (node-type node))

  (:method ((expr node-block) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (tc:unify
     nil
     (node-type expr)
     (typecheck-node (node-block-body expr) env))
    (node-type expr))

  (:method ((expr node-field) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (typecheck-node (node-field-dict expr) env)
    (node-type expr))

  (:method ((expr node-dynamic-extent) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (typecheck-node (node-dynamic-extent-node expr) env)
    (tc:unify
     nil
     (node-type expr)
     (typecheck-node (node-dynamic-extent-body expr) env))
    (node-type expr))

  (:method ((expr node-bind) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (typecheck-node (node-bind-expr expr) env)
    (tc:unify
     nil
     (node-type expr)
     (typecheck-node (node-bind-body expr) env))
    (node-type expr))

  (:method ((expr node-values) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (let ((components (tc:multiple-value-output-types (node-type expr))))
      (unless (= (length components) (length (node-values-nodes expr)))
        (util:coalton-bug "node-values arity mismatch, expected ~D got ~D"
                          (length components)
                          (length (node-values-nodes expr))))
      (loop :for subnode :in (node-values-nodes expr)
            :for comp-ty :in components
            :do (tc:unify nil (typecheck-node subnode env) comp-ty))
      (node-type expr)))

  (:method ((expr node-values-bind) env)
    (declare (type tc:environment env)
             (values tc:ty))
    (let ((components (tc:multiple-value-output-types
                       (node-type (node-values-bind-expr expr)))))
      (unless (= (length components) (length (node-values-bind-vars expr)))
        (util:coalton-bug "node-values-bind arity mismatch, expected ~D got ~D"
                          (length components)
                          (length (node-values-bind-vars expr))))
      (typecheck-node (node-values-bind-expr expr) env)
      (tc:unify nil (node-type expr) (typecheck-node (node-values-bind-body expr) env))
      (node-type expr))))
