(defpackage #:coalton-impl/codegen/scalar-replacement
  (:documentation
   "Scalar replacement eliminates locally constructed immutable structs used
only through known field readers. It binds constructor arguments to scalar
locals and replaces reader calls with those locals, exposing the field values
to the Lisp compiler without allocating the enclosing object.

The optimizer runs this pass in release mode after inlining and direct-call
conversion. Constructor arguments still evaluate exactly once, in order,
including unused fields. Recursive bindings and objects passed to remaining
calls, returned, stored, captured by closures, or exposed to Lisp retain their
allocation. The pass does not change function calling conventions.")
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
   #:traverse)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:node-free-p)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:scalar-replace))

(in-package #:coalton-impl/codegen/scalar-replacement)

(defun constructed-struct (node env)
  "Recognize a fully applied constructor for an ordinary immutable struct."
  (when (and (node-direct-application-p node)
             (null (node-direct-application-keyword-rands node)))
    (let* ((name (node-direct-application-rator node))
           (struct (tc:lookup-struct env name :no-error t)))
      (when struct
        (let ((type (tc:lookup-type env name)))
          (when (and (not (tc:type-entry-newtype type))
                     (null (tc:type-entry-explicit-repr type))
                     (= (length (tc:struct-entry-fields struct))
                        (length (node-direct-application-rands node))))
            struct))))))

(defun replace-struct-reads (body name readers fields)
  "Replace known reads of NAME, returning NIL if any other use remains.

Do not cross function boundaries: a closure capturing NAME keeps the object,
even if it only reads fields. The final free-variable check also catches uses
hidden in Lisp escapes, direct calls, and keyword arguments."
  (let ((body
          (traverse
           body
           (list
            (action (:traverse node-abstraction node) node)
            (action (:after node-direct-application node)
              (let ((args (node-direct-application-rands node))
                    (index (position (node-direct-application-rator node) readers)))
                (when (and index
                           (null (node-direct-application-keyword-rands node))
                           (= 1 (length args))
                           (node-local-variable-p (first args))
                           (eq name (node-variable-value (first args))))
                  (copy-node (nth index fields) (node-type node)))))))))
    (when (node-free-p body (list name))
      body)))

(defun scalar-replace-binding (name expr body env)
  "Replace a nonrecursive binding of a fresh struct, or return NIL.

Inline helper bodies often wrap the constructor in argument bindings. Descend
through those wrappers, keeping their evaluation order and recursive groups.
Identifiers have already been renamed, so extending their scope is safe."
  (typecase expr
    (node-bind
     (alexandria:when-let ((replacement
                            (scalar-replace-binding name (node-bind-body expr) body env)))
       (make-node-bind
        :type (node-type body)
        :name (node-bind-name expr)
        :expr (node-bind-expr expr)
        :body replacement)))
    (node-let
     (alexandria:when-let ((replacement
                            (scalar-replace-binding name (node-let-subexpr expr) body env)))
       (make-node-let
        :type (node-type body)
        :bindings (node-let-bindings expr)
        :subexpr replacement)))
    (node-direct-application
     (alexandria:when-let ((struct (constructed-struct expr env)))
       (let* ((args (node-direct-application-rands expr))
              (fields (loop :for arg :in args
                            :collect (make-node-local-variable
                                      :type (node-type arg)
                                      :value (gensym "FIELD-"))))
              (readers (loop :for field :in (tc:struct-entry-fields struct)
                             :collect (tc:struct-field-accessor-name
                                       (tc:struct-entry-name struct)
                                       (tc:struct-field-index field))))
              (replacement (replace-struct-reads body name readers fields)))
         (when replacement
           ;; Bind every argument, including unused fields, exactly once and in
           ;; constructor order. Keep lambdas as values rather than local FLETs.
           (loop :for field :in (reverse fields)
                 :for arg :in (reverse args)
                 :do (setf replacement
                           (make-node-bind
                            :type (node-type body)
                            :name (node-variable-value field)
                            :expr (if (node-abstraction-p arg)
                                      (make-node-locally
                                       :type (node-type arg)
                                       :noinline-functions nil
                                       :type-check nil
                                       :subexpr arg)
                                      arg)
                            :body replacement)))
           replacement))))))

(defun scalar-replace-let (node env)
  "Consider nonrecursive bindings in NODE, preserving its SCC evaluation order."
  (let ((body (node-let-subexpr node))
        (count 0))
    ;; Use the same dependency order as codegen-let. Simply replacing a binding
    ;; by several fields would change the graph and could reorder effects.
    (dolist (scc (reverse (node-binding-sccs (node-let-bindings node))))
      (let* ((bindings (remove-if-not (lambda (binding) (member (car binding) scc))
                                     (node-let-bindings node)))
             (binding (first bindings))
             (replacement
               (when (and (null (cdr bindings))
                          (node-free-p (cdr binding) (list (car binding))))
                 (scalar-replace-binding (car binding) (cdr binding) body env))))
        (if replacement
            (setf body replacement
                  count (1+ count))
            (setf body (make-node-let
                        :type (node-type node)
                        :bindings bindings
                        :subexpr body)))))
    (values (if (plusp count) body node) count)))

(defun scalar-replace (node env)
  "Eliminate local structs used only through their generated field readers.

Run after inlining and direct-call conversion, in release mode only. Return the
rewritten node and the number of constructor sites removed. This pass neither
changes function calling conventions nor moves allocations across branches."
  (declare (type node node)
           (type tc:environment env)
           (values node unsigned-byte &optional))
  (let ((count 0))
    (values
     (traverse
      node
      (list
       (action (:after node-bind node)
         (alexandria:when-let ((replacement
                                (scalar-replace-binding
                                 (node-bind-name node) (node-bind-expr node)
                                 (node-bind-body node) env)))
           (incf count)
           replacement))
       (action (:after node-let node)
         (multiple-value-bind (replacement removed) (scalar-replace-let node env)
           (incf count removed)
           replacement))))
     count)))
