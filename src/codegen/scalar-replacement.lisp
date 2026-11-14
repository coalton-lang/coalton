(defpackage #:coalton-impl/codegen/scalar-replacement
  (:documentation
   "Scalar replacement eliminates locally constructed immutable structs used
only through known field readers, and unboxes locally constructed
`coalton/cell:Cell` bindings used only through `read`, `write!`, and `swap!`.
It binds constructor arguments or cell contents to scalar locals and replaces
accesses with those locals, exposing the underlying values to the Lisp compiler
without allocating the enclosing object.

The optimizer runs this pass in release mode after inlining and direct-call
conversion. Constructor arguments still evaluate exactly once, in order,
including unused fields. Non-escaping local functions (`flet`/`labels`) may
read and mutate unboxed cells. Recursive bindings and objects passed to
remaining calls, returned, stored, captured by escaping closures, or exposed to
arbitrary Lisp forms retain their allocation. The pass does not change function
calling conventions.")
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
   #:*traverse*
   #:traverse
   #:traverse-with-binding-list)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:node-abstraction-bound-variables
   #:node-free-p
   #:non-escaping-bind-function-p
   #:non-escaping-let-function-names)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:scalar-replace))

(in-package #:coalton-impl/codegen/scalar-replacement)

(defun cell-symbol (name)
  (declare (type simple-string name)
           (values (or null symbol) &optional))
  (alexandria:when-let ((pkg (find-package "COALTON/CELL")))
    (find-symbol name pkg)))

(defun cell-type-element-type (type)
  "If TYPE is `(coalton/cell:Cell :a)`, return `:a`; otherwise return NIL."
  (declare (type tc:ty type)
           (values (or null tc:ty) &optional))
  (let ((cell-sym (cell-symbol "CELL")))
    (when (and cell-sym
               (tc:tapp-p type)
               (tc:tycon-p (tc:tapp-from type))
               (eq cell-sym (tc:tycon-name (tc:tapp-from type))))
      (tc:tapp-to type))))

(defun make-coalton-variable-node (type name env)
  "Construct a variable node for NAME with TYPE in ENV."
  (declare (type tc:ty type)
           (type parser:identifier name)
           (type tc:environment env)
           (values node-variable &optional))
  (cond
    ((util:dynamic-variable-name-p name)
     (make-node-dynamic-variable :type type :value name))
    ((and (symbol-package name)
          (or (member name '(coalton:True coalton:False coalton:Nil coalton:Unit) :test #'eq)
              (tc:lookup-name env name :no-error t)
              (tc:lookup-function env name :no-error t)
              (tc:lookup-instance-by-codegen-sym env name :no-error t)))
     (make-node-global-variable :type type :value name))
    (t
     (make-node-local-variable :type type :value name))))

(defun lisp-cell-allocation-var (node)
  "If NODE is the inlined `(make-cell-internal :inner data)` form from
`coalton/cell:new`, return the Coalton variable symbol supplying `data`."
  (declare (type node node)
           (values (or null parser:identifier) &optional))
  (let ((make-cell-sym (cell-symbol "MAKE-CELL-INTERNAL")))
    (when (and make-cell-sym
               (node-lisp-p node)
               (cell-type-element-type (node-type node))
               (= 1 (length (node-lisp-vars node)))
               (= 1 (length (node-lisp-form node))))
      (let ((form (first (node-lisp-form node)))
            (var-entry (first (node-lisp-vars node))))
        (when (and (consp form)
                   (= 3 (length form))
                   (eq make-cell-sym (first form))
                   (eq :inner (second form))
                   (eq (car var-entry) (third form)))
          (cdr var-entry))))))

(defun constructed-cell-init (node env)
  "If NODE constructs a fresh `coalton/cell:Cell`, return its initial value
node and element type."
  (declare (type node node)
           (type tc:environment env)
           (values (or null node) (or null tc:ty) &optional))
  (let ((element-type (cell-type-element-type (node-type node))))
    (when element-type
      (cond
        ;; (bind tmp init (lisp (make-cell-internal :inner tmp)))
        ((and (node-bind-p node)
              (eq (node-bind-name node)
                  (lisp-cell-allocation-var (node-bind-body node))))
         (values (node-bind-expr node) element-type))

        ;; (let ((tmp init)) (lisp (make-cell-internal :inner tmp)))
        ((and (node-let-p node)
              (= 1 (length (node-let-bindings node)))
              (let ((binding (first (node-let-bindings node))))
                (and (eq (car binding)
                         (lisp-cell-allocation-var (node-let-subexpr node)))
                     (node-free-p (cdr binding) (list (car binding))))))
         (values (cdar (node-let-bindings node)) element-type))

        ;; Bare (lisp (make-cell-internal :inner var)) after alias elimination
        ((lisp-cell-allocation-var node)
         (values (make-coalton-variable-node
                  element-type
                  (lisp-cell-allocation-var node)
                  env)
                 element-type))

        ;; Direct or indirect call to (coalton/cell:new init)
        ((and (typep node '(or node-application node-direct-application))
              (let ((new-sym (cell-symbol "NEW")))
                (and new-sym
                     (eq new-sym (node-rator-name node))))
              (null (getf (node-properties node) :noinline))
              (null (etypecase node
                      (node-application (node-application-keyword-rands node))
                      (node-direct-application (node-direct-application-keyword-rands node))))
              (= 1 (length (node-rands node))))
         (values (first (node-rands node)) element-type))

        (t
         (values nil nil))))))

(defun cell-read-node-p (node name)
  "Return true when NODE reads the cell bound to NAME."
  (declare (type node node)
           (type parser:identifier name)
           (values boolean &optional))
  (let ((inner-sym (cell-symbol "CELL-INTERNAL-INNER"))
        (read-sym (cell-symbol "READ")))
    (cond
      ((and inner-sym
            (node-lisp-p node)
            (= 1 (length (node-lisp-vars node)))
            (= 1 (length (node-lisp-form node))))
       (let ((form (first (node-lisp-form node)))
             (var-entry (first (node-lisp-vars node))))
         (and (consp form)
              (= 2 (length form))
              (eq inner-sym (first form))
              (eq (car var-entry) (second form))
              (eq name (cdr var-entry)))))

      ((and read-sym
            (typep node '(or node-application node-direct-application))
            (eq read-sym (node-rator-name node))
            (null (getf (node-properties node) :noinline))
            (null (etypecase node
                    (node-application (node-application-keyword-rands node))
                    (node-direct-application (node-direct-application-keyword-rands node))))
            (= 1 (length (node-rands node))))
       (let ((arg (first (node-rands node))))
         (and (node-local-variable-p arg)
              (eq name (node-variable-value arg)))))

      (t
       nil))))

(defun cell-write-data-node (node name env)
  "If NODE writes to the cell bound to NAME, return the value node being written."
  (declare (type node node)
           (type parser:identifier name)
           (type tc:environment env)
           (values (or null node) &optional))
  (let ((inner-sym (cell-symbol "CELL-INTERNAL-INNER"))
        (write-sym (cell-symbol "WRITE!")))
    (cond
      ((and inner-sym
            (node-lisp-p node)
            (= 2 (length (node-lisp-vars node)))
            (= 1 (length (node-lisp-form node))))
       (let ((form (first (node-lisp-form node)))
             (vars (node-lisp-vars node)))
         (when (and (consp form)
                    (= 3 (length form))
                    (eq 'cl:setf (first form))
                    (consp (second form))
                    (= 2 (length (second form)))
                    (eq inner-sym (first (second form)))
                    (symbolp (second (second form)))
                    (symbolp (third form)))
           (let ((cel-var (cdr (assoc (second (second form)) vars :test #'eq)))
                 (data-var (cdr (assoc (third form) vars :test #'eq))))
             (when (and cel-var
                        data-var
                        (eq name cel-var))
               (make-coalton-variable-node (node-type node) data-var env))))))

      ((and write-sym
            (typep node '(or node-application node-direct-application))
            (eq write-sym (node-rator-name node))
            (null (getf (node-properties node) :noinline))
            (null (etypecase node
                    (node-application (node-application-keyword-rands node))
                    (node-direct-application (node-direct-application-keyword-rands node))))
            (= 2 (length (node-rands node))))
       (let ((cel-arg (first (node-rands node)))
             (data-arg (second (node-rands node))))
         (when (and (node-local-variable-p cel-arg)
                    (eq name (node-variable-value cel-arg)))
           data-arg)))

      (t
       nil))))

(defun cell-swap-replacement (node name value-var env)
  "If NODE swaps the cell bound to NAME, return the replacement AST."
  (declare (type node node)
           (type parser:identifier name value-var)
           (type tc:environment env)
           (values (or null node) &optional))
  (let ((inner-sym (cell-symbol "CELL-INTERNAL-INNER"))
        (swap-sym (cell-symbol "SWAP!"))
        (type (node-type node)))
    (cond
      ;; Inlined swap! form:
      ;; (cl:let* ((old (cell-internal-inner cel)))
      ;;   (cl:setf (cell-internal-inner cel) data)
      ;;   old)
      ((and inner-sym
            (node-lisp-p node)
            (= 2 (length (node-lisp-vars node)))
            (= 1 (length (node-lisp-form node))))
       (let ((form (first (node-lisp-form node)))
             (vars (node-lisp-vars node)))
         (when (and (consp form)
                    (= 4 (length form))
                    (eq 'cl:let* (first form))
                    (consp (second form))
                    (= 1 (length (second form)))
                    (consp (first (second form)))
                    (= 2 (length (first (second form)))))
           (let ((old-sym (first (first (second form))))
                 (read-form (second (first (second form))))
                 (setf-form (third form))
                 (ret-sym (fourth form)))
             (when (and (symbolp old-sym)
                        (eq old-sym ret-sym)
                        (consp read-form)
                        (= 2 (length read-form))
                        (eq inner-sym (first read-form))
                        (symbolp (second read-form))
                        (consp setf-form)
                        (= 3 (length setf-form))
                        (eq 'cl:setf (first setf-form))
                        (consp (second setf-form))
                        (= 2 (length (second setf-form)))
                        (eq inner-sym (first (second setf-form)))
                        (eq (second read-form) (second (second setf-form)))
                        (symbolp (third setf-form)))
               (let ((cel-var (cdr (assoc (second read-form) vars :test #'eq)))
                     (data-var (cdr (assoc (third setf-form) vars :test #'eq))))
                 (when (and cel-var
                            data-var
                            (eq name cel-var))
                   (let ((old-var (gensym "CELL-OLD-")))
                     (make-node-bind
                      :type type
                      :name old-var
                      :expr (make-node-local-variable :type type :value value-var)
                      :body (make-node-seq
                             :type type
                             :nodes (list (make-node-setq
                                           :type type
                                           :name value-var
                                           :expr (make-coalton-variable-node type data-var env))
                                          (make-node-local-variable
                                           :type type
                                           :value old-var))))))))))))

      ;; Uninlined (coalton/cell:swap! cel data)
      ((and swap-sym
            (typep node '(or node-application node-direct-application))
            (eq swap-sym (node-rator-name node))
            (null (getf (node-properties node) :noinline))
            (null (etypecase node
                    (node-application (node-application-keyword-rands node))
                    (node-direct-application (node-direct-application-keyword-rands node))))
            (= 2 (length (node-rands node))))
       (let ((cel-arg (first (node-rands node)))
             (data-arg (second (node-rands node))))
         (when (and (node-local-variable-p cel-arg)
                    (eq name (node-variable-value cel-arg)))
           (let ((data-var (gensym "CELL-DATA-"))
                 (old-var (gensym "CELL-OLD-")))
             (make-node-bind
              :type type
              :name data-var
              :expr (if (node-abstraction-p data-arg)
                        (make-node-locally
                         :type type
                         :noinline-functions nil
                         :type-check nil
                         :subexpr data-arg)
                        data-arg)
              :body (make-node-bind
                     :type type
                     :name old-var
                     :expr (make-node-local-variable :type type :value value-var)
                     :body (make-node-seq
                            :type type
                            :nodes (list (make-node-setq
                                          :type type
                                          :name value-var
                                          :expr (make-node-local-variable
                                                 :type type
                                                 :value data-var))
                                         (make-node-local-variable
                                          :type type
                                          :value old-var)))))))))

      (t
       nil))))

(defvar *cell-non-escaping-functions* nil
  "Local function identifiers proven not to escape during cell replacement.")

(defun replace-cell-accesses (body name value-var env)
  "Replace known reads, writes, and swaps of cell NAME with lexical variable
VALUE-VAR, returning NIL if any other use of NAME remains.

Allow references inside non-escaping local functions (`flet`/`labels`), loops,
and branches, while leaving escaping closures untouched so the final
`node-free-p` check rejects cells captured by escaping closures."
  (let ((*cell-non-escaping-functions* nil))
    (labels ((unshadowed-p (bound-variables)
               (not (member name bound-variables :test #'eq)))
             (rewrite-non-escaping-abstraction (abstraction bound-variables)
               (make-node-abstraction
                :type (node-type abstraction)
                :vars (node-abstraction-vars abstraction)
                :keyword-params (node-abstraction-keyword-params abstraction)
                :subexpr (funcall *traverse*
                                  (node-abstraction-subexpr abstraction)
                                  (append (node-abstraction-bound-variables abstraction)
                                          bound-variables))))
             (rewrite-cell-op (node bound-variables)
               (when (unshadowed-p bound-variables)
                 (cond
                   ((cell-read-node-p node name)
                    (make-node-local-variable
                     :type (node-type node)
                     :value value-var))
                   ((alexandria:when-let ((data-node (cell-write-data-node node name env)))
                      (make-node-setq
                       :type (node-type node)
                       :name value-var
                       :expr data-node)))
                   ((cell-swap-replacement node name value-var env))))))
      (let ((rewritten
              (traverse-with-binding-list
               body
               (list
                (action (:traverse node-abstraction node _bound-variables)
                  (declare (ignore _bound-variables))
                  node)
                (action (:traverse node-bind node bound-variables)
                  (let* ((bind-name (node-bind-name node))
                         (bind-expr (node-bind-expr node))
                         (bind-body (node-bind-body node))
                         (new-bound (cons bind-name bound-variables)))
                    (if (and (node-abstraction-p bind-expr)
                             (non-escaping-bind-function-p
                              node
                              *cell-non-escaping-functions*
                              bound-variables))
                        (let ((*cell-non-escaping-functions*
                                (adjoin bind-name *cell-non-escaping-functions* :test #'eq)))
                          (make-node-bind
                           :type (node-type node)
                           :name bind-name
                           :expr (rewrite-non-escaping-abstraction bind-expr new-bound)
                           :body (funcall *traverse* bind-body new-bound)))
                        (make-node-bind
                         :type (node-type node)
                         :name bind-name
                         :expr (funcall *traverse* bind-expr new-bound)
                         :body (funcall *traverse* bind-body new-bound)))))
                (action (:traverse node-let node bound-variables)
                  (let* ((new-bound (append (mapcar #'car (node-let-bindings node))
                                            bound-variables))
                         (let-non-escaping (non-escaping-let-function-names
                                            node
                                            *cell-non-escaping-functions*
                                            bound-variables))
                         (*cell-non-escaping-functions*
                           (append let-non-escaping *cell-non-escaping-functions*)))
                    (make-node-let
                     :type (node-type node)
                     :bindings (loop :for (b-name . b-node) :in (node-let-bindings node)
                                     :collect (cons b-name
                                                    (if (and (node-abstraction-p b-node)
                                                             (member b-name
                                                                     *cell-non-escaping-functions*
                                                                     :test #'eq))
                                                        (rewrite-non-escaping-abstraction
                                                         b-node
                                                         new-bound)
                                                        (funcall *traverse* b-node new-bound))))
                     :subexpr (funcall *traverse* (node-let-subexpr node) new-bound))))
                (action (:after node-lisp node bound-variables)
                  (rewrite-cell-op node bound-variables))
                (action (:after node-direct-application node bound-variables)
                  (rewrite-cell-op node bound-variables))
                (action (:after node-application node bound-variables)
                  (rewrite-cell-op node bound-variables))
                (action (:after node-bind node _bound-variables)
                  (declare (ignore _bound-variables))
                  (let ((sub (node-bind-body node)))
                    (when (and (node-setq-p sub)
                               (eq value-var (node-setq-name sub))
                               (node-local-variable-p (node-setq-expr sub))
                               (eq (node-bind-name node)
                                   (node-variable-value (node-setq-expr sub)))
                               (node-free-p (node-bind-expr node)
                                            (list (node-bind-name node))))
                      (make-node-setq
                       :type (node-type node)
                       :name value-var
                       :expr (node-bind-expr node)))))
                (action (:after node-let node _bound-variables)
                  (declare (ignore _bound-variables))
                  (let ((bindings (node-let-bindings node))
                        (sub (node-let-subexpr node)))
                    (when (and (= 1 (length bindings))
                               (node-setq-p sub)
                               (eq value-var (node-setq-name sub))
                               (node-local-variable-p (node-setq-expr sub))
                               (eq (caar bindings)
                                   (node-variable-value (node-setq-expr sub)))
                               (node-free-p (cdar bindings)
                                            (list (caar bindings))))
                      (make-node-setq
                       :type (node-type node)
                       :name value-var
                       :expr (cdar bindings)))))))))
        (when (node-free-p rewritten (list name))
          rewritten)))))

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
  "Replace a nonrecursive binding of a fresh struct or cell, or return NIL.

Inline helper bodies often wrap the constructor in argument bindings. Descend
through those wrappers, keeping their evaluation order and recursive groups.
Identifiers have already been renamed, so extending their scope is safe."
  (multiple-value-bind (init-node element-type)
      (constructed-cell-init expr env)
    (when init-node
      (let* ((value-var (gensym "CELL-"))
             (replacement (replace-cell-accesses body name value-var env)))
        (when replacement
          (return-from scalar-replace-binding
            (make-node-bind
             :type (node-type body)
             :name value-var
             :expr (if (node-abstraction-p init-node)
                       (make-node-locally
                        :type element-type
                        :noinline-functions nil
                        :type-check nil
                        :subexpr init-node)
                       (copy-node init-node element-type))
             :body replacement))))))
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
  "Eliminate local structs used only through their generated field readers and
unbox local cells used only through `read`, `write!`, and `swap!`.

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
         (when (node-free-p (node-bind-expr node) (list (node-bind-name node)))
           (alexandria:when-let ((replacement
                                  (scalar-replace-binding
                                   (node-bind-name node) (node-bind-expr node)
                                   (node-bind-body node) env)))
             (incf count)
             replacement)))
       (action (:after node-let node)
         (multiple-value-bind (replacement removed) (scalar-replace-let node env)
           (incf count removed)
           replacement))))
     count)))
