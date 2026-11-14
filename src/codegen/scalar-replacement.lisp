(defpackage #:coalton-impl/codegen/scalar-replacement
  (:documentation
   "Scalar replacement eliminates locally constructed immutable structs and
algebraic data types used only through known field readers or pattern matching,
and unboxes locally constructed `coalton/cell:Cell` bindings used only through
`read`, `write!`, and `swap!`. It binds constructor arguments (and, for
branching sum-type initializers, an unboxed constructor tag) or cell contents to
typed scalar locals and replaces reader calls, matches, and cell accesses with
those locals, exposing the concrete field types to the Lisp compiler without
allocating or boxing the enclosing object.

The optimizer runs this pass in release mode after inlining and direct-call
conversion. Constructor arguments still evaluate exactly once, in order,
including unused fields. Non-escaping local functions (`flet`/`labels`) may
read and mutate unboxed cells. Recursive bindings and objects passed to
remaining calls, returned, stored, captured by escaping closures, or exposed to
arbitrary Lisp forms retain their allocation. The pass does not change function
calling conventions.")
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
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
   #:non-capturing-higher-order-rator-p
   #:non-escaping-bind-function-p
   #:non-escaping-let-function-names)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:rt #:coalton-impl/runtime)
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:scalar-replace))

(in-package #:coalton-impl/codegen/scalar-replacement)

(defun eligible-adt-type-p (type)
  "Return true when TYPE is an immutable struct or algebraic data type whose
local constructors can be scalar-replaced."
  (declare (type tc:type-entry type)
           (values boolean &optional))
  (and (not (tc:type-entry-newtype type))
       (not (tc:type-entry-enum-repr type))
       (not (tc:type-entry-exception-p type))
       (not (tc:type-entry-resumption-p type))
       (or (null (tc:type-entry-explicit-repr type))
           (and (member (tc:type-entry-name type)
                        '(coalton:Optional coalton:List)
                        :test #'eq)
                t))))

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

(defun cell-pop-replacement (node name value-var)
  "If NODE calls `coalton/cell:pop!` on the cell bound to NAME, return the
replacement `node-match` AST."
  (declare (type node node)
           (type parser:identifier name value-var)
           (values (or null node) &optional))
  (let ((pop-sym (cell-symbol "POP!")))
    (when (and pop-sym
               (typep node '(or node-application node-direct-application))
               (eq pop-sym (node-rator-name node))
               (null (getf (node-properties node) :noinline))
               (null (etypecase node
                       (node-application (node-application-keyword-rands node))
                       (node-direct-application (node-direct-application-keyword-rands node))))
               (= 1 (length (node-rands node))))
      (let ((cel-arg (first (node-rands node))))
        (when (and (node-local-variable-p cel-arg)
                   (eq name (node-variable-value cel-arg))
                   (tc:tapp-p (node-type node)))
          (let* ((opt-type (node-type node))
                 (elt-type (tc:tapp-to opt-type))
                 (list-type (cell-type-element-type (node-type cel-arg)))
                 (fst-var (gensym "CELL-FST-"))
                 (rst-var (gensym "CELL-RST-")))
            (when list-type
              (make-node-match
               :type opt-type
               :expr (make-node-local-variable :type list-type :value value-var)
               :branches
               (list
                (make-match-branch
                 :pattern (make-pattern-constructor
                           :type list-type
                           :name 'coalton:Cons
                           :patterns (list (make-pattern-var :type elt-type :name fst-var)
                                           (make-pattern-var :type list-type :name rst-var)))
                 :body (make-node-seq
                        :type opt-type
                        :nodes (list (make-node-setq
                                      :type list-type
                                      :name value-var
                                      :expr (make-node-local-variable
                                             :type list-type
                                             :value rst-var))
                                     (make-node-direct-application
                                      :type opt-type
                                      :properties nil
                                      :rator-type (tc:make-function-type elt-type opt-type)
                                      :rator 'coalton:Some
                                      :rands (list (make-node-local-variable
                                                    :type elt-type
                                                    :value fst-var))))))
                (make-match-branch
                 :pattern (make-pattern-constructor
                           :type list-type
                           :name 'coalton:Nil
                           :patterns nil)
                 :body (make-node-global-variable
                        :type opt-type
                        :value 'coalton:None)))))))))))

(defvar *cell-non-escaping-functions* nil
  "Local function identifiers proven not to escape during cell replacement.")

(defun replace-cell-accesses (body name value-var env)
  "Replace known reads, writes, swaps, and pops of cell NAME with lexical
variable VALUE-VAR, returning NIL if any other use of NAME remains.

Allow references inside non-escaping local functions (`flet`/`labels`),
immediate anonymous lambda applications, loops, and branches, while leaving
escaping closures untouched so the final `node-free-p` check rejects cells
captured by escaping closures."
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
             (rewrite-call-rands (rator-name rands keyword-rands bound-variables)
               (if (and (non-capturing-higher-order-rator-p rator-name)
                        (null keyword-rands)
                        (= 2 (length rands))
                        (node-abstraction-p (first rands)))
                   (list (rewrite-non-escaping-abstraction (first rands) bound-variables)
                         (funcall *traverse* (second rands) bound-variables))
                   (mapcar (lambda (rand)
                             (funcall *traverse* rand bound-variables))
                           rands)))
             (rewrite-keyword-rands (keyword-rands bound-variables)
               (mapcar (lambda (arg)
                         (make-node-application-keyword-arg
                          :keyword (node-application-keyword-arg-keyword arg)
                          :value (funcall *traverse*
                                          (node-application-keyword-arg-value arg)
                                          bound-variables)
                          :supplied-p (alexandria:when-let
                                          ((sp (node-application-keyword-arg-supplied-p arg)))
                                        (funcall *traverse* sp bound-variables))))
                       keyword-rands))
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
                   ((cell-swap-replacement node name value-var env))
                   ((cell-pop-replacement node name value-var))))))
      (let ((rewritten
              (traverse-with-binding-list
               body
               (list
                (action (:traverse node-abstraction node _bound-variables)
                  (declare (ignore _bound-variables))
                  node)
                (action (:traverse node-direct-application node bound-variables)
                  (make-node-direct-application
                   :type (node-type node)
                   :properties (node-direct-application-properties node)
                   :rator-type (node-direct-application-rator-type node)
                   :rator (node-direct-application-rator node)
                   :rands (rewrite-call-rands
                           (node-direct-application-rator node)
                           (node-direct-application-rands node)
                           (node-direct-application-keyword-rands node)
                           bound-variables)
                   :keyword-rands (rewrite-keyword-rands
                                   (node-direct-application-keyword-rands node)
                                   bound-variables)))
                (action (:traverse node-application node bound-variables)
                  (let ((rator (node-application-rator node)))
                    (make-node-application
                     :type (node-type node)
                     :properties (node-application-properties node)
                     :rator (if (node-abstraction-p rator)
                                (rewrite-non-escaping-abstraction rator bound-variables)
                                (funcall *traverse* rator bound-variables))
                     :rands (rewrite-call-rands
                             (node-rator-name node)
                             (node-application-rands node)
                             (node-application-keyword-rands node)
                             bound-variables)
                     :keyword-rands (rewrite-keyword-rands
                                     (node-application-keyword-rands node)
                                     bound-variables))))
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

(defun inlined-some-arg (node)
  "If NODE is an inlined `coalton:Some` wrapper, return its argument node."
  (declare (type node node)
           (values (or null node-local-variable) &optional))
  (when (and (node-lisp-p node)
             (= 1 (length (node-lisp-vars node)))
             (= 1 (length (node-lisp-form node)))
             (tc:tapp-p (node-type node))
             (tc:tycon-p (tc:tapp-from (node-type node)))
             (eq 'coalton:Optional (tc:tycon-name (tc:tapp-from (node-type node)))))
    (let ((binding (first (node-lisp-vars node)))
          (form (first (node-lisp-form node))))
      (when (and (consp form)
                 (eq 'rt:cl-some (first form))
                 (consp (cdr form))
                 (null (cddr form))
                 (eq (car binding) (second form)))
        (make-node-local-variable
         :type (tc:tapp-to (node-type node))
         :value (cdr binding))))))

(defun inlined-cons-args (node)
  "If NODE is an inlined `coalton:Cons` wrapper, return its argument nodes."
  (declare (type node node)
           (values list boolean &optional))
  (when (and (node-lisp-p node)
             (= 2 (length (node-lisp-vars node)))
             (= 1 (length (node-lisp-form node)))
             (tc:tapp-p (node-type node))
             (tc:tycon-p (tc:tapp-from (node-type node)))
             (eq 'coalton:List (tc:tycon-name (tc:tapp-from (node-type node)))))
    (let ((car-binding (first (node-lisp-vars node)))
          (cdr-binding (second (node-lisp-vars node)))
          (form (first (node-lisp-form node))))
      (when (and (consp form)
                 (eq 'cl:cons (first form))
                 (consp (cdr form))
                 (consp (cddr form))
                 (null (cdddr form))
                 (eq (car car-binding) (second form))
                 (eq (car cdr-binding) (third form)))
        (values
         (list (make-node-local-variable
                :type (tc:tapp-to (node-type node))
                :value (cdr car-binding))
               (make-node-local-variable
                :type (node-type node)
                :value (cdr cdr-binding)))
         t)))))

(defun constructed-adt (node env)
  "Recognize a saturated constructor expression for an eligible struct or ADT.
Returns (VALUES CTOR-ENTRY TYPE-ENTRY ARG-NODES)."
  (declare (type node node)
           (type tc:environment env)
           (values (or null tc:constructor-entry)
                   (or null tc:type-entry)
                   list
                   &optional))
  (typecase node
    ((or node-direct-application node-application)
     (let ((keyword-rands (etypecase node
                            (node-direct-application
                             (node-direct-application-keyword-rands node))
                            (node-application
                             (node-application-keyword-rands node))))
           (name (node-rator-name node)))
       (when (and name (null keyword-rands))
         (let* ((ctor (tc:lookup-constructor env name :no-error t))
                (type (and ctor
                           (tc:lookup-type env
                                           (tc:constructor-entry-constructs ctor)
                                           :no-error t)))
                (rands (node-rands node)))
           (when (and ctor
                      type
                      (eligible-adt-type-p type)
                      (= (tc:constructor-entry-arity ctor) (length rands)))
             (values ctor type rands))))))
    ((or node-local-variable node-global-variable)
     (let* ((name (node-variable-value node))
            (ctor (tc:lookup-constructor env name :no-error t))
            (type (and ctor
                       (tc:lookup-type env
                                       (tc:constructor-entry-constructs ctor)
                                       :no-error t))))
       (when (and ctor
                  type
                  (eligible-adt-type-p type)
                  (zerop (tc:constructor-entry-arity ctor)))
         (values ctor type nil))))
    (node-lisp
     (alexandria:if-let ((some-arg (inlined-some-arg node)))
       (values (tc:lookup-constructor env 'coalton:Some)
               (tc:lookup-type env 'coalton:Optional)
               (list some-arg))
       (multiple-value-bind (cons-args cons-p) (inlined-cons-args node)
         (when cons-p
           (values (tc:lookup-constructor env 'coalton:Cons)
                   (tc:lookup-type env 'coalton:List)
                   cons-args)))))
    (t
     (values nil nil nil))))

(defun constructed-struct (node env)
  "Recognize a fully applied constructor for an ordinary single-constructor
struct or algebraic data type."
  (declare (type node node)
           (type tc:environment env))
  (multiple-value-bind (ctor type rands) (constructed-adt node env)
    (when (and ctor
               type
               (= 1 (length (tc:type-entry-constructors type)))
               (null (tc:type-entry-explicit-repr type)))
      (values ctor type rands))))

(defun force-value-binding-node (node)
  (declare (type node node)
           (values node &optional))
  (if (node-abstraction-p node)
      (make-node-locally
       :type (node-type node)
       :noinline-functions nil
       :type-check nil
       :subexpr node)
      node))

(defun struct-reader-names (type env)
  (declare (type tc:type-entry type)
           (type tc:environment env)
           (values list &optional))
  (alexandria:when-let ((struct (tc:lookup-struct env (tc:type-entry-name type) :no-error t)))
    (loop :for field :in (tc:struct-entry-fields struct)
          :collect (tc:struct-field-accessor-name
                    (tc:struct-entry-name struct)
                    (tc:struct-field-index field)))))

(defun single-constructor-type-p (type)
  (declare (type tc:type-entry type)
           (values boolean &optional))
  (and (= 1 (length (tc:type-entry-constructors type)))
       (not (tc:type-entry-exception-p type))
       (not (tc:type-entry-resumption-p type))
       (or (null (tc:type-entry-explicit-repr type))
           (tc:type-entry-newtype type)
           (tc:type-entry-enum-repr type))))

(defun irrefutable-pattern-p (pat env)
  "Return true when PAT matches every value of its type without runtime tests."
  (declare (type pattern pat)
           (type tc:environment env)
           (values boolean &optional))
  (typecase pat
    ((or pattern-var pattern-wildcard)
     t)
    (pattern-binding
     (irrefutable-pattern-p (pattern-binding-pattern pat) env))
    (pattern-constructor
     (let* ((ctor (tc:lookup-constructor env (pattern-constructor-name pat) :no-error t))
            (type (and ctor
                       (tc:lookup-type env
                                       (tc:constructor-entry-constructs ctor)
                                       :no-error t))))
       (and ctor
            type
            (single-constructor-type-p type)
            (= (tc:constructor-entry-arity ctor)
               (length (pattern-constructor-patterns pat)))
            (every (lambda (subpat)
                     (irrefutable-pattern-p subpat env))
                   (pattern-constructor-patterns pat)))))
    (t
     nil)))

(defun bind-irrefutable-pattern (pat field body)
  "Bind the variables of irrefutable PAT to FIELD around BODY."
  (declare (type pattern pat)
           (type node field body)
           (values node &optional))
  (etypecase pat
    (pattern-wildcard
     body)
    (pattern-var
     (make-node-bind
      :type (node-type body)
      :name (pattern-var-name pat)
      :expr (copy-node field (pattern-type pat))
      :body body))
    (pattern-binding
     (make-node-bind
      :type (node-type body)
      :name (pattern-var-name (pattern-binding-var pat))
      :expr (copy-node field (pattern-type (pattern-binding-var pat)))
      :body (bind-irrefutable-pattern (pattern-binding-pattern pat) field body)))
    (pattern-constructor
     (make-node-match
      :type (node-type body)
      :expr (copy-node field (pattern-type pat))
      :branches (list (make-match-branch
                       :pattern pat
                       :body body))))))

(defun reduce-match-on-known-constructor (match-node ctor fields env)
  "Reduce MATCH-NODE when its scrutinee is constructed by CTOR with FIELDS.

Return (VALUES REPLACEMENT CATCH-ALL-BRANCH), where CATCH-ALL-BRANCH is the
branch struct when CTOR matched a catch-all wildcard or unused-variable branch."
  (declare (type node-match match-node)
           (type tc:constructor-entry ctor)
           (type list fields)
           (type tc:environment env)
           (values (or null node) (or null match-branch) &optional))
  (let ((ctor-name (tc:constructor-entry-name ctor)))
    (loop :for branch :in (node-match-branches match-node)
          :for rest-branches :on (node-match-branches match-node)
          :for pat := (match-branch-pattern branch)
          :for branch-body := (match-branch-body branch)
          :do (cond
                ((pattern-wildcard-p pat)
                 (return (values (copy-node branch-body (node-type match-node))
                                 branch)))

                ((and (pattern-var-p pat)
                      (node-free-p branch-body (list (pattern-var-name pat))))
                 (return (values (copy-node branch-body (node-type match-node))
                                 branch)))

                ((and (pattern-constructor-p pat)
                      (not (eq ctor-name (pattern-constructor-name pat))))
                 nil)

                ((and (pattern-constructor-p pat)
                      (eq ctor-name (pattern-constructor-name pat))
                      (= (length fields) (length (pattern-constructor-patterns pat)))
                      (every (lambda (subpat)
                               (irrefutable-pattern-p subpat env))
                             (pattern-constructor-patterns pat)))
                 (let ((out (copy-node branch-body (node-type match-node))))
                   (loop :for subpat :in (reverse (pattern-constructor-patterns pat))
                         :for field :in (reverse fields)
                         :do (setf out (bind-irrefutable-pattern subpat field out)))
                   (return (values out nil))))

                ((and (= 1 (length fields))
                      (pattern-constructor-p pat)
                      (eq ctor-name (pattern-constructor-name pat))
                      (= 1 (length (pattern-constructor-patterns pat))))
                 (let* ((field (first fields))
                        (field-ty (pattern-type (first (pattern-constructor-patterns pat))))
                        (unwrapped-branches
                          (loop :for rem-branch :in rest-branches
                                :for rem-pat := (match-branch-pattern rem-branch)
                                :for rem-body := (match-branch-body rem-branch)
                                :if (pattern-wildcard-p rem-pat)
                                  :collect (make-match-branch
                                            :pattern (make-pattern-wildcard :type field-ty)
                                            :body rem-body)
                                  :and :do (loop-finish)
                                :else :if (and (pattern-var-p rem-pat)
                                               (node-free-p rem-body
                                                            (list (pattern-var-name rem-pat))))
                                        :collect (make-match-branch
                                                  :pattern (make-pattern-wildcard :type field-ty)
                                                  :body rem-body)
                                        :and :do (loop-finish)
                                :else :if (and (pattern-constructor-p rem-pat)
                                               (not (eq ctor-name
                                                        (pattern-constructor-name rem-pat))))
                                        :do (progn)
                                :else :if (and (pattern-constructor-p rem-pat)
                                               (eq ctor-name
                                                   (pattern-constructor-name rem-pat))
                                               (= 1 (length (pattern-constructor-patterns rem-pat))))
                                        :collect (make-match-branch
                                                  :pattern (first (pattern-constructor-patterns rem-pat))
                                                  :body rem-body)
                                :else
                                  :do (return nil))))
                   (when unwrapped-branches
                     (return (values (make-node-match
                                      :type (node-type match-node)
                                      :expr (copy-node field field-ty)
                                      :branches unwrapped-branches)
                                     nil)))
                   (return (values nil nil))))

                (t
                 (return (values nil nil))))
          :finally (return (values nil nil)))))

(defun replace-adt-reads (body name ctor fields readers env)
  "Replace known field reads and pattern matches of NAME, returning NIL if any
other use remains.

Do not cross function boundaries: a closure capturing NAME keeps the object,
even if it only reads fields. The final free-variable check also catches uses
hidden in Lisp escapes, direct calls, and keyword arguments."
  (declare (type node body)
           (type symbol name)
           (type tc:constructor-entry ctor)
           (type list fields readers)
           (type tc:environment env)
           (values (or null node) &optional))
  (let ((body
          (traverse
           body
           (list
            (action (:traverse node-abstraction node) node)
            (action (:after node-direct-application node)
              (let ((args (node-direct-application-rands node))
                    (index (and readers
                                (position (node-direct-application-rator node) readers))))
                (when (and index
                           (null (node-direct-application-keyword-rands node))
                           (= 1 (length args))
                           (node-local-variable-p (first args))
                           (eq name (node-variable-value (first args))))
                  (copy-node (nth index fields) (node-type node)))))
            (action (:after node-match node)
              (when (and (node-local-variable-p (node-match-expr node))
                         (eq name (node-variable-value (node-match-expr node))))
                (nth-value 0
                           (reduce-match-on-known-constructor node ctor fields env))))))))
    (when (node-free-p body (list name))
      body)))

(defun replace-struct-reads (body name readers fields)
  (declare (type node body)
           (type symbol name)
           (type list readers fields))
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

(defun default-node-for-type (ty env)
  "Return a pure, allocation-free placeholder node of Coalton type TY for
unboxed sum payload slots in branches that construct a different variant, or
NIL when no zero-cost placeholder is available."
  (declare (type tc:ty ty)
           (type tc:environment env)
           (values (or null node) &optional))
  (let* ((base (tc:base-type ty))
         (tcon-name (and (tc:tycon-p base) (tc:tycon-name base)))
         (entry (and tcon-name (tc:lookup-type env tcon-name :no-error t))))
    (cond
      ((and entry (tc:type-entry-newtype entry))
       (let* ((ctor-name (first (tc:type-entry-constructors entry)))
              (scheme (and ctor-name (tc:lookup-value-type env ctor-name :no-error t))))
         (when scheme
           (let* ((ctor-ty (tc:qualified-ty-type (tc:fresh-inst scheme)))
                  (subs (tc:match (tc:function-return-type ctor-ty) ty))
                  (inner-ty (tc:apply-substitution subs (tc:function-type-from ctor-ty)))
                  (inner-node (default-node-for-type inner-ty env)))
             (when inner-node
               (copy-node inner-node ty))))))

      ((member tcon-name
               '(coalton:Bit coalton:U8 coalton:U16 coalton:U32 coalton:U64
                 coalton:I8 coalton:I16 coalton:I32 coalton:I64
                 coalton:IFix coalton:UFix coalton:Integer coalton:Fraction)
               :test #'eq)
       (make-node-literal :type ty :value 0))

      ((eq tcon-name 'coalton:F32)
       (make-node-literal :type ty :value 0.0f0))

      ((eq tcon-name 'coalton:F64)
       (make-node-literal :type ty :value 0.0d0))

      ((eq tcon-name 'coalton:Char)
       (make-node-literal :type ty :value #\Null))

      ((eq tcon-name 'coalton:String)
       (make-node-literal :type ty :value ""))

      ((eq tcon-name 'coalton:Boolean)
       (make-node-global-variable :type ty :value 'coalton:False))

      ((eq tcon-name 'coalton:Unit)
       (make-node-global-variable :type ty :value 'coalton:Unit))

      ((eq tcon-name 'coalton:List)
       (make-node-global-variable :type ty :value 'coalton:Nil))

      ((eq tcon-name 'coalton:Optional)
       (make-node-global-variable :type ty :value 'coalton:None))

      ((and entry
            (tc:type-entry-enum-repr entry)
            (tc:type-entry-constructors entry))
       (make-node-global-variable
        :type ty
        :value (first (tc:type-entry-constructors entry))))

      (t
       (let ((lisp-ty (tc:lisp-type ty env)))
         (cond
           ((or (eq lisp-ty 't)
                (ignore-errors (typep nil lisp-ty)))
            (make-node-lisp :type ty :vars nil :form '(cl:nil)))
           ((ignore-errors (typep 0 lisp-ty))
            (make-node-lisp :type ty :vars nil :form '(0)))
           ((ignore-errors (typep 0.0f0 lisp-ty))
            (make-node-lisp :type ty :vars nil :form '(0.0f0)))
           ((ignore-errors (typep 0.0d0 lisp-ty))
            (make-node-lisp :type ty :vars nil :form '(0.0d0)))
           ((ignore-errors (typep #C(0.0f0 0.0f0) lisp-ty))
            (make-node-lisp :type ty :vars nil :form '(#C(0.0f0 0.0f0))))
           ((ignore-errors (typep #C(0.0d0 0.0d0) lisp-ty))
            (make-node-lisp :type ty :vars nil :form '(#C(0.0d0 0.0d0))))
           #+sbcl
           ((eq lisp-ty 'sb-sys:system-area-pointer)
            (make-node-lisp :type ty :vars nil :form '((sb-sys:int-sap 0))))
           (t
            nil)))))))

(defun coalton-error-symbol-p (sym)
  (declare (type symbol sym)
           (values boolean &optional))
  (let ((pkg (find-package "COALTON/CLASSES")))
    (and pkg
         (eq sym (find-symbol "ERROR" pkg))
         t)))

(defun non-returning-branch-node-p (node)
  "Return true when NODE transfers control and never produces a branch value."
  (declare (type node node)
           (values boolean &optional))
  (or (typep node '(or node-throw node-return-from node-break node-continue node-resume-to))
      (and (node-lisp-p node)
           (null (node-lisp-vars node))
           (= 1 (length (node-lisp-form node)))
           (consp (first (node-lisp-form node)))
           (eq 'cl:error (first (first (node-lisp-form node)))))
      (and (node-direct-application-p node)
           (coalton-error-symbol-p (node-direct-application-rator node)))))

(defun collect-adt-branch-leaves (node env)
  "Collect `(CTOR TYPE ARGS)` entries from every returning leaf of NODE."
  (declare (type node node)
           (type tc:environment env)
           (values list boolean &optional))
  (multiple-value-bind (ctor type args) (constructed-adt node env)
    (cond
      (ctor
       (values (list (list ctor type args)) t))
      ((non-returning-branch-node-p node)
       (values nil t))
      (t
       (typecase node
         (node-bind
          (collect-adt-branch-leaves (node-bind-body node) env))
         (node-let
          (collect-adt-branch-leaves (node-let-subexpr node) env))
         (node-seq
          (let ((nodes (node-seq-nodes node)))
            (if nodes
                (collect-adt-branch-leaves (car (last nodes)) env)
                (values nil nil))))
         (node-locally
          (collect-adt-branch-leaves (node-locally-subexpr node) env))
         (node-values-bind
          (collect-adt-branch-leaves (node-values-bind-body node) env))
         (node-dynamic-let
          (collect-adt-branch-leaves (node-dynamic-let-subexpr node) env))
         (node-dynamic-extent
          (collect-adt-branch-leaves (node-dynamic-extent-body node) env))
         (node-match
          (let ((all-leaves nil))
            (dolist (branch (node-match-branches node) (values all-leaves t))
              (multiple-value-bind (branch-leaves ok?)
                  (collect-adt-branch-leaves (match-branch-body branch) env)
                (unless ok?
                  (return (values nil nil)))
                (setf all-leaves (append all-leaves branch-leaves))))))
         (t
          (values nil nil)))))))

(defun rewrite-non-returning-branch-node (node target-type)
  (declare (type node node)
           (type tc:ty target-type)
           (values node &optional))
  (if (and (node-direct-application-p node)
           (coalton-error-symbol-p (node-direct-application-rator node)))
      (make-node-direct-application
       :type target-type
       :properties (node-direct-application-properties node)
       :rator-type (tc:make-function-type*
                    (mapcar #'node-type (node-direct-application-rands node))
                    target-type)
       :rator (node-direct-application-rator node)
       :rands (node-direct-application-rands node)
       :keyword-rands (node-direct-application-keyword-rands node))
      (copy-node node target-type)))

(defun rewrite-adt-branch-leaves (node target-type leaf-fn env)
  "Rewrite every returning constructor leaf of NODE using LEAF-FN and update
spine node types to TARGET-TYPE."
  (declare (type node node)
           (type tc:ty target-type)
           (type function leaf-fn)
           (type tc:environment env)
           (values node &optional))
  (multiple-value-bind (ctor type args) (constructed-adt node env)
    (declare (ignore type))
    (cond
      (ctor
       (funcall leaf-fn ctor args))
      ((non-returning-branch-node-p node)
       (rewrite-non-returning-branch-node node target-type))
      (t
       (etypecase node
         (node-bind
          (make-node-bind
           :type target-type
           :name (node-bind-name node)
           :expr (node-bind-expr node)
           :body (rewrite-adt-branch-leaves (node-bind-body node) target-type leaf-fn env)))
         (node-let
          (make-node-let
           :type target-type
           :bindings (node-let-bindings node)
           :subexpr (rewrite-adt-branch-leaves (node-let-subexpr node) target-type leaf-fn env)))
         (node-seq
          (let ((nodes (node-seq-nodes node)))
            (make-node-seq
             :type target-type
             :nodes (append (butlast nodes)
                            (list (rewrite-adt-branch-leaves
                                   (car (last nodes))
                                   target-type
                                   leaf-fn
                                   env))))))
         (node-locally
          (make-node-locally
           :type target-type
           :noinline-functions (node-locally-noinline-functions node)
           :type-check (node-locally-type-check node)
           :subexpr (rewrite-adt-branch-leaves (node-locally-subexpr node) target-type leaf-fn env)))
         (node-values-bind
          (make-node-values-bind
           :type target-type
           :vars (node-values-bind-vars node)
           :expr (node-values-bind-expr node)
           :body (rewrite-adt-branch-leaves (node-values-bind-body node) target-type leaf-fn env)))
         (node-dynamic-let
          (make-node-dynamic-let
           :type target-type
           :bindings (node-dynamic-let-bindings node)
           :subexpr (rewrite-adt-branch-leaves (node-dynamic-let-subexpr node) target-type leaf-fn env)))
         (node-dynamic-extent
          (make-node-dynamic-extent
           :type target-type
           :name (node-dynamic-extent-name node)
           :node (node-dynamic-extent-node node)
           :body (rewrite-adt-branch-leaves (node-dynamic-extent-body node) target-type leaf-fn env)))
         (node-match
          (make-node-match
           :type target-type
           :expr (node-match-expr node)
           :branches (loop :for branch :in (node-match-branches node)
                           :collect (make-match-branch
                                     :pattern (match-branch-pattern branch)
                                     :body (rewrite-adt-branch-leaves
                                            (match-branch-body branch)
                                            target-type
                                            leaf-fn
                                            env))))))))))

(defun replace-sum-reads (body name distinct-ctors ctor-fields-alist tag-var boolean-tag-p env)
  "Replace `node-match` uses of sum-type binding NAME with matches on TAG-VAR."
  (declare (type node body)
           (type symbol name)
           (type list distinct-ctors ctor-fields-alist)
           (type node-local-variable tag-var)
           (type boolean boolean-tag-p)
           (type tc:environment env)
           (values (or null node) &optional))
  (let ((body
          (traverse
           body
           (list
            (action (:traverse node-abstraction node) node)
            (action (:after node-match node)
              (when (and (node-local-variable-p (node-match-expr node))
                         (eq name (node-variable-value (node-match-expr node))))
                (let ((reduced-bodies nil)
                      (catch-branches nil)
                      (failed? nil))
                  (dolist (ctor distinct-ctors)
                    (let ((fields (cdr (assoc ctor ctor-fields-alist :test #'eq))))
                      (multiple-value-bind (reduced catch-branch)
                          (reduce-match-on-known-constructor node ctor fields env)
                        (unless reduced
                          (setf failed? t)
                          (return))
                        (push reduced reduced-bodies)
                        (push catch-branch catch-branches))))
                  (unless failed?
                    (setf reduced-bodies (nreverse reduced-bodies)
                          catch-branches (nreverse catch-branches))
                    (let ((first-catch (find-if #'identity catch-branches)))
                      (cond
                        ;; Every constructor hit the same wildcard branch.
                        ((and first-catch
                              (every (lambda (cb) (eq cb first-catch)) catch-branches))
                         (first reduced-bodies))

                        ;; Two-constructor sum uses Boolean tag so codegen emits CL:IF.
                        (boolean-tag-p
                         (make-node-match
                          :type (node-type node)
                          :expr (copy-node tag-var tc:*boolean-type*)
                          :branches
                          (list (make-match-branch
                                 :pattern (make-pattern-constructor
                                           :type tc:*boolean-type*
                                           :name 'coalton:True
                                           :patterns nil)
                                 :body (first reduced-bodies))
                                (make-match-branch
                                 :pattern (make-pattern-constructor
                                           :type tc:*boolean-type*
                                           :name 'coalton:False
                                           :patterns nil)
                                 :body (second reduced-bodies)))))

                        ;; Multi-constructor sum with a shared catch-all branch.
                        (first-catch
                         (let ((catch-body
                                 (loop :for body-j :in reduced-bodies
                                       :for catch-j :in catch-branches
                                       :when catch-j
                                         :do (return body-j))))
                           (make-node-match
                            :type (node-type node)
                            :expr (copy-node tag-var tc:*ufix-type*)
                            :branches
                            (append
                             (loop :for body-j :in reduced-bodies
                                   :for catch-j :in catch-branches
                                   :for index :from 0
                                   :unless catch-j
                                     :collect (make-match-branch
                                               :pattern (make-pattern-literal
                                                         :type tc:*ufix-type*
                                                         :value index)
                                               :body body-j))
                             (list (make-match-branch
                                    :pattern (make-pattern-wildcard :type tc:*ufix-type*)
                                    :body catch-body))))))

                        ;; Multi-constructor sum where each constructor has its own branch.
                        (t
                         (make-node-match
                          :type (node-type node)
                          :expr (copy-node tag-var tc:*ufix-type*)
                          :branches
                          (append
                           (loop :for body-j :in (butlast reduced-bodies)
                                 :for index :from 0
                                 :collect (make-match-branch
                                           :pattern (make-pattern-literal
                                                     :type tc:*ufix-type*
                                                     :value index)
                                           :body body-j))
                           (list (make-match-branch
                                  :pattern (make-pattern-wildcard :type tc:*ufix-type*)
                                  :body (car (last reduced-bodies)))))))))))))))))
    (when (node-free-p body (list name))
      body)))

(defun scalar-replace-branching-binding (name expr body env)
  "Scalar-replace a local binding whose initializer branches across `if`/`match`
and constructs known ADTs at every returning leaf."
  (declare (type symbol name)
           (type node expr body)
           (type tc:environment env)
           (values (or null node) &optional))
  (multiple-value-bind (leaves ok?) (collect-adt-branch-leaves expr env)
    (when (and ok? leaves)
      (let* ((type (second (first leaves)))
             (same-type-p (every (lambda (leaf) (eq type (second leaf))) leaves)))
        (when same-type-p
          (let ((distinct-ctors
                  (remove-duplicates (mapcar #'first leaves) :test #'eq)))
            (cond
              ;; All returning branches construct the same constructor.
              ((= 1 (length distinct-ctors))
               (let* ((ctor (first distinct-ctors))
                      (sample-args (third (first leaves)))
                      (arity (length sample-args)))
                 (when (plusp arity)
                   (let* ((fields (loop :for arg :in sample-args
                                        :collect (make-node-local-variable
                                                  :type (node-type arg)
                                                  :value (gensym "FIELD-"))))
                          (readers (struct-reader-names type env))
                          (replacement
                            (replace-adt-reads body name ctor fields readers env)))
                     (when replacement
                       (if (= 1 arity)
                           (let* ((field (first fields))
                                  (target-type (node-type field))
                                  (rewritten-expr
                                    (rewrite-adt-branch-leaves
                                     expr
                                     target-type
                                     (lambda (leaf-ctor leaf-args)
                                       (declare (ignore leaf-ctor))
                                       (force-value-binding-node (first leaf-args)))
                                     env)))
                             (make-node-bind
                              :type (node-type body)
                              :name (node-variable-value field)
                              :expr rewritten-expr
                              :body replacement))
                           (let* ((target-type
                                    (tc:output-types-result-type
                                     (mapcar #'node-type fields)))
                                  (rewritten-expr
                                    (rewrite-adt-branch-leaves
                                     expr
                                     target-type
                                     (lambda (leaf-ctor leaf-args)
                                       (declare (ignore leaf-ctor))
                                       (make-node-values
                                        :type target-type
                                        :nodes (mapcar #'force-value-binding-node leaf-args)))
                                     env)))
                             (make-node-values-bind
                              :type (node-type body)
                              :vars (mapcar #'node-variable-value fields)
                              :expr rewritten-expr
                              :body replacement))))))))

              ;; Returning branches construct multiple constructors of a sum type.
              (t
               (let ((payload-slots nil)
                     (ctor-fields-alist nil))
                 (dolist (ctor distinct-ctors)
                   (let* ((leaf (find ctor leaves :key #'first :test #'eq))
                          (args (third leaf))
                          (used-in-ctor nil)
                          (ctor-slots
                            (loop :for arg :in args
                                  :for arg-ty := (node-type arg)
                                  :for existing
                                    := (find-if (lambda (slot)
                                                  (and (not (member slot used-in-ctor :test #'eq))
                                                       (tc:ty= arg-ty (node-type slot))))
                                                payload-slots)
                                  :collect (if existing
                                               (progn
                                                 (push existing used-in-ctor)
                                                 existing)
                                               (let ((new-slot
                                                       (make-node-local-variable
                                                        :type arg-ty
                                                        :value (gensym "FIELD-"))))
                                                 (setf payload-slots
                                                       (append payload-slots (list new-slot)))
                                                 (push new-slot used-in-ctor)
                                                 new-slot)))))
                     (push (cons ctor ctor-slots) ctor-fields-alist)))
                 (setf ctor-fields-alist (nreverse ctor-fields-alist))
                 (when (every (lambda (slot)
                                (or (every (lambda (entry)
                                             (member slot (cdr entry) :test #'eq))
                                           ctor-fields-alist)
                                    (not (null (default-node-for-type (node-type slot) env)))))
                              payload-slots)
                   (let* ((boolean-tag-p (= 2 (length distinct-ctors)))
                          (tag-type (if boolean-tag-p tc:*boolean-type* tc:*ufix-type*))
                          (tag-var (make-node-local-variable
                                    :type tag-type
                                    :value (gensym "TAG-")))
                          (replacement
                            (replace-sum-reads body name distinct-ctors ctor-fields-alist
                                               tag-var boolean-tag-p env)))
                     (when replacement
                       (labels ((make-tag-node (ctor)
                                  (let ((index (position ctor distinct-ctors :test #'eq)))
                                    (if boolean-tag-p
                                        (make-node-global-variable
                                         :type tc:*boolean-type*
                                         :value (if (zerop index)
                                                    'coalton:True
                                                    'coalton:False))
                                        (make-node-literal
                                         :type tc:*ufix-type*
                                         :value index)))))
                         (if (null payload-slots)
                             (let ((rewritten-expr
                                     (rewrite-adt-branch-leaves
                                      expr
                                      tag-type
                                      (lambda (leaf-ctor leaf-args)
                                        (declare (ignore leaf-args))
                                        (make-tag-node leaf-ctor))
                                      env)))
                               (make-node-bind
                                :type (node-type body)
                                :name (node-variable-value tag-var)
                                :expr rewritten-expr
                                :body replacement))
                             (let* ((target-type
                                      (tc:output-types-result-type
                                       (cons tag-type (mapcar #'node-type payload-slots))))
                                    (rewritten-expr
                                      (rewrite-adt-branch-leaves
                                       expr
                                       target-type
                                       (lambda (leaf-ctor leaf-args)
                                         (let ((ctor-slots
                                                 (cdr (assoc leaf-ctor ctor-fields-alist :test #'eq)))
                                               (tag-node (make-tag-node leaf-ctor)))
                                           (if (<= (length leaf-args) 1)
                                               (make-node-values
                                                :type target-type
                                                :nodes
                                                (cons tag-node
                                                      (loop :for slot :in payload-slots
                                                            :collect
                                                            (if (eq slot (first ctor-slots))
                                                                (force-value-binding-node (first leaf-args))
                                                                (default-node-for-type (node-type slot) env)))))
                                               (let* ((temp-vars
                                                        (loop :for arg :in leaf-args
                                                              :collect (make-node-local-variable
                                                                        :type (node-type arg)
                                                                        :value (gensym "ARG-"))))
                                                      (slot-temp-alist
                                                        (mapcar #'cons ctor-slots temp-vars))
                                                      (values-node
                                                        (make-node-values
                                                         :type target-type
                                                         :nodes
                                                         (cons tag-node
                                                               (loop :for slot :in payload-slots
                                                                     :for temp := (cdr (assoc slot slot-temp-alist :test #'eq))
                                                                     :collect (or temp
                                                                                  (default-node-for-type
                                                                                   (node-type slot)
                                                                                   env)))))))
                                                 (loop :with out := values-node
                                                       :for temp :in (reverse temp-vars)
                                                       :for arg :in (reverse leaf-args)
                                                       :do (setf out
                                                                 (make-node-bind
                                                                  :type target-type
                                                                  :name (node-variable-value temp)
                                                                  :expr (force-value-binding-node arg)
                                                                  :body out))
                                                       :finally (return out))))))
                                       env)))
                               (make-node-values-bind
                                :type (node-type body)
                                :vars (cons (node-variable-value tag-var)
                                            (mapcar #'node-variable-value payload-slots))
                                :expr rewritten-expr
                                :body replacement))))))))))))))))

(defun collect-cell-branch-leaves (node env)
  "Return true when every returning leaf of NODE constructs a fresh `Cell`."
  (declare (type node node)
           (type tc:environment env)
           (values boolean boolean &optional))
  (multiple-value-bind (init-node element-type) (constructed-cell-init node env)
    (declare (ignore element-type))
    (cond
      (init-node
       (values t t))
      ((non-returning-branch-node-p node)
       (values nil t))
      (t
       (typecase node
         (node-bind
          (collect-cell-branch-leaves (node-bind-body node) env))
         (node-let
          (collect-cell-branch-leaves (node-let-subexpr node) env))
         (node-seq
          (let ((nodes (node-seq-nodes node)))
            (if nodes
                (collect-cell-branch-leaves (car (last nodes)) env)
                (values nil nil))))
         (node-locally
          (collect-cell-branch-leaves (node-locally-subexpr node) env))
         (node-values-bind
          (collect-cell-branch-leaves (node-values-bind-body node) env))
         (node-dynamic-let
          (collect-cell-branch-leaves (node-dynamic-let-subexpr node) env))
         (node-dynamic-extent
          (collect-cell-branch-leaves (node-dynamic-extent-body node) env))
         (node-match
          (let ((any-leaf nil))
            (dolist (branch (node-match-branches node) (values any-leaf t))
              (multiple-value-bind (branch-leaf-p ok?)
                  (collect-cell-branch-leaves (match-branch-body branch) env)
                (unless ok?
                  (return (values nil nil)))
                (when branch-leaf-p
                  (setf any-leaf t))))))
         (t
          (values nil nil)))))))

(defun rewrite-cell-branch-leaves (node target-type env)
  "Rewrite every returning `Cell` constructor leaf of NODE to its initial value
and update spine node types to TARGET-TYPE."
  (declare (type node node)
           (type tc:ty target-type)
           (type tc:environment env)
           (values node &optional))
  (multiple-value-bind (init-node element-type) (constructed-cell-init node env)
    (declare (ignore element-type))
    (cond
      (init-node
       (force-value-binding-node (copy-node init-node target-type)))
      ((non-returning-branch-node-p node)
       (rewrite-non-returning-branch-node node target-type))
      (t
       (etypecase node
         (node-bind
          (make-node-bind
           :type target-type
           :name (node-bind-name node)
           :expr (node-bind-expr node)
           :body (rewrite-cell-branch-leaves (node-bind-body node) target-type env)))
         (node-let
          (make-node-let
           :type target-type
           :bindings (node-let-bindings node)
           :subexpr (rewrite-cell-branch-leaves (node-let-subexpr node) target-type env)))
         (node-seq
          (let ((nodes (node-seq-nodes node)))
            (make-node-seq
             :type target-type
             :nodes (append (butlast nodes)
                            (list (rewrite-cell-branch-leaves
                                   (car (last nodes))
                                   target-type
                                   env))))))
         (node-locally
          (make-node-locally
           :type target-type
           :noinline-functions (node-locally-noinline-functions node)
           :type-check (node-locally-type-check node)
           :subexpr (rewrite-cell-branch-leaves (node-locally-subexpr node) target-type env)))
         (node-values-bind
          (make-node-values-bind
           :type target-type
           :vars (node-values-bind-vars node)
           :expr (node-values-bind-expr node)
           :body (rewrite-cell-branch-leaves (node-values-bind-body node) target-type env)))
         (node-dynamic-let
          (make-node-dynamic-let
           :type target-type
           :bindings (node-dynamic-let-bindings node)
           :subexpr (rewrite-cell-branch-leaves (node-dynamic-let-subexpr node) target-type env)))
         (node-dynamic-extent
          (make-node-dynamic-extent
           :type target-type
           :name (node-dynamic-extent-name node)
           :node (node-dynamic-extent-node node)
           :body (rewrite-cell-branch-leaves (node-dynamic-extent-body node) target-type env)))
         (node-match
          (make-node-match
           :type target-type
           :expr (node-match-expr node)
           :branches (loop :for branch :in (node-match-branches node)
                           :collect (make-match-branch
                                     :pattern (match-branch-pattern branch)
                                     :body (rewrite-cell-branch-leaves
                                            (match-branch-body branch)
                                            target-type
                                            env))))))))))

(defun rewrite-cell-initializer (expr env)
  "If EXPR constructs a fresh `Cell` at every returning leaf, return the
rewritten unboxed initializer node and its element type."
  (declare (type node expr)
           (type tc:environment env)
           (values (or null node) (or null tc:ty) &optional))
  (let ((element-type (cell-type-element-type (node-type expr))))
    (when element-type
      (multiple-value-bind (has-leaf-p ok?)
          (collect-cell-branch-leaves expr env)
        (when (and has-leaf-p ok?)
          (values (rewrite-cell-branch-leaves expr element-type env)
                  element-type))))))

(defun scalar-replace-branching-cell-binding (name expr body env)
  "Unbox a local `Cell` binding whose initializer branches across `if`/`match`."
  (declare (type symbol name)
           (type node expr body)
           (type tc:environment env)
           (values (or null node) &optional))
  (multiple-value-bind (rewritten-expr element-type)
      (rewrite-cell-initializer expr env)
    (declare (ignore element-type))
    (when rewritten-expr
      (let* ((value-var (gensym "CELL-"))
             (replacement (replace-cell-accesses body name value-var env)))
        (when replacement
          (make-node-bind
           :type (node-type body)
           :name value-var
           :expr rewritten-expr
           :body replacement))))))

(defun scalar-replace-binding (name expr body env)
  "Replace a nonrecursive binding of a fresh struct, ADT, or cell, or return NIL.

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
    (t
     (or (scalar-replace-branching-cell-binding name expr body env)
         (multiple-value-bind (ctor type args) (constructed-adt expr env)
           (if ctor
               (let* ((fields (loop :for arg :in args
                                    :collect (make-node-local-variable
                                              :type (node-type arg)
                                              :value (gensym "FIELD-"))))
                      (readers (struct-reader-names type env))
                      (replacement (replace-adt-reads body name ctor fields readers env)))
                 (when replacement
                   ;; Bind every argument, including unused fields, exactly once and in
                   ;; constructor order. Keep lambdas as values rather than local FLETs.
                   (loop :for field :in (reverse fields)
                         :for arg :in (reverse args)
                         :do (setf replacement
                                   (make-node-bind
                                    :type (node-type body)
                                    :name (node-variable-value field)
                                    :expr (force-value-binding-node arg)
                                    :body replacement)))
                   replacement))
               (scalar-replace-branching-binding name expr body env)))))))

(defun identity-for-binding-step-p (binding)
  (declare (type node-for-binding binding)
           (values boolean &optional))
  (let ((step (node-for-binding-step binding)))
    (or (null step)
        (and (node-local-variable-p step)
             (eq (node-for-binding-name binding)
                 (node-variable-value step))))))

(defun scalar-replace-for (node env)
  "Unbox `Cell` bindings with identity/empty steps in `node-for-bindings` that
do not escape the loop."
  (declare (type node-for node)
           (type tc:environment env)
           (values node-for unsigned-byte &optional))
  (let ((bindings (node-for-bindings node))
        (sequential-p (node-for-sequential-p node))
        (termination-expr (node-for-termination-expr node))
        (body (node-for-body node))
        (returns (node-for-returns node))
        (count 0))
    (loop :for index :from 0 :below (length bindings)
          :for binding := (nth index bindings)
          :when (and (identity-for-binding-step-p binding)
                     (node-free-p (node-for-binding-init binding)
                                  (list (node-for-binding-name binding))))
            :do (let ((name (node-for-binding-name binding))
                      (init (node-for-binding-init binding)))
                  (multiple-value-bind (rewritten-init element-type)
                      (rewrite-cell-initializer init env)
                    (when rewritten-init
                      (let* ((value-var (gensym "CELL-"))
                             (new-body (replace-cell-accesses body name value-var env))
                             (new-term (if termination-expr
                                           (replace-cell-accesses termination-expr name value-var env)
                                           t))
                             (new-returns (if returns
                                              (replace-cell-accesses returns name value-var env)
                                              t))
                             (other-bindings-ok-p t)
                             (new-bindings
                               (loop :for b :in bindings
                                     :for j :from 0
                                     :collect
                                     (cond
                                       ((= j index)
                                        (make-node-for-binding
                                         :name value-var
                                         :type element-type
                                         :init rewritten-init
                                         :step nil))
                                       (t
                                        (let ((new-b-init
                                                (if (and sequential-p (< j index))
                                                    (and (node-free-p (node-for-binding-init b)
                                                                      (list name))
                                                         (node-for-binding-init b))
                                                    (replace-cell-accesses
                                                     (node-for-binding-init b)
                                                     name value-var env)))
                                              (new-b-step
                                                (if (node-for-binding-step b)
                                                    (replace-cell-accesses
                                                     (node-for-binding-step b)
                                                     name value-var env)
                                                    t)))
                                          (unless (and new-b-init new-b-step)
                                            (setf other-bindings-ok-p nil))
                                          (make-node-for-binding
                                           :name (node-for-binding-name b)
                                           :type (node-for-binding-type b)
                                           :init (or new-b-init (node-for-binding-init b))
                                           :step (and (node-for-binding-step b)
                                                      new-b-step))))))))
                        (when (and new-body new-term new-returns other-bindings-ok-p)
                          (setf bindings new-bindings
                                body new-body
                                termination-expr (and termination-expr new-term)
                                returns (and returns new-returns))
                          (incf count)))))))
    (values
     (if (plusp count)
         (make-node-for
          :type (node-type node)
          :label (node-for-label node)
          :bindings bindings
          :sequential-p sequential-p
          :returns returns
          :termination-kind (node-for-termination-kind node)
          :termination-expr termination-expr
          :body body)
         node)
     count)))

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

(defun scalar-replace-once (node env)
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
       (action (:after node-dynamic-extent node)
         (when (node-free-p (node-dynamic-extent-node node)
                            (list (node-dynamic-extent-name node)))
           (alexandria:when-let ((replacement
                                  (scalar-replace-binding
                                   (node-dynamic-extent-name node)
                                   (node-dynamic-extent-node node)
                                   (node-dynamic-extent-body node) env)))
             (incf count)
             replacement)))
       (action (:after node-let node)
         (multiple-value-bind (replacement removed) (scalar-replace-let node env)
           (incf count removed)
           replacement))
       (action (:after node-for node)
         (multiple-value-bind (replacement removed) (scalar-replace-for node env)
           (incf count removed)
           replacement))
       (action (:after node-match node)
         (unless (node-local-variable-p (node-match-expr node))
           (let ((temp (gensym "MATCH-")))
             (alexandria:when-let ((replacement
                                    (scalar-replace-binding
                                     temp
                                     (node-match-expr node)
                                     (make-node-match
                                      :type (node-type node)
                                      :expr (make-node-local-variable
                                             :type (node-type (node-match-expr node))
                                             :value temp)
                                      :branches (node-match-branches node))
                                     env)))
               (incf count)
               replacement))))))
     count)))

(defun scalar-replace (node env)
  "Eliminate local structs and ADTs used only through field readers or pattern
matching, and unbox local cells used only through `read`, `write!`, and `swap!`.

Run after inlining and direct-call conversion, in release mode only. Return the
rewritten node and the number of constructor sites removed. This pass neither
changes function calling conventions nor moves allocations across branches."
  (declare (type node node)
           (type tc:environment env)
           (values node unsigned-byte &optional))
  (let ((total-count 0))
    (loop :repeat 8
          :do (multiple-value-bind (new-node count)
                  (scalar-replace-once node env)
                (setf node new-node)
                (incf total-count count)
                (when (zerop count)
                  (return (values node total-count))))
          :finally (return (values node total-count)))))
