(defpackage #:coalton-impl/codegen/unboxed-return
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/traverse
   #:*traverse*
   #:action
   #:traverse
   #:traverse-with-binding-list)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:apply-unboxed-return-optimization
   #:unboxed-return-entry
   #:unboxed-return-entry-name
   #:unboxed-return-entry-raw-name
   #:unboxed-argument-entry
   #:unboxed-argument-entry-name
   #:unboxed-argument-entry-raw-name
   #:unboxed-combined-entry
   #:unboxed-combined-entry-name
   #:unboxed-combined-entry-raw-name))

(in-package #:coalton-impl/codegen/unboxed-return)

(defstruct unboxed-return-entry
  (name (util:required 'name) :type symbol :read-only t)
  (raw-name (util:required 'raw-name) :type symbol :read-only t)
  (constructor (util:required 'constructor) :type tc:constructor-entry :read-only t)
  (product-type (util:required 'product-type) :type tc:ty :read-only t)
  (field-types (util:required 'field-types) :type tc:ty-list :read-only t)
  (result-type (util:required 'result-type) :type tc:ty :read-only t)
  (raw-function-type (util:required 'raw-function-type) :type tc:ty :read-only t))

(defstruct unboxed-argument-slot
  (index (util:required 'index) :type alexandria:non-negative-fixnum :read-only t)
  (var (util:required 'var) :type symbol :read-only t)
  (constructor (util:required 'constructor) :type tc:constructor-entry :read-only t)
  (product-type (util:required 'product-type) :type tc:ty :read-only t)
  (field-types (util:required 'field-types) :type tc:ty-list :read-only t)
  (field-vars (util:required 'field-vars) :type list :read-only t))

(defstruct unboxed-argument-entry
  (name (util:required 'name) :type symbol :read-only t)
  (raw-name (util:required 'raw-name) :type symbol :read-only t)
  (raw-function-type (util:required 'raw-function-type) :type tc:ty :read-only t)
  (slots (util:required 'slots) :type list :read-only t))

(defstruct unboxed-combined-entry
  (name (util:required 'name) :type symbol :read-only t)
  (raw-name (util:required 'raw-name) :type symbol :read-only t)
  (return-entry (util:required 'return-entry) :type unboxed-return-entry :read-only t)
  (argument-entry (util:required 'argument-entry) :type unboxed-argument-entry :read-only t)
  (raw-function-type (util:required 'raw-function-type) :type tc:ty :read-only t))

(defun unboxed-raw-name (name)
  (declare (type symbol name)
           (values symbol &optional))
  (alexandria:format-symbol (symbol-package name)
                            "%~A-UNBOXED-RETURN"
                            (symbol-name name)))

(defun unboxed-arguments-raw-name (name)
  (declare (type symbol name)
           (values symbol &optional))
  (alexandria:format-symbol (symbol-package name)
                            "%~A-UNBOXED-ARGUMENTS"
                            (symbol-name name)))

(defun unboxed-combined-raw-name (name)
  (declare (type symbol name)
           (values symbol &optional))
  (alexandria:format-symbol (symbol-package name)
                            "%~A-UNBOXED-ARGUMENTS-AND-RETURN"
                            (symbol-name name)))

(defun base-tycon (type)
  (declare (type tc:ty type)
           (values (or null tc:tycon) &optional))
  (etypecase type
    (tc:tycon type)
    (tc:tapp (base-tycon (tc:tapp-from type)))
    (tc:tyvar nil)
    (tc:function-ty nil)
    (tc:result-ty nil)
    (tc:tgen nil)))

(defun constructor-field-types (constructor product-type env)
  (declare (type tc:constructor-entry constructor)
           (type tc:ty product-type)
           (type tc:environment env)
           (values tc:ty-list &optional))
  (let* ((constructor-name (tc:constructor-entry-name constructor))
         (generic-constructor-type
           (tc:qualified-ty-type
            (tc:fresh-inst
             (tc:lookup-value-type env constructor-name))))
         (constructor-type
           (tc:apply-substitution
            (tc:match (tc:function-return-type generic-constructor-type)
                      product-type)
            generic-constructor-type)))
    (tc:function-type-arguments constructor-type)))

(defun product-return-constructor (product-type env)
  (declare (type tc:ty product-type)
           (type tc:environment env)
           (values (or null tc:constructor-entry) &optional))
  (let* ((base (base-tycon product-type))
         (type-entry (and base
                          (tc:lookup-type env (tc:tycon-name base) :no-error t))))
    (unless type-entry
      (return-from product-return-constructor nil))

    ;; This first implementation targets ordinary release-mode product
    ;; structs only. Enums and transparent/native representations already have
    ;; specialized runtime layouts, and exceptions/resumptions have non-local
    ;; control-flow behavior.
    (when (or (tc:type-entry-enum-repr type-entry)
              (tc:type-entry-newtype type-entry)
              (tc:type-entry-exception-p type-entry)
              (tc:type-entry-resumption-p type-entry)
              (not (eq (tc:type-entry-runtime-type type-entry)
                       (tc:type-entry-name type-entry))))
      (return-from product-return-constructor nil))

    (let ((constructors (tc:type-entry-constructors type-entry)))
      (unless (= 1 (length constructors))
        (return-from product-return-constructor nil))
      (let ((constructor (tc:lookup-constructor env (first constructors))))
        (and (plusp (tc:constructor-entry-arity constructor))
             (tc:constructor-entry-classname constructor)
             constructor)))))

(defun maybe-unboxed-return-entry (name node env)
  (declare (type symbol name)
           (type node node)
           (type tc:environment env)
           (values (or null unboxed-return-entry) &optional))
  (unless (and (settings:coalton-release-p)
               (node-abstraction-p node)
               (typep (node-type node) 'tc:function-ty)
               (null (node-abstraction-keyword-params node))
               (null (tc:function-ty-keyword-input-types (node-type node)))
               (not (tc:function-ty-keyword-open-p (node-type node))))
    (return-from maybe-unboxed-return-entry nil))

  (let ((outputs (tc:function-output-types (node-type node))))
    (unless (= 1 (length outputs))
      (return-from maybe-unboxed-return-entry nil))
    (let* ((product-type (first outputs))
           (constructor (product-return-constructor product-type env)))
      (unless constructor
        (return-from maybe-unboxed-return-entry nil))
      (let* ((field-types (constructor-field-types constructor product-type env))
             (result-type (tc:output-types-result-type field-types))
             (raw-function-type
               (tc:make-function-ty
                :alias (tc:ty-alias (node-type node))
                :positional-input-types
                (tc:function-ty-positional-input-types (node-type node))
                :keyword-input-types nil
                :keyword-open-p nil
                :output-types field-types)))
        (make-unboxed-return-entry
         :name name
         :raw-name (unboxed-raw-name name)
         :constructor constructor
         :product-type product-type
         :field-types field-types
         :result-type result-type
         :raw-function-type raw-function-type)))))

(defun make-entry-candidates (definitions env)
  (declare (type binding-list definitions)
           (type tc:environment env)
           (values hash-table &optional))
  (let ((table (make-hash-table :test #'eq)))
    (loop :for (name . node) :in definitions
          :for entry := (maybe-unboxed-return-entry name node env)
          :when entry
            :do (setf (gethash name table) entry))
    table))

(defun copy-entry-table (table)
  (declare (type hash-table table)
           (values hash-table &optional))
  (let ((out (make-hash-table :test #'eq)))
    (maphash (lambda (key value)
               (setf (gethash key out) value))
             table)
    out))

(defun keyword-rands (node)
  (declare (type (or node-application node-direct-application) node)
           (values keyword-arg-list &optional))
  (etypecase node
    (node-application
     (node-application-keyword-rands node))
    (node-direct-application
     (node-direct-application-keyword-rands node))))

(defun call-entry (node entry-table)
  (declare (type node node)
           (type hash-table entry-table)
           (values (or null unboxed-return-entry) &optional))
  (when (typep node '(or node-application node-direct-application))
    (let ((name (node-rator-name node)))
      (and name
           (null (keyword-rands node))
           (gethash name entry-table)))))

(defun raw-call (application entry)
  (declare (type (or node-application node-direct-application) application)
           (type unboxed-return-entry entry)
           (values node-direct-application &optional))
  (make-node-direct-application
   :type (unboxed-return-entry-result-type entry)
   :properties (node-properties application)
   :rator-type (unboxed-return-entry-raw-function-type entry)
   :rator (unboxed-return-entry-raw-name entry)
   :rands (node-rands application)
   :keyword-rands nil))

(defun product-constructor-call-p (node entry env)
  (declare (type node node)
           (type unboxed-return-entry entry)
           (type tc:environment env)
           (values boolean &optional))
  (unless (typep node '(or node-application node-direct-application))
    (return-from product-constructor-call-p nil))
  (let ((name (node-rator-name node)))
    (unless (and name (null (keyword-rands node)))
      (return-from product-constructor-call-p nil))
    (let ((constructor (tc:lookup-constructor env name :no-error t)))
      (and constructor
           (eq (tc:constructor-entry-name constructor)
               (tc:constructor-entry-name
                (unboxed-return-entry-constructor entry)))
           (= (length (node-rands node))
              (length (unboxed-return-entry-field-types entry)))))))

(defun combined-raw-argument-call-entry (node combined-entry-table)
  (declare (type node node)
           (type hash-table combined-entry-table)
           (values (or null unboxed-combined-entry) &optional))
  (when (typep node '(or node-application node-direct-application))
    (let ((name (node-rator-name node)))
      (and name
           (null (keyword-rands node))
           (loop :for entry :being :the :hash-values :of combined-entry-table
                 :thereis
                 (and (eq name
                          (unboxed-argument-entry-raw-name
                           (unboxed-combined-entry-argument-entry entry)))
                      entry))))))

(defun lower-tail-to-values
    (node entry entry-table env &optional combined-entry-table)
  (declare (type node node)
           (type unboxed-return-entry entry)
           (type hash-table entry-table)
           (type tc:environment env)
           (type (or null hash-table) combined-entry-table)
           (values (or null node) &optional))
  (cond
    ((product-constructor-call-p node entry env)
     (make-node-values
      :type (unboxed-return-entry-result-type entry)
      :nodes (node-rands node)))

    ((call-entry node entry-table)
     (let ((callee-entry (call-entry node entry-table)))
       (when (equalp (unboxed-return-entry-field-types callee-entry)
                     (unboxed-return-entry-field-types entry))
         (raw-call node callee-entry))))

    ((and combined-entry-table
          (combined-raw-argument-call-entry node combined-entry-table))
     (let* ((combined-entry
              (combined-raw-argument-call-entry node combined-entry-table))
            (callee-entry
              (unboxed-combined-entry-return-entry combined-entry)))
       (when (equalp (unboxed-return-entry-field-types callee-entry)
                     (unboxed-return-entry-field-types entry))
         (make-node-direct-application
          :type (unboxed-return-entry-result-type entry)
          :properties (node-properties node)
          :rator-type (unboxed-combined-entry-raw-function-type combined-entry)
          :rator (unboxed-combined-entry-raw-name combined-entry)
          :rands (node-rands node)
          :keyword-rands nil))))

    ((typep node 'node-match)
     (let ((branches
             (loop :for branch :in (node-match-branches node)
                   :for lowered := (lower-tail-to-values
                                    (match-branch-body branch)
                                    entry
                                    entry-table
                                    env
                                    combined-entry-table)
                   :unless lowered
                     :do (return-from lower-tail-to-values nil)
                   :collect (make-match-branch
                             :pattern (match-branch-pattern branch)
                             :body lowered))))
       (make-node-match
        :type (unboxed-return-entry-result-type entry)
        :expr (node-match-expr node)
        :branches branches)))

    ((node-let-p node)
     (alexandria:when-let ((lowered (lower-tail-to-values
                                     (node-let-subexpr node)
                                     entry
                                     entry-table
                                     env
                                     combined-entry-table)))
       (make-node-let
        :type (unboxed-return-entry-result-type entry)
        :bindings (node-let-bindings node)
        :subexpr lowered)))

    ((typep node 'node-bind)
     (alexandria:when-let ((lowered (lower-tail-to-values
                                     (node-bind-body node)
                                     entry
                                     entry-table
                                     env
                                     combined-entry-table)))
       (make-node-bind
        :type (unboxed-return-entry-result-type entry)
        :name (node-bind-name node)
        :expr (node-bind-expr node)
        :body lowered)))

    ((node-values-bind-p node)
     (alexandria:when-let ((lowered (lower-tail-to-values
                                     (node-values-bind-body node)
                                     entry
                                     entry-table
                                     env
                                     combined-entry-table)))
       (make-node-values-bind
        :type (unboxed-return-entry-result-type entry)
        :vars (node-values-bind-vars node)
        :expr (node-values-bind-expr node)
        :body lowered)))

    ((node-locally-p node)
     (alexandria:when-let ((lowered (lower-tail-to-values
                                     (node-locally-subexpr node)
                                     entry
                                     entry-table
                                     env
                                     combined-entry-table)))
       (make-node-locally
        :type (unboxed-return-entry-result-type entry)
        :noinline-functions (node-locally-noinline-functions node)
        :type-check (node-locally-type-check node)
        :subexpr lowered)))

    ((node-dynamic-let-p node)
     (alexandria:when-let ((lowered (lower-tail-to-values
                                     (node-dynamic-let-subexpr node)
                                     entry
                                     entry-table
                                     env
                                     combined-entry-table)))
       (make-node-dynamic-let
        :type (unboxed-return-entry-result-type entry)
        :bindings (node-dynamic-let-bindings node)
        :subexpr lowered)))

    ((typep node 'node-dynamic-extent)
     (alexandria:when-let ((lowered (lower-tail-to-values
                                     (node-dynamic-extent-body node)
                                     entry
                                     entry-table
                                     env
                                     combined-entry-table)))
       (make-node-dynamic-extent
        :type (unboxed-return-entry-result-type entry)
        :name (node-dynamic-extent-name node)
        :node (node-dynamic-extent-node node)
        :body lowered)))

    ((typep node 'node-seq)
     (let ((nodes (node-seq-nodes node)))
       (unless nodes
         (return-from lower-tail-to-values nil))
       (let ((lowered (lower-tail-to-values
                       (car (last nodes))
                       entry
                       entry-table
                       env
                       combined-entry-table)))
         (and lowered
              (make-node-seq
               :type (unboxed-return-entry-result-type entry)
               :nodes (append (butlast nodes) (list lowered)))))))

    (t
     nil)))

(defun eligible-entry-table (definitions candidates env)
  (declare (type binding-list definitions)
           (type hash-table candidates)
           (type tc:environment env)
           (values hash-table &optional))
  (let ((eligible (make-hash-table :test #'eq))
        (definition-table (alexandria:alist-hash-table definitions :test #'eq))
        (changed t))
    (loop :while changed :do
      (setf changed nil)
      (maphash
       (lambda (name entry)
         (unless (gethash name eligible)
           (let ((table (copy-entry-table eligible)))
             ;; Self-recursive tail calls can use the raw entry point once the
             ;; function's other tail exits are lowerable.
             (setf (gethash name table) entry)
             (let ((node (gethash name definition-table)))
               (when (and node
                          (lower-tail-to-values
                           (node-abstraction-subexpr node)
                           entry
                           table
                           env))
                 (setf (gethash name eligible) entry
                       changed t))))))
       candidates))
    eligible))

(defun entry-list (table)
  (declare (type hash-table table)
           (values list &optional))
  (let (entries)
    (maphash (lambda (_ entry)
               (declare (ignore _))
               (push entry entries))
             table)
    entries))

(defun constructor-reader-symbol (constructor i)
  (declare (type tc:constructor-entry constructor)
           (type alexandria:non-negative-fixnum i)
           (values symbol &optional))
  (alexandria:format-symbol
   (symbol-package (tc:constructor-entry-classname constructor))
   "~A-_~D"
   (tc:constructor-entry-classname constructor)
   i))

(defun make-reader-table (entry-table)
  (declare (type hash-table entry-table)
           (values hash-table &optional))
  (let ((readers (make-hash-table :test #'eq)))
    (dolist (entry (entry-list entry-table))
      (loop :for nil :in (unboxed-return-entry-field-types entry)
            :for i :from 0
            :for reader := (constructor-reader-symbol
                            (unboxed-return-entry-constructor entry)
                            i)
            :do (setf (gethash reader readers) (cons entry i))))
    readers))

(defun simple-constructor-pattern-vars (pattern entry)
  (declare (type pattern pattern)
           (type unboxed-return-entry entry)
           (values (or null list) &optional))
  (unless (and (pattern-constructor-p pattern)
               (eq (pattern-constructor-name pattern)
                   (tc:constructor-entry-name
                    (unboxed-return-entry-constructor entry)))
               (= (length (pattern-constructor-patterns pattern))
                  (length (unboxed-return-entry-field-types entry))))
    (return-from simple-constructor-pattern-vars nil))
  (loop :for subpattern :in (pattern-constructor-patterns pattern)
        :collect
        (etypecase subpattern
          (pattern-var
           (pattern-var-name subpattern))
          (pattern-wildcard
           (gensym "_"))
          ((or pattern-constructor pattern-literal pattern-binding)
           (return-from simple-constructor-pattern-vars nil)))))

(defun unused-pattern-var-p (pattern body)
  (declare (type pattern-var pattern)
           (type node body)
           (values boolean &optional))
  (not (member (pattern-var-name pattern)
               (node-variables body)
               :test #'eq)))

(defun field-gensyms (entry)
  (declare (type unboxed-return-entry entry)
           (values list &optional))
  (loop :for nil :in (unboxed-return-entry-field-types entry)
        :collect (gensym "_")))

(defun unboxed-return-call (application entry-table combined-entry-table env)
  (declare (type node application)
           (type hash-table entry-table)
           (type (or null hash-table) combined-entry-table)
           (type tc:environment env)
           (values (or null unboxed-return-entry) (or null node) &optional))
  (alexandria:when-let
      ((combined-entry
         (and combined-entry-table
              (combined-call-entry application combined-entry-table))))
    (alexandria:when-let
        ((raw (raw-combined-call
               application
               combined-entry
               entry-table
               combined-entry-table
               env)))
      (return-from unboxed-return-call
        (values (unboxed-combined-entry-return-entry combined-entry)
                raw))))
  (alexandria:when-let ((entry (call-entry application entry-table)))
    (return-from unboxed-return-call
      (values entry (raw-call application entry))))
  (values nil nil))

(defun rewrite-unboxed-match (node entry-table combined-entry-table env)
  (declare (type node-match node)
           (type hash-table entry-table)
           (type (or null hash-table) combined-entry-table)
           (type tc:environment env)
           (values (or null node-values-bind) &optional))
  (multiple-value-bind (entry raw-expr)
      (unboxed-return-call
       (node-match-expr node)
       entry-table
       combined-entry-table
       env)
    (unless entry
      (return-from rewrite-unboxed-match nil))
    (let* ((branch (first (node-match-branches node)))
           (pattern (and branch (match-branch-pattern branch)))
           (body (and branch (match-branch-body branch)))
           (vars
             (cond
               ((null branch)
                nil)
               ((pattern-constructor-p pattern)
                (simple-constructor-pattern-vars pattern entry))
               ((pattern-wildcard-p pattern)
                (field-gensyms entry))
               ((and (pattern-var-p pattern)
                     (unused-pattern-var-p pattern body))
                (field-gensyms entry))
               (t
                nil))))
      (when vars
        (make-node-values-bind
         :type (node-type node)
         :vars vars
         :expr raw-expr
         :body body)))))

(defun application-rator-name-and-rands (node)
  (declare (type (or node-application node-direct-application) node)
           (values (or null symbol) node-list boolean &optional))
  (etypecase node
    (node-direct-application
     (values (node-direct-application-rator node)
             (node-direct-application-rands node)
             (null (node-direct-application-keyword-rands node))))
    (node-application
     (values (and (node-variable-p (node-application-rator node))
                  (node-variable-value (node-application-rator node)))
             (node-application-rands node)
             (null (node-application-keyword-rands node))))))

(defun rewrite-unboxed-accessor
    (node reader-table entry-table combined-entry-table env)
  (declare (type (or node-application node-direct-application) node)
           (type hash-table reader-table entry-table)
           (type (or null hash-table) combined-entry-table)
           (type tc:environment env)
           (values (or null node-values-bind) &optional))
  (multiple-value-bind (rator-name rands no-keywords-p)
      (application-rator-name-and-rands node)
    (unless (and rator-name
                 no-keywords-p
                 (= 1 (length rands)))
      (return-from rewrite-unboxed-accessor nil))
    (alexandria:when-let ((reader (gethash rator-name reader-table)))
      (let* ((reader-entry (car reader))
             (field-index (cdr reader))
             (arg (first rands)))
        (multiple-value-bind (arg-entry raw-expr)
            (unboxed-return-call
             arg
             entry-table
             combined-entry-table
             env)
          (when (eq reader-entry arg-entry)
            (let* ((vars (field-gensyms arg-entry))
                   (field-type (nth field-index
                                    (unboxed-return-entry-field-types arg-entry)))
                   (field-var (nth field-index vars)))
              (make-node-values-bind
               :type (node-type node)
               :vars vars
               :expr raw-expr
               :body (make-node-variable
                      :type field-type
                      :value field-var)))))))))

(defun argument-slot-field-node (slot index)
  (declare (type unboxed-argument-slot slot)
           (type alexandria:non-negative-fixnum index)
           (values node-variable &optional))
  (make-node-variable
   :type (nth index (unboxed-argument-slot-field-types slot))
   :value (nth index (unboxed-argument-slot-field-vars slot))))

(defun make-argument-reader-table (slots)
  (declare (type list slots)
           (values hash-table &optional))
  (let ((readers (make-hash-table :test #'eq)))
    (dolist (slot slots)
      (loop :for nil :in (unboxed-argument-slot-field-types slot)
            :for i :from 0
            :for reader := (constructor-reader-symbol
                            (unboxed-argument-slot-constructor slot)
                            i)
            :do (push (cons slot i) (gethash reader readers))))
    readers))

(defun tracked-argument-slot (node slots bound-variables)
  (declare (type node node)
           (type list slots)
           (type list bound-variables)
           (values (or null unboxed-argument-slot) &optional))
  (when (node-variable-p node)
    (let ((name (node-variable-value node)))
      (and (not (member name bound-variables :test #'eq))
           (find name slots
                 :key #'unboxed-argument-slot-var
                 :test #'eq)))))

(defun simple-argument-constructor-pattern-bindings (pattern slot)
  (declare (type pattern pattern)
           (type unboxed-argument-slot slot)
           (values list boolean &optional))
  (unless (and (pattern-constructor-p pattern)
               (eq (pattern-constructor-name pattern)
                   (tc:constructor-entry-name
                    (unboxed-argument-slot-constructor slot)))
               (= (length (pattern-constructor-patterns pattern))
                  (length (unboxed-argument-slot-field-types slot))))
    (return-from simple-argument-constructor-pattern-bindings
      (values nil nil)))
  (let ((bindings nil))
    (loop :for subpattern :in (pattern-constructor-patterns pattern)
          :for i :from 0
          :do
             (etypecase subpattern
               (pattern-var
                (push (cons (pattern-var-name subpattern)
                            (argument-slot-field-node slot i))
                      bindings))
               (pattern-wildcard)
               ((or pattern-constructor pattern-literal pattern-binding)
                (return-from simple-argument-constructor-pattern-bindings
                  (values nil nil)))))
    (values (nreverse bindings) t)))

(defun wrap-field-bindings (bindings body)
  (declare (type list bindings)
           (type node body)
           (values node &optional))
  (reduce
   (lambda (binding body)
     (make-node-bind
      :type (node-type body)
      :name (car binding)
      :expr (cdr binding)
      :body body))
   bindings
   :from-end t
   :initial-value body))

(defun rewrite-tracked-argument-match (node slot bound-variables)
  (declare (type node-match node)
           (type unboxed-argument-slot slot)
           (type list bound-variables)
           (values (or null node) &optional))
  (let ((branches (node-match-branches node)))
    (unless (= 1 (length branches))
      (return-from rewrite-tracked-argument-match nil))
    (let* ((branch (first branches))
           (pattern (match-branch-pattern branch))
           (body (match-branch-body branch)))
      (multiple-value-bind (bindings ok)
          (cond
            ((pattern-constructor-p pattern)
             (simple-argument-constructor-pattern-bindings pattern slot))
            ((pattern-wildcard-p pattern)
             (values nil t))
            ((and (pattern-var-p pattern)
                  (unused-pattern-var-p pattern body))
             (values nil t))
            (t
             (values nil nil)))
        (unless ok
          (return-from rewrite-tracked-argument-match nil))
        (let* ((pattern-vars (pattern-variables pattern))
               (rewritten-body
                 (funcall *traverse*
                          body
                          (append pattern-vars bound-variables))))
          (wrap-field-bindings bindings rewritten-body))))))

(defun rewrite-argument-keyword-rands (keyword-rands bound-variables)
  (declare (type keyword-arg-list keyword-rands)
           (type list bound-variables)
           (values keyword-arg-list &optional))
  (mapcar
   (lambda (arg)
     (make-node-application-keyword-arg
      :keyword (node-application-keyword-arg-keyword arg)
      :value (funcall *traverse*
                      (node-application-keyword-arg-value arg)
                      bound-variables)
      :supplied-p
      (alexandria:when-let
          ((supplied-p (node-application-keyword-arg-supplied-p arg)))
        (funcall *traverse* supplied-p bound-variables))))
   keyword-rands))

(defun rewrite-tracked-argument-accessor (node reader-table slots bound-variables)
  (declare (type (or node-application node-direct-application) node)
           (type hash-table reader-table)
           (type list slots bound-variables)
           (values (or null node-variable) &optional))
  (multiple-value-bind (rator-name rands no-keywords-p)
      (application-rator-name-and-rands node)
    (unless (and rator-name
                 no-keywords-p
                 (= 1 (length rands)))
      (return-from rewrite-tracked-argument-accessor nil))
    (alexandria:when-let* ((readers (gethash rator-name reader-table))
                           (arg-slot (tracked-argument-slot
                                      (first rands)
                                      slots
                                      bound-variables))
                           (reader (find arg-slot
                                         readers
                                         :key #'car
                                         :test #'eq)))
      (argument-slot-field-node arg-slot (cdr reader)))))

(defun argument-slots-compatible-p (from to)
  (declare (type unboxed-argument-slot from to)
           (values boolean &optional))
  (and (eq (tc:constructor-entry-name
            (unboxed-argument-slot-constructor from))
           (tc:constructor-entry-name
            (unboxed-argument-slot-constructor to)))
       (equalp (unboxed-argument-slot-field-types from)
               (unboxed-argument-slot-field-types to))))

(defun rewrite-tracked-argument-forwarding-call
    (node slots bound-variables argument-entry-table return-entry-table
     combined-entry-table env)
  (declare (type (or node-application node-direct-application) node)
           (type list slots bound-variables)
           (type hash-table argument-entry-table return-entry-table)
           (type (or null hash-table) combined-entry-table)
           (type tc:environment env)
           (values (or null node) &optional))
  (multiple-value-bind (rator-name rands no-keywords-p)
      (application-rator-name-and-rands node)
    (unless (and rator-name no-keywords-p)
      (return-from rewrite-tracked-argument-forwarding-call nil))
    (alexandria:when-let ((entry (gethash rator-name argument-entry-table)))
      (unless (= (length rands)
                 (length (tc:function-type-arguments
                          (node-rator-type node))))
        (return-from rewrite-tracked-argument-forwarding-call nil))
      (let ((raw-rands nil)
            (steps nil)
            (forwarded-tracked-var-p nil))
        (loop :for arg :in rands
              :for i :from 0
              :for callee-slot
                := (find i
                         (unboxed-argument-entry-slots entry)
                         :key #'unboxed-argument-slot-index
                         :test #'=)
              :do
                 (if callee-slot
                     (alexandria:if-let
                         ((arg-slot
                            (tracked-argument-slot
                             arg
                             slots
                             bound-variables)))
                       (progn
                         (unless (argument-slots-compatible-p
                                  arg-slot
                                  callee-slot)
                           (return-from
                               rewrite-tracked-argument-forwarding-call
                             nil))
                         (setf forwarded-tracked-var-p t
                               raw-rands
                               (append
                                (reverse
                                 (loop :for field-index
                                         :below (length
                                                 (unboxed-argument-slot-field-types
                                                  arg-slot))
                                       :collect
                                       (argument-slot-field-node
                                        arg-slot
                                        field-index)))
                                raw-rands)))
                       (multiple-value-bind (field-rands field-steps)
                           (flatten-unboxed-argument
                            arg
                            callee-slot
                            return-entry-table
                            combined-entry-table
                            env)
                         (unless field-rands
                           (return-from
                               rewrite-tracked-argument-forwarding-call
                             nil))
                         (setf raw-rands (append (reverse field-rands)
                                                 raw-rands)
                               steps (append (reverse field-steps)
                                             steps))))
                     (multiple-value-bind (var rand)
                         (temp-variable-node (node-type arg))
                       (push (list :bind
                                   var
                                   (funcall *traverse*
                                            arg
                                            bound-variables))
                             steps)
                       (push rand raw-rands))))
        (unless forwarded-tracked-var-p
          (return-from rewrite-tracked-argument-forwarding-call nil))
        (wrap-argument-call-steps
         (nreverse steps)
         (make-node-direct-application
          :type (node-type node)
          :properties (node-properties node)
          :rator-type (unboxed-argument-entry-raw-function-type entry)
          :rator (unboxed-argument-entry-raw-name entry)
          :rands (nreverse raw-rands)
          :keyword-rands nil))))))

(defun rewrite-product-argument-body
    (body slots &key argument-entry-table return-entry-table
                combined-entry-table env)
  (declare (type node body)
           (type list slots)
           (type (or null hash-table) argument-entry-table return-entry-table
                 combined-entry-table)
           (type (or null tc:environment) env)
           (values node boolean &optional))
  (let ((failed nil)
        (reader-table (make-argument-reader-table slots)))
    (labels ((fail ()
               (setf failed t)
               nil)
             (tracked-vars-in-node-p (node bound-variables)
               (loop :for slot :in slots
                     :for var := (unboxed-argument-slot-var slot)
                     :thereis
                     (and (member var
                                  (node-variables node)
                                  :test #'eq)
                          (not (member var bound-variables :test #'eq))))))
      (let ((rewritten
              (traverse-with-binding-list
               body
               (list
                (action (:after node-variable node bound-variables)
                  (when (tracked-argument-slot node slots bound-variables)
                    (fail))
                  (values))
                (action (:after node-lisp node bound-variables)
                  (when (tracked-vars-in-node-p node bound-variables)
                    (fail))
                  (values))
                (action (:traverse node-application node bound-variables)
                  (or (rewrite-tracked-argument-accessor
                       node
                       reader-table
                       slots
                       bound-variables)
                      (and argument-entry-table
                           return-entry-table
                           env
                           (rewrite-tracked-argument-forwarding-call
                            node
                            slots
                            bound-variables
                            argument-entry-table
                            return-entry-table
                            combined-entry-table
                            env))
                      (make-node-application
                       :type (node-type node)
                       :properties (node-properties node)
                       :rator (funcall *traverse*
                                       (node-application-rator node)
                                       bound-variables)
                       :rands (loop :for rand :in (node-application-rands node)
                                    :collect (funcall *traverse*
                                                      rand
                                                      bound-variables))
                       :keyword-rands
                       (rewrite-argument-keyword-rands
                        (node-application-keyword-rands node)
                        bound-variables))))
                (action (:traverse node-direct-application node bound-variables)
                  (or (rewrite-tracked-argument-accessor
                       node
                       reader-table
                       slots
                       bound-variables)
                      (and argument-entry-table
                           return-entry-table
                           env
                           (rewrite-tracked-argument-forwarding-call
                            node
                            slots
                            bound-variables
                            argument-entry-table
                            return-entry-table
                            combined-entry-table
                            env))
                      (make-node-direct-application
                       :type (node-type node)
                       :properties (node-properties node)
                       :rator-type (node-direct-application-rator-type node)
                       :rator (node-direct-application-rator node)
                       :rands (loop :for rand :in (node-direct-application-rands node)
                                    :collect (funcall *traverse*
                                                      rand
                                                      bound-variables))
                       :keyword-rands
                       (rewrite-argument-keyword-rands
                        (node-direct-application-keyword-rands node)
                        bound-variables))))
                (action (:traverse node-match node bound-variables)
                  (alexandria:if-let
                      ((slot (tracked-argument-slot
                              (node-match-expr node)
                              slots
                              bound-variables)))
                    (or (rewrite-tracked-argument-match
                         node
                         slot
                         bound-variables)
                        (progn
                          (fail)
                          node))
                    (make-node-match
                     :type (node-type node)
                     :expr (funcall *traverse*
                                    (node-match-expr node)
                                    bound-variables)
                     :branches
                     (mapcar
                      (lambda (branch)
                        (make-match-branch
                         :pattern (match-branch-pattern branch)
                         :body (funcall *traverse*
                                        (match-branch-body branch)
                                        (append
                                         (pattern-variables
                                          (match-branch-pattern branch))
                                         bound-variables))))
                      (node-match-branches node)))))))))
        (values rewritten (not failed))))))

(defun rewrite-unboxed-return-call-sites
    (node entry-table combined-entry-table env)
  (declare (type node node)
           (type hash-table entry-table)
           (type (or null hash-table) combined-entry-table)
           (type tc:environment env)
           (values node &optional))
  (let ((reader-table (make-reader-table entry-table)))
    (labels ((rewrite-match (node)
               (or (rewrite-unboxed-match
                    node
                    entry-table
                    combined-entry-table
                    env)
                   node))
             (rewrite-accessor (node)
               (or (rewrite-unboxed-accessor
                    node
                    reader-table
                    entry-table
                    combined-entry-table
                    env)
                   node)))
      (traverse
       node
       (list
        (action (:after node-match) #'rewrite-match)
        (action (:after node-direct-application) #'rewrite-accessor)
        (action (:after node-application) #'rewrite-accessor))))))

(defun maybe-unboxed-argument-slot (index var type env)
  (declare (type alexandria:non-negative-fixnum index)
           (type symbol var)
           (type tc:ty type)
           (type tc:environment env)
           (values (or null unboxed-argument-slot) &optional))
  (alexandria:when-let ((constructor (product-return-constructor type env)))
    (let ((field-types (constructor-field-types constructor type env)))
      (make-unboxed-argument-slot
       :index index
       :var var
       :constructor constructor
       :product-type type
       :field-types field-types
       :field-vars (loop :for nil :in field-types
                         :collect (gensym (format nil "~A-" var)))))))

(defun raw-argument-input-types (argument-types slots)
  (declare (type tc:ty-list argument-types)
           (type list slots)
           (values tc:ty-list &optional))
  (loop :for type :in argument-types
        :for i :from 0
        :for slot := (find i slots
                           :key #'unboxed-argument-slot-index
                           :test #'=)
        :append (if slot
                    (unboxed-argument-slot-field-types slot)
                    (list type))))

(defun raw-argument-vars (vars slots)
  (declare (type list vars slots)
           (values list &optional))
  (loop :for var :in vars
        :for i :from 0
        :for slot := (find i slots
                           :key #'unboxed-argument-slot-index
                           :test #'=)
        :append (if slot
                    (unboxed-argument-slot-field-vars slot)
                    (list var))))

(defun maybe-unboxed-argument-entry
    (name node env return-entry-table argument-entry-table combined-entry-table)
  (declare (type symbol name)
           (type node node)
           (type tc:environment env)
           (type hash-table return-entry-table argument-entry-table)
           (type (or null hash-table) combined-entry-table)
           (values (or null unboxed-argument-entry) &optional))
  (unless (and (settings:coalton-release-p)
               (node-abstraction-p node)
               (typep (node-type node) 'tc:function-ty)
               (null (node-abstraction-keyword-params node))
               (null (tc:function-ty-keyword-input-types (node-type node)))
               (not (tc:function-ty-keyword-open-p (node-type node))))
    (return-from maybe-unboxed-argument-entry nil))
  (let* ((argument-types
           (tc:function-ty-positional-input-types (node-type node)))
         (vars (node-abstraction-vars node))
         (candidate-slots
           (loop :for type :in argument-types
                 :for var :in vars
                 :for index :from 0
                 :for slot := (maybe-unboxed-argument-slot
                               index
                               var
                               type
                               env)
                 :when slot
                   :collect slot))
         (eligible-slots
           (loop :for slot :in candidate-slots
                 :when (nth-value
                        1
                        (rewrite-product-argument-body
                         (node-abstraction-subexpr node)
                         (list slot)
                         :argument-entry-table argument-entry-table
                         :return-entry-table return-entry-table
                         :combined-entry-table combined-entry-table
                         :env env))
                   :collect slot)))
    (unless eligible-slots
      (return-from maybe-unboxed-argument-entry nil))
    (unless (nth-value
             1
             (rewrite-product-argument-body
              (node-abstraction-subexpr node)
              eligible-slots
              :argument-entry-table argument-entry-table
              :return-entry-table return-entry-table
              :combined-entry-table combined-entry-table
              :env env))
      (return-from maybe-unboxed-argument-entry nil))
    (make-unboxed-argument-entry
     :name name
     :raw-name (unboxed-arguments-raw-name name)
     :raw-function-type
     (tc:make-function-ty
      :alias (tc:ty-alias (node-type node))
      :positional-input-types (raw-argument-input-types
                               argument-types
                               eligible-slots)
      :keyword-input-types nil
      :keyword-open-p nil
      :output-types (tc:function-ty-output-types (node-type node)))
     :slots eligible-slots)))

(defun argument-entry-slot-indices (entry)
  (declare (type unboxed-argument-entry entry)
           (values list &optional))
  (mapcar #'unboxed-argument-slot-index
          (unboxed-argument-entry-slots entry)))

(defun argument-entry-tables-equivalent-p (left right)
  (declare (type hash-table left right)
           (values boolean &optional))
  (and (= (hash-table-count left)
          (hash-table-count right))
       (loop :for name :being :the :hash-keys :of left
             :for left-entry := (gethash name left)
             :for right-entry := (gethash name right)
             :always
             (and right-entry
                  (equal (argument-entry-slot-indices left-entry)
                         (argument-entry-slot-indices right-entry))))))

(defun make-argument-entry-table (definitions env return-entry-table)
  (declare (type binding-list definitions)
           (type tc:environment env)
           (type hash-table return-entry-table)
           (values hash-table &optional))
  (loop
    :with table := (make-hash-table :test #'eq)
    :for combined-entry-table := (make-combined-entry-table
                                  return-entry-table
                                  table)
    :for next-table := (make-hash-table :test #'eq)
    :do
       (loop :for (name . node) :in definitions
             :for entry := (maybe-unboxed-argument-entry
                            name
                            node
                            env
                            return-entry-table
                            table
                            combined-entry-table)
             :when entry
               :do (setf (gethash name next-table) entry))
    :until (argument-entry-tables-equivalent-p table next-table)
    :do (setf table next-table)
    :finally (return table)))

(defun make-combined-entry-table (return-entries argument-entries)
  (declare (type hash-table return-entries argument-entries)
           (values hash-table &optional))
  (let ((table (make-hash-table :test #'eq)))
    (maphash
     (lambda (name return-entry)
       (alexandria:when-let ((argument-entry (gethash name argument-entries)))
         (setf
          (gethash name table)
          (make-unboxed-combined-entry
           :name name
           :raw-name (unboxed-combined-raw-name name)
           :return-entry return-entry
           :argument-entry argument-entry
           :raw-function-type
           (tc:make-function-ty
            :alias (tc:ty-alias
                    (unboxed-argument-entry-raw-function-type argument-entry))
            :positional-input-types
            (tc:function-ty-positional-input-types
             (unboxed-argument-entry-raw-function-type argument-entry))
            :keyword-input-types nil
            :keyword-open-p nil
            :output-types
            (unboxed-return-entry-field-types return-entry))))))
     return-entries)
    table))

(defun slot-constructor-call-fields (node slot env)
  (declare (type node node)
           (type unboxed-argument-slot slot)
           (type tc:environment env)
           (values (or null node-list) &optional))
  (unless (typep node '(or node-application node-direct-application))
    (return-from slot-constructor-call-fields nil))
  (let ((name (node-rator-name node)))
    (unless (and name (null (keyword-rands node)))
      (return-from slot-constructor-call-fields nil))
    (let ((constructor (tc:lookup-constructor env name :no-error t)))
      (when (and constructor
                 (eq (tc:constructor-entry-name constructor)
                     (tc:constructor-entry-name
                      (unboxed-argument-slot-constructor slot)))
                 (= (length (node-rands node))
                    (length (unboxed-argument-slot-field-types slot))))
        (node-rands node)))))

(defun compatible-return-call-entry (node slot return-entry-table)
  (declare (type node node)
           (type unboxed-argument-slot slot)
           (type hash-table return-entry-table)
           (values (or null unboxed-return-entry) &optional))
  (alexandria:when-let ((entry (call-entry node return-entry-table)))
    (and (return-entry-compatible-with-slot-p entry slot)
         entry)))

(defun return-entry-compatible-with-slot-p (entry slot)
  (declare (type unboxed-return-entry entry)
           (type unboxed-argument-slot slot)
           (values boolean &optional))
  (and (eq (tc:constructor-entry-name
            (unboxed-return-entry-constructor entry))
           (tc:constructor-entry-name
            (unboxed-argument-slot-constructor slot)))
       (equalp (unboxed-return-entry-field-types entry)
               (unboxed-argument-slot-field-types slot))))

(defun compatible-combined-call-entry (node slot combined-entry-table)
  (declare (type node node)
           (type unboxed-argument-slot slot)
           (type hash-table combined-entry-table)
           (values (or null unboxed-combined-entry) &optional))
  (alexandria:when-let ((entry (combined-call-entry node combined-entry-table)))
    (and (eq (tc:constructor-entry-name
              (unboxed-return-entry-constructor
               (unboxed-combined-entry-return-entry entry)))
             (tc:constructor-entry-name
              (unboxed-argument-slot-constructor slot)))
         (equalp (unboxed-return-entry-field-types
                  (unboxed-combined-entry-return-entry entry))
                 (unboxed-argument-slot-field-types slot))
         entry)))

(defun temp-variable-node (type &optional (prefix "ARG"))
  (declare (type tc:ty type)
           (type string prefix)
           (values symbol node-variable &optional))
  (let ((var (gensym prefix)))
    (values var
            (make-node-variable
             :type type
             :value var))))

(defun flatten-unboxed-argument
    (arg slot return-entry-table combined-entry-table env)
  (declare (type node arg)
           (type unboxed-argument-slot slot)
           (type hash-table return-entry-table)
           (type (or null hash-table) combined-entry-table)
           (type tc:environment env)
           (values (or null list) list &optional))
  (alexandria:when-let ((fields (slot-constructor-call-fields arg slot env)))
    (let ((steps nil)
          (rands nil))
      (loop :for field :in fields
            :for type :in (unboxed-argument-slot-field-types slot)
            :do (multiple-value-bind (var node)
                    (temp-variable-node type "FIELD")
                  (push (list :bind var field) steps)
                  (push node rands)))
      (return-from flatten-unboxed-argument
        (values (nreverse rands) (nreverse steps)))))
  (alexandria:when-let
      ((combined-entry
         (and combined-entry-table
              (compatible-combined-call-entry
               arg
               slot
               combined-entry-table))))
    (alexandria:when-let
        ((raw (raw-combined-call
               arg
               combined-entry
               return-entry-table
               combined-entry-table
               env)))
      (let* ((return-entry
               (unboxed-combined-entry-return-entry combined-entry))
             (vars (field-gensyms return-entry)))
        (return-from flatten-unboxed-argument
          (values
           (loop :for var :in vars
                 :for type :in (unboxed-return-entry-field-types return-entry)
                 :collect (make-node-variable
                           :type type
                           :value var))
           (list (list :values-bind vars raw)))))))
  (alexandria:if-let ((return-entry
                       (compatible-return-call-entry
                        arg
                        slot
                        return-entry-table)))
    (let ((vars (field-gensyms return-entry)))
      (values
       (loop :for var :in vars
             :for type :in (unboxed-return-entry-field-types return-entry)
             :collect (make-node-variable
                       :type type
                       :value var))
       (list (list :values-bind
                   vars
                   (raw-call arg return-entry)))))
    (values nil nil)))

(defun wrap-argument-call-steps (steps body)
  (declare (type list steps)
           (type node body)
           (values node &optional))
  (reduce
   (lambda (step body)
     (ecase (first step)
       (:bind
        (make-node-bind
         :type (node-type body)
         :name (second step)
         :expr (third step)
         :body body))
       (:values-bind
        (make-node-values-bind
         :type (node-type body)
         :vars (second step)
         :expr (third step)
         :body body))))
   steps
   :from-end t
   :initial-value body))

(defun flatten-unboxed-argument-rands
    (node entry return-entry-table combined-entry-table env)
  (declare (type (or node-application node-direct-application) node)
           (type unboxed-argument-entry entry)
           (type hash-table return-entry-table)
           (type (or null hash-table) combined-entry-table)
           (type tc:environment env)
           (values list list boolean &optional))
  (multiple-value-bind (_ rands no-keywords-p)
      (application-rator-name-and-rands node)
    (declare (ignore _))
    (unless no-keywords-p
      (return-from flatten-unboxed-argument-rands
        (values nil nil nil)))
    (let ((slots (unboxed-argument-entry-slots entry))
          (raw-rands nil)
          (steps nil))
      (unless (= (length rands)
                 (length (tc:function-type-arguments
                          (node-rator-type node))))
        (return-from flatten-unboxed-argument-rands
          (values nil nil nil)))
      (loop :for arg :in rands
            :for i :from 0
            :for slot := (find i slots
                                :key #'unboxed-argument-slot-index
                                :test #'=)
            :do
               (if slot
                   (multiple-value-bind (field-rands field-steps)
                       (flatten-unboxed-argument
                        arg
                        slot
                        return-entry-table
                        combined-entry-table
                        env)
                     (unless field-rands
                       (return-from flatten-unboxed-argument-rands
                         (values nil nil nil)))
                     (setf raw-rands (append (reverse field-rands)
                                             raw-rands)
                           steps (append (reverse field-steps)
                                         steps)))
                   (multiple-value-bind (var rand)
                       (temp-variable-node (node-type arg))
                     (push (list :bind var arg) steps)
                     (push rand raw-rands))))
      (values (nreverse raw-rands)
              (nreverse steps)
              t))))

(defun combined-call-entry (node entry-table)
  (declare (type node node)
           (type hash-table entry-table)
           (values (or null unboxed-combined-entry) &optional))
  (when (typep node '(or node-application node-direct-application))
    (let ((name (node-rator-name node)))
      (and name
           (null (keyword-rands node))
           (gethash name entry-table)))))

(defun raw-combined-call
    (application entry return-entry-table combined-entry-table env)
  (declare (type (or node-application node-direct-application) application)
           (type unboxed-combined-entry entry)
           (type hash-table return-entry-table)
           (type (or null hash-table) combined-entry-table)
           (type tc:environment env)
           (values (or null node) &optional))
  (multiple-value-bind (raw-rands steps ok)
      (flatten-unboxed-argument-rands
       application
       (unboxed-combined-entry-argument-entry entry)
       return-entry-table
       combined-entry-table
       env)
    (when ok
      (wrap-argument-call-steps
       steps
       (make-node-direct-application
        :type (unboxed-return-entry-result-type
               (unboxed-combined-entry-return-entry entry))
        :properties (node-properties application)
        :rator-type (unboxed-combined-entry-raw-function-type entry)
        :rator (unboxed-combined-entry-raw-name entry)
        :rands raw-rands
        :keyword-rands nil)))))

(defun rewrite-unboxed-argument-call
    (node argument-entry-table return-entry-table combined-entry-table env)
  (declare (type (or node-application node-direct-application) node)
           (type hash-table argument-entry-table return-entry-table)
           (type (or null hash-table) combined-entry-table)
           (type tc:environment env)
           (values (or null node) &optional))
  (multiple-value-bind (rator-name rands no-keywords-p)
      (application-rator-name-and-rands node)
    (declare (ignore rands))
    (unless (and rator-name no-keywords-p)
      (return-from rewrite-unboxed-argument-call nil))
    (alexandria:when-let ((entry (gethash rator-name argument-entry-table)))
      (multiple-value-bind (raw-rands steps ok)
          (flatten-unboxed-argument-rands
           node
           entry
           return-entry-table
           combined-entry-table
           env)
        (when ok
          (wrap-argument-call-steps
           steps
           (make-node-direct-application
            :type (node-type node)
            :properties (node-properties node)
            :rator-type (unboxed-argument-entry-raw-function-type entry)
            :rator (unboxed-argument-entry-raw-name entry)
            :rands raw-rands
            :keyword-rands nil)))))))

(defun rewrite-unboxed-argument-call-sites
    (node argument-entry-table return-entry-table combined-entry-table env)
  (declare (type node node)
           (type hash-table argument-entry-table return-entry-table)
           (type (or null hash-table) combined-entry-table)
           (type tc:environment env)
           (values node &optional))
  (labels ((traverse-application (node)
             (make-node-application
              :type (node-type node)
              :properties (node-properties node)
              :rator (funcall *traverse* (node-application-rator node))
              :rands (loop :for rand :in (node-application-rands node)
                           :collect (funcall *traverse* rand))
              :keyword-rands
              (mapcar
               (lambda (arg)
                 (make-node-application-keyword-arg
                  :keyword (node-application-keyword-arg-keyword arg)
                  :value (funcall *traverse*
                                  (node-application-keyword-arg-value arg))
                  :supplied-p
                  (alexandria:when-let
                      ((supplied-p
                         (node-application-keyword-arg-supplied-p arg)))
                    (funcall *traverse* supplied-p))))
               (node-application-keyword-rands node))))
           (traverse-direct-application (node)
             (make-node-direct-application
              :type (node-type node)
              :properties (node-properties node)
              :rator-type (node-direct-application-rator-type node)
              :rator (node-direct-application-rator node)
              :rands (loop :for rand :in (node-direct-application-rands node)
                           :collect (funcall *traverse* rand))
              :keyword-rands
              (mapcar
               (lambda (arg)
                 (make-node-application-keyword-arg
                  :keyword (node-application-keyword-arg-keyword arg)
                  :value (funcall *traverse*
                                  (node-application-keyword-arg-value arg))
                  :supplied-p
                  (alexandria:when-let
                      ((supplied-p
                         (node-application-keyword-arg-supplied-p arg)))
                    (funcall *traverse* supplied-p))))
               (node-direct-application-keyword-rands node))))
           (rewrite-application (node)
             (or (rewrite-unboxed-argument-call
                  node
                  argument-entry-table
                  return-entry-table
                  combined-entry-table
                  env)
                 (etypecase node
                   (node-application
                    (traverse-application node))
                   (node-direct-application
                    (traverse-direct-application node))))))
    (traverse
     node
     (list
      (action (:traverse node-direct-application) #'rewrite-application)
      (action (:traverse node-application) #'rewrite-application)))))

(defun make-raw-argument-binding
    (name node entry argument-entry-table return-entry-table
     combined-entry-table env)
  (declare (type symbol name)
           (type node-abstraction node)
           (type unboxed-argument-entry entry)
           (type hash-table argument-entry-table return-entry-table)
           (type (or null hash-table) combined-entry-table)
           (type tc:environment env)
           (ignore name)
           (values cons &optional))
  (multiple-value-bind (body ok)
      (rewrite-product-argument-body
       (node-abstraction-subexpr node)
       (unboxed-argument-entry-slots entry)
       :argument-entry-table argument-entry-table
       :return-entry-table return-entry-table
       :combined-entry-table combined-entry-table
       :env env)
    (unless ok
      (util:coalton-bug "Unable to lower unboxed argument function ~S"
                        (unboxed-argument-entry-name entry)))
    (cons
     (unboxed-argument-entry-raw-name entry)
     (make-node-abstraction
      :type (unboxed-argument-entry-raw-function-type entry)
      :vars (raw-argument-vars
             (node-abstraction-vars node)
             (unboxed-argument-entry-slots entry))
      :keyword-params nil
      :subexpr body))))

(defun make-raw-combined-binding
    (name node entry argument-entry-table return-entry-table
     combined-entry-table env)
  (declare (type symbol name)
           (type node-abstraction node)
           (type unboxed-combined-entry entry)
           (type hash-table argument-entry-table return-entry-table
                 combined-entry-table)
           (type tc:environment env)
           (ignore name)
           (values cons &optional))
  (let* ((argument-entry (unboxed-combined-entry-argument-entry entry))
         (return-entry (unboxed-combined-entry-return-entry entry)))
    (multiple-value-bind (body ok)
        (rewrite-product-argument-body
         (node-abstraction-subexpr node)
         (unboxed-argument-entry-slots argument-entry)
         :argument-entry-table argument-entry-table
         :return-entry-table return-entry-table
         :combined-entry-table combined-entry-table
         :env env)
      (unless ok
        (util:coalton-bug "Unable to lower combined unboxed function ~S"
                          (unboxed-combined-entry-name entry)))
      (let ((lowered
              (lower-tail-to-values
               body
               return-entry
               return-entry-table
               env
               combined-entry-table)))
        (unless lowered
          (util:coalton-bug "Unable to lower combined unboxed function ~S"
                            (unboxed-combined-entry-name entry)))
        (cons
         (unboxed-combined-entry-raw-name entry)
         (make-node-abstraction
          :type (unboxed-combined-entry-raw-function-type entry)
          :vars (raw-argument-vars
                 (node-abstraction-vars node)
                 (unboxed-argument-entry-slots argument-entry))
          :keyword-params nil
          :subexpr lowered))))))

(defun make-raw-binding (name node entry entry-table env)
  (declare (type symbol name)
           (type node-abstraction node)
           (type unboxed-return-entry entry)
           (type hash-table entry-table)
           (type tc:environment env)
           (ignore name)
           (values cons &optional))
  (let ((lowered (lower-tail-to-values
                  (node-abstraction-subexpr node)
                  entry
                  entry-table
                  env)))
    (unless lowered
      (util:coalton-bug "Unable to lower unboxed return function ~S"
                        (unboxed-return-entry-name entry)))
    (cons
     (unboxed-return-entry-raw-name entry)
     (make-node-abstraction
      :type (unboxed-return-entry-raw-function-type entry)
      :vars (node-abstraction-vars node)
      :keyword-params nil
      :subexpr lowered))))

(defun make-wrapper-binding (name node entry)
  (declare (type symbol name)
           (type node-abstraction node)
           (type unboxed-return-entry entry)
           (ignore name)
           (values cons &optional))
  (let* ((arg-types
           (tc:function-ty-positional-input-types (node-type node)))
         (args
           (loop :for var :in (node-abstraction-vars node)
                 :for type :in arg-types
                 :collect (make-node-variable
                           :type type
                           :value var)))
         (raw-call
           (make-node-direct-application
            :type (unboxed-return-entry-result-type entry)
            :properties '()
            :rator-type (unboxed-return-entry-raw-function-type entry)
            :rator (unboxed-return-entry-raw-name entry)
            :rands args
            :keyword-rands nil))
         (field-vars (field-gensyms entry))
         (field-nodes
           (loop :for var :in field-vars
                 :for type :in (unboxed-return-entry-field-types entry)
                 :collect (make-node-variable
                           :type type
                           :value var)))
         (constructor-type
           (tc:make-function-type*
            (unboxed-return-entry-field-types entry)
            (unboxed-return-entry-product-type entry))))
    (cons
     (unboxed-return-entry-name entry)
     (make-node-abstraction
      :type (node-type node)
      :vars (node-abstraction-vars node)
      :keyword-params nil
      :subexpr
      (make-node-values-bind
       :type (unboxed-return-entry-product-type entry)
       :vars field-vars
       :expr raw-call
       :body (make-node-direct-application
              :type (unboxed-return-entry-product-type entry)
              :properties '()
              :rator-type constructor-type
              :rator (tc:constructor-entry-name
                      (unboxed-return-entry-constructor entry))
              :rands field-nodes
              :keyword-rands nil))))))

(defun apply-unboxed-return-optimization (definitions env)
  "Add raw multiple-value entry points for simple product functions.

Returns four values: the rewritten binding list, the table of raw return
metadata, the table of raw argument metadata, and the table of combined raw
argument/raw return metadata. In non-release mode, returns DEFINITIONS, NIL,
NIL, and NIL."
  (declare (type binding-list definitions)
           (type tc:environment env)
           (values binding-list
                   (or null hash-table)
                   (or null hash-table)
                   (or null hash-table)
                   &optional))
  (unless (settings:coalton-release-p)
    (return-from apply-unboxed-return-optimization
      (values definitions nil nil nil)))

  (let* ((return-candidates (make-entry-candidates definitions env))
         (return-entries
           (eligible-entry-table definitions return-candidates env))
         (argument-entries
           (make-argument-entry-table definitions env return-entries))
         (combined-entries
           (make-combined-entry-table return-entries argument-entries))
         (return-rewritten-definitions
           (if (zerop (hash-table-count return-entries))
               definitions
               (loop :for (name . node) :in definitions
                     :collect (cons name
                                    (rewrite-unboxed-return-call-sites
                                     node
                                     return-entries
                                     combined-entries
                                     env)))))
         (rewritten-definitions
           (if (zerop (hash-table-count argument-entries))
               return-rewritten-definitions
               (loop :for (name . node) :in return-rewritten-definitions
                     :collect (cons name
                                    (rewrite-unboxed-argument-call-sites
                                     node
                                     argument-entries
                                     return-entries
                                     combined-entries
                                     env)))))
         (return-raw-bindings
           (loop :for (name . node) :in return-rewritten-definitions
                 :for entry := (gethash name return-entries)
                 :when entry
                   :collect (make-raw-binding
                             name
                             node
                             entry
                             return-entries
                             env)))
         (argument-raw-bindings
           (loop :for (name . node) :in rewritten-definitions
                 :for entry := (gethash name argument-entries)
                 :when entry
                   :collect (make-raw-argument-binding
                             name
                             node
                             entry
                             argument-entries
                             return-entries
                             combined-entries
                             env)))
         (combined-raw-bindings
           (loop :for (name . node) :in return-rewritten-definitions
                 :for entry := (gethash name combined-entries)
                 :when entry
                   :collect (make-raw-combined-binding
                             name
                             node
                             entry
                             argument-entries
                             return-entries
                             combined-entries
                             env)))
         (wrapped-definitions
           (loop :for (name . node) :in rewritten-definitions
                 :for entry := (gethash name return-entries)
                 :collect (if entry
                              (make-wrapper-binding name node entry)
                              (cons name node)))))
    (values (append return-raw-bindings
                    argument-raw-bindings
                    combined-raw-bindings
                    wrapped-definitions)
            return-entries
            argument-entries
            combined-entries)))
