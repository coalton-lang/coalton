;;;
;;; Multiple values optimization for Tuple
;;;

(defpackage #:coalton-impl/codegen/multiple-values
  (:use
   #:cl
   #:coalton-impl/codegen/ast
   #:coalton-impl/codegen/pattern)
  (:import-from
   #:coalton-impl/codegen/codegen-type-definition
   #:constructor-slot-name)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:util #:coalton-impl/util))
  (:export
   #:transform-tuples-to-multiple-values))

(in-package #:coalton-impl/codegen/multiple-values)

;; This pass rewrites Tuple-producing and Tuple-consuming code into
;; multiple-value calling conventions when possible.
;;
;; The pass tracks two pieces of state while transforming:
;; - MV map: function names that have an unboxed %VALUES variant.
;; - Values-var map: lexically bound tuple variables currently represented
;;   as a set of component variables.

(defun fst-symbol ()
  "Return the runtime symbol for `coalton:fst`."
  (let* ((pkg (cl:find-package "COALTON/TUPLE"))
         (sym (and pkg (cl:find-symbol "FST" pkg))))
    (when (and pkg (null sym))
      (util:coalton-bug "Unable to find symbol with name ~A in package ~A"
                        "FST" pkg))
    sym))

(defun snd-symbol ()
  "Return the runtime symbol for `coalton:snd`."
  (let* ((pkg (cl:find-package "COALTON/TUPLE"))
         (sym (and pkg (cl:find-symbol "SND" pkg))))
    (when (and pkg (null sym))
      (util:coalton-bug "Unable to find symbol with name ~A in package ~A"
                        "SND" pkg))
    sym))

(defun tuple-constructor-entry (env)
  "Look up the Tuple constructor metadata in ENV."
  (declare (type tc:environment env)
           (values (or null tc:constructor-entry) &optional))
  (alexandria:when-let ((tuple-sym (tc:tuple-symbol)))
    (tc:lookup-constructor env tuple-sym :no-error t)))

(defun tuple-accessor-symbol (env idx)
  "Return the generated struct accessor symbol for Tuple slot IDX."
  (declare (type tc:environment env)
           (type (integer 0 1) idx)
           (values (or null symbol) &optional))
  (let ((ctor (tuple-constructor-entry env)))
    (when ctor
      (let* ((classname (tc:constructor-entry-classname ctor))
             (slot (constructor-slot-name ctor idx)))
        (alexandria:format-symbol (symbol-package classname)
                                  "~A-~A"
                                  classname
                                  slot)))))

(defun projection-call-index (node env)
  "Return projected Tuple index for NODE, or NIL when NODE is not a projection."
  (declare (type node node)
           (type tc:environment env)
           (values (or null (integer 0 1)) &optional))
  (when (node-direct-application-p node)
    (let ((rator (node-direct-application-rator node))
          (fst (fst-symbol))
          (snd (snd-symbol)))
      (cond
        ((and fst (eq rator fst)) 0)
        ((and snd (eq rator snd)) 1)
        ((eq rator (tuple-accessor-symbol env 0)) 0)
        ((eq rator (tuple-accessor-symbol env 1)) 1)
        (t nil)))))

(defun mv-unboxed-name (name &key local)
  "Return symbol for NAME's unboxed variant (suffixed with `%VALUES`)."
  (if local
      (gensym (format nil "~A%VALUES" (symbol-name name)))
      (alexandria:format-symbol (symbol-package name) "~A%VALUES" (symbol-name name))))

(defun abstraction-return-type (node)
  "Return NODE's final return type by peeling off argument arrows."
  (declare (type node-abstraction node)
           (values tc:ty &optional))
  (let ((type (node-type node)))
    (dolist (_ (node-abstraction-vars node))
      (declare (ignore _))
      (unless (tc:function-type-p type)
        (util:coalton-bug "Expected function type for abstraction, got ~S" type))
      (setf type (tc:function-type-to type)))
    type))

(defun tuple-returning-abstraction-p (node)
  "Return true when NODE is an abstraction returning a Tuple."
  (and (node-abstraction-p node)
       (tc:tuple-type-p (abstraction-return-type node))))

(defun abstraction-argument-types (node)
  "Return NODE's argument types in source order."
  (declare (type node-abstraction node)
           (values tc:ty-list &optional))
  (let ((type (node-type node))
        (arg-types nil))
    (dolist (_ (node-abstraction-vars node))
      (declare (ignore _))
      (unless (tc:function-type-p type)
        (util:coalton-bug "Expected function type for abstraction, got ~S" type))
      (push (tc:function-type-from type) arg-types)
      (setf type (tc:function-type-to type)))
    (nreverse arg-types)))

(defun abstraction-arg-conventions (node)
  "Compute calling convention for each argument of NODE."
  (declare (type node-abstraction node)
           (values list &optional))
  (loop :for arg-type :in (abstraction-argument-types node)
        :collect (if (tc:tuple-type-p arg-type) ':values ':boxed)))

(defun argument-conventions->types (arg-types arg-conventions)
  "Expand ARG-TYPES according to ARG-CONVENTIONS for unboxed call sites."
  (declare (type tc:ty-list arg-types)
           (type list arg-conventions)
           (values tc:ty-list &optional))
  (unless (= (length arg-types) (length arg-conventions))
    (util:coalton-bug "Argument convention arity mismatch: ~D vs ~D"
                      (length arg-types)
                      (length arg-conventions)))
  (loop :for arg-type :in arg-types
        :for convention :in arg-conventions
        :append
          (ecase convention
            (:boxed
             (list arg-type))
            (:values
             (alexandria:if-let ((components (tc:tuple-component-types arg-type)))
               components
               (util:coalton-bug
                "Expected Tuple argument for :values convention, got ~S"
                arg-type))))))

(defstruct (mv-entry
            (:constructor make-mv-entry
                (boxed-name unboxed-name arg-conventions unboxed-type return-convention)))
  ;; Original (boxed) function name.
  (boxed-name nil :type symbol :read-only t)
  ;; Generated function name implementing unboxed calling convention.
  (unboxed-name nil :type symbol :read-only t)
  ;; Per-argument convention for calls into the unboxed function.
  (arg-conventions nil :type list :read-only t)
  ;; Type of the generated unboxed function.
  (unboxed-type nil :type tc:ty :read-only t)
  ;; Whether the unboxed variant returns boxed Tuple or multiple values.
  (return-convention ':boxed :type (member :boxed :values) :read-only t))

(defun abstraction-needs-mv-entry-p (node)
  "Return true when NODE benefits from an unboxed `%VALUES` sibling."
  (declare (type node-abstraction node)
           (values boolean &optional))
  (or (tuple-returning-abstraction-p node)
      (and (member ':values (abstraction-arg-conventions node))
           t)))

(defun make-mv-entry-for-binding (name node &key local)
  "Create metadata describing NAME's unboxed variant, if needed."
  (declare (type symbol name)
           (type node-abstraction node)
           (values (or null mv-entry) &optional))
  (unless (abstraction-needs-mv-entry-p node)
    (return-from make-mv-entry-for-binding nil))
  (let* ((arg-types (abstraction-argument-types node))
         (arg-conventions (abstraction-arg-conventions node))
         (ret-type (abstraction-return-type node))
         (unboxed-arg-types (argument-conventions->types arg-types arg-conventions))
         (return-convention (if (tc:tuple-type-p ret-type) ':values ':boxed)))
    (make-mv-entry
     name
     (mv-unboxed-name name :local local)
     arg-conventions
     (tc:make-function-type* unboxed-arg-types ret-type)
     return-convention)))

(defun lookup-mv-entry-by-boxed-name (name mv-map)
  "Find mv-entry by original function NAME."
  (declare (type symbol name)
           (type list mv-map)
           (values (or null mv-entry) &optional))
  (cdr (assoc name mv-map :test #'eq)))

(defun lookup-mv-entry-by-unboxed-name (name mv-map)
  "Find mv-entry by generated unboxed NAME."
  (declare (type symbol name)
           (type list mv-map)
           (values (or null mv-entry) &optional))
  (loop :for (_ . entry) :in mv-map
        :when (eq name (mv-entry-unboxed-name entry))
          :do (return entry)))

(defun unboxed-name-p (name mv-map)
  "Return true when NAME refers to an unboxed `%VALUES` entry point."
  (declare (type symbol name)
           (type list mv-map)
           (values boolean &optional))
  (or (and (lookup-mv-entry-by-unboxed-name name mv-map) t)
      (alexandria:ends-with-subseq "%VALUES" (symbol-name name) :test #'char=)))

(defstruct (values-var-entry
            (:constructor make-values-var-entry
                (name tuple-type component-types component-vars)))
  ;; Original lexical binding name with Tuple type.
  (name nil :type symbol :read-only t)
  ;; The Tuple type of NAME.
  (tuple-type nil :type tc:ty :read-only t)
  ;; Component types of TUPLE-TYPE in order.
  (component-types nil :type tc:ty-list :read-only t)
  ;; Lexical variables currently carrying the unboxed component values.
  (component-vars nil :type list :read-only t))

(defun lookup-values-var-entry (name values-var-map)
  "Find NAME in VALUES-VAR-MAP."
  (declare (type symbol name)
           (type list values-var-map)
           (values (or null values-var-entry) &optional))
  (find name values-var-map :key #'values-var-entry-name :test #'eq))

(defun remove-values-var-bindings (names values-var-map)
  "Drop entries shadowed by NAMES from VALUES-VAR-MAP."
  (declare (type list names)
           (type list values-var-map)
           (values list &optional))
  (remove-if (lambda (entry)
               (member (values-var-entry-name entry) names :test #'eq))
             values-var-map))

(defun values-var-entry-as-values (entry)
  "Materialize ENTRY as a node returning multiple values."
  (declare (type values-var-entry entry)
           (values node-values &optional))
  (make-node-values
   :type (values-var-entry-tuple-type entry)
   :nodes (loop :for var :in (values-var-entry-component-vars entry)
                :for type :in (values-var-entry-component-types entry)
                :collect (make-node-variable :type type :value var))))

(defun values-var-entry-as-boxed (entry)
  "Materialize ENTRY as an explicit Tuple constructor application."
  (declare (type values-var-entry entry)
           (values node-direct-application &optional))
  (let ((tuple-ctor (tc:tuple-symbol)))
    (unless tuple-ctor
      (util:coalton-bug "Unable to resolve Tuple constructor"))
    (make-node-direct-application
     :type (values-var-entry-tuple-type entry)
     :properties '()
     :rator-type (tc:make-function-type* (values-var-entry-component-types entry)
                                         (values-var-entry-tuple-type entry))
     :rator tuple-ctor
     :rands (loop :for var :in (values-var-entry-component-vars entry)
                  :for type :in (values-var-entry-component-types entry)
                  :collect (make-node-variable :type type :value var)))))

(defun make-tuple-accessor-call (env tuple-type idx tuple-node)
  "Build a direct call that projects IDX from TUPLE-NODE."
  (declare (type tc:environment env)
           (type tc:ty tuple-type)
           (type (integer 0 1) idx)
           (type node tuple-node)
           (values node-direct-application))
  (let* ((components (tc:tuple-component-types tuple-type))
         (component-type (nth idx components))
         (accessor (tuple-accessor-symbol env idx)))
    (unless accessor
      (util:coalton-bug "Unable to resolve Tuple accessor for index ~D" idx))
    (make-node-direct-application
     :type component-type
     :properties '()
     :rator-type (tc:make-function-type* (list tuple-type) component-type)
     :rator accessor
     :rands (list tuple-node))))

(defun unbox-tuple (env boxed-expr tuple-type)
  "Convert a boxed Tuple expression to a `node-values` expression."
  (declare (type tc:environment env)
           (type node boxed-expr)
           (type tc:ty tuple-type)
           (values node))
  (let* ((components (tc:tuple-component-types tuple-type))
         (tmp (gensym "TUPLE"))
         (tmp-node (make-node-variable :type tuple-type :value tmp))
         (accessors
           (loop :for idx :from 0
                 :for _ :in components
                 :collect (make-tuple-accessor-call env tuple-type idx tmp-node))))
    (make-node-bind
     :type tuple-type
     :name tmp
     :expr boxed-expr
     :body (make-node-values
            :type tuple-type
            :nodes accessors))))

(defun boxify-values (env values-expr tuple-type)
  "Convert a multiple-values expression back into an explicit Tuple."
  (declare (type tc:environment env)
           (type node values-expr)
           (type tc:ty tuple-type)
           (values node))
  (let* ((components (tc:tuple-component-types tuple-type))
         (vars (loop :for _ :in components :collect (gensym "MV")))
         (var-nodes (loop :for var :in vars
                          :for ty :in components
                          :collect (make-node-variable :type ty :value var)))
         (tuple-ctor (tc:tuple-symbol)))
    (unless tuple-ctor
      (util:coalton-bug "Unable to resolve Tuple constructor"))
    (make-node-values-bind
     :type tuple-type
     :vars vars
     :expr values-expr
     :body (make-node-direct-application
            :type tuple-type
            :properties '()
            :rator-type (tc:make-function-type* components tuple-type)
            :rator tuple-ctor
            :rands var-nodes))))

(defun make-boxed-wrapper (env node entry)
  "Create boxed facade for ENTRY that delegates to unboxed implementation."
  (declare (type tc:environment env)
           (type node-abstraction node)
           (type mv-entry entry)
           (values node-abstraction))
  (let* ((vars (node-abstraction-vars node))
         (arg-types (abstraction-argument-types node))
         (arg-conventions (mv-entry-arg-conventions entry))
         (ret-type (abstraction-return-type node))
         (value-bindings nil)
         (call-rands
           (loop :for var :in vars
                 :for arg-type :in arg-types
                 :for convention :in arg-conventions
                 :append
                   (ecase convention
                     (:boxed
                      (list (make-node-variable :type arg-type :value var)))
                     (:values
                      (alexandria:if-let ((components (tc:tuple-component-types arg-type)))
                        (let* ((component-vars (loop :for _ :in components
                                                     :collect (gensym "MVARG")))
                               (var-node (make-node-variable :type arg-type :value var)))
                          (push (list component-vars
                                      (unbox-tuple env var-node arg-type))
                                value-bindings)
                          (loop :for component-var :in component-vars
                                :for component-type :in components
                                :collect (make-node-variable :type component-type
                                                             :value component-var)))
                        (util:coalton-bug
                         "Expected Tuple argument for :values convention, got ~S"
                         arg-type)))))))
    (let ((call (make-node-direct-application
                 :type ret-type
                 :properties '()
                 :rator-type (mv-entry-unboxed-type entry)
                 :rator (mv-entry-unboxed-name entry)
                 :rands call-rands)))
      (dolist (binding (reverse value-bindings))
        (destructuring-bind (component-vars value-expr) binding
          (setf call
                (make-node-values-bind
                 :type (node-type call)
                 :vars component-vars
                 :expr value-expr
                 :body call))))
      (when (eq ':values (mv-entry-return-convention entry))
        (setf call (boxify-values env call ret-type)))
      (make-node-abstraction
       :type (node-type node)
       :vars vars
       :return-convention ':boxed
       :subexpr call))))

(defun match-values-eligible-p (patterns env)
  "Return true when PATTERNS can safely match unboxed Tuple values."
  (declare (type branch-list patterns)
           (type tc:environment env)
           (values boolean &optional))
  (declare (ignore env))
  (let ((tuple-sym (tc:tuple-symbol)))
    (when tuple-sym
      (every (lambda (pattern)
               (or (pattern-wildcard-p pattern)
                   (pattern-var-p pattern)
                   (and (pattern-constructor-p pattern)
                        (tc:tuple-component-types (pattern-type pattern))
                        (= 2 (length (pattern-constructor-patterns pattern))))))
             (mapcar #'match-branch-pattern patterns)))))

(defun transform-tuples-to-multiple-values (bindings env)
  "Rewrite BINDINGS to introduce Tuple multiple-value calling conventions.

The transform runs in three broad phases:
1. Discover candidate functions and assign `%VALUES` entry points.
2. Rewrite function definitions and call sites to use boxed/unboxed variants.
3. Insert explicit boxing/unboxing only at boundaries where representation changes."
  (declare (type binding-list bindings)
           (type tc:environment env)
           (values binding-list &optional))

  (let ((tuple-sym (tc:tuple-symbol)))
    (unless tuple-sym
      (return-from transform-tuples-to-multiple-values bindings))

    (labels
        (;; Rewrite one direct call through ENTRY's unboxed calling convention.
         (transform-direct-call-to-unboxed (node desired mv-map values-var-map entry)
           (let* ((arg-types (tc:function-type-arguments
                              (node-direct-application-rator-type node)))
                  (arg-conventions (mv-entry-arg-conventions entry)))
             (unless (= (length arg-types) (length arg-conventions))
               (util:coalton-bug
                "Direct call arity mismatch for ~A: ~D args, ~D conventions"
                (node-direct-application-rator node)
                (length arg-types)
                (length arg-conventions)))
             (let* ((plans
                      (loop :for rand :in (node-direct-application-rands node)
                            :for arg-type :in arg-types
                            :for convention :in arg-conventions
                            :collect
                              (ecase convention
                                (:boxed
                                 (let ((tmp (gensym "ARG")))
                                   (list ':boxed
                                         tmp
                                         (transform rand ':boxed mv-map values-var-map)
                                         arg-type)))
                                (:values
                                 (alexandria:if-let ((components (tc:tuple-component-types arg-type)))
                                   (list ':values
                                         (loop :for _ :in components :collect (gensym "MVARG"))
                                         (transform rand ':values mv-map values-var-map)
                                         components)
                                   (util:coalton-bug
                                    "Expected Tuple argument for :values convention, got ~S"
                                    arg-type))))))
                    (call-rands
                      (loop :for plan :in plans
                            :append
                              (ecase (first plan)
                                (:boxed
                                 (destructuring-bind (_i1 var _i2 arg-type) plan
                                   (declare (ignore _i1 _i2))
                                   (list (make-node-variable :type arg-type :value var))))
                                (:values
                                 (destructuring-bind (_i1 vars _i2 components) plan
                                   (declare (ignore _i1 _i2))
                                   (loop :for var :in vars
                                         :for type :in components
                                         :collect (make-node-variable :type type :value var)))))))
                    (call
                      (make-node-direct-application
                       :type (node-type node)
                       :properties (node-properties node)
                       :rator-type (mv-entry-unboxed-type entry)
                       :rator (mv-entry-unboxed-name entry)
                       :rands call-rands)))
               (dolist (plan (reverse plans))
                 (setf call
                       (ecase (first plan)
                         (:boxed
                          (destructuring-bind (_ var expr _arg-type) plan
                            (declare (ignore _ _arg-type))
                            (make-node-bind
                             :type (node-type call)
                             :name var
                             :expr expr
                             :body call)))
                         (:values
                          (destructuring-bind (_ vars expr _components) plan
                            (declare (ignore _ _components))
                            (make-node-values-bind
                             :type (node-type call)
                             :vars vars
                             :expr expr
                             :body call))))))
               (if (eq ':values (mv-entry-return-convention entry))
                   (if (eq ':values desired)
                       (make-node-mv-call
                        :type (node-type node)
                        :expr call)
                       (boxify-values env call (node-type node)))
                   (if (and (eq ':values desired)
                            (tc:tuple-type-p (node-type node)))
                       (unbox-tuple env call (node-type node))
                       call)))))

         ;; Core recursive node transform.
         (transform (node desired mv-map values-var-map)
           (typecase node
             (node-literal
              (if (and (eq ':values desired) (tc:tuple-type-p (node-type node)))
                  (unbox-tuple env node (node-type node))
                  node))

             (node-variable
              (alexandria:if-let ((entry (lookup-values-var-entry
                                          (node-variable-value node)
                                          values-var-map)))
                (if (eq ':values desired)
                    (values-var-entry-as-values entry)
                    (values-var-entry-as-boxed entry))
                (if (and (eq ':values desired) (tc:tuple-type-p (node-type node)))
                    (unbox-tuple env node (node-type node))
                    node)))

             (node-lisp
              (let ((tuple-type (node-type node))
                    (convention (node-lisp-return-convention node)))
                (cond
                  ((eq ':values convention)
                   (if (eq ':boxed desired)
                       (boxify-values env node tuple-type)
                       node))
                  ((and (eq ':values desired)
                        (tc:tuple-type-p tuple-type))
                   (unbox-tuple env node tuple-type))
                  (t
                   node))))

             (node-direct-application
              (let ((projection-rewrite
                      (let ((proj-idx (projection-call-index node env)))
                        (when proj-idx
                          ;; Rewrite direct Tuple projections to bind
                          ;; the unboxed components once, then pick INDEX.
                          (let* ((arg (first (node-direct-application-rands node)))
                                 (arg-type (node-type arg))
                                 (components (tc:tuple-component-types arg-type)))
                            (when components
                              (let* ((values-expr (transform arg ':values mv-map values-var-map))
                                     (vars (loop :for _ :in components
                                                 :collect (gensym "MV")))
                                     (body (make-node-variable
                                            :type (nth proj-idx components)
                                            :value (nth proj-idx vars))))
                                (make-node-values-bind
                                 :type (node-type node)
                                 :vars vars
                                 :expr values-expr
                                 :body body))))))))
                (if projection-rewrite
                    projection-rewrite
                    (let* ((rator (node-direct-application-rator node))
                           (entry (lookup-mv-entry-by-boxed-name rator mv-map))
                           (use-unboxed
                             (and entry
                                  (or (eq ':values (mv-entry-return-convention entry))
                                      (member ':values (mv-entry-arg-conventions entry))))))
                      (cond
                        ;; Tuple constructor in values-position can be emitted
                        ;; directly as `node-values`.
                        ((and (eq ':values desired) (tc:tuple-type-p (node-type node))
                              (eq rator tuple-sym))
                         (make-node-values
                          :type (node-type node)
                          :nodes (mapcar (lambda (sub)
                                           (transform sub ':boxed mv-map values-var-map))
                                         (node-direct-application-rands node))))

                        ;; Prefer dedicated unboxed `%VALUES` call path when
                        ;; callsite or return convention needs it.
                        (use-unboxed
                         (transform-direct-call-to-unboxed
                          node desired mv-map values-var-map entry))

                        ;; Unknown direct callee but values are desired:
                        ;; keep call boxed then unbox at boundary.
                        ((and (eq ':values desired) (tc:tuple-type-p (node-type node)))
                         (let* ((unboxedp (unboxed-name-p rator mv-map))
                                (call (make-node-direct-application
                                       :type (node-type node)
                                       :properties (node-properties node)
                                       :rator-type (node-direct-application-rator-type node)
                                       :rator rator
                                       :rands (mapcar
                                               (lambda (sub)
                                                 (transform sub ':boxed mv-map values-var-map))
                                               (node-direct-application-rands node)))))
                           (if unboxedp
                               (make-node-mv-call
                                :type (node-type node)
                               :expr call)
                               (unbox-tuple env call (node-type node)))))

                        ;; Box the result if desired representation is boxed
                        ;; and we are calling an unboxed `%VALUES` symbol.
                        ((and (eq ':boxed desired) (tc:tuple-type-p (node-type node))
                              (unboxed-name-p rator mv-map))
                         (let ((call (make-node-direct-application
                                      :type (node-type node)
                                      :properties (node-properties node)
                                      :rator-type (node-direct-application-rator-type node)
                                      :rator rator
                                      :rands (mapcar
                                              (lambda (sub)
                                                (transform sub ':boxed mv-map values-var-map))
                                              (node-direct-application-rands node)))))
                           (boxify-values env call (node-type node))))

                        (t
                         (make-node-direct-application
                          :type (node-type node)
                          :properties (node-properties node)
                          :rator-type (node-direct-application-rator-type node)
                          :rator rator
                          :rands (mapcar (lambda (sub)
                                           (transform sub ':boxed mv-map values-var-map))
                                         (node-direct-application-rands node)))))))))

             (node-application
              (cond
                ((and (eq ':values desired) (tc:tuple-type-p (node-type node)))
                 (unbox-tuple env
                              (make-node-application
                               :type (node-type node)
                               :properties (node-properties node)
                               :rator (transform (node-application-rator node)
                                                 ':boxed
                                                 mv-map
                                                 values-var-map)
                               :rands (mapcar
                                       (lambda (sub)
                                         (transform sub ':boxed mv-map values-var-map))
                                       (node-application-rands node)))
                              (node-type node)))
                (t
                 (make-node-application
                  :type (node-type node)
                  :properties (node-properties node)
                  :rator (transform (node-application-rator node)
                                    ':boxed
                                    mv-map
                                    values-var-map)
                  :rands (mapcar
                          (lambda (sub)
                            (transform sub ':boxed mv-map values-var-map))
                          (node-application-rands node))))))

             (node-let
              (let* ((bound-names (mapcar #'car (node-let-bindings node)))
                     (values-var-map* (remove-values-var-bindings bound-names values-var-map))
                     (local-mv-map
                       (loop :for (name . bnode) :in (node-let-bindings node)
                             :for entry := (and (node-abstraction-p bnode)
                                                (make-mv-entry-for-binding name bnode :local t))
                             :when entry
                               :collect (cons name entry)))
                     (mv-map* (append local-mv-map mv-map))
                     (new-bindings
                       (mapcan
                        (lambda (binding)
                          (destructuring-bind (name . bnode) binding
                            (cond
                              ((node-abstraction-p bnode)
                               (alexandria:if-let ((entry (lookup-mv-entry-by-boxed-name
                                                           name mv-map*)))
                                 (list
                                  (cons name (make-boxed-wrapper env bnode entry))
                                  (cons (mv-entry-unboxed-name entry)
                                        (transform-abstraction
                                         bnode
                                         (mv-entry-return-convention entry)
                                         mv-map*
                                         values-var-map*
                                         entry)))
                                 (list
                                 (cons name
                                        (transform-abstraction
                                         bnode
                                         ':boxed
                                         mv-map*
                                         values-var-map*)))))
                              (t
                               (list (cons name
                                           (transform bnode
                                                      ':boxed
                                                      mv-map*
                                                      values-var-map*)))))))
                        (node-let-bindings node)))
                     (new-subexpr (transform (node-let-subexpr node)
                                             desired
                                             mv-map*
                                             values-var-map*)))
                (make-node-let
                 :type (node-type node)
                 :bindings new-bindings
                 :subexpr new-subexpr)))

             (node-bind
              (let* ((name (node-bind-name node))
                     (expr (node-bind-expr node))
                     (body-values-var-map
                       (remove-values-var-bindings (list name) values-var-map)))
                (cond
                  ((node-abstraction-p expr)
                   (alexandria:if-let ((entry (make-mv-entry-for-binding name expr :local t)))
                     (let ((mv-map* (acons name entry mv-map)))
                       (make-node-bind
                        :type (node-type node)
                        :name (mv-entry-unboxed-name entry)
                        :expr (transform-abstraction
                               expr
                               (mv-entry-return-convention entry)
                               mv-map*
                               values-var-map
                               entry)
                        :body (make-node-bind
                               :type (node-type node)
                               :name name
                               :expr (make-boxed-wrapper env expr entry)
                               :body (transform (node-bind-body node)
                                                desired
                                                mv-map*
                                                body-values-var-map))))
                     (make-node-bind
                      :type (node-type node)
                      :name name
                      :expr (transform-abstraction expr ':boxed mv-map values-var-map)
                      :body (transform (node-bind-body node)
                                       desired
                                       mv-map
                                       body-values-var-map))))
                  (t
                   (make-node-bind
                    :type (node-type node)
                    :name name
                    :expr (transform expr ':boxed mv-map values-var-map)
                    :body (transform (node-bind-body node)
                                     desired
                                     mv-map
                                     body-values-var-map))))))

             (node-match
              (let* ((branches
                       (mapcar
                        (lambda (branch)
                          (let* ((pattern (match-branch-pattern branch))
                                 (branch-values-var-map
                                   (remove-values-var-bindings
                                    (pattern-variables pattern)
                                    values-var-map)))
                            (make-match-branch
                             :pattern pattern
                             :body (transform (match-branch-body branch)
                                              desired
                                              mv-map
                                              branch-values-var-map))))
                        (node-match-branches node)))
                     (can-values?
                       (and (tc:tuple-type-p (node-type (node-match-expr node)))
                            (match-values-eligible-p (node-match-branches node) env))))
                (if can-values?
                    (make-node-values-match
                     :type (node-type node)
                     :expr (transform (node-match-expr node)
                                      ':values
                                      mv-map
                                      values-var-map)
                     :branches branches)
                    (make-node-match
                     :type (node-type node)
                     :expr (transform (node-match-expr node)
                                      ':boxed
                                      mv-map
                                      values-var-map)
                     :branches branches))))

             (node-values-match
              (make-node-values-match
               :type (node-type node)
               :expr (transform (node-values-match-expr node)
                                ':values
                                mv-map
                                values-var-map)
               :branches (mapcar
                          (lambda (branch)
                            (let* ((pattern (match-branch-pattern branch))
                                   (branch-values-var-map
                                     (remove-values-var-bindings
                                      (pattern-variables pattern)
                                      values-var-map)))
                              (make-match-branch
                               :pattern pattern
                               :body (transform (match-branch-body branch)
                                                desired
                                                mv-map
                                                branch-values-var-map))))
                          (node-values-match-branches node))))

             (node-catch
              (make-node-catch
               :type (node-type node)
               :expr (transform (node-catch-expr node)
                                desired
                                mv-map
                                values-var-map)
               :branches (mapcar
                          (lambda (branch)
                            (let* ((pattern (catch-branch-pattern branch))
                                   (branch-values-var-map
                                     (remove-values-var-bindings
                                      (pattern-variables pattern)
                                      values-var-map)))
                              (make-catch-branch
                               :pattern pattern
                               :body (transform (catch-branch-body branch)
                                                desired
                                                mv-map
                                                branch-values-var-map))))
                          (node-catch-branches node))))

             (node-resumable
              (make-node-resumable
               :type (node-type node)
               :expr (transform (node-resumable-expr node)
                                desired
                                mv-map
                                values-var-map)
               :branches (mapcar
                          (lambda (branch)
                            (let* ((pattern (resumable-branch-pattern branch))
                                   (branch-values-var-map
                                     (remove-values-var-bindings
                                      (pattern-variables pattern)
                                      values-var-map)))
                              (make-resumable-branch
                               :pattern pattern
                               :body (transform (resumable-branch-body branch)
                                                desired
                                                mv-map
                                                branch-values-var-map))))
                          (node-resumable-branches node))))

             (node-seq
              (let* ((nodes (node-seq-nodes node))
                     (count (length nodes))
                     (new-nodes
                       (loop :for subnode :in nodes
                             :for idx :from 0
                             :collect (if (= idx (1- count))
                                          (transform subnode
                                                     desired
                                                     mv-map
                                                     values-var-map)
                                         (transform subnode
                                                     ':boxed
                                                     mv-map
                                                     values-var-map)))))
                (make-node-seq
                 :type (node-type node)
                 :nodes new-nodes)))

             (node-return-from
              (make-node-return-from
               :type (node-type node)
               :name (node-return-from-name node)
               :expr (transform (node-return-from-expr node)
                                desired
                                mv-map
                                values-var-map)))

             (node-block
              (make-node-block
               :type (node-type node)
               :name (node-block-name node)
               :body (transform (node-block-body node)
                                desired
                                mv-map
                                values-var-map)))

             (node-locally
              (make-node-locally
               :type (node-type node)
               :noinline-functions (node-locally-noinline-functions node)
               :subexpr (transform (node-locally-subexpr node)
                                   desired
                                   mv-map
                                   values-var-map)))

             (node-while
              (make-node-while
               :type (node-type node)
               :label (node-while-label node)
               :expr (transform (node-while-expr node)
                                ':boxed
                                mv-map
                                values-var-map)
               :body (transform (node-while-body node)
                                ':boxed
                                mv-map
                                values-var-map)))

             (node-while-let
              (let ((body-values-var-map
                      (remove-values-var-bindings
                       (pattern-variables (node-while-let-pattern node))
                       values-var-map)))
                (make-node-while-let
                 :type (node-type node)
                 :label (node-while-let-label node)
                 :pattern (node-while-let-pattern node)
                 :expr (transform (node-while-let-expr node)
                                  ':boxed
                                  mv-map
                                  values-var-map)
                 :body (transform (node-while-let-body node)
                                  ':boxed
                                  mv-map
                                  body-values-var-map))))

             (node-loop
              (make-node-loop
               :type (node-type node)
               :label (node-loop-label node)
               :body (transform (node-loop-body node)
                                ':boxed
                                mv-map
                                values-var-map)))

             (node-throw
              (make-node-throw
               :type (node-type node)
               :expr (transform (node-throw-expr node)
                                ':boxed
                                mv-map
                                values-var-map)))

             (node-resume-to
              (make-node-resume-to
               :type (node-type node)
               :expr (transform (node-resume-to-expr node)
                                ':boxed
                                mv-map
                                values-var-map)))

             (node-field
              (make-node-field
               :type (node-type node)
               :name (node-field-name node)
               :dict (transform (node-field-dict node)
                                ':boxed
                                mv-map
                                values-var-map)))

             (node-dynamic-extent
              (let ((body-values-var-map
                      (remove-values-var-bindings
                       (list (node-dynamic-extent-name node))
                       values-var-map)))
                (make-node-dynamic-extent
                 :type (node-type node)
                 :name (node-dynamic-extent-name node)
                 :node (transform (node-dynamic-extent-node node)
                                  ':boxed
                                  mv-map
                                  values-var-map)
                 :body (transform (node-dynamic-extent-body node)
                                  desired
                                  mv-map
                                  body-values-var-map))))

             (node-values
              (if (eq ':boxed desired)
                  (boxify-values env
                                 (make-node-values
                                  :type (node-type node)
                                  :nodes (mapcar
                                          (lambda (sub)
                                            (transform sub ':boxed mv-map values-var-map))
                                          (node-values-nodes node)))
                                 (node-type node))
                  (make-node-values
                   :type (node-type node)
                   :nodes (mapcar
                           (lambda (sub)
                             (transform sub ':boxed mv-map values-var-map))
                           (node-values-nodes node)))))

             (node-mv-call
              (if (eq ':boxed desired)
                  (boxify-values env
                                 (make-node-mv-call
                                  :type (node-type node)
                                  :expr (transform (node-mv-call-expr node)
                                                   ':boxed
                                                   mv-map
                                                   values-var-map))
                                 (node-type node))
                  (make-node-mv-call
                   :type (node-type node)
                   :expr (transform (node-mv-call-expr node)
                                    ':boxed
                                    mv-map
                                    values-var-map))))

             (node-values-bind
              (let ((body-values-var-map
                      (remove-values-var-bindings
                       (node-values-bind-vars node)
                       values-var-map)))
                (make-node-values-bind
                 :type (node-type node)
                 :vars (node-values-bind-vars node)
                 :expr (transform (node-values-bind-expr node)
                                  ':values
                                  mv-map
                                  values-var-map)
                 :body (transform (node-values-bind-body node)
                                  desired
                                  mv-map
                                  body-values-var-map))))

             (node-abstraction
              (transform-abstraction node ':boxed mv-map values-var-map))

             (t
              node)))

         ;; Guard requested representation with the node's actual type.
         (transform-expr (node desired mv-map values-var-map)
           (let ((desired (if (and (eq ':values desired)
                                   (null (tc:tuple-component-types (node-type node))))
                              ':boxed
                              desired)))
             (transform node desired mv-map values-var-map)))

         ;; Rewrite abstraction arguments/results according to conventions.
         (transform-abstraction (node desired mv-map values-var-map &optional entry)
           (let* ((ret-type (abstraction-return-type node))
                  (desired (if (and (eq ':values desired)
                                    (tc:tuple-type-p ret-type))
                               ':values
                               ':boxed))
                  (vars (node-abstraction-vars node))
                  (arg-types (abstraction-argument-types node))
                  (arg-conventions
                    (if entry
                        (mv-entry-arg-conventions entry)
                        (loop :for _ :in vars :collect ':boxed))))
             (unless (= (length arg-types) (length arg-conventions))
               (util:coalton-bug
                "Abstraction convention arity mismatch: ~D args, ~D conventions"
                (length arg-types)
                (length arg-conventions)))
             (let ((outer-values-var-map
                     (remove-values-var-bindings vars values-var-map))
                   (expanded-vars nil)
                   (expanded-arg-types nil)
                   (local-values-var-map nil))
               (loop :for var :in vars
                     :for arg-type :in arg-types
                     :for convention :in arg-conventions
                     :do
                        (ecase convention
                          (:boxed
                           (setf expanded-vars (append expanded-vars (list var))
                                 expanded-arg-types (append expanded-arg-types (list arg-type))))
                          (:values
                           (alexandria:if-let ((components (tc:tuple-component-types arg-type)))
                             (let ((component-vars
                                     (loop :for _ :in components :collect (gensym "MVARG"))))
                               (setf expanded-vars (append expanded-vars component-vars)
                                     expanded-arg-types (append expanded-arg-types components))
                               (push (make-values-var-entry
                                      var
                                      arg-type
                                      components
                                      component-vars)
                                     local-values-var-map))
                             (util:coalton-bug
                              "Expected Tuple argument for :values convention, got ~S"
                              arg-type)))))
                (make-node-abstraction
                 :type (tc:make-function-type* expanded-arg-types ret-type)
                 :vars expanded-vars
                 :return-convention (if (eq ':values desired) ':values ':boxed)
                 :subexpr (transform-expr (node-abstraction-subexpr node)
                                          desired
                                          mv-map
                                          (append
                                           (nreverse local-values-var-map)
                                           outer-values-var-map))))))

         ;; Rewrite a top-level binding; abstraction bindings may split in two.
         (transform-binding (binding mv-map)
           (destructuring-bind (name . node) binding
             (cond
               ((node-abstraction-p node)
                (alexandria:if-let ((entry (lookup-mv-entry-by-boxed-name name mv-map)))
                  (list
                   (cons name (make-boxed-wrapper env node entry))
                   (cons (mv-entry-unboxed-name entry)
                         (transform-abstraction
                          node
                          (mv-entry-return-convention entry)
                          mv-map
                          nil
                          entry)))
                  (list
                   (cons name
                         (transform-abstraction node ':boxed mv-map nil)))))
               (t
                (list (cons name (transform-expr node ':boxed mv-map nil)))))))

         ;; Build initial table of globally discoverable `%VALUES` siblings.
         (compute-global-mv-map (bindings)
           (loop :for (name . node) :in bindings
                 :for entry := (and (node-abstraction-p node)
                                    (make-mv-entry-for-binding name node))
                 :when entry
                   :collect (cons name entry))))

      (let* ((global-mv-map (compute-global-mv-map bindings))
             (new-bindings (mapcan (lambda (binding)
                                     (transform-binding binding global-mv-map))
                                   bindings)))
        new-bindings))))
