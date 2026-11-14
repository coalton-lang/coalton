(defpackage #:coalton-impl/codegen/scalar-replacement
  (:documentation
   "Scalar replacement eliminates locally constructed immutable structs and
algebraic data types used only through known field readers or pattern matching.
It binds constructor arguments (and, for branching sum-type initializers, an
unboxed constructor tag) to typed scalar locals and replaces reader calls and
matches with those locals, exposing the concrete field types to the Lisp
compiler without allocating or boxing the enclosing object.

The optimizer runs this pass in release mode after inlining and direct-call
conversion. Constructor arguments still evaluate exactly once, in order,
including unused fields. Recursive bindings and objects passed to remaining
calls, returned, stored, captured by closures, or exposed to Lisp retain their
allocation. The pass does not change function calling conventions.")
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
   #:traverse)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:node-free-p)
  (:local-nicknames
   (#:rt #:coalton-impl/runtime)
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

(defun scalar-replace-binding (name expr body env)
  "Replace a nonrecursive binding of a fresh struct or ADT, or return NIL.

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
    (t
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
           (scalar-replace-branching-binding name expr body env))))))

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
         (alexandria:when-let ((replacement
                                (scalar-replace-binding
                                 (node-bind-name node) (node-bind-expr node)
                                 (node-bind-body node) env)))
           (incf count)
           replacement))
       (action (:after node-dynamic-extent node)
         (alexandria:when-let ((replacement
                                (scalar-replace-binding
                                 (node-dynamic-extent-name node)
                                 (node-dynamic-extent-node node)
                                 (node-dynamic-extent-body node) env)))
           (incf count)
           replacement))
       (action (:after node-let node)
         (multiple-value-bind (replacement removed) (scalar-replace-let node env)
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
matching.

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
