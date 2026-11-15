(defpackage #:coalton-impl/codegen/calling-convention
  (:documentation
   "First-class calling convention and multi-entrypoint architecture for Coalton.

Supports multiple coordinated entrypoints per function:
  - :LISP     Canonical boxed Common Lisp calling convention (first-class values,
              higher-order calls, keyword arguments, REPL/FFI interop).
  - :UNPACKED Flattened product parameters and multi-value sum/product returns
              over standard Common Lisp DEFUNs.
  - :UNBOXED  SBCL register-specialized worker ABI using SB-INT:SPECIALIZED-XEP
              and block compilation so U64, I64, F64, Complex, and SAP values
              remain unboxed in machine registers across call boundaries.")
  (:use
   #:cl
   #:coalton-impl/codegen/pattern
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
   #:*traverse*
   #:traverse)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:node-variables
   #:node-free-p
   #:rename-type-variables)
  (:import-from
   #:coalton-impl/codegen/typecheck-node
   #:explicit-nullary-callable-p
   #:typecheck-node)
  (:import-from
   #:coalton-impl/codegen/codegen-expression
   #:abstraction-lambda-list
   #:annotate-function-body
   #:codegen-expression
   #:function-declarations
   #:node-output-values-type)
  (:import-from
   #:coalton-impl/codegen/scalar-replacement
   #:default-node-for-type
   #:constructed-adt
   #:non-returning-branch-node-p
   #:rewrite-non-returning-branch-node
   #:scalar-replace)
  (:import-from
   #:coalton-impl/codegen/lawnmower
   #:lawnmow)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:settings #:coalton-impl/settings)
   (#:tc #:coalton-impl/typechecker)
   (#:util #:coalton-impl/util))
  (:export
   #:*calling-convention-registry*
   #:*enable-unboxed-abi*
   #:calling-convention-descriptor
   #:make-calling-convention-descriptor
   #:calling-convention-descriptor-name
   #:calling-convention-descriptor-priority
   #:calling-convention-descriptor-worker-suffix
   #:calling-convention-descriptor-requires-composites-p
   #:calling-convention-descriptor-exclude-from-block-compile-p
   #:calling-convention-descriptor-eligible-p
   #:calling-convention-descriptor-emit-pre-declarations-fn
   #:calling-convention-descriptor-emit-defun-fn
   #:calling-convention-descriptor-emit-stale-bridge-fn
   #:register-calling-convention
   #:find-calling-convention
   #:abi-layout-type
   #:abi-layout-leaf-types
   #:abi-layout-result-type
   #:abi-layout-identity-p
   #:compute-function-entrypoints
   #:refine-scc-function-entrypoints
   #:lookup-function-entrypoint
   #:best-function-entrypoint
   #:rewrite-callsite-entrypoints
   #:synthesize-worker-abstraction
   #:synthesize-wrapper-abstraction
   #:emit-standard-defun
   #:*current-block-compilation-names*
   #:emit-stale-entrypoint-stubs))

(in-package #:coalton-impl/codegen/calling-convention)

;;;
;;; Configuration & Limits
;;;

(defparameter *max-unpacked-inputs* 16
  "Maximum number of flattened positional parameters for an :UNPACKED/:UNBOXED entrypoint.")

(defparameter *max-unpacked-outputs* 16
  "Maximum number of flattened return values for an :UNPACKED/:UNBOXED entrypoint.")

(defparameter *max-unpack-depth* 1
  "Maximum nesting depth when unpacking product types (1 = unpack direct fields of top-level product arguments and returns).")

(defparameter *max-unpacked-sum-constructors* 8
  "Maximum number of constructors in a sum type eligible for multi-value return unpacking.")

;;;
;;; Calling Convention Registry
;;;
;;; Each calling convention is described by a `calling-convention-descriptor`
;;; registered in `*calling-convention-registry*`. This protocol decouples the
;;; portable calling-convention transformation pass (`calling-convention.lisp`
;;; and `program.lisp`) from compiler- or architecture-specific backends (such
;;; as `calling-convention-sbcl.lisp`).

(defun always-eligible-p (flat-input-types flat-output-types node env)
  "Default eligibility predicate returning T."
  (declare (ignore flat-input-types flat-output-types node env)
           (values boolean &optional))
  t)

(defun no-extra-forms (&rest args)
  "Default hook returning no additional forms."
  (declare (ignore args)
           (values list &optional))
  nil)

(defstruct calling-convention-descriptor
  "Protocol descriptor for a Coalton function calling convention."
  ;; Keyword identifying the convention (e.g., `:lisp`, `:unpacked`, `:unboxed`).
  (name                         (util:required 'name)          :type keyword          :read-only t)
  ;; Numeric priority used when selecting the best entrypoint at direct call sites
  ;; (higher numbers are preferred: `:lisp` = 10, `:unpacked` = 20, `:unboxed` = 30).
  (priority                     (util:required 'priority)      :type fixnum           :read-only t)
  ;; Suffix appended to the canonical function symbol to form the worker symbol
  ;; (e.g., `"%UNPACKED"`, `"%UNBOXED"`, or `NIL` for `:lisp`).
  (worker-suffix                (util:required 'worker-suffix) :type (or null string) :read-only t)
  ;; When true, this convention is only emitted for functions that have at least
  ;; one non-identity composite (`abi-product` or `abi-sum`) input or output layout.
  ;; True for `:unpacked`; false for `:unboxed` (which also optimizes pure scalar
  ;; functions such as `U64 * U64 -> U64`).
  (requires-composites-p        nil                            :type boolean          :read-only t)
  ;; When true, worker functions emitted under this convention must not be included
  ;; in `sb-ext:start-block` declarations (used by SBCL `:unboxed` workers because
  ;; SBCL's block compiler cannot merge tail-sets across specialized and standard XEPs).
  (exclude-from-block-compile-p nil                            :type boolean          :read-only t)
  ;; Function symbol `(f flat-input-types flat-output-types node env) -> boolean`
  ;; returning true when a function with the given flattened leaf input/output types
  ;; and AST node is eligible to generate a worker for this convention.
  (eligible-p                   'always-eligible-p             :type symbol           :read-only t)
  ;; Function symbol `(f worker-sym worker-node env) -> list-of-forms` returning
  ;; compile-time pre-declaration forms to emit before an SCC's definitions so
  ;; mutually recursive calls within the SCC can target the worker ABI.
  (emit-pre-declarations-fn     'no-extra-forms                :type symbol           :read-only t)
  ;; Function symbol `(f worker-sym worker-node env &key local-notinline-names) -> form`
  ;; that emits the top-level Lisp definition form for the primary worker entrypoint.
  (emit-defun-fn                (util:required 'emit-defun-fn) :type symbol           :read-only t)
  ;; Function symbol `(f old-worker-sym old-entrypoint-spec env) -> list-of-forms`
  ;; returning cleanup/bridge forms when a function is redefined with an incompatible
  ;; signature and `old-entrypoint-spec` is retired.
  (emit-stale-bridge-fn         'no-extra-forms                :type symbol           :read-only t))

(defvar *calling-convention-registry* (make-hash-table :test #'eq)
  "Registry mapping calling convention keywords to CALLING-CONVENTION-DESCRIPTORs.")

(defun register-calling-convention (descriptor)
  "Register DESCRIPTOR in `*calling-convention-registry*` under its convention name."
  (declare (type calling-convention-descriptor descriptor)
           (values calling-convention-descriptor &optional))
  (setf (gethash (calling-convention-descriptor-name descriptor)
                 *calling-convention-registry*)
        descriptor))

(defun find-calling-convention (name &key no-error)
  "Look up the CALLING-CONVENTION-DESCRIPTOR for keyword NAME."
  (declare (type keyword name)
           (values (or null calling-convention-descriptor) &optional))
  (or (gethash name *calling-convention-registry*)
      (unless no-error
        (util:coalton-bug "Unknown calling convention ~S" name))))

(defun worker-symbol (base-name convention)
  "Return the deterministic entrypoint symbol for BASE-NAME and CONVENTION."
  (declare (type symbol base-name)
           (type keyword convention)
           (values symbol &optional))
  (let* ((desc (find-calling-convention convention))
         (suffix (calling-convention-descriptor-worker-suffix desc)))
    (if (null suffix)
        base-name
        (alexandria:format-symbol (or (symbol-package base-name) *package*)
                                  "~A~A"
                                  (symbol-name base-name)
                                  suffix))))

;;;
;;; ABI Layout Queries & Substitution
;;;

(defun abi-layout-type (layout)
  "Return the logical Coalton type represented by LAYOUT."
  (declare (values tc:ty &optional))
  (etypecase layout
    (tc:abi-leaf (tc:abi-leaf-type layout))
    (tc:abi-product (tc:abi-product-type layout))
    (tc:abi-sum (tc:abi-sum-type layout))
    (tc:abi-multi-value (tc:abi-multi-value-type layout))))

(defun abi-layout-leaf-types (layout)
  "Return the flat list of physical leaf Coalton types for LAYOUT."
  (declare (values list &optional))
  (etypecase layout
    (tc:abi-leaf
     (list (tc:abi-leaf-type layout)))
    (tc:abi-product
     (mapcan #'abi-layout-leaf-types
             (copy-list (tc:abi-product-fields layout))))
    (tc:abi-sum
     (cons (tc:abi-sum-tag-type layout)
           (copy-list (tc:abi-sum-payload-types layout))))
    (tc:abi-multi-value
     (mapcan #'abi-layout-leaf-types
             (copy-list (tc:abi-multi-value-components layout))))))

(defun abi-layout-leaf-count (layout)
  (declare (values fixnum &optional))
  (length (abi-layout-leaf-types layout)))

(defun abi-layout-leaf-count-list (layouts)
  (declare (type list layouts)
           (values fixnum &optional))
  (loop :for l :in layouts :sum (abi-layout-leaf-count l)))

(defun abi-layout-result-type (output-layout)
  "Return the physical Coalton return type (or RESULT-TY) for OUTPUT-LAYOUT."
  (declare (values tc:ty &optional))
  (tc:output-types-result-type (abi-layout-leaf-types output-layout)))

(defun abi-layout-identity-p (layout)
  "Return true when LAYOUT requires no structural packing or unpacking."
  (declare (values boolean &optional))
  (etypecase layout
    (tc:abi-leaf t)
    (tc:abi-product nil)
    (tc:abi-sum nil)
    (tc:abi-multi-value
     (every #'tc:abi-leaf-p (tc:abi-multi-value-components layout)))))

(defun apply-substitution-to-abi-layout (subs layout)
  "Apply type substitution SUBS to LAYOUT without altering its structural shape."
  (etypecase layout
    (tc:abi-leaf
     (tc:make-abi-leaf
      :type (tc:apply-substitution subs (tc:abi-leaf-type layout))))
    (tc:abi-product
     (tc:make-abi-product
      :type (tc:apply-substitution subs (tc:abi-product-type layout))
      :constructor (tc:abi-product-constructor layout)
      :type-name (tc:abi-product-type-name layout)
      :fields (mapcar (lambda (f)
                        (apply-substitution-to-abi-layout subs f))
                      (tc:abi-product-fields layout))))
    (tc:abi-sum
     (tc:make-abi-sum
      :type (tc:apply-substitution subs (tc:abi-sum-type layout))
      :type-name (tc:abi-sum-type-name layout)
      :tag-type (tc:apply-substitution subs (tc:abi-sum-tag-type layout))
      :payload-types (tc:apply-substitution subs (tc:abi-sum-payload-types layout))
      :variants (mapcar (lambda (v)
                          (tc:make-abi-sum-variant
                           :constructor (tc:abi-sum-variant-constructor v)
                           :tag-value (tc:abi-sum-variant-tag-value v)
                           :field-layouts (mapcar (lambda (fl)
                                                    (apply-substitution-to-abi-layout subs fl))
                                                  (tc:abi-sum-variant-field-layouts v))
                           :slot-indices (tc:abi-sum-variant-slot-indices v)))
                        (tc:abi-sum-variants layout))))
    (tc:abi-multi-value
     (tc:make-abi-multi-value
      :type (tc:apply-substitution subs (tc:abi-multi-value-type layout))
      :components (mapcar (lambda (c)
                            (apply-substitution-to-abi-layout subs c))
                          (tc:abi-multi-value-components layout))))))

(defun abi-layout= (a b)
  "Return true when layouts A and B have identical structure and Coalton types."
  (declare (values boolean &optional))
  (cond
    ((and (tc:abi-leaf-p a) (tc:abi-leaf-p b))
     (tc:ty= (tc:abi-leaf-type a) (tc:abi-leaf-type b)))
    ((and (tc:abi-product-p a) (tc:abi-product-p b))
     (and (eq (tc:abi-product-constructor a) (tc:abi-product-constructor b))
          (eq (tc:abi-product-type-name a) (tc:abi-product-type-name b))
          (tc:ty= (tc:abi-product-type a) (tc:abi-product-type b))
          (= (length (tc:abi-product-fields a))
             (length (tc:abi-product-fields b)))
          (every #'abi-layout=
                 (tc:abi-product-fields a)
                 (tc:abi-product-fields b))))
    ((and (tc:abi-sum-p a) (tc:abi-sum-p b))
     (and (eq (tc:abi-sum-type-name a) (tc:abi-sum-type-name b))
          (tc:ty= (tc:abi-sum-type a) (tc:abi-sum-type b))
          (tc:ty= (tc:abi-sum-tag-type a) (tc:abi-sum-tag-type b))
          (= (length (tc:abi-sum-payload-types a))
             (length (tc:abi-sum-payload-types b)))
          (every #'tc:ty=
                 (tc:abi-sum-payload-types a)
                 (tc:abi-sum-payload-types b))
          (= (length (tc:abi-sum-variants a))
             (length (tc:abi-sum-variants b)))
          (every (lambda (va vb)
                   (and (eq (tc:abi-sum-variant-constructor va)
                            (tc:abi-sum-variant-constructor vb))
                        (equal (tc:abi-sum-variant-tag-value va)
                               (tc:abi-sum-variant-tag-value vb))
                        (equal (tc:abi-sum-variant-slot-indices va)
                               (tc:abi-sum-variant-slot-indices vb))
                        (= (length (tc:abi-sum-variant-field-layouts va))
                           (length (tc:abi-sum-variant-field-layouts vb)))
                        (every #'abi-layout=
                               (tc:abi-sum-variant-field-layouts va)
                               (tc:abi-sum-variant-field-layouts vb))))
                 (tc:abi-sum-variants a)
                 (tc:abi-sum-variants b))))
    ((and (tc:abi-multi-value-p a) (tc:abi-multi-value-p b))
     (and (= (length (tc:abi-multi-value-components a))
             (length (tc:abi-multi-value-components b)))
          (every #'abi-layout=
                 (tc:abi-multi-value-components a)
                 (tc:abi-multi-value-components b))))
    (t nil)))

;;;
;;; ABI Layout Derivation
;;;

(defun instantiated-constructor-field-types (ctor-name target-type env)
  "Return the instantiated field types of CTOR-NAME when constructing TARGET-TYPE."
  (declare (type symbol ctor-name)
           (type tc:ty target-type)
           (type tc:environment env)
           (values list boolean &optional))
  (let ((scheme (tc:lookup-value-type env ctor-name :no-error t)))
    (unless scheme
      (return-from instantiated-constructor-field-types (values nil nil)))
    (let* ((fresh-qual (tc:fresh-inst scheme))
           (ctor-ty (tc:qualified-ty-type fresh-qual)))
      (handler-case
          (if (tc:function-type-p ctor-ty)
              (let ((subs (tc:match (tc:function-return-type ctor-ty) target-type)))
                (values (tc:apply-substitution subs (tc:function-type-arguments ctor-ty))
                        t))
              (progn
                (tc:match ctor-ty target-type)
                (values nil t)))
        (error ()
          (values nil nil))))))

(defun eligible-product-type-info (ty env &optional seen-type-names)
  "If TY is an eligible single-constructor product type, return
(VALUES CTOR-NAME TYPE-NAME FIELD-TYPES)."
  (declare (type tc:ty ty)
           (type tc:environment env)
           (type list seen-type-names)
           (values (or null symbol) (or null symbol) list &optional))
  (when (typep ty '(or tc:tyvar tc:tgen tc:function-ty tc:result-ty))
    (return-from eligible-product-type-info (values nil nil nil)))
  (let* ((base (tc:base-type ty))
         (tcon-name (and (tc:tycon-p base) (tc:tycon-name base)))
         (entry (and tcon-name
                     (not (member tcon-name seen-type-names :test #'eq))
                     (tc:lookup-type env tcon-name :no-error t))))
    (when (and entry
               (= 1 (length (tc:type-entry-constructors entry)))
               (not (tc:type-entry-newtype entry))
               (not (tc:type-entry-enum-repr entry))
               (not (tc:type-entry-exception-p entry))
               (not (tc:type-entry-resumption-p entry))
               (null (tc:type-entry-explicit-repr entry))
               (eq (tc:lisp-type ty env) (tc:type-entry-runtime-type entry)))
      (let* ((ctor-name (first (tc:type-entry-constructors entry)))
             (ctor (and ctor-name (tc:lookup-constructor env ctor-name :no-error t))))
        (when (and ctor (plusp (tc:constructor-entry-arity ctor)))
          (multiple-value-bind (field-types ok?)
              (instantiated-constructor-field-types ctor-name ty env)
            (when (and ok?
                       (= (length field-types) (tc:constructor-entry-arity ctor)))
              (values ctor-name tcon-name field-types))))))))

(defun compute-type-product-layout (ty env &optional (depth 0) seen-type-names)
  "Compute the recursive product ABI layout for TY."
  (declare (type tc:ty ty)
           (type tc:environment env)
           (type fixnum depth)
           (type list seen-type-names))
  (when (>= depth *max-unpack-depth*)
    (return-from compute-type-product-layout (tc:make-abi-leaf :type ty)))
  (multiple-value-bind (ctor-name type-name field-types)
      (eligible-product-type-info ty env seen-type-names)
    (if (null ctor-name)
        (tc:make-abi-leaf :type ty)
        (let* ((next-seen (cons type-name seen-type-names))
               (field-layouts
                 (mapcar (lambda (ft)
                           (compute-type-product-layout ft env (1+ depth) next-seen))
                         field-types)))
          (if (<= (abi-layout-leaf-count-list field-layouts) *max-unpacked-inputs*)
              (tc:make-abi-product
               :type ty
               :constructor ctor-name
               :type-name type-name
               :fields field-layouts)
              (tc:make-abi-leaf :type ty))))))

(defun compute-sum-abi-layout (ty env)
  "If TY is an eligible sum type with concrete payload leaves that all have
zero-cost default values, return its ABI-SUM layout; otherwise return NIL."
  (declare (type tc:ty ty)
           (type tc:environment env))
  (when (typep ty '(or tc:tyvar tc:tgen tc:function-ty tc:result-ty))
    (return-from compute-sum-abi-layout nil))
  (let* ((base (tc:base-type ty))
         (tcon-name (and (tc:tycon-p base) (tc:tycon-name base)))
         (entry (and tcon-name (tc:lookup-type env tcon-name :no-error t))))
    (unless (and entry
                 (>= (length (tc:type-entry-constructors entry)) 2)
                 (<= (length (tc:type-entry-constructors entry)) *max-unpacked-sum-constructors*)
                 (not (tc:type-entry-newtype entry))
                 (not (tc:type-entry-enum-repr entry))
                 (not (tc:type-entry-exception-p entry))
                 (not (tc:type-entry-resumption-p entry))
                 (or (and (null (tc:type-entry-explicit-repr entry))
                          (eq (tc:lisp-type ty env) (tc:type-entry-runtime-type entry)))
                     (eq tcon-name 'coalton:Optional)))
      (return-from compute-sum-abi-layout nil))
    (let* ((ctors (tc:type-entry-constructors entry))
           (boolean-tag-p (= 2 (length ctors)))
           (tag-type (if boolean-tag-p tc:*boolean-type* tc:*ufix-type*))
           (payload-types nil)
           (variants nil))
      (loop :for ctor-name :in ctors
            :for ctor-idx :from 0
            :for ctor-entry := (tc:lookup-constructor env ctor-name :no-error t)
            :do (unless ctor-entry
                  (return-from compute-sum-abi-layout nil))
                (multiple-value-bind (field-types ok?)
                    (instantiated-constructor-field-types ctor-name ty env)
                  (unless (and ok?
                               (= (length field-types)
                                  (tc:constructor-entry-arity ctor-entry)))
                    (return-from compute-sum-abi-layout nil))
                  (let* ((field-layouts
                           (mapcar (lambda (ft)
                                     (compute-type-product-layout
                                      ft env 0 (list tcon-name)))
                                   field-types))
                         (leaf-types
                           (mapcan #'abi-layout-leaf-types
                                   (copy-list field-layouts)))
                         (used-indices nil)
                         (slot-indices nil))
                    ;; Every payload leaf must be concrete (no free tyvars/tgens)
                    ;; and have a pure zero-cost default node.
                    (dolist (lt leaf-types)
                      (when (or (tc:type-variables lt)
                                (tc:ty= lt ty)
                                (null (default-node-for-type lt env)))
                        (return-from compute-sum-abi-layout nil))
                      (let ((existing-idx
                              (loop :for pt :in payload-types
                                    :for i :from 0
                                    :when (and (not (member i used-indices :test #'eql))
                                               (tc:ty= lt pt))
                                      :do (return i))))
                        (if existing-idx
                            (progn
                              (push existing-idx used-indices)
                              (push existing-idx slot-indices))
                            (let ((new-idx (length payload-types)))
                              (setf payload-types (append payload-types (list lt)))
                              (push new-idx used-indices)
                              (push new-idx slot-indices)))))
                    (push (tc:make-abi-sum-variant
                           :constructor ctor-name
                           :tag-value (if boolean-tag-p
                                          (if (zerop ctor-idx)
                                              'coalton:True
                                              'coalton:False)
                                          ctor-idx)
                           :field-layouts field-layouts
                           :slot-indices (nreverse slot-indices))
                          variants))))
      (when (<= (1+ (length payload-types)) *max-unpacked-outputs*)
        (tc:make-abi-sum
         :type ty
         :type-name tcon-name
         :tag-type tag-type
         :payload-types payload-types
         :variants (nreverse variants))))))

(defun compute-input-abi-layouts (input-types env &optional forced-leaf-indices)
  "Compute the list of ABI layouts for positional INPUT-TYPES."
  (declare (type list input-types forced-leaf-indices)
           (type tc:environment env)
           (values list &optional))
  (let ((layouts (loop :for ty :in input-types
                       :for idx :from 0
                       :collect (if (member idx forced-leaf-indices :test #'eql)
                                    (tc:make-abi-leaf :type ty)
                                    (compute-type-product-layout ty env 0)))))
    (if (<= (abi-layout-leaf-count-list layouts) *max-unpacked-inputs*)
        layouts
        (mapcar (lambda (ty) (tc:make-abi-leaf :type ty)) input-types))))

(defun compute-output-abi-layout (return-type env)
  "Compute the ABI layout for RETURN-TYPE."
  (declare (type tc:ty return-type)
           (type tc:environment env))
  (if (typep return-type 'tc:result-ty)
      (let ((out-types (tc:result-ty-output-types return-type)))
        (if (null out-types)
            (tc:make-abi-multi-value :type return-type :components nil)
            (let ((comp-layouts
                    (mapcar (lambda (ct)
                              (compute-type-product-layout ct env 0))
                            out-types)))
              (if (<= (abi-layout-leaf-count-list comp-layouts) *max-unpacked-outputs*)
                  (tc:make-abi-multi-value
                   :type return-type
                   :components comp-layouts)
                  (tc:make-abi-multi-value
                   :type return-type
                   :components (mapcar (lambda (ct) (tc:make-abi-leaf :type ct))
                                       out-types))))))
      (or (compute-sum-abi-layout return-type env)
          (compute-type-product-layout return-type env 0))))

;;;
;;; Entrypoint Bundle Computation
;;;

(defvar *enable-unboxed-abi*
  (not (find (uiop:getenv "COALTON_DISABLE_UNBOXED_ABI")
             '("t" "true" "1")
             :test #'string-equal))
  "When true, allow generating compiler/architecture-specific `:UNBOXED` register
worker entrypoints when a backend for `:UNBOXED` is registered. When false (or on
hosts without an `:UNBOXED` backend), functions automatically use the portable
`:UNPACKED` and `:LISP` conventions.")

(defun sorted-worker-convention-descriptors ()
  "Return all registered non-`:LISP` calling convention descriptors sorted by ascending priority."
  (declare (values list &optional))
  (let ((descs nil))
    (maphash (lambda (name desc)
               (unless (eq name ':lisp)
                 (push desc descs)))
             *calling-convention-registry*)
    (sort descs #'< :key #'calling-convention-descriptor-priority)))

(defun function-eligible-for-specialized-entrypoints-p (name node env)
  "Return true when top-level function NAME with abstraction NODE can expose
specialized worker entrypoints."
  (declare (type symbol name)
           (type node node)
           (type tc:environment env)
           (values boolean &optional))
  (and (node-abstraction-p node)
       (not (util:dynamic-variable-name-p name))
       (plusp (length (node-abstraction-vars node)))
       (null (node-abstraction-keyword-params node))
       ;; Exclude data constructors, instance dictionary builders, and class
       ;; method wrappers or error intrinsics with whole-result tyvars.
       (null (tc:lookup-constructor env name :no-error t))
       (null (tc:lookup-instance-by-codegen-sym env name :no-error t))
       (let ((name-entry (tc:lookup-name env name :no-error t)))
         (not (and name-entry (eq :constructor (tc:name-entry-type name-entry)))))
       (let ((ret-ty (tc:function-return-type (node-type node))))
         (not (and (tc:tyvar-p ret-ty)
                   (tc:tyvar-allow-result-p ret-ty))))))

(defun compute-function-entrypoints (name node env &optional requested-convention forced-leaf-indices)
  "Compute `(VALUES PRIMARY-CONVENTION ENTRYPOINT-SPECS)` for function NAME."
  (declare (type symbol name)
           (type node node)
           (type tc:environment env)
           (type (or null keyword) requested-convention)
           (type list forced-leaf-indices)
           (values keyword list &optional))
  (let ((effective-mode
          (or requested-convention
              (if (settings:coalton-release-p) ':auto ':lisp))))
    (when (or (eq effective-mode ':lisp)
              (not (function-eligible-for-specialized-entrypoints-p name node env)))
      (return-from compute-function-entrypoints (values ':lisp nil)))

    (let* ((fn-ty (node-type node))
           (input-types (tc:function-type-arguments fn-ty))
           (return-type (tc:function-return-type fn-ty))
           (unpacked-inputs (compute-input-abi-layouts input-types env forced-leaf-indices))
           (unpacked-output (compute-output-abi-layout return-type env))
           (has-unpacked-composites-p
             (or (notevery #'abi-layout-identity-p unpacked-inputs)
                 (not (abi-layout-identity-p unpacked-output))))
           (leaf-inputs (mapcar (lambda (ty) (tc:make-abi-leaf :type ty)) input-types))
           (leaf-output (if (typep return-type 'tc:result-ty)
                            (tc:make-abi-multi-value
                             :type return-type
                             :components (mapcar (lambda (ct) (tc:make-abi-leaf :type ct))
                                                 (tc:result-ty-output-types return-type)))
                            (tc:make-abi-leaf :type return-type)))
           (chosen-inputs (if has-unpacked-composites-p unpacked-inputs leaf-inputs))
           (chosen-output (if has-unpacked-composites-p unpacked-output leaf-output))
           (flat-input-types (mapcan #'abi-layout-leaf-types (copy-list chosen-inputs)))
           (flat-output-types (abi-layout-leaf-types chosen-output))
           (flat-fn-type
             (tc:make-function-ty
              :positional-input-types flat-input-types
              :keyword-input-types nil
              :keyword-open-p nil
              :output-types flat-output-types))
           (requested-desc
             (and (not (eq effective-mode ':auto))
                  (find-calling-convention effective-mode :no-error t)))
           (max-priority
             (if requested-desc
                 (calling-convention-descriptor-priority requested-desc)
                 most-positive-fixnum))
           (entrypoints nil)
           (primary ':lisp))

      (dolist (desc (sorted-worker-convention-descriptors))
        (let ((conv-name (calling-convention-descriptor-name desc)))
          (when (and (<= (calling-convention-descriptor-priority desc) max-priority)
                     (or (not (calling-convention-descriptor-requires-composites-p desc))
                         has-unpacked-composites-p)
                     (funcall (calling-convention-descriptor-eligible-p desc)
                              flat-input-types
                              flat-output-types
                              node
                              env))
            (push (tc:make-entrypoint-spec
                   :convention conv-name
                   :symbol (worker-symbol name conv-name)
                   :arity (length flat-input-types)
                   :input-layouts chosen-inputs
                   :output-layout chosen-output
                   :function-type flat-fn-type)
                  entrypoints)
            (setf primary conv-name))))

      (values primary (nreverse entrypoints)))))

(defun lookup-function-entrypoint (entry convention)
  "Find the ENTRYPOINT-SPEC for CONVENTION in ENTRY, or NIL."
  (declare (type tc:function-env-entry entry)
           (type keyword convention)
           (values (or null tc:entrypoint-spec) &optional))
  (find convention (tc:function-env-entry-entrypoints entry)
        :key #'tc:entrypoint-spec-convention
        :test #'eq))

(defun best-function-entrypoint (entry)
  "Return the highest-priority specialized ENTRYPOINT-SPEC in ENTRY, or NIL if only :LISP."
  (declare (type tc:function-env-entry entry)
           (values (or null tc:entrypoint-spec) &optional))
  (let ((primary (tc:function-env-entry-primary-convention entry)))
    (unless (eq primary ':lisp)
      (or (lookup-function-entrypoint entry primary)
          (first (last (tc:function-env-entry-entrypoints entry)))))))

;;;
;;; AST Marshaling (Pack / Unpack) Builders
;;;

(defun force-value-node (node)
  (declare (type node node)
           (values node &optional))
  (if (node-abstraction-p node)
      (make-node-locally
       :type (node-type node)
       :noinline-functions nil
       :type-check nil
       :subexpr node)
      node))

(defun pure-atomic-node-p (node)
  "Return true when NODE is a side-effect-free variable or literal reference."
  (declare (type node node)
           (values boolean &optional))
  (typep node '(or node-local-variable node-literal)))

(defun build-pattern-from-abi-layout (layout)
  "Build an irrefutable PATTERN for unpacking LAYOUT, returning
`(VALUES PATTERN LEAF-VAR-NODES)`."
  (declare (values pattern list &optional))
  (etypecase layout
    (tc:abi-leaf
     (let* ((ty (tc:abi-leaf-type layout))
            (var (gensym "UNPACK-"))
            (var-node (make-node-local-variable :type ty :value var)))
       (values (make-pattern-var :type ty :name var)
               (list var-node))))
    (tc:abi-product
     (let ((subpatterns nil)
           (leaf-vars nil))
       (dolist (field-layout (tc:abi-product-fields layout))
         (multiple-value-bind (subpat subvars)
             (build-pattern-from-abi-layout field-layout)
           (push subpat subpatterns)
           (setf leaf-vars (append leaf-vars subvars))))
       (values (make-pattern-constructor
                :type (tc:abi-product-type layout)
                :name (tc:abi-product-constructor layout)
                :patterns (nreverse subpatterns))
               leaf-vars)))))

(defun build-unpack-inputs-ast (arg-nodes input-layouts body-builder)
  "Evaluate ARG-NODES left-to-right, unpack any ABI-PRODUCT layouts, and pass
the flattened leaf argument nodes to BODY-BUILDER."
  (declare (type list arg-nodes input-layouts)
           (type function body-builder)
           (values node &optional))
  (assert (= (length arg-nodes) (length input-layouts)))
  ;; Fast path: no product inputs to unpack.
  (when (every #'tc:abi-leaf-p input-layouts)
    (return-from build-unpack-inputs-ast
      (funcall body-builder
               (loop :for arg :in arg-nodes
                     :for layout :in input-layouts
                     :collect (copy-node arg (tc:abi-leaf-type layout))))))

  ;; General path: preserve strict left-to-right argument evaluation order.
  (let ((temp-bindings nil)
        (eval-vars nil))
    (loop :for arg :in arg-nodes
          :for layout :in input-layouts
          :for ty := (abi-layout-type layout)
          :do (if (node-local-variable-p arg)
                  (push (copy-node arg ty) eval-vars)
                  (let ((tmp (gensym "CALL-ARG-")))
                    (push (cons tmp (copy-node (force-value-node arg) ty))
                          temp-bindings)
                    (push (make-node-local-variable :type ty :value tmp)
                          eval-vars))))
    (setf temp-bindings (nreverse temp-bindings)
          eval-vars (nreverse eval-vars))

    (let ((product-matches nil)
          (flat-leaf-nodes nil))
      (loop :for eval-var :in eval-vars
            :for layout :in input-layouts
            :do (if (tc:abi-leaf-p layout)
                    (setf flat-leaf-nodes
                          (append flat-leaf-nodes
                                  (list (copy-node eval-var (tc:abi-leaf-type layout)))))
                    (multiple-value-bind (pat leaf-vars)
                        (build-pattern-from-abi-layout layout)
                      (push (cons eval-var pat) product-matches)
                      (setf flat-leaf-nodes (append flat-leaf-nodes leaf-vars)))))

      (let ((out (funcall body-builder flat-leaf-nodes)))
        (loop :for (scrutinee . pat) :in product-matches
              :do (setf out
                        (make-node-match
                         :type (node-type out)
                         :expr scrutinee
                         :branches (list (make-match-branch
                                          :pattern pat
                                          :body out)))))
        (loop :for (tmp . expr) :in (reverse temp-bindings)
              :do (setf out
                        (make-node-bind
                         :type (node-type out)
                         :name tmp
                         :expr expr
                         :body out)))
        out))))

(defun build-pack-from-vars-ast (leaf-var-nodes layout)
  "Construct a Coalton AST node of type `(abi-layout-type layout)` from
LEAF-VAR-NODES, returning `(VALUES PACKED-NODE REMAINING-VAR-NODES)`."
  (etypecase layout
    (tc:abi-leaf
     (values (copy-node (first leaf-var-nodes) (tc:abi-leaf-type layout))
             (rest leaf-var-nodes)))

    (tc:abi-product
     (let ((field-nodes nil)
           (rest-vars leaf-var-nodes))
       (dolist (field-layout (tc:abi-product-fields layout))
         (multiple-value-bind (field-node next-vars)
             (build-pack-from-vars-ast rest-vars field-layout)
           (push field-node field-nodes)
           (setf rest-vars next-vars)))
       (setf field-nodes (nreverse field-nodes))
       (let ((prod-ty (tc:abi-product-type layout))
             (ctor-name (tc:abi-product-constructor layout)))
         (values
          (make-node-direct-application
           :type prod-ty
           :properties nil
           :rator-type (tc:make-function-type* (mapcar #'node-type field-nodes) prod-ty)
           :rator ctor-name
           :rands field-nodes)
          rest-vars))))

    (tc:abi-sum
     (let* ((sum-ty (tc:abi-sum-type layout))
            (tag-ty (tc:abi-sum-tag-type layout))
            (payload-tys (tc:abi-sum-payload-types layout))
            (num-payload (length payload-tys))
            (tag-var (copy-node (first leaf-var-nodes) tag-ty))
            (payload-vars (subseq (rest leaf-var-nodes) 0 num-payload))
            (rest-vars (nthcdr (1+ num-payload) leaf-var-nodes))
            (variants (tc:abi-sum-variants layout))
            (variant-bodies
              (loop :for variant :in variants
                    :for ctor-name := (tc:abi-sum-variant-constructor variant)
                    :for variant-leaves
                      := (mapcar (lambda (idx) (nth idx payload-vars))
                                 (tc:abi-sum-variant-slot-indices variant))
                    :collect
                    (let ((ctor-args nil)
                          (rem variant-leaves))
                      (dolist (fl (tc:abi-sum-variant-field-layouts variant))
                        (multiple-value-bind (arg-node next-rem)
                            (build-pack-from-vars-ast rem fl)
                          (push arg-node ctor-args)
                          (setf rem next-rem)))
                      (setf ctor-args (nreverse ctor-args))
                      (if (null ctor-args)
                          (make-node-global-variable
                           :type sum-ty
                           :value ctor-name)
                          (make-node-direct-application
                           :type sum-ty
                           :properties nil
                           :rator-type (tc:make-function-type*
                                        (mapcar #'node-type ctor-args)
                                        sum-ty)
                           :rator ctor-name
                           :rands ctor-args))))))
       (values
        (if (tc:ty= tag-ty tc:*boolean-type*)
            (make-node-match
             :type sum-ty
             :expr tag-var
             :branches
             (list (make-match-branch
                    :pattern (make-pattern-constructor
                              :type tc:*boolean-type*
                              :name 'coalton:True
                              :patterns nil)
                    :body (first variant-bodies))
                   (make-match-branch
                    :pattern (make-pattern-constructor
                              :type tc:*boolean-type*
                              :name 'coalton:False
                              :patterns nil)
                    :body (second variant-bodies))))
            (make-node-match
             :type sum-ty
             :expr tag-var
             :branches
             (append
              (loop :for body :in (butlast variant-bodies)
                    :for idx :from 0
                    :collect (make-match-branch
                              :pattern (make-pattern-literal
                                        :type tc:*ufix-type*
                                        :value idx)
                              :body body))
              (list (make-match-branch
                     :pattern (make-pattern-wildcard :type tc:*ufix-type*)
                     :body (car (last variant-bodies)))))))
        rest-vars)))

    (tc:abi-multi-value
     (let ((comps (tc:abi-multi-value-components layout))
           (mv-ty (tc:abi-multi-value-type layout)))
       (if (null comps)
           (values (make-node-values :type mv-ty :nodes nil)
                   leaf-var-nodes)
           (let ((comp-nodes nil)
                 (rest-vars leaf-var-nodes))
             (dolist (comp-layout comps)
               (multiple-value-bind (comp-node next-vars)
                   (build-pack-from-vars-ast rest-vars comp-layout)
                 (push comp-node comp-nodes)
                 (setf rest-vars next-vars)))
             (values (make-node-values
                      :type mv-ty
                      :nodes (nreverse comp-nodes))
                     rest-vars)))))))

(defun wrap-call-with-output-pack (call-node output-layout)
  "Wrap worker CALL-NODE with an AST adapter that packs OUTPUT-LAYOUT back into
its logical Coalton return type."
  (declare (type node call-node))
  (when (abi-layout-identity-p output-layout)
    (return-from wrap-call-with-output-pack call-node))
  (let* ((leaf-types (abi-layout-leaf-types output-layout))
         (vars (loop :for _ :in leaf-types :collect (gensym "RET-")))
         (var-nodes (loop :for v :in vars
                          :for ty :in leaf-types
                          :collect (make-node-local-variable :type ty :value v)))
         (packed-node (nth-value 0 (build-pack-from-vars-ast var-nodes output-layout))))
    (if (= 1 (length leaf-types))
        (make-node-bind
         :type (abi-layout-type output-layout)
         :name (first vars)
         :expr call-node
         :body packed-node)
        (make-node-values-bind
         :type (abi-layout-type output-layout)
         :vars vars
         :expr call-node
         :body packed-node))))

;;;
;;; Worker Output Unmarshaling (Spine-Pushing & Tail-Call Preserving)
;;;

(defun make-output-values-node (leaf-nodes target-type)
  (declare (type list leaf-nodes)
           (type tc:ty target-type)
           (values node &optional))
  (cond
    ((null leaf-nodes)
     (make-node-values :type target-type :nodes nil))
    ((null (cdr leaf-nodes))
     (copy-node (force-value-node (first leaf-nodes)) target-type))
    (t
     (make-node-values
      :type target-type
      :nodes (mapcar #'force-value-node leaf-nodes)))))

(defun make-sum-tag-node (tag-type tag-value)
  (declare (type tc:ty tag-type)
           (values node &optional))
  (if (tc:ty= tag-type tc:*boolean-type*)
      (make-node-global-variable :type tc:*boolean-type* :value tag-value)
      (make-node-literal :type tc:*ufix-type* :value tag-value)))

(defun make-sum-variant-values-node (variant leaf-arg-nodes sum-layout target-type env)
  "Build the `(VALUES TAG SLOT_1 ... SLOT_K)` AST for VARIANT with LEAF-ARG-NODES."
  (declare (type tc:abi-sum-variant variant)
           (type list leaf-arg-nodes)
           (type tc:abi-sum sum-layout)
           (type tc:ty target-type)
           (type tc:environment env)
           (values node &optional))
  (let* ((tag-node (make-sum-tag-node (tc:abi-sum-tag-type sum-layout)
                                      (tc:abi-sum-variant-tag-value variant)))
         (payload-types (tc:abi-sum-payload-types sum-layout))
         (slot-indices (tc:abi-sum-variant-slot-indices variant)))
    (if (<= (length leaf-arg-nodes) 1)
        (let ((arg0 (and leaf-arg-nodes (force-value-node (first leaf-arg-nodes))))
              (idx0 (first slot-indices)))
          (make-node-values
           :type target-type
           :nodes (cons tag-node
                        (loop :for slot-ty :in payload-types
                              :for i :from 0
                              :collect (if (and idx0 (= i idx0))
                                           (copy-node arg0 slot-ty)
                                           (default-node-for-type slot-ty env))))))
        (let* ((temp-vars
                 (loop :for arg :in leaf-arg-nodes
                       :collect (make-node-local-variable
                                 :type (node-type arg)
                                 :value (gensym "SUM-ARG-"))))
               (idx-var-alist (mapcar #'cons slot-indices temp-vars))
               (values-node
                 (make-node-values
                  :type target-type
                  :nodes (cons tag-node
                               (loop :for slot-ty :in payload-types
                                     :for i :from 0
                                     :for temp := (cdr (assoc i idx-var-alist :test #'eql))
                                     :collect (if temp
                                                  (copy-node temp slot-ty)
                                                  (default-node-for-type slot-ty env)))))))
          (loop :with out := values-node
                :for temp :in (reverse temp-vars)
                :for arg :in (reverse leaf-arg-nodes)
                :do (setf out
                          (make-node-bind
                           :type target-type
                           :name (node-variable-value temp)
                           :expr (force-value-node arg)
                           :body out))
                :finally (return out))))))

(defun build-opaque-output-unpack-ast (expr output-layout target-type env)
  "Unpack an arbitrary expression EXPR of type `(abi-layout-type output-layout)`
into `target-type` via pattern matching."
  (declare (type node expr)
           (type tc:ty target-type)
           (type tc:environment env)
           (values node &optional))
  (etypecase output-layout
    (tc:abi-leaf
     (copy-node expr target-type))

    (tc:abi-product
     (multiple-value-bind (pat leaf-vars)
         (build-pattern-from-abi-layout output-layout)
       (make-node-match
        :type target-type
        :expr expr
        :branches (list (make-match-branch
                         :pattern pat
                         :body (make-output-values-node leaf-vars target-type))))))

    (tc:abi-sum
     (let ((branches
             (loop :for variant :in (tc:abi-sum-variants output-layout)
                   :collect
                   (let ((subpats nil)
                         (leaf-vars nil))
                     (dolist (fl (tc:abi-sum-variant-field-layouts variant))
                       (multiple-value-bind (subpat subvars)
                           (build-pattern-from-abi-layout fl)
                         (push subpat subpats)
                         (setf leaf-vars (append leaf-vars subvars))))
                     (make-match-branch
                      :pattern (make-pattern-constructor
                                :type (tc:abi-sum-type output-layout)
                                :name (tc:abi-sum-variant-constructor variant)
                                :patterns (nreverse subpats))
                      :body (make-sum-variant-values-node
                             variant leaf-vars output-layout target-type env))))))
       (make-node-match
        :type target-type
        :expr expr
        :branches branches)))

    (tc:abi-multi-value
     (let ((comps (tc:abi-multi-value-components output-layout)))
       (if (null comps)
           (make-node-seq
            :type target-type
            :nodes (list expr (make-node-values :type target-type :nodes nil)))
           (let* ((comp-vars (loop :for _ :in comps :collect (gensym "MV-COMP-")))
                  (comp-var-nodes
                    (loop :for v :in comp-vars
                          :for c :in comps
                          :collect (make-node-local-variable
                                    :type (abi-layout-type c)
                                    :value v)))
                  (all-leaves nil)
                  (matches nil))
             (loop :for cv :in comp-var-nodes
                   :for c :in comps
                   :do (if (tc:abi-leaf-p c)
                           (setf all-leaves (append all-leaves (list cv)))
                           (multiple-value-bind (pat lvars)
                               (build-pattern-from-abi-layout c)
                             (push (cons cv pat) matches)
                             (setf all-leaves (append all-leaves lvars)))))
             (let ((inner (make-output-values-node all-leaves target-type)))
               (loop :for (cv . pat) :in matches
                     :do (setf inner
                               (make-node-match
                                :type target-type
                                :expr cv
                                :branches (list (make-match-branch
                                                 :pattern pat
                                                 :body inner)))))
               (make-node-values-bind
                :type target-type
                :vars comp-vars
                :expr expr
                :body inner))))))))

(defun values-eta-identity-p (vars body-node)
  "Return true when BODY-NODE is `(node-values v1 ... vk)` with variables equal to VARS."
  (declare (type list vars)
           (type node body-node)
           (values boolean &optional))
  (and (node-values-p body-node)
       (= (length vars) (length (node-values-nodes body-node)))
       (every (lambda (var subnode)
                (and (node-local-variable-p subnode)
                     (eq var (node-variable-value subnode))))
              vars
              (node-values-nodes body-node))))

(defun sum-values-eta-identity-p (vars body-node sum-layout env)
  "Return true when BODY-NODE is a `node-match` on `(first vars)` that reconstructs
the exact `(values tag slot_1 ... slot_k)` tuples already bound by VARS."
  (declare (type list vars)
           (type node body-node)
           (type tc:abi-sum sum-layout)
           (type tc:environment env)
           (values boolean &optional))
  (let ((variants (tc:abi-sum-variants sum-layout))
        (payload-types (tc:abi-sum-payload-types sum-layout)))
    (and (= (length vars) (1+ (length payload-types)))
         (= 2 (length variants))
         (tc:ty= (tc:abi-sum-tag-type sum-layout) tc:*boolean-type*)
         (typep body-node 'node-match)
         (node-local-variable-p (node-match-expr body-node))
         (eq (first vars) (node-variable-value (node-match-expr body-node)))
         (= 2 (length (node-match-branches body-node)))
         (let ((b1 (first (node-match-branches body-node)))
               (b2 (second (node-match-branches body-node)))
               (payload-vars (rest vars)))
           (labels ((branch-matches-variant-p (branch variant expected-ctor-sym)
                      (let ((pat (match-branch-pattern branch))
                            (body (match-branch-body branch))
                            (slot-indices (tc:abi-sum-variant-slot-indices variant)))
                        (and (pattern-constructor-p pat)
                             (eq expected-ctor-sym (pattern-constructor-name pat))
                             (node-values-p body)
                             (= (length vars) (length (node-values-nodes body)))
                             (let ((tag-out (first (node-values-nodes body)))
                                   (slots-out (rest (node-values-nodes body))))
                               (and (node-global-variable-p tag-out)
                                    (eq expected-ctor-sym (node-variable-value tag-out))
                                    (loop :for out-node :in slots-out
                                          :for pvar :in payload-vars
                                          :for slot-ty :in payload-types
                                          :for idx :from 0
                                          :always
                                          (if (member idx slot-indices :test #'eql)
                                              (and (node-local-variable-p out-node)
                                                   (eq pvar (node-variable-value out-node)))
                                              (or (and (node-local-variable-p out-node)
                                                       (eq pvar (node-variable-value out-node)))
                                                  (let ((def (default-node-for-type slot-ty env)))
                                                    (and def (equalp (node-type def) (node-type out-node)))))))))))))
             (and (branch-matches-variant-p b1 (first variants) 'coalton:True)
                  (branch-matches-variant-p b2 (second variants) 'coalton:False)))))))

(defun rewrite-return-from-targets (node block-name output-layout env)
  "Rewrite all unshadowed `node-return-from` nodes targeting BLOCK-NAME in NODE
so their expressions unpack to OUTPUT-LAYOUT."
  (declare (type node node)
           (type symbol block-name)
           (type tc:environment env)
           (values node &optional))
  (traverse
   node
   (list
    (action (:traverse node-abstraction subnode)
      subnode)
    (action (:traverse node-block subnode)
      (if (eq block-name (node-block-name subnode))
          subnode
          (make-node-block
           :type (node-type subnode)
           :name (node-block-name subnode)
           :body (funcall *traverse* (node-block-body subnode)))))
    (action (:after node-return-from subnode)
      (when (eq block-name (node-return-from-name subnode))
        (make-node-return-from
         :type (node-type subnode)
         :name block-name
         :expr (unpack-output-node (node-return-from-expr subnode)
                                   output-layout
                                   env)))))))

(defun unpack-output-node (node output-layout env)
  "Push output unmarshaling for OUTPUT-LAYOUT down the returning spine of NODE,
preserving tail calls and eliminating constructor allocations at leaves."
  (declare (type node node)
           (type tc:environment env)
           (values node &optional))
  (when (abi-layout-identity-p output-layout)
    (return-from unpack-output-node node))

  (let ((target-type (abi-layout-result-type output-layout)))
    ;; 1. Non-returning control transfer leaf
    (when (non-returning-branch-node-p node)
      (return-from unpack-output-node
        (rewrite-non-returning-branch-node node target-type)))

    ;; 2. Direct constructor leaf for ABI-PRODUCT
    (when (and (tc:abi-product-p output-layout)
               (every #'tc:abi-leaf-p (tc:abi-product-fields output-layout)))
      (multiple-value-bind (ctor _type args) (constructed-adt node env)
        (declare (ignore _type))
        (when (and ctor
                   (eq (tc:constructor-entry-name ctor)
                       (tc:abi-product-constructor output-layout))
                   (= (length args) (length (tc:abi-product-fields output-layout))))
          (return-from unpack-output-node
            (make-output-values-node
             (loop :for arg :in args
                   :for fl :in (tc:abi-product-fields output-layout)
                   :collect (copy-node arg (tc:abi-leaf-type fl)))
             target-type)))))

    ;; 3. Direct constructor leaf for ABI-SUM
    (when (tc:abi-sum-p output-layout)
      (multiple-value-bind (ctor _type args) (constructed-adt node env)
        (declare (ignore _type))
        (when ctor
          (let ((variant (find (tc:constructor-entry-name ctor)
                               (tc:abi-sum-variants output-layout)
                               :key #'tc:abi-sum-variant-constructor
                               :test #'eq)))
            (when (and variant
                       (= (length args) (length (tc:abi-sum-variant-field-layouts variant))))
              (return-from unpack-output-node
                (build-unpack-inputs-ast
                 args
                 (tc:abi-sum-variant-field-layouts variant)
                 (lambda (flat-arg-nodes)
                   (make-sum-variant-values-node
                    variant
                    flat-arg-nodes
                    output-layout
                    target-type
                    env)))))))))

    ;; 4. Spine nodes
    (typecase node
      (node-bind
       (let ((unpacked-body
               (unpack-output-node (node-bind-body node) output-layout env)))
         ;; Cancel 1-value identity `(bind v expr v) -> expr`
         (if (and (node-local-variable-p unpacked-body)
                  (eq (node-bind-name node) (node-variable-value unpacked-body))
                  (tc:ty= (node-type (node-bind-expr node)) target-type)
                  (node-free-p (node-bind-expr node) (list (node-bind-name node))))
             (copy-node (node-bind-expr node) target-type)
             (make-node-bind
              :type target-type
              :name (node-bind-name node)
              :expr (node-bind-expr node)
              :body unpacked-body))))

      (node-let
       (make-node-let
        :type target-type
        :bindings (node-let-bindings node)
        :subexpr (unpack-output-node (node-let-subexpr node) output-layout env)))

      (node-seq
       (let ((nodes (node-seq-nodes node)))
         (if (null nodes)
             (build-opaque-output-unpack-ast node output-layout target-type env)
             (make-node-seq
              :type target-type
              :nodes (append (butlast nodes)
                             (list (unpack-output-node (car (last nodes))
                                                       output-layout
                                                       env)))))))

      (node-locally
       (make-node-locally
        :type target-type
        :noinline-functions (node-locally-noinline-functions node)
        :type-check (node-locally-type-check node)
        :subexpr (unpack-output-node (node-locally-subexpr node) output-layout env)))

      (node-values-bind
       (let ((unpacked-body
               (unpack-output-node (node-values-bind-body node) output-layout env))
             (vars (node-values-bind-vars node))
             (expr (node-values-bind-expr node)))
         ;; Cancel multi-value identity `(values-bind (v...) expr (values v...)) -> expr`
         ;; and unpacked sum identity in tail position!
         (if (and (tc:ty= (node-type expr) target-type)
                  (or (values-eta-identity-p vars unpacked-body)
                      (and (tc:abi-sum-p output-layout)
                           (sum-values-eta-identity-p vars unpacked-body output-layout env))))
             (copy-node expr target-type)
             (make-node-values-bind
              :type target-type
              :vars vars
              :expr expr
              :body unpacked-body))))

      (node-dynamic-let
       (make-node-dynamic-let
        :type target-type
        :bindings (node-dynamic-let-bindings node)
        :subexpr (unpack-output-node (node-dynamic-let-subexpr node) output-layout env)))

      (node-dynamic-extent
       (make-node-dynamic-extent
        :type target-type
        :name (node-dynamic-extent-name node)
        :node (node-dynamic-extent-node node)
        :body (unpack-output-node (node-dynamic-extent-body node) output-layout env)))

      (node-match
       (make-node-match
        :type target-type
        :expr (node-match-expr node)
        :branches (loop :for branch :in (node-match-branches node)
                        :collect (make-match-branch
                                  :pattern (match-branch-pattern branch)
                                  :body (unpack-output-node
                                         (match-branch-body branch)
                                         output-layout
                                         env)))))

      (node-block
       (let* ((block-name (node-block-name node))
              (rewritten-body
                (rewrite-return-from-targets
                 (node-block-body node)
                 block-name
                 output-layout
                 env)))
         (make-node-block
          :type target-type
          :name block-name
          :body (unpack-output-node rewritten-body output-layout env))))

      (t
       (build-opaque-output-unpack-ast node output-layout target-type env)))))

;;;
;;; Call-Site Rewriting & Worker/Wrapper Synthesis
;;;

(defun collect-layout-type-variables (layout)
  (etypecase layout
    (tc:abi-leaf
     (tc:type-variables (tc:abi-leaf-type layout)))
    (tc:abi-product
     (remove-duplicates
      (append (tc:type-variables (tc:abi-product-type layout))
              (mapcan #'collect-layout-type-variables
                      (copy-list (tc:abi-product-fields layout))))
      :test #'tc:ty=))
    (tc:abi-sum
     (remove-duplicates
      (append (tc:type-variables (tc:abi-sum-type layout))
              (tc:type-variables (tc:abi-sum-payload-types layout))
              (loop :for v :in (tc:abi-sum-variants layout)
                    :append (mapcan #'collect-layout-type-variables
                                    (copy-list (tc:abi-sum-variant-field-layouts v)))))
      :test #'tc:ty=))
    (tc:abi-multi-value
     (remove-duplicates
      (append (tc:type-variables (tc:abi-multi-value-type layout))
              (mapcan #'collect-layout-type-variables
                      (copy-list (tc:abi-multi-value-components layout))))
      :test #'tc:ty=))))

(defun rename-entrypoint-layouts (input-layouts output-layout)
  (let ((tyvars (remove-duplicates
                 (append (mapcan #'collect-layout-type-variables
                                 (copy-list input-layouts))
                         (collect-layout-type-variables output-layout))
                 :test #'tc:ty=)))
    (if (null tyvars)
        (values input-layouts output-layout)
        (let ((subs (mapcar (lambda (tv)
                              (tc:make-substitution :from tv :to (tc:fresh-type-renamer tv)))
                            tyvars)))
          (values (mapcar (lambda (l) (apply-substitution-to-abi-layout subs l))
                          input-layouts)
                  (apply-substitution-to-abi-layout subs output-layout))))))

(defun instantiate-entrypoint-for-callsite (spec call-node)
  "Instantiate SPEC's input and output ABI layouts and function type for CALL-NODE."
  (declare (type tc:entrypoint-spec spec)
           (type node-direct-application call-node))
  (multiple-value-bind (renamed-inputs renamed-output)
      (rename-entrypoint-layouts (tc:entrypoint-spec-input-layouts spec)
                                 (tc:entrypoint-spec-output-layout spec))
    (handler-case
        (let ((subs nil))
          (loop :for layout :in renamed-inputs
                :for rand :in (node-direct-application-rands call-node)
                :do (setf subs (tc:unify subs (abi-layout-type layout) (node-type rand))))
          (setf subs (tc:unify subs
                               (abi-layout-type renamed-output)
                               (node-type call-node)))
          (let* ((inst-inputs
                   (mapcar (lambda (l) (apply-substitution-to-abi-layout subs l))
                           renamed-inputs))
                 (inst-output
                   (apply-substitution-to-abi-layout subs renamed-output))
                 (flat-in-tys
                   (mapcan #'abi-layout-leaf-types (copy-list inst-inputs)))
                 (flat-out-tys
                   (abi-layout-leaf-types inst-output))
                 (inst-fn-ty
                   (tc:make-function-ty
                    :positional-input-types flat-in-tys
                    :keyword-input-types nil
                    :keyword-open-p nil
                    :output-types flat-out-tys)))
            (values inst-inputs inst-output inst-fn-ty t)))
      (error ()
        (values nil nil nil nil)))))

(defun rewrite-direct-application-to-entrypoint (call-node spec)
  "Rewrite `node-direct-application` CALL-NODE to invoke SPEC."
  (declare (type node-direct-application call-node)
           (type tc:entrypoint-spec spec)
           (values node &optional))
  (multiple-value-bind (inst-inputs inst-output inst-fn-ty ok?)
      (instantiate-entrypoint-for-callsite spec call-node)
    (unless ok?
      (return-from rewrite-direct-application-to-entrypoint call-node))
    ;; Fast path: when all inputs and output are identity (pure :unboxed scalar call),
    ;; rewrite the rator in-place on the node-direct-application.
    (when (and (every #'tc:abi-leaf-p inst-inputs)
               (abi-layout-identity-p inst-output))
      (return-from rewrite-direct-application-to-entrypoint
        (make-node-direct-application
         :type (node-type call-node)
         :properties (node-direct-application-properties call-node)
         :rator-type inst-fn-ty
         :rator (tc:entrypoint-spec-symbol spec)
         :rands (node-direct-application-rands call-node)
         :keyword-rands nil)))

    (let ((worker-result-type (abi-layout-result-type inst-output)))
      (build-unpack-inputs-ast
       (node-direct-application-rands call-node)
       inst-inputs
       (lambda (flat-arg-nodes)
         (let ((worker-call
                 (make-node-direct-application
                  :type worker-result-type
                  :properties (node-direct-application-properties call-node)
                  :rator-type inst-fn-ty
                  :rator (tc:entrypoint-spec-symbol spec)
                  :rands flat-arg-nodes
                  :keyword-rands nil)))
           (wrap-call-with-output-pack worker-call inst-output)))))))

(defun rewrite-callsite-entrypoints (node env &key print-rewrites)
  "Rewrite direct applications in NODE to call the best available worker
entrypoint in ENV."
  (declare (type node node)
           (type tc:environment env)
           (type boolean print-rewrites)
           (values node &optional))
  (traverse
   node
   (list
    (action (:after node-direct-application call-node)
      (when (null (node-direct-application-keyword-rands call-node))
        (let* ((rator (node-direct-application-rator call-node))
               (entry (tc:lookup-function env rator :no-error t))
               (spec (and entry (best-function-entrypoint entry))))
          (when (and spec
                     (= (length (node-direct-application-rands call-node))
                        (length (tc:entrypoint-spec-input-layouts spec)))
                     ;; Do not rewrite if the residual call is an explicit-nullary
                     ;; function value auto-invoked by codegen-expression.
                     (let ((residual-type
                             (tc:function-remove-arguments
                              (node-direct-application-rator-type call-node)
                              (length (node-direct-application-rands call-node)))))
                       (not (and (tc:function-type-p residual-type)
                                 (explicit-nullary-callable-p residual-type)
                                 (not (tc:function-type-p (node-type call-node)))))))
            (when print-rewrites
              (format t "~&;; Calling ~S via ~S entrypoint ~S~%"
                      rator
                      (tc:entrypoint-spec-convention spec)
                      (tc:entrypoint-spec-symbol spec)))
            (rewrite-direct-application-to-entrypoint call-node spec))))))))

(defun synthesize-worker-abstraction (node spec env)
  "Synthesize the primary worker `node-abstraction` for SPEC from canonical
abstraction NODE.

For each positional parameter with a non-leaf input layout (e.g., `abi-product`),
this function:
  1. Replaces the single parameter variable `orig-var` with flattened leaf
     parameter variables `part-0 ... part-k`.
  2. Pushes `output-layout` down the returning spine of NODE's body via
     `unpack-output-node` so returning constructor leaves become direct
     `node-values` returns without allocating.
  3. Binds `orig-var` around the unpacked body to a synthetic constructor
     application `(Ctor part-0 ... part-k)` and runs `scalar-replace` to
     eliminate the synthetic constructor and rewrite field reads/pattern matches
     directly to `part-0 ... part-k`."
  (declare (type node-abstraction node)
           (type tc:entrypoint-spec spec)
           (type tc:environment env)
           (values node-abstraction &optional))
  (let* ((orig-vars (node-abstraction-vars node))
         (input-layouts (tc:entrypoint-spec-input-layouts spec))
         (output-layout (tc:entrypoint-spec-output-layout spec))
         (worker-fn-type (tc:entrypoint-spec-function-type spec))
         (worker-vars nil)
         (repack-bindings nil))
    (assert (= (length orig-vars) (length input-layouts)))
    (loop :for orig-var :in orig-vars
          :for layout :in input-layouts
          :do (if (tc:abi-leaf-p layout)
                  (push orig-var worker-vars)
                  (let* ((leaf-tys (abi-layout-leaf-types layout))
                         (part-vars (loop :for i :from 0 :below (length leaf-tys)
                                          :collect (gensym (format nil "~A-~D-"
                                                                   (symbol-name orig-var)
                                                                   i))))
                         (part-nodes (loop :for pv :in part-vars
                                           :for ty :in leaf-tys
                                           :collect (make-node-local-variable
                                                     :type ty
                                                     :value pv)))
                         (packed-expr (nth-value 0 (build-pack-from-vars-ast part-nodes layout))))
                    (dolist (pv part-vars)
                      (push pv worker-vars))
                    (push (cons orig-var packed-expr) repack-bindings))))
    (setf worker-vars (nreverse worker-vars)
          repack-bindings (nreverse repack-bindings))

    (let ((body (unpack-output-node (node-abstraction-subexpr node) output-layout env)))
      (loop :for (orig-var . packed-expr) :in (reverse repack-bindings)
            :do (setf body
                      (make-node-bind
                       :type (node-type body)
                       :name orig-var
                       :expr packed-expr
                       :body body)))
      (let* ((worker-abs
               (make-node-abstraction
                :type worker-fn-type
                :vars worker-vars
                :keyword-params nil
                :subexpr body))
             (replaced (if (or repack-bindings
                               (not (abi-layout-identity-p output-layout))
                               (settings:coalton-release-p))
                           (scalar-replace worker-abs env)
                           worker-abs)))
        replaced))))

(defun refine-scc-function-entrypoints (bindings inline-p-table env &optional calling-convention-table)
  "Compute and refine multi-entrypoint specifications for the functions in
BINDINGS to a fixed point so that no worker entrypoint unpacks an input
parameter whose synthetic repack binding would survive scalar replacement.

If a parameter `orig-var` remains referenced as a whole value inside a function
(for example, stored into a struct field, passed to a higher-order function,
or captured in the variable list of a `node-lisp` form), `scalar-replace` leaves
`orig-var` free in the tentative worker body. Demoting that parameter index to
`abi-leaf` prevents the worker from allocating a duplicate copy on the heap."
  (declare (type list bindings)
           (type hash-table inline-p-table)
           (type tc:environment env)
           (type (or null hash-table) calling-convention-table)
           (values tc:environment &optional))
  (let ((functions (loop :for (name . node) :in bindings
                         :when (and (node-abstraction-p node)
                                    (not (util:dynamic-variable-name-p name)))
                           :collect (cons name node)))
        (forced-leaf-table (make-hash-table :test #'eq)))
    (loop
      (let ((changed nil))
        ;; 1. Compute candidate entrypoints under current forced-leaf constraints.
        (loop :for (name . node) :in functions
              :for requested-conv := (and calling-convention-table
                                          (gethash name calling-convention-table))
              :for forced-leaves := (gethash name forced-leaf-table)
              :do (multiple-value-bind (primary-convention entrypoints)
                      (compute-function-entrypoints name node env requested-conv forced-leaves)
                    (setf env
                          (tc:set-function
                           env
                           name
                           (tc:make-function-env-entry
                            :name name
                            :arity (length (node-abstraction-vars node))
                            :inline-p (and (gethash name inline-p-table) t)
                            :primary-convention primary-convention
                            :entrypoints entrypoints)))))
        ;; 2. Synthesize tentative workers and demote any unpacked parameter whose
        ;; synthetic repack binding was not eliminated by scalar replacement.
        (loop :for (name . node) :in functions
              :for entry := (tc:lookup-function env name :no-error t)
              :for spec := (and entry (best-function-entrypoint entry))
              :when (and spec
                         (some (lambda (l) (not (tc:abi-leaf-p l)))
                               (tc:entrypoint-spec-input-layouts spec)))
                :do (let* ((rewritten-node
                             (lawnmow
                              (rewrite-callsite-entrypoints node env)
                              env))
                           (worker-abs
                             (synthesize-worker-abstraction rewritten-node spec env))
                           (worker-body
                             (node-abstraction-subexpr worker-abs)))
                      (loop :for orig-var :in (node-abstraction-vars node)
                            :for layout :in (tc:entrypoint-spec-input-layouts spec)
                            :for idx :from 0
                            :when (and (not (tc:abi-leaf-p layout))
                                       (not (node-free-p worker-body (list orig-var))))
                              :do (pushnew idx (gethash name forced-leaf-table))
                                  (setf changed t))))
        (unless changed
          (return))))
    env))

(defun synthesize-wrapper-abstraction (from-type from-input-layouts from-output-layout target-spec)
  "Synthesize a trampoline `node-abstraction` of type FROM-TYPE (with
FROM-INPUT-LAYOUTS and FROM-OUTPUT-LAYOUT) that unpacks its arguments, delegates
to TARGET-SPEC, and packs the return value.

Used both for the canonical `:lisp` entrypoint `f` (where FROM-INPUT-LAYOUTS and
FROM-OUTPUT-LAYOUT are leaf layouts) and for secondary worker entrypoints (such
as `f%UNPACKED` forwarding to `f%UNBOXED`)."
  (declare (type tc:ty from-type)
           (type list from-input-layouts)
           (type tc:entrypoint-spec target-spec)
           (values node-abstraction &optional))
  (let* ((from-in-tys (tc:function-type-arguments from-type))
         (vars (loop :for _ :in from-in-tys
                     :for i :from 0
                     :collect (gensym (format nil "WRAP-ARG-~D-" i))))
         (var-nodes (loop :for v :in vars
                          :for ty :in from-in-tys
                          :collect (make-node-local-variable :type ty :value v)))
         (target-fn-ty (tc:entrypoint-spec-function-type target-spec))
         (target-out-ty (abi-layout-result-type (tc:entrypoint-spec-output-layout target-spec))))
    (make-node-abstraction
     :type from-type
     :vars vars
     :keyword-params nil
     :subexpr
     (if (and (every #'abi-layout= from-input-layouts (tc:entrypoint-spec-input-layouts target-spec))
              (abi-layout= from-output-layout (tc:entrypoint-spec-output-layout target-spec)))
         ;; Identity forwarding (e.g., :UNPACKED wrapper delegating to :UNBOXED worker)
         (make-node-direct-application
          :type target-out-ty
          :properties nil
          :rator-type target-fn-ty
          :rator (tc:entrypoint-spec-symbol target-spec)
          :rands var-nodes
          :keyword-rands nil)
         ;; Unpack inputs, call target worker, and pack output back to FROM-TYPE's return
         (build-unpack-inputs-ast
          var-nodes
          (tc:entrypoint-spec-input-layouts target-spec)
          (lambda (flat-arg-nodes)
            (let ((worker-call
                    (make-node-direct-application
                     :type target-out-ty
                     :properties nil
                     :rator-type target-fn-ty
                     :rator (tc:entrypoint-spec-symbol target-spec)
                     :rands flat-arg-nodes
                     :keyword-rands nil)))
              (wrap-call-with-output-pack
               worker-call
               (tc:entrypoint-spec-output-layout target-spec)))))))))

;;;
;;; Code Emission for Multi-Entrypoint Functions & Stale Redefinition Stubs
;;;

(defun emit-standard-defun (name node env &key local-notinline-names)
  "Emit a portable Common Lisp `DEFUN` with parameter type declarations and an
annotated return body."
  (declare (type symbol name)
           (type node-abstraction node)
           (type tc:environment env)
           (type list local-notinline-names))
  `(defun ,name ,(abstraction-lambda-list node)
     (declare (ignorable ,@(loop :for param :in (node-abstraction-keyword-params node)
                                 :append (list (keyword-param-var param)
                                               (keyword-param-supplied-p-var param)))
                         ,@(node-abstraction-vars node))
              ,@(when settings:*emit-type-annotations*
                  (loop :for var :in (node-abstraction-vars node)
                        :for ty :in (tc:function-type-arguments (node-type node))
                        :collect `(type ,(tc:lisp-type ty env) ,var)))
              ,@(when local-notinline-names
                  `((notinline ,@local-notinline-names))))
     ,(annotate-function-body
       node
       (codegen-expression (node-abstraction-subexpr node) env)
       env)))

(defvar *current-block-compilation-names* nil
  "When non-nil, the list of function symbols in the active block-compilation declaration.")

(defun register-portable-calling-conventions ()
  "Register the built-in portable calling conventions (`:lisp` and `:unpacked`)."
  (register-calling-convention
   (make-calling-convention-descriptor
    :name ':lisp
    :priority 10
    :worker-suffix nil
    :requires-composites-p nil
    :exclude-from-block-compile-p nil
    :eligible-p 'always-eligible-p
    :emit-defun-fn 'emit-standard-defun))
  (register-calling-convention
   (make-calling-convention-descriptor
    :name ':unpacked
    :priority 20
    :worker-suffix "%UNPACKED"
    :requires-composites-p t
    :exclude-from-block-compile-p nil
    :eligible-p 'always-eligible-p
    :emit-defun-fn 'emit-standard-defun)))

(register-portable-calling-conventions)

(defun emit-stale-entrypoint-stubs (name old-entry new-entry env)
  "Emit safe forwarding stubs for any worker entrypoints on OLD-ENTRY that were
removed or changed signature in NEW-ENTRY after a type-changing redefinition."
  (declare (type symbol name)
           (type (or null tc:function-env-entry) old-entry new-entry)
           (type tc:environment env)
           (values list &optional))
  (when (null old-entry)
    (return-from emit-stale-entrypoint-stubs nil))
  (let ((new-specs (and new-entry (tc:function-env-entry-entrypoints new-entry)))
        (forms nil))
    (dolist (old-spec (tc:function-env-entry-entrypoints old-entry))
      (let* ((old-sym (tc:entrypoint-spec-symbol old-spec))
             (old-conv (tc:entrypoint-spec-convention old-spec))
             (old-desc (find-calling-convention old-conv :no-error t))
             (matching-new
               (find old-sym new-specs :key #'tc:entrypoint-spec-symbol :test #'eq))
             (same-abi-p
               (and matching-new
                    (eq old-conv (tc:entrypoint-spec-convention matching-new))
                    (= (length (tc:entrypoint-spec-input-layouts old-spec))
                       (length (tc:entrypoint-spec-input-layouts matching-new)))
                    (every #'abi-layout=
                           (tc:entrypoint-spec-input-layouts old-spec)
                           (tc:entrypoint-spec-input-layouts matching-new))
                    (abi-layout= (tc:entrypoint-spec-output-layout old-spec)
                                 (tc:entrypoint-spec-output-layout matching-new)))))
        (unless same-abi-p
          ;; 1. Delegate convention-specific stale entrypoint bridging to the
          ;; convention descriptor (e.g., SBCL SPECIALIZED-XEP cleanup).
          (when old-desc
            (dolist (bridge-form
                     (funcall (calling-convention-descriptor-emit-stale-bridge-fn old-desc)
                              old-sym
                              old-spec
                              env))
              (push bridge-form forms)))

          ;; 2. If the worker symbol is not being redefined by a new entrypoint,
          ;; emit a forwarding DEFUN on OLD-SYM that packs the old worker inputs,
          ;; calls the new canonical NAME through SYMBOL-FUNCTION, and unpacks
          ;; the result back to the old output layout.
          (unless matching-new
            (let* ((old-fn-ty (tc:entrypoint-spec-function-type old-spec))
                   (old-in-leaf-tys (tc:function-type-arguments old-fn-ty))
                   (stub-vars (loop :for _ :in old-in-leaf-tys
                                    :for i :from 0
                                    :collect (gensym (format nil "STALE-ARG-~D-" i))))
                   (stub-var-nodes
                     (loop :for v :in stub-vars
                           :for ty :in old-in-leaf-tys
                           :collect (make-node-local-variable :type ty :value v)))
                   (packed-call-args nil)
                   (rem-vars stub-var-nodes))
              (dolist (in-layout (tc:entrypoint-spec-input-layouts old-spec))
                (multiple-value-bind (packed-arg next-rem)
                    (build-pack-from-vars-ast rem-vars in-layout)
                  (push (codegen-expression packed-arg env) packed-call-args)
                  (setf rem-vars next-rem)))
              (setf packed-call-args (nreverse packed-call-args))
              (let* ((canonical-call-form
                       `(funcall (symbol-function ',name) ,@packed-call-args))
                     (old-out-layout (tc:entrypoint-spec-output-layout old-spec))
                     (stub-body-form
                       (if (abi-layout-identity-p old-out-layout)
                           canonical-call-form
                           (let* ((res-var (gensym "STALE-RES-"))
                                  (res-ty (abi-layout-type old-out-layout))
                                  (unpack-ast
                                    (build-opaque-output-unpack-ast
                                     (make-node-local-variable :type res-ty :value res-var)
                                     old-out-layout
                                     (abi-layout-result-type old-out-layout)
                                     env)))
                             `(let ((,res-var ,canonical-call-form))
                                ,(codegen-expression unpack-ast env))))))
                (push `(declaim (ftype function ,old-sym)) forms)
                (push `(defun ,old-sym ,stub-vars
                         (declare (ignorable ,@stub-vars))
                         ,stub-body-form)
                      forms)))))))
    (nreverse forms)))
