;;;; calling-convention-sbcl.lisp
;;;;
;;;; SBCL (:x86-64 / :arm64) backend for the `:unboxed` calling convention.
;;;;
;;;; This file is loaded conditionally via `coalton-compiler.asd` under
;;;; `:if-feature (:and :sbcl (:or :x86-64 :arm64))`. Deleting or disabling
;;;; this file leaves the compiler fully functional with the portable `:lisp`
;;;; and `:unpacked` calling conventions.
;;;;
;;;; How SBCL's `SB-INT:SPECIALIZED-XEP` Works
;;;; -----------------------------------------
;;;; On x86-64 and arm64, SBCL supports a secondary External Entry Point named
;;;; `(sb-int:specialized-xep <sym> (<arg-type>...) (values <ret-type>... &optional))`.
;;;; When registered via `sb-c:%compiler-defun` and `sb-impl::%defun-specialized-xep`,
;;;; SBCL's `ir1final` pass (`src/compiler/ir1final.lisp`) rewrites known calls to
;;;; `<sym>` into calls to the specialized XEP, and `sb-vm::fixed-call-arg-location`
;;;; (`src/compiler/generic/utils.lisp`) wires parameters and return values of
;;;; primitive types `double-float`, `single-float`, `system-area-pointer`,
;;;; `unsigned-byte-64`, and `signed-byte-64` directly to machine XMM and
;;;; general-purpose registers without heap boxing.
;;;;
;;;; Interaction with Block Compilation (`sb-ext:start-block`)
;;;; ---------------------------------------------------------
;;;; As noted in SBCL's `sb-impl::specialized-xep-for-type-p` (`src/code/macros.lisp`),
;;;; specialized XEPs must not be compiled inside an active `sb-ext:start-block`
;;;; component because SBCL's GTN/LTN passes merge tail-sets across local calls,
;;;; which would overwrite the standard boxed XEP return convention of polymorphic
;;;; callees in the same block. Accordingly, `:unboxed` sets
;;;; `:exclude-from-block-compile-p t` on its descriptor and brackets its
;;;; `sb-impl::%defun-specialized-xep` emission with `end-block` / `start-block`.

(in-package #:coalton-impl/codegen/calling-convention)

(defun sbcl-specialized-xep-symbol ()
  "Return the `SPECIALIZED-XEP` symbol (`SB-INT:SPECIALIZED-XEP` on SBCL > 2.6.10,
or `SB-IMPL::SPECIALIZED-XEP` on SBCL 2.6.8–2.6.10), or NIL if unavailable."
  (or (find-symbol "SPECIALIZED-XEP" "SB-INT")
      (find-symbol "SPECIALIZED-XEP" "SB-IMPL")))

(defun sbcl-specialized-xep-available-p ()
  "Return true when the running SBCL image supports `SPECIALIZED-XEP` and
includes the SBCL 2.6.8+ fix for ignored specialized-XEP parameters (SBCL
commit `06df2ad72`)."
  (and (ignore-errors (sb-ext:assert-version->= 2 6 8) t)
       (sbcl-specialized-xep-symbol)
       (find-symbol "%DEFUN-SPECIALIZED-XEP" "SB-IMPL")
       (find-symbol "REMOVE-SPECIALIZED-XEP" "SB-IMPL")
       (or (find-symbol "STUB-LEXPR-FOR-SPECIALIZED-NAME" "SB-IMPL")
           (find-symbol "MAKE-SPECIALIZED-XEP-STUB" "SB-IMPL"))
       t))

(defun sbcl-unboxable-lisp-type-p (lisp-ty)
  "Return true when LISP-TY is assigned an unboxed machine register or stack
location by SBCL's `SB-VM::FIXED-CALL-ARG-LOCATION` on x86-64 / arm64."
  (let* ((ctype (ignore-errors (sb-kernel:specifier-type lisp-ty)))
         (primtype (and ctype (sb-c:primitive-type ctype)))
         (prim-name (and primtype (sb-c:primitive-type-name primtype))))
    (and prim-name
         (member (symbol-name prim-name)
                 '("DOUBLE-FLOAT"
                   "SINGLE-FLOAT"
                   "SYSTEM-AREA-POINTER"
                   "UNSIGNED-BYTE-64"
                   "SIGNED-BYTE-64")
                 :test #'string=)
         t)))

(defun sbcl-unboxed-eligible-p (flat-input-types flat-output-types node env)
  "Return true when a flattened worker with FLAT-INPUT-TYPES and
FLAT-OUTPUT-TYPES benefits from SBCL's `SB-INT:SPECIALIZED-XEP` calling convention."
  (declare (type list flat-input-types flat-output-types)
           (type node node)
           (type tc:environment env)
           (ignore node)
           (values boolean &optional))
  (and *enable-unboxed-abi*
       (sbcl-specialized-xep-available-p)
       settings:*emit-type-annotations*
       (plusp (length flat-input-types))
       (or (loop :for ty :in flat-input-types
                 :thereis (sbcl-unboxable-lisp-type-p (tc:lisp-type ty env)))
           (loop :for ty :in flat-output-types
                 :thereis (sbcl-unboxable-lisp-type-p (tc:lisp-type ty env))))))

(defun sbcl-specialized-xep-spec (node env)
  "Return the SBCL `SB-INT:SPECIALIZED-XEP` `(ARG-TYPES VALUES-TYPE)` list for NODE."
  (declare (type node-abstraction node)
           (type tc:environment env)
           (values list &optional))
  (let ((arg-types
          (loop :for nil :in (node-abstraction-vars node)
                :for ty :in (tc:function-type-arguments (node-type node))
                :collect (tc:lisp-type ty env)))
        (ret-type (node-output-values-type node env)))
    (list arg-types ret-type)))

(defun emit-sbcl-unboxed-pre-declarations (worker-sym worker-node env)
  "Return compile-time and load-time registration forms for an `:UNBOXED` worker in an SCC."
  (declare (type symbol worker-sym)
           (type node-abstraction worker-node)
           (type tc:environment env)
           (values list &optional))
  (if (not (and *enable-unboxed-abi*
                (sbcl-specialized-xep-available-p)
                settings:*emit-type-annotations*))
      nil
      (let* ((xep-spec (sbcl-specialized-xep-spec worker-node env))
             (specialized-xep-sym (sbcl-specialized-xep-symbol))
             (xep-name `(,specialized-xep-sym ,worker-sym ,@xep-spec))
             (compiler-defun-sym (find-symbol "%COMPILER-DEFUN" "SB-C")))
        (list `(declaim (ftype (function ,(first xep-spec) ,(second xep-spec))
                               ,xep-name))
              `(eval-when (:compile-toplevel)
                 (,compiler-defun-sym ',worker-sym t nil nil ',xep-spec))
              `(eval-when (:execute)
                 (,compiler-defun-sym ',worker-sym nil nil nil ',xep-spec))))))

(defun emit-sbcl-unboxed-defun (name node env &key local-notinline-names)
  "Emit an SBCL `SB-INT:SPECIALIZED-XEP` unboxed register worker definition."
  (declare (type symbol name)
           (type node-abstraction node)
           (type tc:environment env)
           (type list local-notinline-names))
  (if (not (and *enable-unboxed-abi*
                (sbcl-specialized-xep-available-p)
                settings:*emit-type-annotations*))
      (emit-standard-defun name node env :local-notinline-names local-notinline-names)
      (let* ((spec (sbcl-specialized-xep-spec node env))
             (specialized-xep-sym (sbcl-specialized-xep-symbol))
             (defun-specialized-xep-sym (find-symbol "%DEFUN-SPECIALIZED-XEP" "SB-IMPL"))
             (xep-name `(,specialized-xep-sym ,name ,@spec))
             (vars (abstraction-lambda-list node))
             (xep-var (gensym "XEP-"))
             (body-code
               (annotate-function-body
                node
                (codegen-expression (node-abstraction-subexpr node) env)
                env))
             (defun-form
               `(let ((,xep-var
                        (sb-int:named-lambda ,xep-name ,vars
                          (declare (ignorable ,@vars)
                                   ,@(loop :for var :in (node-abstraction-vars node)
                                           :for ty :in (tc:function-type-arguments (node-type node))
                                           :collect `(type ,(tc:lisp-type ty env) ,var))
                                   ,@(when local-notinline-names
                                       `((notinline ,@local-notinline-names))))
                          (block ,name
                            ,body-code))))
                  (,defun-specialized-xep-sym
                   ',name
                   (sb-int:named-lambda ,name ,vars
                     (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                     (multiple-value-prog1 (funcall ,xep-var ,@vars) (values)))
                   ,xep-var
                   ',spec))))
        ;; SBCL does not allow mixing block compilation with specialized XEPs
        ;; (see `sb-impl::specialized-xep-for-type-p` in `src/code/macros.lisp`).
        (if *current-block-compilation-names*
            `(progn
               (declaim (sb-ext:end-block))
               ,defun-form
               (declaim (sb-ext:start-block ,@*current-block-compilation-names*)))
            defun-form))))

(defun emit-sbcl-unboxed-stale-bridge (old-sym old-spec env)
  "Emit forms to clear and bridge a retired `:UNBOXED` `SB-INT:SPECIALIZED-XEP`
when its owning function undergoes a type-changing redefinition."
  (declare (type symbol old-sym)
           (type tc:entrypoint-spec old-spec)
           (type tc:environment env)
           (values list &optional))
  (if (not (and *enable-unboxed-abi*
                (sbcl-specialized-xep-available-p)
                settings:*emit-type-annotations*))
      nil
      (let* ((old-fn-ty (tc:entrypoint-spec-function-type old-spec))
             (old-arg-lisp-types
               (mapcar (lambda (ty) (tc:lisp-type ty env))
                       (tc:function-type-arguments old-fn-ty)))
             (old-out-lisp-types
               (mapcar (lambda (ty) (tc:lisp-type ty env))
                       (tc:function-output-types old-fn-ty)))
             (old-ret-values-type
               (if old-out-lisp-types
                   `(values ,@old-out-lisp-types &optional)
                   '(values)))
             (old-xep-spec (list old-arg-lisp-types old-ret-values-type))
             (specialized-xep-sym (sbcl-specialized-xep-symbol))
             (set-fname-sym (find-symbol "SET-FNAME-SPECIALIZED-XEP-NAME" "SB-INT"))
             (clear-info-sym (find-symbol "CLEAR-INFO" "SB-INT"))
             (remove-xep-sym (find-symbol "REMOVE-SPECIALIZED-XEP" "SB-IMPL"))
             (stub-fn (find-symbol "STUB-LEXPR-FOR-SPECIALIZED-NAME" "SB-IMPL"))
             (legacy-stub-fn (find-symbol "MAKE-SPECIALIZED-XEP-STUB" "SB-IMPL"))
             (old-xep-name `(,specialized-xep-sym ,old-sym ,@old-xep-spec))
             (stub (if stub-fn
                       (funcall stub-fn old-xep-name)
                       (funcall legacy-stub-fn old-sym old-xep-spec old-xep-name))))
        (list
         (if set-fname-sym
             `(eval-when (:compile-toplevel)
                (,set-fname-sym ',old-sym nil))
             `(eval-when (:compile-toplevel)
                (,clear-info-sym :function :specialized-xep ',old-sym)))
         `(,remove-xep-sym ',old-sym)
         `(when (fboundp ',old-xep-name)
            (setf (fdefinition ',old-xep-name) ,stub))))))

(register-calling-convention
 (make-calling-convention-descriptor
  :name ':unboxed
  :priority 30
  :worker-suffix "%UNBOXED"
  :requires-composites-p nil
  :exclude-from-block-compile-p t
  :eligible-p 'sbcl-unboxed-eligible-p
  :emit-pre-declarations-fn 'emit-sbcl-unboxed-pre-declarations
  :emit-defun-fn 'emit-sbcl-unboxed-defun
  :emit-stale-bridge-fn 'emit-sbcl-unboxed-stale-bridge))
