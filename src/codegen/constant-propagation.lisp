(defpackage #:coalton-impl/codegen/constant-propagation
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
   #:*traverse*
   #:traverse)
  (:import-from
   #:coalton-impl/codegen/monomorphize
   #:dictionary-node-p)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:node-mutated-variables)
  (:export
   #:propagate-constants))

(in-package #:coalton-impl/codegen/constant-propagation)

(defun constant-var-value (var constant-bindings &key no-error)
  (declare (type symbol var)
           (values (or node null) &optional))
  (let ((may-be-value (cdr (assoc var constant-bindings))))
    (cond (may-be-value
           ;; Whenever it exists, it is always a NODE (thus, non-NIL)
           may-be-value)
          (no-error
           nil)
          (t
           (error "Expected VAR ~S to be a constant but it is not" var)))))

(defun enum-or-nil-constructor-p (name env)
  (declare (type symbol name)
           (type tc:environment env)
           (values boolean &optional))
  (or (eq name 'coalton:Nil)
      (alexandria:when-let ((ctor (tc:lookup-constructor env name :no-error t)))
        (let ((type (tc:lookup-type env (tc:constructor-entry-constructs ctor) :no-error t)))
          (and type (tc:type-entry-enum-repr type))))))

(defun global-constant-node (env name)
  "In release mode, if NAME is a monomorphic top-level binding whose optimized
code is a scalar constant, return that constant node."
  (declare (type tc:environment env)
           (type symbol name)
           (values (or node null) &optional))
  (when (and (settings:coalton-release-p)
             (not (util:dynamic-variable-name-p name)))
    (let ((code (tc:lookup-code env name :no-error t))
          (scheme (tc:lookup-value-type env name :no-error t)))
      (when (and code
                 scheme
                 (null (tc:ty-scheme-kinds scheme))
                 (null (tc:qualified-ty-predicates (tc:ty-scheme-type scheme))))
        (cond
          ((and (node-literal-p code)
                (or (numberp (node-literal-value code))
                    (characterp (node-literal-value code))))
           code)
          ((and (node-global-variable-p code)
                (enum-or-nil-constructor-p (node-variable-value code) env))
           code)
          (t
           nil))))))

(defun scalar-constant-node-p (env node constant-bindings)
  "If NODE is a scalar constant (literal or enum/Nil constructor), return its constant node."
  (declare (type tc:environment env)
           (type node node)
           (type list constant-bindings)
           (values (or node null) &optional))
  (cond ((node-literal-p node)
         (if (or (numberp (node-literal-value node))
                 (characterp (node-literal-value node)))
             node
             nil))
        ((node-global-variable-p node)
         (cond ((enum-or-nil-constructor-p (node-variable-value node) env)
                node)
               ((alexandria:when-let ((const (global-constant-node env (node-variable-value node))))
                  (when (tc:ty= (node-type const) (node-type node))
                    const)))
               (t nil)))
        ((node-local-variable-p node)
         (let ((val (constant-var-value (node-variable-value node) constant-bindings :no-error t)))
           (when val
             (scalar-constant-node-p env val nil))))
        (t
         nil)))

(defun constant-node-p (env node constant-bindings)
  "If NODE is a constant, returns the constant value corresponding to it.
If not, returns NIL"
  (declare (type tc:environment env)
           (type node node)
           (type list constant-bindings))
  (cond ((scalar-constant-node-p env node constant-bindings))
        ((node-global-variable-p node)
         (if (tc:lookup-instance-by-codegen-sym env (node-variable-value node) :no-error t)
             node
             nil))
        ((node-local-variable-p node)
         (constant-var-value (node-variable-value node) constant-bindings :no-error t))
        ((node-lisp-p node)
         (if (and (null (node-lisp-vars node))
                  (null (cdr (node-lisp-form node)))
                  (cl:constantp (first (node-lisp-form node))))
             node
             nil))
        ;; special support for dictionaries
        ((dictionary-node-p node env)
         node)
        (t
         nil)))

(defun limit-name (x)
  (if (< (length x) 16)
      x
      (subseq x 0 16)))

(defun fold-inlined-integer-conversion-lisp-node (node lisp-var constant-node env)
  "If NODE is an inlined `fromInt` or `unsafe-cast` (`into`) `node-lisp` form
applied to integer literal CONSTANT-NODE, fold it to a `node-literal`."
  (declare (type node-lisp node)
           (type symbol lisp-var)
           (type node constant-node)
           (type tc:environment env)
           (values (or null node-literal) &optional))
  (when (and (node-literal-p constant-node)
             (integerp (node-literal-value constant-node))
             (= 1 (length (node-lisp-form node))))
    (let* ((val (node-literal-value constant-node))
           (target-ty (node-type node))
           (form (first (node-lisp-form node)))
           (lisp-ty (tc:lisp-type target-ty env)))
      (cond
        ;; (lisp (-> :to) (x) x) from unsafe-cast / into, or (cl:identity x) from Num Integer
        ((or (eq form lisp-var)
             (and (consp form)
                  (= 2 (length form))
                  (eq 'cl:identity (first form))
                  (eq lisp-var (second form))))
         (when (ignore-errors (typep val lisp-ty))
           (make-node-literal :type target-ty :value val)))

        ;; (cl:values (cl:mod x modulus)) or (cl:mod x modulus) from unsigned fromInt
        ((let ((mod-form (if (and (consp form)
                                  (= 2 (length form))
                                  (eq 'cl:values (first form)))
                             (second form)
                             form)))
           (and (consp mod-form)
                (= 3 (length mod-form))
                (eq 'cl:mod (first mod-form))
                (eq lisp-var (second mod-form))
                (integerp (third mod-form))
                (plusp (third mod-form))
                (let ((wrapped (mod val (third mod-form))))
                  (when (ignore-errors (typep wrapped lisp-ty))
                    (make-node-literal :type target-ty :value wrapped))))))

        ;; (cl:ldb (cl:byte 1 0) x) from Num Bit fromInt
        ((and (consp form)
              (= 3 (length form))
              (eq 'cl:ldb (first form))
              (equal '(cl:byte 1 0) (second form))
              (eq lisp-var (third form)))
         (make-node-literal :type target-ty :value (ldb (byte 1 0) val)))

        ;; (%handle-Nbit-overflow x) from signed fromInt when in-bounds
        ((and (consp form)
              (= 2 (length form))
              (symbolp (first form))
              (eq lisp-var (second form))
              (member (symbol-name (first form))
                      '("%HANDLE-8BIT-OVERFLOW"
                        "%HANDLE-16BIT-OVERFLOW"
                        "%HANDLE-32BIT-OVERFLOW"
                        "%HANDLE-64BIT-OVERFLOW"
                        "%HANDLE-FIXNUM-OVERFLOW")
                      :test #'string=)
              (ignore-errors (typep val lisp-ty)))
         (make-node-literal :type target-ty :value val))

        (t
         nil)))))

(defun propagate-constants (node env &aux (mutated-variables (node-mutated-variables node)))
  "Propagate constant values without eliminating bindings assigned in NODE."
  (declare (optimize debug))
  (labels ((constant-binding-value (name value constant-bindings &key scalar-only)
             (unless (member name mutated-variables :test #'eq)
               (if scalar-only
                   (scalar-constant-node-p env value constant-bindings)
                   (constant-node-p env value constant-bindings))))

           (propagate-constants-node-variable (node constant-bindings)
             (declare (type node-variable node))
             (let ((x (constant-node-p env node constant-bindings)))
               (if (not (null x))
                   x
                   node)))

           (propagate-constants-node-locally (node constant-bindings)
             (declare (type node-locally node)
                      (ignore constant-bindings))
             (if (node-literal-p (node-locally-subexpr node))
                 (copy-node (node-locally-subexpr node) (node-type node))
                 node))

           (propagate-constants-node-bind (node constant-bindings)
             (declare (type node-bind node))
             (let* ((var (node-bind-name node))
                    (propagated-value-node (funcall *traverse* (node-bind-expr node) constant-bindings))
                    (scalar-const (constant-binding-value var propagated-value-node
                                                          constant-bindings :scalar-only t)))
               (if scalar-const
                   (funcall *traverse*
                            (node-bind-body node)
                            (acons var scalar-const constant-bindings))
                   (make-node-bind
                    :type (node-type node)
                    :name var
                    :expr propagated-value-node
                    :body (funcall *traverse* (node-bind-body node) constant-bindings)))))

           (propagate-constants-node-let (node constant-bindings)
             (declare (type node-let node))
             (let ((node-bindings (node-let-bindings node))
                   (new-constant-bindings nil)
                   (nonconstant-bindings nil))
               (loop :for (var . value) :in node-bindings
                     :for propagated-value-node := (funcall *traverse* value constant-bindings)
                     :do (cond
                           ((constant-binding-value var propagated-value-node constant-bindings)
                            (push (cons var propagated-value-node)
                                  new-constant-bindings))
                           (t
                            (push (cons var propagated-value-node)
                                  nonconstant-bindings))))

               ;; Collect all of our constant bindings into a single
               ;; list. We can mutate NEW-CONSTANT-BINDINGS because
               ;; it's fresh, and CONSTANT-BINDINGS will remain
               ;; unmodified. If there were no new bindings, then (EQ
               ;; NEW-CONSTANT-BINDINGS CONSTANT-BINDINGS) will be
               ;; true. (We use this fact below.)
               (setf new-constant-bindings (nconc new-constant-bindings constant-bindings))
               
               ;; Propagate new constant bindings into the values of
               ;; the remaining nonconstant bindings.
               ;;
               ;; Warning : This loop attempts to compute a fixed
               ;; point among the let bindings with respect to
               ;; constant propagation. The loop will still be correct
               ;; if `continue` is initialized to `t`. However, when
               ;; `new-constant-bindings` is empty, the loop has no
               ;; effect on the result, but creates an exponential
               ;; compile-time cost in some cases (such as a
               ;; `make-list` with many elements).
               (loop :with continue := (not (eq constant-bindings new-constant-bindings))
                     :while continue
                     :do (setf continue nil)
                         ;; Propagate constants into the bindings. If
                         ;; it's detected a binding is/became
                         ;; constant, then save it to our constants
                         ;; list, remove it from the binding list, and
                         ;; set up for another pass.
                         (loop :for binding :in nonconstant-bindings
                               :for (var . value) := binding
                               :for new-value := (funcall *traverse* value new-constant-bindings)
                               :if (constant-binding-value var new-value new-constant-bindings)
                                 :do (push (cons var new-value) new-constant-bindings)
                                     (setf continue t)
                               :else
                                 :collect (cons var new-value) :into remaining-nonconstant-bindings
                               :finally (setf nonconstant-bindings remaining-nonconstant-bindings)))

               (if (endp nonconstant-bindings)
                   (funcall *traverse* (node-let-subexpr node) new-constant-bindings)
                   (make-node-let
                    :type (node-type node)
                    :bindings nonconstant-bindings
                    :subexpr (funcall *traverse* (node-let-subexpr node) new-constant-bindings)))))

           (propagate-constants-node-lisp (node constant-bindings)
             (declare (type node-lisp node))
             (let* ((new-lisp-vars nil)
                    (let-bindings
                      (loop :for (lisp-var . coalton-var) :in (node-lisp-vars node)
                            :for constant-value := (or (constant-var-value coalton-var constant-bindings :no-error t)
                                                       (global-constant-node env coalton-var))
                            :if constant-value
                              :collect (let ((new-coalton-var (gentemp (limit-name (symbol-name coalton-var)))))
                                         (push (cons lisp-var new-coalton-var) new-lisp-vars)
                                         (cons new-coalton-var constant-value))
                            :else
                              :do (push (cons lisp-var coalton-var) new-lisp-vars))))
               (cond
                 ((and (= 1 (length (node-lisp-vars node)))
                       (= 1 (length let-bindings)))
                  (or (fold-inlined-integer-conversion-lisp-node
                       node
                       (caar new-lisp-vars)
                       (cdar let-bindings)
                       env)
                      (make-node-let
                       :type (node-type node)
                       :bindings let-bindings
                       :subexpr (make-node-lisp
                                 :type (node-type node)
                                 :vars (nreverse new-lisp-vars)
                                 :form (node-lisp-form node)))))
                 (let-bindings
                  (make-node-let
                   :type (node-type node)
                   :bindings let-bindings
                   :subexpr (make-node-lisp
                             :type (node-type node)
                             :vars (nreverse new-lisp-vars)
                             :form (node-lisp-form node))))
                 (t
                  (make-node-lisp
                   :type (node-type node)
                   :vars (nreverse new-lisp-vars)
                   :form (node-lisp-form node))))))

           (direct-application-better-infer-types (node constant-bindings)
             (declare (type node-direct-application node)
                      (ignore constant-bindings))
             ;; FIXME: Can we do something similar for non-direct APPLICATION?
             (make-node-direct-application
              :type (node-type node)
              :properties (node-properties node)
              :rator-type (tc:make-function-ty
                           :positional-input-types (mapcar #'node-type (node-direct-application-rands node))
                           :keyword-input-types
                           (typecase (node-direct-application-rator-type node)
                             (tc:function-ty
                               (tc:function-ty-keyword-input-types (node-direct-application-rator-type node)))
                             (t
                               nil))
                           :keyword-open-p
                           (typecase (node-direct-application-rator-type node)
                             (tc:function-ty
                              (tc:function-ty-keyword-open-p (node-direct-application-rator-type node)))
                             (t
                              nil))
                           :output-types (tc:function-output-types (node-direct-application-rator-type node)))
              :rator (node-direct-application-rator node)
              :rands (node-direct-application-rands node)
              :keyword-rands (node-direct-application-keyword-rands node))))

    (traverse
     node
     (list
      (action (:after node-local-variable) #'propagate-constants-node-variable)
      (action (:after node-global-variable) #'propagate-constants-node-variable)
      (action (:traverse node-bind) #'propagate-constants-node-bind)
      (action (:traverse node-let) #'propagate-constants-node-let)
      (action (:after node-lisp) #'propagate-constants-node-lisp)
      (action (:after node-locally) #'propagate-constants-node-locally)
      (action (:after node-direct-application) #'direct-application-better-infer-types))
     nil)))
