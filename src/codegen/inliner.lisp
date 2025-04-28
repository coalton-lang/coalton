(defpackage #:coalton-impl/codegen/inliner
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:ast #:coalton-impl/codegen/ast)
   (#:traverse #:coalton-impl/codegen/traverse)
   (#:transformations #:coalton-impl/codegen/transformations)
   (#:substitutions #:coalton-impl/codegen/ast-substitutions)
   (#:settings #:coalton-impl/settings)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:*inliner-max-depth*                ; VARIABLE
   #:*inliner-max-unroll*               ; VARIABLE
   #:*inliner-heuristic*                ; VARIABLE
   #:null-heuristic                     ; FUNCTION
   #:gentle-heuristic                   ; FUNCTION
   #:aggresive-heuristic                ; FUNCTION
   #:inline-applications                ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/inliner)

;; Public Settings for Optimization

(defparameter *inliner-max-unroll* 4
  "Limit depth to unroll recursive functions to.")
(defparameter *inliner-max-depth*  16
  "Limit total inliner stack depth.")
(defparameter *inliner-heuristic*  'gentle-heuristic
  "Heuristic to determine if a function should be inlined
even if the user didn't explicity declare it to be.")

;; Private Settings for Debugging

(defvar *inline-methods-p* t
  "Allow inlining of methods.")
(defvar *inline-globals-p* t
  "Allow inlining of known global functions.")
(defvar *inline-lambdas-p* t
  "Allow inlining of lambdas.")

;; Dynamic Variable

(defvar *functions-inlined*
  "Any functions or methods that are inlined have their names pushed to this.
Then it is returned from `inline-applications' to tell the optimizer if it needs
to rerun optimizations.")

(defun debug! (fmt &rest args)
  "Convenience function to print debug when `settigns:*print-inlining-occurences*' is enabled."
  (when settings:*print-inlining-occurrences*
    (fresh-line)
    (apply #'format t fmt args)))

;;; Heuristics

(defun null-heuristic (node)
  "An inlining heuristic that doesn't inline."
  (declare (ignore node))
  nil)

(defun gentle-heuristic (node)
  "This will probably inline more than is optimal."
  (and (<= (traverse:count-applications node)
           4)
       (<= (traverse:count-nodes node)
           8)))

(defun aggressive-heuristic (node)
  "This will probably inline more than is optimal."
  (and (<= (traverse:count-applications node)
           8)
       (<= (traverse:count-nodes node)
           32)))

(defun heuristic-inline-p (node)
  "Determine if the node should be inlined based on heuristics."
  (declare (type ast:node-abstraction node)
           (values boolean &optional))

  (funcall *inliner-heuristic* node))

;; Utilities

(defun unrolledp (node stack)
  "Determine if the inliner has fully unrolled a recursive call."
  (declare (type (or ast:node-application ast:node-direct-application) node)
           (type list stack)
           (values boolean &optional))

  (a:if-let ((name (ast:node-rator-name node)))
    (<= *inliner-max-unroll* (count name stack))
    nil))

(defun max-depth-p (stack)
  "Determine if the inliner is at its maximum depth, by default this is 16."
  (declare (type list stack)
           (values boolean &optional))

  (<= *inliner-max-depth* (length stack)))

(defun lookup-code-global (node env)
  "Try to lookup the code of a globally known function, returns null or abstraction."
  (declare (type (or ast:node-application ast:node-direct-application) node)
           (values (or null ast:node-abstraction) &optional))

  (let ((code (tc:lookup-code env (ast:node-rator-name node) :no-error t)))
    (if (ast:node-abstraction-p code) code nil)))

(defun lookup-code-anonymous (node)
  "Try to lookup the code of an anonymous application, returns null or abstraction."
  (declare (type ast:node-application node)
           (values (or null ast:node-abstraction) &optional))

  (let ((code (ast:node-application-rator node)))
    (if (ast:node-abstraction-p code) code nil)))

(defun inlinable-function-p (name env)
  "Check if a function is declared inlinable at its definition."
  (declare (type symbol name)
           (type tc:environment env)
           (values boolean &optional))

  (a:if-let ((entry (tc:lookup-function env name :no-error t)))
    (tc:function-env-entry-inline-p entry)
    nil))

(defun fully-applied-p (node code)
  "Check if an an application node constitutes a fully applied abstraction."
  (declare (type (or ast:node-application ast:node-direct-application) node)
           (type ast:node-abstraction code)
           (values boolean &optional))

  (= (length (ast:node-abstraction-vars code))
     (length (ast:node-rands node))))

;;; Inlining

(defun inline-code-from-application (node code)
  "Swap an application node with a let node where the body is the inlined function."
  (declare (type (or ast:node-application ast:node-direct-application) node)
           (type ast:node-abstraction code)
           (values ast:node-let &optional))

  (let* ((bindings
           (mapcar (lambda (var val)
                     (cons (gensym (symbol-name var)) val))
                   (ast:node-abstraction-vars code)
                   (ast:node-rands node)))
         (substitutions
           (mapcar (lambda (var binding)
                     (destructuring-bind (new-var . val) binding
                       (substitutions:make-ast-substitution
                        :from var
                        :to (ast:make-node-variable
                             :type (ast:node-type val)
                             :value new-var))))
                   (ast:node-abstraction-vars code)
                   bindings))
         (new-code
           (transformations:rename-type-variables
            (substitutions:apply-ast-substitution
             substitutions
             (ast:node-abstraction-subexpr code) t)))
         (new-substitutions
           (coalton-impl/typechecker/unify::mgu
            (ast:node-type node)
            (ast:node-type new-code))))
    (ast:make-node-let
     :type     (ast:node-type node)
     :bindings bindings
     :subexpr  (tc:apply-substitution new-substitutions new-code))))

(defun inline-application (node env stack noinline-functions)
  "Try to inline an application, checking traversal stack, heuristics,
and user-supplied declarations to determine if it is appropriate."
  (declare (type (or ast:node-application ast:node-direct-application) node)
           (values ast:node &optional))

  (let ((name (ast:node-rator-name node)))
    (cond
      ((find name noinline-functions)
       (debug! ";; Locally noinline reached ~a" name)
       node)

      ((unrolledp node stack)
       (debug! ";; Fully unrolled ~a" name)
       (ast:make-node-locally
        :type (ast:node-type node)
        :noinline-functions (list name)
        :subexpr node))

      ((max-depth-p stack)
       (debug! ";; Max depth reached ~a" name)
       node)

      ;; Case #1: (f e1 ... en) where f is global, known, and arity n.
      ((let ((code (lookup-code-global node env)))
         (and *inline-globals-p*
              code
              (fully-applied-p node code)
              (or (heuristic-inline-p code)
                  (inlinable-function-p name env))))
       (debug! ";; Inlining globally known function ~a" name)
       (push name *functions-inlined*)
       (inline-applications*
        (inline-code-from-application node (lookup-code-global node env))
        env
        stack
        noinline-functions))

      ;; Case #2: ((fn (x1 ... xn) ...) e1 ... en)
      ((and *inline-lambdas-p*
            (ast:node-application-p node)
            (let ((code (lookup-code-anonymous node)))
              (and code
                   (heuristic-inline-p code)
                   (fully-applied-p node code))))
       (debug! ";; Inlining anonymous function ~a" name)
       (push name *functions-inlined*)
       (inline-applications*
        (inline-code-from-application node (lookup-code-anonymous node))
        env
        stack
        noinline-functions))

      (t
       (debug! ";; Failed to inline ~a" name)
       node))))

(defun extract-dict (rands)
  (declare (type ast:node-list rands)
           (values (or null parser:identifier) ast:node-list))

  (cond
    ((ast:node-variable-p (first rands))
     (values (ast:node-variable-value (first rands))
             (cdr rands)))

    ((and (ast:node-application-p (first rands))
          (ast:node-variable-p (ast:node-application-rator (first rands))))

     (values (ast:node-variable-value (ast:node-application-rator (first rands)))
             (append (ast:node-application-rands (first rands)) (cdr rands))))

    (t
     (values nil nil))))

(defun inline-method (node env)
  (declare (type ast:node-application node)
           (type tc:environment env)
           (values ast:node &optional))

  (unless *inline-methods-p*
    (return-from inline-method
      node))

  (let ((rator (ast:node-application-rator node))
        (rands (ast:node-application-rands node)))
    (multiple-value-bind (dict inner-rands) (extract-dict rands)
      (if (and dict (ast:node-variable-p rator))
          (let ((method-name (tc:lookup-method-inline env (ast:node-variable-value rator) dict :no-error t)))
            (cond
              ((null method-name)
               node)

              ((null inner-rands)
               (debug! ";; Inlining method to variable ~a" method-name)
               (push method-name *functions-inlined*)
               (ast:make-node-variable
                :type (ast:node-type node)
                :value method-name))

              (t
               (debug! ";; Inlining method to application ~a" method-name)
               (push method-name *functions-inlined*)
               (ast:make-node-application
                :type (ast:node-type node)
                :rator (ast:make-node-variable
                        :type (tc:make-function-type*
                               (mapcar #'ast:node-type inner-rands)
                               (ast:node-type node))
                        :value method-name)
                :rands inner-rands))))
          node))))

(defun inline-direct-method (node env)
  (declare (type ast:node-direct-application node)
           (type tc:environment env)
           (values ast:node &optional))

  (unless *inline-methods-p*
    (return-from inline-direct-method
      node))

  (let ((rands (ast:node-direct-application-rands node)))
    (multiple-value-bind (dict inner-rands) (extract-dict rands)
      (if dict
          (let ((method-name (tc:lookup-method-inline env (ast:node-direct-application-rator node) dict :no-error t)))
            (cond
              ((null method-name)
               node)

              ((null inner-rands)
               (debug! ";; Inlining direct method to variable ~a" method-name)
               (push method-name *functions-inlined*)
               (ast:make-node-variable
                :type (ast:node-type node)
                :value method-name))

              (t
               (debug! ";; Inlining direct method to application ~a" method-name)
               (push method-name *functions-inlined*)
               (ast:make-node-application
                :type (ast:node-type node)
                :rator (ast:make-node-variable
                        :type (tc:make-function-type*
                               (mapcar #'ast:node-type inner-rands)
                               (ast:node-type node))
                        :value method-name)
                :rands inner-rands))))
          node))))

(defun inline-applications* (node env stack noinline-functions)
  (declare (type ast:node node)
           (type tc:environment env)
           (type list stack)
           (type list noinline-functions)
           (values ast:node &optional))

  (traverse:traverse
   node
   (list
    (traverse:action (:before ast:node-locally node)
      (dolist (sym (ast:node-locally-noinline-functions node))
        (push sym noinline-functions))
      node)
    (traverse:action (:before ast:node-application node)
      (push (ast:node-rator-name node) stack)
      node)
    (traverse:action (:before ast:node-direct-application node)
      (push (ast:node-rator-name node) stack)
      node)

    (traverse:action (:after ast:node-locally node)
      (dolist (sym (ast:node-locally-noinline-functions node))
        (pop noinline-functions))
      node)
    (traverse:action (:after ast:node-application node)
      (prog1 (let ((methods-inlined (inline-method node env)))
               (etypecase methods-inlined
                 ((or ast:node-application ast:node-direct-application)
                  (inline-application methods-inlined env stack noinline-functions))
                 (ast:node
                  methods-inlined)))
        (pop stack)))
    (traverse:action (:after ast:node-direct-application node)
      (prog1 (let ((methods-inlined (inline-direct-method node env)))
               (etypecase methods-inlined
                 ((or ast:node-application ast:node-direct-application)
                  (inline-application methods-inlined env stack noinline-functions))
                 (ast:node
                  methods-inlined)))
        (pop stack))))))

(defun inline-applications (node env)
  "Traverse node, inlining methods, functions, and lambdas where possible."
  (declare (type ast:node node)
           (type tc:environment env))

  (let ((*functions-inlined* ()))
    (values
     (inline-applications* node env () '(coalton:cons))
     *functions-inlined*)))
