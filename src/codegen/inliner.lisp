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
   (#:tc #:coalton-impl/typechecker)
   (#:util #:coalton-impl/util))
  (:export
   #:*inliner-max-depth*                ; VARIABLE
   #:*inliner-max-unroll*               ; VARIABLE
   #:*inliner-heuristic*                ; VARIABLE
   #:null-heuristic                     ; FUNCTION
   #:gentle-heuristic                   ; FUNCTION
   #:aggresive-heuristic                ; FUNCTION
   #:inline-applications                ; FUNCTION
   #:function-declared-inline-p         ; FUNCTION
   ))

(in-package #:coalton-impl/codegen/inliner)

;;; Public Settings for Optimization

(defparameter *inliner-max-unroll* 3
  "Limit depth to unroll recursive functions to.")
(defparameter *inliner-max-depth*  16
  "Limit total inliner stack depth.")
(defparameter *inliner-heuristic*  'gentle-heuristic
  "Heuristic to determine if a function should be inlined
even if the user didn't explicity declare it to be.")

;;; Private Settings for Debugging

(defvar *print-extra-debugging-info* nil
  "Print additional information to help debug the inliner.")
(defvar *inline-methods-p* t
  "Allow inlining of methods.")
(defvar *inline-globals-p* t
  "Allow inlining of known global functions.")
(defvar *inline-lambdas-p* t
  "Allow inlining of lambdas.")

;;; Dynamic Variable

(declaim (type list *functions-inlined*))
(defvar *functions-inlined* nil
  "Any functions or methods that are inlined have their names pushed to this.
Then it is returned from `inline-applications' to tell the optimizer if it needs
to rerun optimizations.")

(defun debug! (fmt &rest args)
  "Convenience function to print debug info."
  (when *print-extra-debugging-info*
    (fresh-line)
    (apply #'format t fmt args)))

(defun print-inline-success! (fmt &rest args)
  "Convenience function to print when there's an inlining success,
controlled by `settings:*print-inlining-occurences*' is enabled."
  (when (or settings:*print-inlining-occurrences*
            *print-extra-debugging-info*)
    (fresh-line)
    (apply #'format t fmt args)))

;;; Heuristics

(defun null-heuristic (abstraction)
  "An inlining heuristic that doesn't inline."
  (declare (ignore abstraction))
  nil)

(defun gentle-heuristic (abstraction)
  "This will probably inline more than is optimal."
  (and (<= (traverse:count-applications abstraction)
           4)
       (<= (traverse:count-nodes abstraction)
           8)))

(defun aggressive-heuristic (abstraction)
  "This will probably inline more than is optimal."
  (and (<= (traverse:count-applications abstraction)
           8)
       (<= (traverse:count-nodes abstraction)
           32)))

(defun heuristic-inline-p (abstraction)
  "Determine if the node should be inlined based on heuristics."
  (declare (type ast:node-abstraction abstraction)
           (values boolean &optional))

  (and settings:*coalton-heuristic-inlining*
       (funcall *inliner-heuristic* abstraction)))

;;; Utilities

(defun unrolling-forbidden-p (application stack)
  "Determine if the inliner has fully unrolled a recursive call."
  (declare (type (or ast:node-application ast:node-direct-application) application)
           (type list stack)
           (values boolean &optional))

  (a:if-let ((name (ast:node-rator-name application)))
    (<= *inliner-max-unroll* (count name stack))
    nil))

(defun stack-reached-max-depth-p (stack)
  "Determine if the inliner is at its maximum depth, by default this is 16."
  (declare (type list stack)
           (values boolean &optional))

  (<= *inliner-max-depth* (length stack)))

(defun lookup-global-application-body (application env)
  "Try to lookup the code of a globally known function, returns null or abstraction."
  (declare (type (or ast:node-application ast:node-direct-application) application)
           (values (or null ast:node-abstraction) &optional))

  (let ((abstraction (tc:lookup-code env (ast:node-rator-name application) :no-error t)))
    (if (ast:node-abstraction-p abstraction) abstraction nil)))

(defun lookup-anonymous-application-body (application)
  "Try to lookup the code of an anonymous application, returns null or abstraction."
  (declare (type ast:node-application application)
           (values (or null ast:node-abstraction) &optional))

  (let ((abstraction (ast:node-application-rator application)))
    (if (ast:node-abstraction-p abstraction) abstraction nil)))

(defun function-declared-inline-p (name env)
  "Check if a function is declared inlinable at its definition."
  (declare (type symbol name)
           (type tc:environment env)
           (values boolean &optional))

  (a:if-let ((entry (tc:lookup-function env name :no-error t)))
    (tc:function-env-entry-inline-p entry)
    nil))

(defun application-saturates-abstraction-p (application abstraction)
  "Check if an an application node constitutes a fully applied abstraction."
  (declare (type (or ast:node-application ast:node-direct-application) application)
           (type ast:node-abstraction abstraction)
           (values boolean &optional))

  (= (length (ast:node-abstraction-vars abstraction))
     (length (ast:node-rands application))))

(defun call-marked-for-inline-p (application)
  "Check if the the application ought to be inlined at the callsite."
  (declare (type (or ast:node-application ast:node-direct-application) application)
           (values t &optional))

  (getf (ast:node-properties application) ':inline))

(defun call-marked-for-noinline-p (application)
  "Check if the the application ought to be prevented from being inlined at the callsite."
  (declare (type (or ast:node-application ast:node-direct-application) application)
           (values t &optional))

  (getf (ast:node-properties application) ':noinline))

;;; Inlining

(defun inline-code-from-application (application abstraction)
  "Swap an application node with a let node where the body is the inlined function."
  (declare (type (or ast:node-application ast:node-direct-application) application)
           (type ast:node-abstraction abstraction)
           (values ast:node-let &optional))

  (let* ((fresh-abstraction
           (transformations:rename-type-variables abstraction))
         (bindings
           (mapcar (lambda (var val) (cons (gensym (symbol-name var)) val))
                   (ast:node-abstraction-vars fresh-abstraction)
                   (ast:node-rands application)))
         (substitutions
           (mapcar (lambda (var binding)
                     (destructuring-bind (new-var . val) binding
                       (substitutions:make-ast-substitution
                        :from var
                        :to (ast:make-node-variable
                             :type (ast:node-type val)
                             :value new-var))))
                   (ast:node-abstraction-vars fresh-abstraction)
                   bindings))
         (new-subexpr
           (substitutions:apply-ast-substitution
            substitutions
            (ast:node-abstraction-subexpr fresh-abstraction)
            t))
         (call-type
           (tc:make-function-type*
            (mapcar #'ast:node-type (ast:node-rands application))
            (ast:node-type application)))
         (new-substitutions
           (coalton-impl/typechecker/unify::mgu
            (ast:node-type fresh-abstraction)
            call-type)))
    (ast:make-node-let
     :type     (ast:node-type application)
     :bindings (loop :for (name . expr) :in bindings
                     :collect (cons name (tc:apply-substitution new-substitutions expr)))
     :subexpr  (tc:apply-substitution new-substitutions new-subexpr))))

(defun try-inline-application (application env stack noinline-functions)
  "Try to inline an application node, checking internal traversal stack,
heuristics, and user-supplied inline declarations to determine if it
is appropriate."
  (declare (type (or ast:node-application ast:node-direct-application) application)
           (type tc:environment env)
           (type list stack)
           (type parser:identifier-list noinline-functions)
           (values ast:node &optional))

  (let ((name (ast:node-rator-name application)))
    (cond
      ((find name noinline-functions)
       (debug! ";; Locally noinline reached ~a" name)
       application)

      ((unrolling-forbidden-p application stack)
       (debug! ";; Fully unrolled ~a" name)
       (ast:make-node-locally
        :type (ast:node-type application)
        :noinline-functions (list name)
        :subexpr application))

      ((stack-reached-max-depth-p stack)
       (debug! ";; Max depth reached ~a" name)
       application)

      ;; Case #1: (f e1 ... en) where f is global, known, and arity n.
      ((let ((abstraction (lookup-global-application-body application env)))
         (and *inline-globals-p*
              abstraction
              (not (call-marked-for-noinline-p application))
              (application-saturates-abstraction-p application abstraction)
              (or (heuristic-inline-p abstraction)
                  (call-marked-for-inline-p application)
                  (function-declared-inline-p name env))))
       (print-inline-success! ";; Inlining global function ~a" name)
       (push name *functions-inlined*)
       (inline-applications*
        (inline-code-from-application
         application
         (lookup-global-application-body application env))
        env
        stack
        noinline-functions))

      ;; Case #2: ((fn (x1 ... xn) ...) e1 ... en)
      ((and *inline-lambdas-p*
            (ast:node-application-p application)
            (let ((abstraction (lookup-anonymous-application-body application)))
              (and abstraction
                   (not (call-marked-for-noinline-p application))
                   (application-saturates-abstraction-p application abstraction)
                   (or (heuristic-inline-p abstraction)
                       (call-marked-for-inline-p application)))))
       (print-inline-success! ";; Inlining anonymous function")
       (push name *functions-inlined*)
       (inline-applications*
        (inline-code-from-application
         application
         (lookup-anonymous-application-body application))
        env
        stack
        noinline-functions))

      (t
       (cond
         ((null name)
          (debug! ";; Failed to inline anonymous application"))
         (t
          (debug! ";; Failed to inline ~A" name)))
       application))))

(defun extract-dict (rands)
  (declare (type ast:node-list rands)
           (values (or null parser:identifier) ast:node-list &optional))

  (cond
    ((ast:node-variable-p (first rands))
     (values (ast:node-variable-value (first rands))
             (rest rands)))

    ((and (ast:node-application-p (first rands))
          (ast:node-variable-p (ast:node-application-rator (first rands))))
     (values (ast:node-variable-value (ast:node-application-rator (first rands)))
             (append (ast:node-application-rands (first rands)) (rest rands))))

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
      (if (or (null dict) (not (ast:node-variable-p rator)))
          node
          (let ((method-name (tc:lookup-method-inline env (ast:node-variable-value rator) dict :no-error t)))
            (cond
              ((null method-name)
               node)

              ((null inner-rands)
               (print-inline-success! ";; Inlining method to variable ~a" method-name)
               (push method-name *functions-inlined*)
               (ast:make-node-variable
                :type (ast:node-type node)
                :value method-name))

              (t
               (print-inline-success! ";; Inlining method to application ~a" method-name)
               (push method-name *functions-inlined*)
               (ast:make-node-application
                :type (ast:node-type node)
                :properties (ast:node-properties node)
                :rator (ast:make-node-variable
                        :type (tc:make-function-type*
                               (mapcar #'ast:node-type inner-rands)
                               (ast:node-type node))
                        :value method-name)
                :rands inner-rands))))))))

(defun inline-direct-method (node env)
  (declare (type ast:node-direct-application node)
           (type tc:environment env)
           (values ast:node &optional))

  (unless *inline-methods-p*
    (return-from inline-direct-method
      node))

  (let ((rands (ast:node-direct-application-rands node)))
    (multiple-value-bind (dict inner-rands) (extract-dict rands)
      (if (null dict)
          node
          (let ((method-name (tc:lookup-method-inline env (ast:node-direct-application-rator node) dict :no-error t)))
            (cond
              ((null method-name)
               node)

              ((null inner-rands)
               (print-inline-success! ";; Inlining direct method to variable ~a" method-name)
               (push method-name *functions-inlined*)
               (ast:make-node-variable
                :type (ast:node-type node)
                :value method-name))

              (t
               (print-inline-success! ";; Inlining direct method to application ~a" method-name)
               (push method-name *functions-inlined*)
               (ast:make-node-application
                :type (ast:node-type node)
                :properties (ast:node-properties node)
                :rator (ast:make-node-variable
                        :type (tc:make-function-type*
                               (mapcar #'ast:node-type inner-rands)
                               (ast:node-type node))
                        :value method-name)
                :rands inner-rands))))))))

(defun inline-applications* (node env stack noinline-functions)
  (declare (type ast:node node)
           (type tc:environment env)
           (type list stack)
           (type parser:identifier-list noinline-functions)
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
                  (try-inline-application methods-inlined env stack noinline-functions))
                 (ast:node
                  methods-inlined)))
        (pop stack)))

    (traverse:action (:after ast:node-direct-application node)
      (prog1 (let ((methods-inlined (inline-direct-method node env)))
               (etypecase methods-inlined
                 ((or ast:node-application ast:node-direct-application)
                  (try-inline-application methods-inlined env stack noinline-functions))
                 (ast:node
                  methods-inlined)))
        (pop stack))))))

(defun inline-applications (node env)
  "Traverse node, inlining methods, functions, and lambdas where possible."
  (declare (type ast:node node)
           (type tc:environment env)
           (values ast:node list &optional))

  (let ((*functions-inlined* '()))
    (values
     (inline-applications* node env '() '(coalton:cons))
     *functions-inlined*)))
