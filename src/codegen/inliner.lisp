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

(defparameter *inliner-max-unroll* 2)
(defparameter *inliner-max-depth*  16)
(defparameter *inliner-heuristic*  'gentle-heuristic)

(defvar *functions-inlined*)

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
  (when node
    (funcall *inliner-heuristic* node)))

(defun debug! (fmt &rest args)
  (when settings:*print-inlining-occurrences*
    (apply #'format t (uiop:strcat "~&" fmt) args)
    (force-output t)))

(defun unrolledp (node stack)
  (a:when-let ((name (ast:node-rator-name node)))
    (<= *inliner-max-unroll* (count name stack))))

(defun max-depth-p (stack)
  (<= *inliner-max-depth* (length stack)))

(defun lookup-code-global (node env)
  (let ((code (tc:lookup-code env (ast:node-rator-name node) :no-error t)))
    (when (ast:node-abstraction-p code)
      code)))

(defun lookup-code-anonymous (node)
  (let ((code (ast:node-application-rator node)))
    (when (ast:node-abstraction-p code)
      code)))

(defun inlinable-function-p (name env)
  (a:when-let ((entry (tc:lookup-function env name :no-error t)))
    (tc:function-env-entry-inline-p entry)))

(defun fully-applied-p (node code)
  (when code
    (= (length (ast:node-abstraction-vars code))
       (length (ast:node-rands node)))))

(defun inline-code-from-application (node code)
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
  (declare (type (or ast:node-application ast:node-direct-application) node))
  (let ((name (ast:node-rator-name node)))
    (debug! "TIA:  ~a ~a" stack name)
    (debug! "Can't inline: ~a" noinline-functions)
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
      ((and (fully-applied-p node (lookup-code-global node env))
            (or (heuristic-inline-p (lookup-code-global node env))
                (inlinable-function-p name env)))
       ;; Case #1: (f e1 ... en) where f is global, known, and arity n.
       (debug! ";; Inlining globally known function ~a" name)
       (push name *functions-inlined*)
       (inline-applications*
        (inline-code-from-application node (lookup-code-global node env))
        env
        stack
        noinline-functions))
      ((and (ast:node-application-p node)
            (heuristic-inline-p (lookup-code-anonymous node))
            (fully-applied-p node (lookup-code-anonymous node)))
       ;; Case #2: ((fn (x1 ... xn) ...) e1 ... en)
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
  (let ((rator (ast:node-application-rator node))
        (rands (ast:node-application-rands node)))
    (multiple-value-bind (dict inner-rands) (extract-dict rands)
      (if (and dict (ast:node-variable-p rator))
          (let ((method-name (tc:lookup-method-inline env (ast:node-variable-value rator) dict :no-error t)))
            (cond
              ((null method-name)
               node)
              ((null inner-rands)
               (debug! "Inlining method to variable ~a" method-name)
               (ast:make-node-variable
                :type (ast:node-type node)
                :value method-name))
              (t
               (debug! "Inlining method to application ~a" method-name)
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
  (let ((rands (ast:node-direct-application-rands node)))
    (multiple-value-bind (dict inner-rands) (extract-dict rands)
      (if dict
          (let ((method-name (tc:lookup-method-inline env (ast:node-direct-application-rator node) dict :no-error t)))
            (cond
              ((null method-name)
               node)
              ((null inner-rands)
               (debug! "Inlining direct method to variable ~a" method-name)
               (ast:make-node-variable
                :type (ast:node-type node)
                :value method-name))
              (t
               (debug! "Inlining direct method to application ~a" method-name)
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
  (traverse:traverse
   node
   (list
    (traverse:action (:before ast:node-locally node)
      (dolist (sym (ast:node-locally-noinline-functions node))
        (debug! "Pushed: ~a" sym)
        (push sym noinline-functions))
      node)
    (traverse:action (:before ast:node-application node)
      (debug! "Entering: ~a" (ast:node-rator-name node))
      (push (ast:node-rator-name node) stack)
      node)
    (traverse:action (:before ast:node-direct-application node)
      (push (ast:node-rator-name node) stack)
      node)

    (traverse:action (:after ast:node-locally node)
      (dolist (sym (ast:node-locally-noinline-functions node))
        (debug! "Popped: ~a" 
                (pop noinline-functions)))
      node)
    (traverse:action (:after ast:node-application node)

      (debug! "Leaving: ~a" (ast:node-rator-name node))
      (let ((node (inline-method node env)))
        (prog1 (inline-application node env stack noinline-functions)
          (pop stack))))
    (traverse:action (:after ast:node-direct-application node)
      (let ((node (inline-direct-method node env)))
        (prog1 (inline-application node env stack noinline-functions)
          (pop stack)))))))

(defun inline-applications (node env)
  (debug! "INLINING: ~a" node)
  (let ((*functions-inlined* ())) 
    (values
     (inline-applications* node env () ())
     *functions-inlined*)))
