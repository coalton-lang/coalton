(defpackage #:coalton-impl/codegen/inliner
  (:use
   #:cl
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/traverse
   #:action
   #:count-applications
   #:count-nodes
   #:make-traverse-let-action-skipping-cons-bindings
   #:*traverse*
   #:traverse
   #:traverse-with-binding-list)
  (:import-from
   #:coalton-impl/codegen/transformations
   #:rename-type-variables)
  (:import-from
   #:coalton-impl/codegen/ast-substitutions
   #:apply-ast-substitution
   #:make-ast-substitution)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:null-heuristic                     ; FUNCTION
   #:aggressive-inline-heuristic        ; FUNCTION
   #:*inliner-max-depth*                ; VARIABLE
   #:*inliner-max-unroll*               ; VARIABLE
   #:inline-applications                ; FUNCTION
   #:inline-methods))                   ; FUNCTION

(in-package #:coalton-impl/codegen/inliner)

(defun null-heuristic (node)
  "An inlining heuristic that doesn't inline."
  (declare (ignore node))
  nil)

(defun aggressive-heuristic (node)
  "This will probably inline more than is optimal."
  (and (<= (count-applications node)
           8)
       (<= (count-nodes node)
           32)))

(declaim (type (integer 0) *inliner-max-depth*))
(defvar *inliner-max-depth* 16
  "This is the maximum depth of the function call stack that will get
inlined.")

(declaim (type (integer 0) *inliner-max-unroll*))
(defvar *inliner-max-unroll* 4
  "This is the maximum number of times a specific function can appear in
the call stack that is being inlined.")

(declaim (type (or symbol function) *inliner-heuristic*))
(defvar *inliner-heuristic* 'aggressive-heuristic
  "A function that takes a NODE and decides if it should inline it.")

(defun heuristic-inline-applications (node
                                      env
                                      &key
                                        (max-depth  *inliner-max-depth*)
                                        (max-unroll *inliner-max-unroll*)
                                        (heuristic  *inliner-heuristic*))
  "Recursively inline function calls in `node`, using the environment
`env` to look up the function bodies to substitute. The traversal
keeps a call stack and will not continue past the specified
`max-depth`. For recursive functions, traversal will not continue if
the function already appears more than `max-unroll` times in the call
stack. Otherwise, if a function's code can be found in `env` and the
`heuristic` returns true, then the body will be inlined.

Special logic prevents inlining `coalton:Cons` in let bindings to
avoid breaking recursive data let expressions, since the codegen
requires direct constructor calls."
  (declare (type node                 node)
           (type tc:environment       env)
           (type (integer 0)          max-depth)
           (type (integer 0)          max-unroll)
           (type (or symbol function) heuristic)
           (values node &optional))
  (labels ((inline-abstraction-from-application (node abstraction process-body)
             "If NODE is an abstraction (f e1 e2 ... en) and ABSTRACTION is a
              function (fn (x1 x2 ... xn) body), then produce a node which is
              something like

                  `(let ((x1 e1) (x2 e2) ... (xn en))
                     ,(funcall process-body body))

              where body has been processed by PROCESS-BODY. (The
              body may be processed by this function in other ways.)"
             (declare (type (or node-application node-direct-application) node)
                      (type node-abstraction                              abstraction)
                      (type function                                      process-body)
                      (values node-let &optional))
             (loop
               ;; (x1 ... xn)
               :with vars := (node-abstraction-vars abstraction)
               ;; (e1 ... en)
               :with vals := (node-rands node)
               ;; body, as described above
               :with body := (node-abstraction-subexpr abstraction)

               ;; Collect the LET-node bindings. We also generate new
               ;; generated variable names (i.e., each xi is renamed to
               ;; a unique xi').
               :for var :in vars
               :for val :in vals
               :for new-var := (gensym (symbol-name var))
               :collect (cons new-var val)
                 :into bindings
               :collect (make-ast-substitution
                         :from var
                         :to (make-node-variable
                              :type (node-type val)
                              :value new-var))
                 :into subs

               ;; Return the inlined abstraction as a LET.
               :finally (return
                          (make-node-let
                           :type     (node-type node)
                           :bindings bindings
                           :subexpr  (funcall
                                      process-body
                                      ;; Rename the type variables
                                      ;; so they aren't
                                      ;; inadvertently unified
                                      ;; across substitutions.
                                      (rename-type-variables
                                       (apply-ast-substitution subs body t)))))))

           (try-inline (node call-stack)
             "Attempt to perform an inlining of the application node NODE. The
            CALL-STACK is a stack (represented as a list) of node names seen so
            far recursively by the inliner."
             (declare (type (or node-application node-direct-application) node)
                      (type parser:identifier-list                        call-stack)
                      (values (or node-let node-application node-direct-application) &optional))
             ;; There are multiple cases that can be inlined.
             ;;
             ;; Case #1: (f e1 ... en) where f is global, known, and arity n.
             (alexandria:when-let*
                 ((name (node-rator-name node))
                  (code (tc:lookup-code env name :no-error t))
                  (_    (and (node-abstraction-p code)
                             (or (alexandria:when-let (fun-env-entry (tc:lookup-function env name :no-error t))
                                   (tc:function-env-entry-inline-p fun-env-entry))
                                 (funcall heuristic code))
                             (<= (length call-stack)
                                 max-depth)
                             (<= (count name call-stack)
                                 max-unroll)
                             (= (length (node-abstraction-vars code))
                                (length (node-rands node))))))
               (return-from try-inline
                 (inline-abstraction-from-application
                  node code (lambda (body)
                              (funcall *traverse* body (cons name call-stack))))))

             ;; Case #2: ((fn (x1 ... xn) ...) e1 ... en)
             (when (node-application-p node)
               (let ((code (node-application-rator node)))
                 (when (and (node-abstraction-p code)
                            (funcall heuristic code)
                            (= (length (node-abstraction-vars code))
                               (length (node-rands node))))
                   (return-from try-inline
                     (inline-abstraction-from-application
                      node code (lambda (body)
                                  (funcall *traverse* body call-stack)))))))

             ;; Inlining did not satisfy heuristics or was inapplicable.
             (return-from try-inline node)))
    (traverse
     node
     (list
      (action (:after node-application) #'try-inline)
      (action (:after node-direct-application) #'try-inline)
      (make-traverse-let-action-skipping-cons-bindings))
     nil)))

(defun inline-applications (node env)
  "Perform inlining on NODE in the environment ENV.

If heuristic inlining is enabled, then a default inlining heuristic
*INLINER-HEURISTIC* will be used to decide if a node should be
inlined."
  (if settings:*coalton-heuristic-inlining*
      (heuristic-inline-applications node env :heuristic *inliner-heuristic*)
      (heuristic-inline-applications node env :heuristic 'null-heuristic)))

(defun inline-methods (node env)
  (declare (type node node)
           (type tc:environment env)
           (values node &optional))
  (labels ((inline-method (node)
             (let ((rator (node-application-rator node))
                   (rands (node-application-rands node)))
               (when (node-variable-p rator)
                 (let (dict rands_)
                   (cond
                     ((node-variable-p (first rands))
                      (setf dict (node-variable-value (first rands)))
                      (setf rands_ (cdr rands)))

                     ((and (node-application-p (first rands))
                           (node-variable-p (node-application-rator (first rands))))
                      (setf dict (node-variable-value (node-application-rator (first rands))))
                      (setf rands_ (append (node-application-rands (first rands)) (cdr rands))))

                     (t
                      (return-from inline-method nil)))

                   (let* ((method-name (node-variable-value rator))
                          (inline-method-name (tc:lookup-method-inline env method-name dict :no-error t)))

                     (when inline-method-name
                       (if (null rands_)
                           (make-node-variable
                            :type (node-type node)
                            :value inline-method-name)

                           (make-node-application
                            :type (node-type node)
                            :rator (make-node-variable
                                    :type (tc:make-function-type*
                                           (mapcar #'node-type rands_)
                                           (node-type node))
                                    :value inline-method-name)
                            :rands rands_))))))))

           (inline-direct-method (node)
             (let ((rands (node-direct-application-rands node)))
               (let (dict rands_)
                 (cond
                   ((node-variable-p (first rands))
                    (setf dict (node-variable-value (first rands)))
                    (setf rands_ (cdr rands)))

                   ((and (node-application-p (first rands))
                         (node-variable-p (node-application-rator (first rands))))
                    (setf dict (node-variable-value (node-application-rator (first rands))))
                    (setf rands_ (append (node-application-rands (first rands)) (cdr rands))))

                   (t
                    (return-from inline-direct-method nil)))

                 (let* ((method-name (node-direct-application-rator node))
                        (inline-method-name (tc:lookup-method-inline env method-name dict :no-error t)))
                   (when inline-method-name
                     (if (null rands_)
                         (make-node-variable
                          :type (node-type node)
                          :value inline-method-name)

                         (make-node-application
                          :type (node-type node)
                          :rator (make-node-variable
                                  :type (tc:make-function-type*
                                         (mapcar #'node-type rands_)
                                         (node-type node))
                                  :value inline-method-name)
                          :rands rands_))))))))

    (traverse
     node
     (list
      (action (:after node-application) #'inline-method)
      (action (:after node-direct-application) #'inline-direct-method)))))

