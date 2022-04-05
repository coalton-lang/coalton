(defpackage #:coalton-impl/codegen/transformations
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/codegen/hoister
   #:hoister
   #:push-hoist-point
   #:pop-hoist-point
   #:hoist-definition)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker)
   (#:ast #:coalton-impl/ast))
  (:export
   #:canonicalize
   #:pointfree
   #:direct-application
   #:inline-methods
   #:static-dict-lift))

(in-package #:coalton-impl/codegen/transformations)

(defun pointfree (node)
  (declare (type node node)
           (values node))
  (declare (type node node)
           (values node))
  (if (node-abstraction-p node)
      (return-from pointfree node))

  (if (not (tc:function-type-p (node-type node)))
      (return-from pointfree node))

  (let* ((arguments (tc:function-type-arguments (node-type node)))
         (argument-names (loop :for argument :in arguments
                               :collect (gensym))))

         (node-abstraction
           (node-type node)
           argument-names
           (node-application
            (tc:function-return-type (node-type node))
            node
            (loop :for ty :in arguments
                  :for name :in argument-names
                  :collect (node-variable ty name))))))

(defun canonicalize (node)
  (declare (type node node)
           (values node &optional))
  (labels ((rewrite-application (node)
             (let ((rator (node-application-rator node))
                   (rands (node-application-rands node)))
               (when (node-application-p rator)
                 (node-application
                  (node-type node)
                  (node-application-rator rator)
                  (append
                   (node-application-rands rator)
                   rands))))))
    (traverse
     node
     (list
      (cons :application #'rewrite-application)))))

(defun direct-application (node table)
  (declare (type node node)
           (type hash-table table)
           (values node &optional))
  (labels ((rewrite-direct-application (node)
             (when (node-variable-p (node-application-rator node))
               (let ((name (node-variable-value (node-application-rator node))))
                 (when (and (gethash name table)
                            (equalp (gethash name table)
                                    (length (node-application-rands node))))
                   (return-from rewrite-direct-application
                     (node-direct-application
                      (node-type node)
                      (node-type (node-application-rator node))
                      name
                      (mapcar
                       (lambda (node)
                         (direct-application node table))
                       (node-application-rands node))))))))

           (add-local-funs (node)
             (loop :for (name . node) :in (node-let-bindings node)
                   :when (node-abstraction-p node) :do
                     (setf (gethash name table) (length (node-abstraction-vars node))))))

    (traverse
     node
     (list
      (cons :application #'rewrite-direct-application)
      (cons :before-let #'add-local-funs)))))

(defun inline-methods (node env)
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
                           (node-variable
                            (node-type node)
                            inline-method-name)

                           (node-application
                            (node-type node)
                            (node-variable
                             (tc:make-function-type*
                              (mapcar #'node-type rands_)
                              (node-type node))
                             inline-method-name)
                            rands_))))))))) 
    (traverse
     node
     (list
      (cons :application #'inline-method)))))

(defun static-dict-lift (node hoister package env)
  (declare (type node node)
           (type hoister hoister)
           (type package package)
           (ignore env))
  (labels ((lift-static-dict (node)
             ;; Only pure functions can be lifted
             (unless (node-application-pure node)
               (return-from lift-static-dict nil))

             (hoist-definition node package hoister))

           (handle-push-hoist-point (node)
             (push-hoist-point (node-abstraction-vars node) hoister)
             nil)

           (handle-push-bare-hoist-point (node)
             (push-hoist-point (node-bare-abstraction-vars node) hoister)
             nil)

           (handle-pop-hoist-point (node)
             ;; If definitions were hoisted to this lambda
             ;; then add them to the ast
             (let ((hoisted (pop-hoist-point hoister)))
               (if hoisted
                   (node-abstraction
                    (node-type node)
                    (node-abstraction-vars node)
                    (node-let
                     (node-type (node-abstraction-subexpr node))
                     hoisted
                     (node-abstraction-subexpr node)))

                   node)))

           (handle-pop-bare-hoist-point (node)
             (let ((hoisted (pop-hoist-point hoister)))
               (if hoisted
                   (node-bare-abstraction
                    (node-type node)
                    (node-bare-abstraction-vars node)
                    (node-let
                     (node-type (node-bare-abstraction-subexpr node))
                     hoisted
                     (node-bare-abstraction-subexpr node)))

                   node))))

    (traverse
     node
     (list
      (cons :application #'lift-static-dict)
      (cons :before-abstraction #'handle-push-hoist-point)
      (cons :abstraction #'handle-pop-hoist-point)
      (cons :before-bare-abstraction #'handle-push-bare-hoist-point)
      (cons :bare-abstraction #'handle-pop-bare-hoist-point)))))

(defun call-if (node key funs)
  (declare (type node node)
           (type symbol key)
           (values node &optional))
  (let ((f (cdr (find key funs :key #'car))))
    (if f
        (or (funcall f node) node)
        node)))

(defgeneric traverse (node funs)
  (:method ((node node-literal) funs)
    (call-if node :literal funs))

  (:method ((node node-variable) funs)
    (call-if node :variable funs))

  (:method ((node node-application) funs)
    (let ((node
            (node-application
             (node-type node)
             (traverse (node-application-rator node) funs)
             (mapcar
              (lambda (node)
                (traverse node funs))
              (node-application-rands node))
             :pure (node-application-pure node))))
      (call-if node :application funs)))

  (:method ((node node-direct-application) funs)
    (let ((node
            (node-direct-application
             (node-type node)
             (node-direct-application-rator-type node)
             (node-direct-application-rator node)
             (mapcar
              (lambda (node)
                (traverse node funs))
              (node-direct-application-rands node)))))
      (call-if node :direct-application funs)))

  (:method ((node node-abstraction) funs)
    (call-if node :before-abstraction funs)
    (let ((node
            (node-abstraction
             (node-type node)
             (node-abstraction-vars node)
             (traverse (node-abstraction-subexpr node) funs))))
      (call-if node :abstraction funs)))

  (:method ((node node-bare-abstraction) funs)
    (call-if node :before-bare-abstraction funs)
    (let ((node
            (node-bare-abstraction
             (node-type node)
             (node-bare-abstraction-vars node)
             (traverse (node-bare-abstraction-subexpr node) funs))))
      (call-if node :bare-abstraction funs)))

  (:method ((node node-let) funs)
    (call-if node :before-let funs)
    (let ((node
            (node-let
             (node-type node)
             (loop :for (name . node) :in (node-let-bindings node)
                   :collect (cons name (traverse node funs)))
             (traverse (node-let-subexpr node) funs))))
      (call-if node :let funs)))

  (:method ((node node-lisp) funs)
    (call-if node :lisp funs))

  (:method ((node match-branch) funs)
    (match-branch
     (match-branch-pattern node)
     (match-branch-bindings node)
     (traverse (match-branch-body node) funs)))

  (:method ((node node-match) funs)
    (let ((node
            (node-match
             (node-type node)
             (traverse (node-match-expr node) funs)
             (mapcar
              (lambda (node)
                (traverse node funs))
              (node-match-branches node)))))
      (call-if node :match funs)))

  (:method ((node node-seq) funs)
    (let ((node
            (node-seq
             (node-type node)
             (mapcar
              (lambda (node)
                (traverse node funs))
              (node-seq-nodes node)))))
      (call-if node :seq funs)))

  (:method ((node node-return) funs)
    (let ((node
            (node-return
             (node-type node)
             (traverse (node-return-expr node) funs))))
      (call-if node :return funs))))
