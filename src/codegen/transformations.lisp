(defpackage #:coalton-impl/codegen/transformations
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:canonicalize
   #:pointfree
   #:direct-application))

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
              (node-application-rands node)))))
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
    (let ((node
            (node-abstraction
             (node-type node)
             (node-abstraction-vars node)
             (traverse (node-abstraction-subexpr node) funs))))
      (call-if node :direct-application funs)))

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
      (call-if node :seq funs))))
