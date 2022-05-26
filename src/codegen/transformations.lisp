(defpackage #:coalton-impl/codegen/transformations
  (:use
   #:cl
   #:coalton-impl/util
   #:coalton-impl/codegen/ast)
  (:import-from
   #:coalton-impl/algorithm
   #:immutable-map-data)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:traverse
   #:traverse-bindings
   #:update-function-env
   #:make-function-t))

(in-package #:coalton-impl/codegen/transformations)

(defun traverse-bindings (bindings funs)
  (declare (type binding-list bindings)
           (values binding-list))
  (loop :for (name . node) :in bindings
        :collect (cons name (traverse node funs nil))))

(defun call-if (node key funs bound-variables)
  (declare (type node node)
           (type symbol key)
           (values node &optional))
  (let ((f (cdr (find key funs :key #'car))))
    (if f
        (or (funcall f node :bound-variables bound-variables) node)
        node)))

(defgeneric traverse (node funs bound-variables)
  (:method ((node node-literal) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (call-if node :literal funs bound-variables))

  (:method ((node node-variable) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (call-if node :variable funs bound-variables))

  (:method ((node node-application) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (let ((node
            (node-application
             (node-type node)
             (traverse (node-application-rator node) funs bound-variables)
             (mapcar
              (lambda (node)
                (traverse node funs bound-variables))
              (node-application-rands node)))))
      (call-if node :application funs bound-variables)))

  (:method ((node node-direct-application) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (let ((node
            (node-direct-application
             (node-type node)
             (node-direct-application-rator-type node)
             (node-direct-application-rator node)
             (mapcar
              (lambda (node)
                (traverse node funs bound-variables))
              (node-direct-application-rands node)))))
      (call-if node :direct-application funs bound-variables)))

  (:method ((node node-abstraction) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (call-if node :before-abstraction funs bound-variables)
    (let ((node
            (node-abstraction
             (node-type node)
             (node-abstraction-vars node)
             (traverse (node-abstraction-subexpr node) funs (append (node-abstraction-vars node) bound-variables)))))
      (call-if node :abstraction funs bound-variables)))

  (:method ((node node-bare-abstraction) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (call-if node :before-bare-abstraction funs bound-variables)
    (let ((node
            (node-bare-abstraction
             (node-type node)
             (node-bare-abstraction-vars node)
             (traverse (node-bare-abstraction-subexpr node) funs (append (node-bare-abstraction-vars node) bound-variables)))))
      (call-if node :bare-abstraction funs bound-variables)))

  (:method ((node node-let) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (call-if node :before-let funs bound-variables)
    (let* ((new-bound-variables (append (mapcar #'car (node-let-bindings node)) bound-variables))
           (node
             (node-let
              (node-type node)
              (loop :for (name . node) :in (node-let-bindings node)
                    :collect (cons name (traverse node funs new-bound-variables)))
              (traverse (node-let-subexpr node) funs new-bound-variables))))
      (call-if node :let funs bound-variables)))

  (:method ((node node-lisp) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (call-if node :lisp funs bound-variables))

  (:method ((node match-branch) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (match-branch
     (match-branch-pattern node)
     (match-branch-bindings node)
     (traverse (match-branch-body node) funs (append (mapcar #'car (match-branch-bindings node)) bound-variables))))

  (:method ((node node-match) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (let ((node
            (node-match
             (node-type node)
             (traverse (node-match-expr node) funs bound-variables)
             (mapcar
              (lambda (node)
                (traverse node funs bound-variables))
              (node-match-branches node)))))
      (call-if node :match funs bound-variables)))

  (:method ((node node-seq) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (let ((node
            (node-seq
             (node-type node)
             (mapcar
              (lambda (node)
                (traverse node funs bound-variables))
              (node-seq-nodes node)))))
      (call-if node :seq funs bound-variables)))

  (:method ((node node-return) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (let ((node
            (node-return
             (node-type node)
             (traverse (node-return-expr node) funs bound-variables))))
      (call-if node :return funs bound-variables)))

  (:method ((node node-field) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (let ((node
            (node-field
             (node-type node)
             (node-field-name node)
             (traverse (node-field-dict node) funs bound-variables))))
      (call-if node :field funs bound-variables)))

  (:method ((node node-dynamic-extent) funs bound-variables)
    (declare (type symbol-list bound-variables))
    (let* ((new-bound-variables (cons (node-dynamic-extent-name node) bound-variables))

           (node
             (node-dynamic-extent
              (node-type node)
              (node-dynamic-extent-name node)
              (traverse (node-dynamic-extent-node node) funs new-bound-variables)
              (traverse (node-dynamic-extent-body node) funs new-bound-variables))))
      (call-if node :dynamic-extent funs bound-variables))))

(defun split-binding-definitions (bindings)
  (let ((functions nil)
        (variables nil))
    (loop :for (name . node) :in bindings
          :do (if (node-abstraction-p node)
                  (push (cons name (length (node-abstraction-vars node))) functions)
                  (push name variables)))
    (values functions variables)))

(defun update-function-env (bindings env)
  (declare (type binding-list bindings)
           (type tc:environment env)
           (values tc:environment))
  (multiple-value-bind (toplevel-functions toplevel-values)
      (split-binding-definitions bindings)
    (loop :for (name . arity) :in toplevel-functions
          :do
             (setf env
                   (tc:set-function
                    env
                    name
                    (tc:make-function-env-entry
                     :name name
                     :arity arity))))
    (loop :for name :in toplevel-values
          :do
             (setf env (tc:unset-function env name))))
  env)

(defun make-function-table (env)
  (declare (type tc:environment env)
           (values hash-table))
  (let ((table (make-hash-table)))
    (fset:do-map (name entry (immutable-map-data (tc:environment-function-environment env)))
      (setf (gethash name table) (tc:function-env-entry-arity entry)))
    table))
