(defpackage #:coalton-impl/analysis/unused-variables
  (:use
   #:cl)
  (:local-nicknames
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:find-unused-variables))

(in-package #:coalton-impl/analysis/unused-variables)

(defun keyword-stage-entry-count (qualified-type)
  (declare (type tc:qualified-ty qualified-type)
           (values (integer 0 #.most-positive-fixnum) &optional))
  (let ((ty (tc:qualified-ty-type qualified-type)))
    (loop :while (and (tc:function-type-p ty)
                      (not (typep ty 'tc:keyword-stage-ty)))
          :do (setf ty (tc:function-type-to ty)))
    (if (typep ty 'tc:keyword-stage-ty)
        (length (tc:keyword-stage-ty-entries ty))
        0)))

(defun simple-pattern-var-name (pattern)
  (declare (type tc:pattern pattern)
           (values (or null symbol) &optional))
  (when (typep pattern 'tc:pattern-var)
    (tc:pattern-var-name pattern)))

(defun keyword-default-branch-kind (branch)
  (declare (type tc:node-match-branch branch)
           (values (member nil :some :none) &optional))
  (let ((pattern (tc:node-match-branch-pattern branch)))
    (unless (typep pattern 'tc:pattern-constructor)
      (return-from keyword-default-branch-kind nil))
    (cond
      ((eq (tc:pattern-constructor-name pattern) 'coalton:Some)
       (if (= 1 (length (tc:pattern-constructor-patterns pattern)))
           ':some
           nil))
      ((eq (tc:pattern-constructor-name pattern) 'coalton:None)
       (if (null (tc:pattern-constructor-patterns pattern))
           ':none
           nil))
      (t nil))))

(defun keyword-default-match-p (node hidden-name)
  (declare (type tc:node node)
           (type symbol hidden-name)
           (values boolean &optional))
  (unless (typep node 'tc:node-match)
    (return-from keyword-default-match-p nil))
  (let ((expr (tc:node-match-expr node))
        (branches (tc:node-match-branches node)))
    (unless (and (typep expr 'tc:node-variable)
                 (eq (tc:node-variable-name expr) hidden-name)
                 (= 2 (length branches)))
      (return-from keyword-default-match-p nil))
    (let ((branch-a (keyword-default-branch-kind (first branches)))
          (branch-b (keyword-default-branch-kind (second branches))))
      (or (and (eq branch-a ':some)
               (eq branch-b ':none))
          (and (eq branch-a ':none)
               (eq branch-b ':some))))))

(defun keyword-bind-expression-p (node hidden-name)
  (declare (type tc:node node)
           (type symbol hidden-name)
           (values boolean &optional))
  (or (and (typep node 'tc:node-variable)
           (eq (tc:node-variable-name node) hidden-name))
      (keyword-default-match-p node hidden-name)))

(defun mark-keyword-binder-names-in-lambda-shape (params body qualified-type ignored-names)
  (declare (type tc:pattern-list params)
           (type tc:node-body body)
           (type tc:qualified-ty qualified-type)
           (type hash-table ignored-names)
           (values null &optional))
  (let ((keyword-count (keyword-stage-entry-count qualified-type)))
    (when (plusp keyword-count)
      (let* ((body-nodes (tc:node-body-nodes body))
             (param-count (length params)))
        (when (and (<= keyword-count param-count)
                   (<= keyword-count (length body-nodes)))
          (loop :for hidden-param :in (nthcdr (- param-count keyword-count) params)
                :for body-node :in body-nodes
                :repeat keyword-count
                :for hidden-name := (and (typep hidden-param 'tc:pattern-var)
                                         (tc:pattern-var-name hidden-param))
                :when (and hidden-name
                           (typep body-node 'tc:node-bind))
                  :do (let ((bind-name (simple-pattern-var-name (tc:node-bind-pattern body-node))))
                        (when (and bind-name
                                   (keyword-bind-expression-p (tc:node-bind-expr body-node)
                                                              hidden-name))
                          (setf (gethash bind-name ignored-names) t))))))))
  nil)

(defun collect-keyword-binder-names (binding)
  (declare (type (or tc:toplevel-define tc:instance-method-definition) binding)
           (values hash-table &optional))
  (let ((ignored-names (make-hash-table :test #'eq)))
    (mark-keyword-binder-names-in-lambda-shape
     (tc:binding-parameters binding)
     (tc:binding-value binding)
     (tc:binding-type binding)
     ignored-names)
    (tc:traverse
     (tc:binding-value binding)
     (tc:make-traverse-block
      :abstraction (lambda (node)
                     (declare (type tc:node-abstraction node))
                     (mark-keyword-binder-names-in-lambda-shape
                      (tc:node-abstraction-params node)
                      (tc:node-abstraction-body node)
                      (tc:node-type node)
                      ignored-names)
                     node)))
    ignored-names))

(defun find-unused-variables (binding)
  (declare (type (or tc:toplevel-define tc:instance-method-definition) binding))
  (let ((used-variables (make-hash-table :test #'eq))
        (ignored-keyword-binders (collect-keyword-binder-names binding)))

    ;; Mark used variables
    (tc:traverse
     (tc:binding-value binding)
     (tc:make-traverse-block
      :variable (lambda (node)
                  (declare (type tc:node-variable node))
                  (unless (gethash (tc:node-variable-name node) used-variables)
                    (setf (gethash (tc:node-variable-name node) used-variables) node))
                  node)))

    ;; Check for unused parameters in the binding
    (loop :for var :in (tc:pattern-variables (tc:binding-parameters binding))
          :do (variable-binding var used-variables ignored-keyword-binders))

    ;; Check for unused variables in the body
    (tc:traverse
     (tc:binding-value binding)
     (tc:make-traverse-block
      :bind (lambda (node)
              (declare (type tc:node-bind))
              (loop :for var :in (tc:pattern-variables (tc:node-bind-pattern node))
                    :do (variable-binding var used-variables ignored-keyword-binders))
              node)
      :do-bind (lambda (node)
                 (declare (type tc:node-do-bind))
                 (loop :for var :in (tc:pattern-variables (tc:node-do-bind-pattern node))
                       :do (variable-binding var used-variables ignored-keyword-binders))
                 node)
      :match-branch (lambda (node)
                      (declare (type tc:node-match-branch))
                      (loop :for var :in (tc:pattern-variables (tc:node-match-branch-pattern node))
                            :do (variable-binding var used-variables ignored-keyword-binders))
                      node)
      :let (lambda (node)
             (declare (type tc:node-let node))
             (loop :for binding :in (tc:node-let-bindings node)
                   :do (variable-binding (tc:node-let-binding-name binding)
                                         used-variables
                                         ignored-keyword-binders))
             node)
      :abstraction (lambda (node)
                     (declare (type tc:node-abstraction node))
                     (loop :for var :in (tc:pattern-variables (tc:node-abstraction-params node))
                           :do (variable-binding var used-variables ignored-keyword-binders))
                     node)))))

(defun variable-binding (var used-variables ignored-keyword-binders)
  (declare (type (or tc:node-variable tc:pattern-var) var)
           (type hash-table used-variables)
           (type hash-table ignored-keyword-binders))

  (destructuring-bind (name . location)
      (etypecase var
        (tc:node-variable
         (cons
          (tc:node-variable-name var)
          (source:location var)))
        (tc:pattern-var
         (cons
          (tc:pattern-var-name var)
          (source:location var))))

    (unless (or (char= (aref (symbol-name name) 0) #\_)
                (gethash name ignored-keyword-binders))
        (unless (gethash name used-variables)
          (source:warn "Unused variable"
                       (source:note location "variable defined here")
                       (source:help location
                                    (lambda (name)
                                      (concatenate 'string "_" name))
                                    "prefix the variable with '_' to declare it unused"))))))
