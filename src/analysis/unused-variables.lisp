(defpackage #:coalton-impl/analysis/unused-variables
  (:use
   #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:find-unused-variables              ; FUNCTION
   #:unused-variable-warning            ; CONDITION
   ))

(in-package #:coalton-impl/analysis/unused-variables)

(define-condition unused-variable-warning (error:coalton-base-warning)
  ())

(defun find-unused-variables (binding file)
  (declare (type (or tc:toplevel-define tc:instance-method-definition) binding)
           (type error:coalton-file file))

  (let ((used-variables (make-hash-table :test #'eq)))

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
    (unless (tc:binding-nullary binding)
      (loop :for var :in (tc:binding-parameters binding)
            :do (variable-binding var used-variables file)))

    ;; Check for unused variables in the body
    (tc:traverse
     (tc:binding-value binding)
     (tc:make-traverse-block
      :bind (lambda (node)
              (declare (type tc:node-bind))
              (loop :for var :in (tc:pattern-variables (tc:node-bind-pattern node))
                    :do (variable-binding var used-variables file))
              node)
      :do-bind (lambda (node)
                 (declare (type tc:node-do-bind))
                 (loop :for var :in (tc:pattern-variables (tc:node-do-bind-pattern node))
                       :do (variable-binding var used-variables file))
                 node)
      :match-branch (lambda (node)
                      (declare (type tc:node-match-branch))
                      (loop :for var :in (tc:pattern-variables (tc:node-match-branch-pattern node))
                            :do (variable-binding var used-variables file))
                      node)
      :let (lambda (node)
             (declare (type tc:node-let node))
             (loop :for binding :in (tc:node-let-bindings node)
                   :do (variable-binding (tc:node-let-binding-name binding) used-variables file))
             node)
      :abstraction (lambda (node)
                     (declare (type tc:node-abstraction node))
                     (unless (tc:node-abstraction-nullary node)
                       (loop :for var :in (tc:node-abstraction-vars node)
                             :do (variable-binding var used-variables file)))
                     node)))))

(defun variable-binding (var used-variables file)
  (declare (type (or tc:node-variable tc:pattern-var) var)
           (type hash-table used-variables)
           (type error:coalton-file file))

  (destructuring-bind (name . source)
      (etypecase var
        (tc:node-variable
         (cons
          (tc:node-variable-name var)
          (tc:node-source var)))
        (tc:pattern-var
         (cons
          (tc:pattern-var-name var)
          (tc:pattern-source var))))

    (unless (char= (aref (symbol-name name) 0) #\_)
        (unless (gethash name used-variables)
          (warn 'unused-variable-warning
                :err (error:coalton-error
                      :type :warn
                      :file file
                      :span source
                      :message "Unused variable"
                      :primary-note "variable defined here"
                      :help-notes
                      (list
                       (error:make-coalton-error-help
                        :span source
                        :replacement (lambda (name)
                                       (concatenate 'string "_" name))
                        :message "prefix the variable with '_' to decare it unused"))))))))
