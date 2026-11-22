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

(defun find-unused-variables (binding)
  (declare (type (or tc:toplevel-define tc:instance-method-definition) binding))
  (let ((used-variables (make-hash-table :test #'eq)))

    ;; Mark used variables
    (tc:visit
     (tc:binding-value binding)
     (tc:make-traverse-block
      :variable (lambda (node)
                  (declare (type tc:node-variable node))
                  (setf (gethash (tc:node-variable-name node) used-variables) t))))

    ;; Check for unused parameters in the binding
    (loop :for var :in (tc:pattern-variables (tc:binding-parameters binding))
          :do (variable-binding var used-variables))

    ;; Check for unused variables in the body
    (tc:visit
     (tc:binding-value binding)
     (tc:make-traverse-block
      :bind (lambda (node)
              (declare (type tc:node-bind node))
              (loop :for var :in (tc:pattern-variables (tc:node-bind-pattern node))
                    :do (variable-binding var used-variables)))
      :values-bind (lambda (node)
                     (declare (type tc:node-values-bind node))
                     (loop :for var :in (tc:pattern-variables (tc:node-values-bind-patterns node))
                           :do (variable-binding var used-variables)))
      :do-bind (lambda (node)
                 (declare (type tc:node-do-bind node))
                 (loop :for var :in (tc:pattern-variables (tc:node-do-bind-pattern node))
                       :do (variable-binding var used-variables)))
      :match-branch (lambda (node)
                      (declare (type tc:node-match-branch node))
                      (loop :for var :in (tc:pattern-variables (tc:node-match-branch-pattern node))
                            :do (variable-binding var used-variables)))
      :let (lambda (node)
             (declare (type tc:node-let node))
             (loop :for binding :in (tc:node-let-bindings node)
                   :do (variable-binding (tc:node-let-binding-name binding) used-variables)))
      :abstraction (lambda (node)
                     (declare (type tc:node-abstraction node))
                     (loop :for var :in (tc:pattern-variables (tc:node-abstraction-params node))
                           :do (variable-binding var used-variables)))))))

(defun variable-binding (var used-variables)
  (declare (type (or tc:node-variable tc:pattern-var) var)
           (type hash-table used-variables))

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

    (unless (char= (aref (symbol-name name) 0) #\_)
        (unless (gethash name used-variables)
          (source:warn "Unused variable"
                       (source:note location "variable defined here")
                       (source:help location
                                    (lambda (name)
                                      (concatenate 'string "_" name))
                                    "prefix the variable with '_' to declare it unused"))))))
