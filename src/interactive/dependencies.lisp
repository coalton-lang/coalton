(defpackage #:coalton-impl/interactive/dependencies
  (:use
   #:cl)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:extract-function-dependencies))
(in-package #:coalton-impl/interactive/dependencies)

;;;
;;; Dependency Extraction with Lexical Scope Tracking
;;;

(defun extract-function-dependencies (node env)
  "Extract global function calls from NODE.
   Returns a list of symbols that are verified to be functions in ENV.
   Filters out locally-bound variables by tracking lexical scope."
  (declare (type parser:node node)
           (type tc:environment env)
           (values list))

  (let ((deps nil)
        (seen (make-hash-table :test #'eq))) ; For deduplication

    (labels ((add-dependency (name)
               "Add NAME to dependencies if it's a function in ENV."
               (when (and (not (gethash name seen))
                         (tc:lookup-value-type env name :no-error t))
                 (setf (gethash name seen) t)
                 (push name deps)))

             (traverse (node local-bindings)
               "Traverse NODE with LOCAL-BINDINGS hash-set tracking local variables."
               (declare (type parser:node node)
                        (type hash-table local-bindings))

               (etypecase node
                 ;; Variable reference - check if local or global
                 (parser:node-variable
                  (let ((name (parser:node-variable-name node)))
                    (unless (gethash name local-bindings)
                      (add-dependency name))))

                 ;; Let binding - add bindings to scope
                 (parser:node-let
                  (let ((new-locals (alexandria:copy-hash-table local-bindings)))
                    ;; First traverse values with current scope
                    ;; (bindings can't reference each other in same let)
                    (dolist (binding (parser:node-let-bindings node))
                      (traverse (parser:node-let-binding-value binding)
                                local-bindings))
                    ;; Add let-bound names to scope
                    (dolist (binding (parser:node-let-bindings node))
                      (let* ((name-node (parser:node-let-binding-name binding))
                             (name (parser:node-variable-name name-node)))
                        (setf (gethash name new-locals) t)))
                    ;; Traverse body with new scope
                    (traverse (parser:node-let-body node) new-locals)))

                 ;; Lambda - parameters shadow
                 (parser:node-abstraction
                  (let ((new-locals (alexandria:copy-hash-table local-bindings)))
                    ;; Add parameters to local scope
                    ;; node-abstraction-params returns pattern-list
                    (dolist (pattern (parser:node-abstraction-params node))
                      (dolist (pvar (parser:pattern-variables pattern))
                        (setf (gethash (parser:pattern-var-name pvar) new-locals) t)))
                    (traverse (parser:node-abstraction-body node) new-locals)))

                 ;; Function application
                 (parser:node-application
                  (traverse (parser:node-application-rator node) local-bindings)
                  (dolist (rand (parser:node-application-rands node))
                    (traverse rand local-bindings)))

                 ;; Literals have no variables
                 (parser:node-literal
                  nil)

                 (parser:node-integer-literal
                  nil)

                 ;; Body node (used in let, lambda, etc.)
                 (parser:node-body
                  (dolist (elem (parser:node-body-nodes node))
                    (traverse elem local-bindings))
                  (traverse (parser:node-body-last-node node) local-bindings))

                 ;; TODO: Add more node types incrementally
                 )))

      ;; Start traversal with empty local bindings
      (traverse node (make-hash-table :test #'eq))

      ;; Return dependencies in order found (reversed due to push)
      (nreverse deps))))
