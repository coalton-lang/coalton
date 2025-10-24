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

                 ;; Match - pattern variables shadow
                 (parser:node-match
                  (traverse (parser:node-match-expr node) local-bindings)
                  (dolist (branch (parser:node-match-branches node))
                    (let ((new-locals (alexandria:copy-hash-table local-bindings))
                          (pattern (parser:node-match-branch-pattern branch)))
                      ;; Extract and add pattern variables
                      (dolist (pvar (parser:pattern-variables pattern))
                        (setf (gethash (parser:pattern-var-name pvar) new-locals) t))
                      (traverse (parser:node-match-branch-body branch) new-locals))))

                 ;; Conditionals - simple traversal
                 (parser:node-if
                  (traverse (parser:node-if-expr node) local-bindings)
                  (traverse (parser:node-if-then node) local-bindings)
                  (traverse (parser:node-if-else node) local-bindings))

                 (parser:node-when
                  (traverse (parser:node-when-expr node) local-bindings)
                  (traverse (parser:node-when-body node) local-bindings))

                 (parser:node-unless
                  (traverse (parser:node-unless-expr node) local-bindings)
                  (traverse (parser:node-unless-body node) local-bindings))

                 (parser:node-and
                  (dolist (subnode (parser:node-and-nodes node))
                    (traverse subnode local-bindings)))

                 (parser:node-or
                  (dolist (subnode (parser:node-or-nodes node))
                    (traverse subnode local-bindings)))

                 (parser:node-cond
                  (dolist (clause (parser:node-cond-clauses node))
                    (traverse (parser:node-cond-clause-expr clause) local-bindings)
                    (traverse (parser:node-cond-clause-body clause) local-bindings)))

                 ;; The (type annotation)
                 (parser:node-the
                  (traverse (parser:node-the-expr node) local-bindings))

                 ;; Progn
                 (parser:node-progn
                  (traverse (parser:node-progn-body node) local-bindings))

                 ;; Do notation - bind shadows sequentially
                 (parser:node-do
                  ;; Process do elements sequentially with accumulating scope
                  (let ((current-locals local-bindings))
                    (dolist (elem (parser:node-do-nodes node))
                      (etypecase elem
                        (parser:node-do-bind
                         (traverse (parser:node-do-bind-expr elem) current-locals)
                         ;; Add bindings for next elements
                         (let ((new-locals (alexandria:copy-hash-table current-locals)))
                           (dolist (pvar (parser:pattern-variables
                                          (parser:node-do-bind-pattern elem)))
                             (setf (gethash (parser:pattern-var-name pvar) new-locals) t))
                           (setf current-locals new-locals)))
                        (parser:node-bind
                         (traverse (parser:node-bind-expr elem) current-locals)
                         ;; Regular let binding in do
                         (let ((new-locals (alexandria:copy-hash-table current-locals)))
                           (dolist (pvar (parser:pattern-variables
                                          (parser:node-bind-pattern elem)))
                             (setf (gethash (parser:pattern-var-name pvar) new-locals) t))
                           (setf current-locals new-locals)))
                        (parser:node
                         ;; Any other expression node
                         (traverse elem current-locals))))
                    ;; Last node
                    (traverse (parser:node-do-last-node node) current-locals)))

                 ;; Bind (shorthand let binding)
                 (parser:node-bind
                  (traverse (parser:node-bind-expr node) local-bindings))

                 ;; Loops
                 (parser:node-while
                  (traverse (parser:node-while-expr node) local-bindings)
                  (traverse (parser:node-while-body node) local-bindings))

                 (parser:node-while-let
                  (traverse (parser:node-while-let-expr node) local-bindings)
                  (let ((new-locals (alexandria:copy-hash-table local-bindings)))
                    (dolist (pvar (parser:pattern-variables
                                   (parser:node-while-let-pattern node)))
                      (setf (gethash (parser:pattern-var-name pvar) new-locals) t))
                    (traverse (parser:node-while-let-body node) new-locals)))

                 (parser:node-for
                  (traverse (parser:node-for-expr node) local-bindings)
                  (let ((new-locals (alexandria:copy-hash-table local-bindings)))
                    (dolist (pvar (parser:pattern-variables
                                   (parser:node-for-pattern node)))
                      (setf (gethash (parser:pattern-var-name pvar) new-locals) t))
                    (traverse (parser:node-for-body node) new-locals)))

                 (parser:node-loop
                  (traverse (parser:node-loop-body node) local-bindings))

                 (parser:node-break
                  nil) ; No expressions

                 (parser:node-continue
                  nil) ; No expressions

                 ;; Effect handlers - pattern variables shadow
                 (parser:node-catch
                  (traverse (parser:node-catch-expr node) local-bindings)
                  (dolist (branch (parser:node-catch-branches node))
                    (let ((new-locals (alexandria:copy-hash-table local-bindings))
                          (pattern (parser:node-catch-branch-pattern branch)))
                      (dolist (pvar (parser:pattern-variables pattern))
                        (setf (gethash (parser:pattern-var-name pvar) new-locals) t))
                      (traverse (parser:node-catch-branch-body branch) new-locals))))

                 (parser:node-resumable
                  (traverse (parser:node-resumable-expr node) local-bindings)
                  (dolist (branch (parser:node-resumable-branches node))
                    (let ((new-locals (alexandria:copy-hash-table local-bindings))
                          (pattern (parser:node-resumable-branch-pattern branch)))
                      (dolist (pvar (parser:pattern-variables pattern))
                        (setf (gethash (parser:pattern-var-name pvar) new-locals) t))
                      (traverse (parser:node-resumable-branch-body branch) new-locals))))

                 (parser:node-return
                  (when (parser:node-return-expr node)
                    (traverse (parser:node-return-expr node) local-bindings)))

                 (parser:node-throw
                  (traverse (parser:node-throw-expr node) local-bindings))

                 (parser:node-resume-to
                  (traverse (parser:node-resume-to-expr node) local-bindings))

                 ;; Lisp FFI - vars are explicitly listed
                 (parser:node-lisp
                  (dolist (var (parser:node-lisp-vars node))
                    (let ((name (parser:node-variable-name var)))
                      (unless (gethash name local-bindings)
                        (add-dependency name)))))

                 ;; Accessor - field accessors don't reference functions
                 (parser:node-accessor
                  nil))))

      ;; Start traversal with empty local bindings
      (traverse node (make-hash-table :test #'eq))

      ;; Return dependencies in order found (reversed due to push)
      (nreverse deps))))
