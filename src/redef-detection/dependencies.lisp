(defpackage #:coalton-impl/redef-detection/dependencies
  (:use #:cl)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc-env #:coalton-impl/typechecker/environment))
  (:export
   #:extract-function-dependencies
   #:dependency-registry
   #:dependency-registry-forward-deps
   #:dependency-registry-reverse-deps
   #:make-dependency-registry
   #:*dependency-registry*
   #:record-dependencies
   #:get-function-callers
   #:get-function-location
   #:find-affected-functions))
(in-package #:coalton-impl/redef-detection/dependencies)

;;;
;;; Dependency Extraction with Lexical Scope Tracking
;;;

(defun extract-function-dependencies (node env)
  "Extract global function calls from NODE.
   Returns a list of symbols that are verified to be functions in ENV.
   Filters out locally-bound variables by tracking lexical scope."
  (declare (type tc-env:environment env)
           (values list))

  (let ((deps nil)
        (seen (make-hash-table :test #'eq))) ; For deduplication

    (labels ((add-dependency (name)
               "Add NAME to dependencies if it's a function in ENV."
               (when (and (not (gethash name seen))
                          (tc-env:lookup-value-type env name :no-error t))
                 (setf (gethash name seen) t)
                 (push name deps)))

             (traverse-builder-clauses (clauses local-bindings)
               "Traverse builder CLAUSES, returning the lexical scope visible to the builder head."
               (declare (type hash-table local-bindings)
                        (values hash-table &optional))
               (loop :with current-locals := local-bindings
                     :for clause :in clauses
                     :do
                        (etypecase clause
                          (parser:builder-with-clause
                           (traverse (parser:builder-with-clause-expr clause) current-locals)
                           (let ((new-locals (alexandria:copy-hash-table current-locals)))
                             (setf (gethash (parser:node-variable-name
                                             (parser:builder-with-clause-binder clause))
                                            new-locals)
                                   t)
                             (setf current-locals new-locals)))
                          (parser:builder-for-clause
                           (traverse (parser:builder-for-clause-expr clause) current-locals)
                           (let ((new-locals (alexandria:copy-hash-table current-locals)))
                             (setf (gethash (parser:node-variable-name
                                             (parser:builder-for-clause-binder clause))
                                            new-locals)
                                   t)
                             (setf current-locals new-locals)))
                          (parser:builder-below-clause
                           (traverse (parser:builder-below-clause-expr clause) current-locals)
                           (let ((new-locals (alexandria:copy-hash-table current-locals)))
                             (setf (gethash (parser:node-variable-name
                                             (parser:builder-below-clause-binder clause))
                                            new-locals)
                                   t)
                             (setf current-locals new-locals)))
                          (parser:builder-when-clause
                           (traverse (parser:builder-when-clause-expr clause) current-locals)))
                     :finally (return current-locals)))

             (traverse (node local-bindings)
               "Traverse NODE with LOCAL-BINDINGS hash-set tracking local variables."
               (declare (type hash-table local-bindings))

               (etypecase node
                 ;; Variable reference - check if local or global
                 (parser:node-variable
                  (let ((name (parser:node-variable-name node)))
                    (unless (gethash name local-bindings)
                      (add-dependency name))))

                 ;; Let binding - add bindings to scope sequentially
                 (parser:node-let
                  (let ((new-locals (alexandria:copy-hash-table local-bindings)))
                    (dolist (binding (parser:node-let-bindings node))
                      (traverse (parser:node-let-binding-value binding)
                                new-locals)
                      (let* ((name-node (parser:node-let-binding-name binding))
                             (name (parser:node-variable-name name-node)))
                        (setf (gethash name new-locals) t)))
                    (traverse (parser:node-let-body node) new-locals)))

                 ;; REC lowers to an outer let for init bindings, plus a local
                 ;; recursive function and immediate application.
                 (parser:node-rec
                  (dolist (binding (parser:node-rec-bindings node))
                    (traverse (parser:node-let-binding-value binding)
                              local-bindings))
                  (let ((outer-locals (alexandria:copy-hash-table local-bindings)))
                    (dolist (binding (parser:node-rec-bindings node))
                      (setf (gethash (parser:node-variable-name
                                      (parser:node-let-binding-name binding))
                                     outer-locals)
                            t))
                    (let ((body-locals (alexandria:copy-hash-table outer-locals)))
                      (setf (gethash (parser:node-variable-name
                                      (parser:node-rec-name node))
                                     body-locals)
                            t)
                      (dolist (pvar (parser:pattern-variables
                                     (parser:node-rec-params node)))
                        (setf (gethash (parser:pattern-var-name pvar) body-locals) t))
                      (traverse (parser:node-rec-body node) body-locals))
                    (dolist (arg (parser:node-rec-call-args node))
                      (traverse arg outer-locals))))

                 ;; Dynamic binding - initializers and body both see the same
                 ;; lexical scope because dynamic bindings do not introduce
                 ;; lexical variables.
                 (parser:node-dynamic-let
                  (dolist (binding (parser:node-dynamic-let-bindings node))
                    (traverse (parser:node-dynamic-binding-value binding)
                              local-bindings))
                  (traverse (parser:node-dynamic-let-subexpr node) local-bindings))

                 ;; Lambda - parameters shadow
                 (parser:node-abstraction
                  (let ((new-locals (alexandria:copy-hash-table local-bindings)))
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

                 (parser:node-collection-builder
                  (dolist (element (parser:node-collection-builder-elements node))
                    (traverse element local-bindings)))

                 (parser:node-association-builder
                  (dolist (entry (parser:node-association-builder-entries node))
                    (traverse (parser:association-entry-key entry) local-bindings)
                    (traverse (parser:association-entry-value entry) local-bindings)))

                 (parser:node-collection-comprehension
                  (let ((head-locals
                          (traverse-builder-clauses
                           (parser:node-collection-comprehension-clauses node)
                           local-bindings)))
                    (traverse (parser:node-collection-comprehension-head node)
                              head-locals)))

                 (parser:node-association-comprehension
                  (let ((head-locals
                          (traverse-builder-clauses
                           (parser:node-association-comprehension-clauses node)
                           local-bindings)))
                    (traverse (parser:node-association-comprehension-key node)
                              head-locals)
                    (traverse (parser:node-association-comprehension-value node)
                              head-locals)))

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

                 ;; Static type reflection traverses the reflected expression
                 (parser:node-type-of
                  (traverse (parser:node-type-of-expr node) local-bindings))

                 ;; Progn
                 (parser:node-progn
                  (traverse (parser:node-progn-body node) local-bindings))

                 (parser:node-unsafe
                  (traverse (parser:node-unsafe-body node) local-bindings))

                 ;; Do notation - bind shadows sequentially
                 (parser:node-do
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
                        (parser:node-values-bind
                         (traverse (parser:node-values-bind-expr elem) current-locals)
                         (let ((new-locals (alexandria:copy-hash-table current-locals)))
                           (dolist (pvar (parser:pattern-variables
                                          (parser:node-values-bind-patterns elem)))
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

                 (parser:node-values-bind
                  (traverse (parser:node-values-bind-expr node) local-bindings))

                 (parser:node-values
                  (mapc (lambda (subnode)
                          (traverse subnode local-bindings))
                        (parser:node-values-nodes node)))

                 ;; Loops
                 (parser:node-for
                  (cond
                    ((parser:node-for-sequential-p node)
                     (let ((new-locals (alexandria:copy-hash-table local-bindings)))
                       (dolist (binding (parser:node-for-bindings node))
                         (traverse (parser:node-for-binding-init binding) new-locals)
                         (setf (gethash (parser:node-variable-name
                                         (parser:node-for-binding-name binding))
                                        new-locals)
                               t))
                       (dolist (binding (parser:node-for-bindings node))
                         (when (parser:node-for-binding-step binding)
                           (traverse (parser:node-for-binding-step binding) new-locals)))
                       (when (parser:node-for-returns node)
                         (traverse (parser:node-for-returns node) new-locals))
                       (when (parser:node-for-termination-expr node)
                         (traverse (parser:node-for-termination-expr node) new-locals))
                       (traverse (parser:node-for-body node) new-locals)))
                    (t
                     (dolist (binding (parser:node-for-bindings node))
                       (traverse (parser:node-for-binding-init binding) local-bindings))
                     (let ((new-locals (alexandria:copy-hash-table local-bindings)))
                       (dolist (binding (parser:node-for-bindings node))
                         (setf (gethash (parser:node-variable-name
                                         (parser:node-for-binding-name binding))
                                        new-locals)
                               t)
                         (when (parser:node-for-binding-step binding)
                           (traverse (parser:node-for-binding-step binding) new-locals)))
                       (when (parser:node-for-returns node)
                         (traverse (parser:node-for-returns node) new-locals))
                       (when (parser:node-for-termination-expr node)
                         (traverse (parser:node-for-termination-expr node) new-locals))
                       (traverse (parser:node-for-body node) new-locals)))))

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

                 (parser:node-block
                  (traverse (parser:node-block-body node) local-bindings))

                 (parser:node-return
                  (when (parser:node-return-expr node)
                    (traverse (parser:node-return-expr node) local-bindings)))

                 (parser:node-return-from
                  (traverse (parser:node-return-from-expr node) local-bindings))

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

      (traverse node (make-hash-table :test #'eq))

      (nreverse deps))))

;;;
;;; Dependency Registry
;;;

(defstruct dependency-registry
  "Tracks function call relationships."
  ;; Map: function-name (symbol) -> list of function names it calls
  (forward-deps (make-hash-table :test 'eq) :type hash-table :read-only t)
  ;; Map: function-name (symbol) -> list of function names that call it
  (reverse-deps (make-hash-table :test 'eq) :type hash-table :read-only t))

(defvar *dependency-registry* (make-dependency-registry)
  "Global registry of function dependencies.")

(defun record-dependencies (function-name code env)
  "Record that FUNCTION-NAME depends on functions called in CODE."
  (declare (type symbol function-name)
           (type tc-env:environment env))

  (let* ((forward-deps (dependency-registry-forward-deps *dependency-registry*))
         (reverse-deps (dependency-registry-reverse-deps *dependency-registry*))
         (called-functions (extract-function-dependencies code env)))

    ;; Remove duplicates
    (setf called-functions (remove-duplicates called-functions :test #'eq))

    ;; Remove self-reference (recursive calls)
    (setf called-functions (remove function-name called-functions :test #'eq))

    ;; Store forward dependencies
    (setf (gethash function-name forward-deps) called-functions)

    ;; Update reverse dependencies
    ;; First, clear old reverse dependencies for this function
    (maphash
     (lambda (callee callers)
       (setf (gethash callee reverse-deps)
             (remove function-name callers :test #'eq)))
     reverse-deps)

    ;; Add new reverse dependencies
    (dolist (callee called-functions)
      (push function-name (gethash callee reverse-deps nil)))

    (values)))

(defun get-function-callers (function-name)
  "Get list of functions that directly call FUNCTION-NAME."
  (declare (type symbol function-name)
           (values list))
  (gethash function-name (dependency-registry-reverse-deps *dependency-registry*) nil))

(defun get-function-location (function-name env)
  "Get source location for FUNCTION-NAME from environment, or NIL if not available."
  (declare (type symbol function-name)
           (type tc-env:environment env)
           (values (or null source:location)))
  (let ((name-entry (tc-env:lookup-name env function-name :no-error t)))
    (when name-entry
      (source:location name-entry))))

(defun find-affected-functions (function-name &optional (visited (make-hash-table :test 'eq)))
  "Find all functions transitively affected by changes to FUNCTION-NAME.
Handles circular dependencies by tracking VISITED functions."
  (declare (type symbol function-name)
           (type hash-table visited)
           (values list))

  (let ((result nil))
    (labels ((visit (fname)
               (unless (gethash fname visited)
                 (setf (gethash fname visited) t)
                 (let ((callers (get-function-callers fname)))
                   (dolist (caller callers)
                     (push caller result)
                     (visit caller))))))
      (visit function-name))

    ;; Return unique, sorted list
    (sort (remove-duplicates result :test #'eq) #'string< :key #'symbol-name)))
