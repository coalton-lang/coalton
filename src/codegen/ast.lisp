(defpackage #:coalton-impl/codegen/ast
  (:use
   #:cl
   #:coalton-impl/codegen/pattern)
  (:local-nicknames
   (#:util #:coalton-impl/util)
   (#:algo #:coalton-impl/algorithm)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker))
  (:export
   #:node                               ; STRUCT
   #:node-type                          ; READER
   #:copy-node                          ; FUNCTION
   #:node-list                          ; TYPE
   #:binding-list                       ; TYPE
   #:node-literal                       ; STRUCT
   #:make-node-literal                  ; CONSTRUCTOR
   #:node-literal-p                     ; FUNCTION
   #:node-literal-value                 ; READER
   #:node-variable                      ; STRUCT
   #:make-node-variable                 ; CONSTRUCTOR
   #:node-variable-p                    ; FUNCTION
   #:node-variable-value                ; READER
   #:node-application                   ; STRUCT
   #:make-node-application              ; CONSTRUCTOR
   #:node-application-p                 ; FUNCTION
   #:node-application-properties        ; READER
   #:node-application-rator             ; READER
   #:node-application-rands             ; READER
   #:node-application-keyword-rands     ; READER
   #:node-application-keyword-arg       ; STRUCT
   #:make-node-application-keyword-arg  ; CONSTRUCTOR
   #:node-application-keyword-arg-keyword ; READER
   #:node-application-keyword-arg-value ; READER
   #:node-application-keyword-arg-supplied-p ; READER
   #:keyword-arg-list                   ; TYPE
   #:node-direct-application            ; STRUCT
   #:make-node-direct-application       ; CONSTRUCTOR
   #:node-direct-application-properties ; READER
   #:node-direct-application-rator-type ; READER
   #:node-direct-application-rator      ; READER
   #:node-direct-application-rands      ; READER
   #:node-direct-application-keyword-rands ; READER
   #:node-direct-application-p          ; FUNCTION
   #:node-abstraction                   ; STRUCT
   #:make-node-abstraction              ; CONSTRUCTOR
   #:node-abstraction-vars              ; READER
   #:node-abstraction-keyword-params    ; READER
   #:node-abstraction-subexpr           ; READER
   #:node-abstraction-p                 ; FUNCTION
   #:keyword-param                      ; STRUCT
   #:make-keyword-param                 ; CONSTRUCTOR
   #:keyword-param-keyword              ; READER
   #:keyword-param-var                  ; READER
   #:keyword-param-supplied-p-var       ; READER
   #:keyword-param-list                 ; TYPE
   #:node-let                           ; STRUCT
   #:make-node-let                      ; CONSTRUCTOR
   #:node-let-p                         ; FUNCTION
   #:node-let-bindings                  ; READER
   #:node-let-subexpr                   ; READER
   #:node-dynamic-binding               ; STRUCT
   #:make-node-dynamic-binding          ; CONSTRUCTOR
   #:node-dynamic-binding-name          ; READER
   #:node-dynamic-binding-value         ; READER
   #:node-dynamic-binding-list          ; TYPE
   #:node-dynamic-let                   ; STRUCT
   #:make-node-dynamic-let              ; CONSTRUCTOR
   #:node-dynamic-let-p                 ; FUNCTION
   #:node-dynamic-let-bindings          ; READER
   #:node-dynamic-let-subexpr           ; READER
   #:node-lisp                          ; STRUCT
   #:make-node-lisp                     ; CONSTRUCTOR
   #:node-lisp-p                        ; FUNCTION
   #:node-lisp-vars                     ; READER
   #:node-lisp-form                     ; READER
   #:node-locally                       ; STRUCT
   #:make-node-locally                  ; CONSTRUCTOR
   #:node-locally-p                     ; FUNCTION
   #:node-locally-noinline-functions    ; READER
   #:node-locally-type-check            ; READER
   #:node-locally-subexpr               ; READER
   #:match-branch                       ; STRUCT
   #:make-match-branch                  ; CONSTRUCTOR
   #:match-branch-pattern               ; READER
   #:match-branch-body                  ; READER
   #:branch-list                        ; TYPE
   #:node-match                         ; STRUCT
   #:make-node-match                    ; CONSTRUCTOR
   #:node-match-expr                    ; READER
   #:node-match-branches                ; READER
   #:catch-branch                       ; STRUCT
   #:make-catch-branch                  ; CONSTRUCTOR
   #:catch-branch-pattern               ; READER
   #:catch-branch-body                  ; READER
   #:catch-branch-list                  ; TYPE
   #:node-catch                         ; STRUCT
   #:make-node-catch                    ; CONSTRUCTOR
   #:node-catch-expr                    ; READER
   #:node-catch-branches                ; READER
   #:resumable-branch                   ; STRUCT
   #:make-resumable-branch              ; CONSTRUCTOR
   #:resumable-branch-pattern           ; READER
   #:resumable-branch-body              ; READER
   #:resumable-branch-list              ; TYPE
   #:node-resumable                     ; STRUCT
   #:make-node-resumable                ; CONSTRUCTOR
   #:node-resumable-expr                ; READER
   #:node-resumable-branches            ; READER
   #:node-for-binding                  ; STRUCT
   #:make-node-for-binding             ; CONSTRUCTOR
   #:node-for-binding-name             ; READER
   #:node-for-binding-type             ; READER
   #:node-for-binding-init             ; READER
   #:node-for-binding-step             ; READER
   #:node-for-binding-list             ; TYPE
   #:node-for                          ; STRUCT
   #:make-node-for                     ; CONSTRUCTOR
   #:node-for-bindings                 ; READER
   #:node-for-sequential-p             ; READER
   #:node-for-returns                  ; READER
   #:node-for-termination-kind         ; READER
   #:node-for-termination-expr         ; READER
   #:node-for-body                     ; READER
   #:node-for-label                    ; READER
   #:node-break                         ; STRUCT
   #:make-node-break                    ; CONSTRUCTOR
   #:node-break-label                   ; READER
   #:node-continue                      ; STRUCT
   #:make-node-continue                 ; CONSTRUCTOR
   #:node-continue-label                ; READER
   #:node-seq                           ; STRUCT
   #:make-node-seq                      ; CONSTRUCTOR
   #:node-seq-nodes                     ; READER
   #:node-return-from                   ; STRUCT
   #:make-node-return-from              ; CONSTRUCTOR
   #:node-return-from-name              ; READER
   #:node-return-from-expr              ; READER
   #:node-throw                         ; STRUCT
   #:make-node-throw                    ; CONSTRUCTOR
   #:node-throw-expr                    ; READER
   #:node-resume-to                     ; STRUCT
   #:make-node-resume-to                ; CONSTRUCTOR
   #:node-resume-to-expr                ; READER
   #:node-block                         ; STRUCT
   #:make-node-block                    ; CONSTRUCTOR
   #:node-block-name                    ; READER
   #:node-block-body                    ; READER
   #:node-field                         ; STRUCT
   #:make-node-field                    ; CONSTRUCTOR
   #:node-field-name                    ; READER
   #:node-field-dict                    ; READER
   #:node-field-p                       ; FUNCTION
   #:node-dynamic-extent                ; STRUCT
   #:make-node-dynamic-extent           ; CONSTRUCTOR
   #:node-dynamic-extent-name           ; READER
   #:node-dynamic-extent-node           ; READER
   #:node-dynamic-extent-body           ; READER
   #:node-bind                          ; STRUCT
   #:make-node-bind                     ; CONSTRUCTOR
   #:node-bind-name                     ; READER
   #:node-bind-expr                     ; READER
   #:node-bind-body                     ; READER
   #:node-values                        ; STRUCT
   #:make-node-values                   ; CONSTRUCTOR
   #:node-values-p                      ; FUNCTION
   #:node-values-nodes                  ; READER
   #:node-values-bind                   ; STRUCT
   #:make-node-values-bind              ; CONSTRUCTOR
   #:node-values-bind-p                 ; FUNCTION
   #:node-values-bind-vars              ; READER
   #:node-values-bind-expr              ; READER
   #:node-values-bind-body              ; READER
   #:node-variables                     ; FUNCTION
   #:node-binding-sccs                  ; FUNCTION
   #:node-free-p                        ; FUNCTION
   #:node-application-symbol-rator      ; FUNCTION
   #:node-rands                         ; FUNCTION
   #:node-rator-name                    ; FUNCTION
   #:node-rator-type                    ; FUNCTION
   #:node-properties                    ; FUNCTION
   ))

;;;;
;;;; Codegen AST - Typed Expression Nodes  
;;;;
;;;; This module defines the Abstract Syntax Tree structures used during code
;;;; generation, after type checking. These nodes include complete type information
;;;; and are optimized for translation to Common Lisp code.
;;;;
;;;; This is the SECOND of two AST systems in the Coalton compiler:
;;;; 1. Parser AST (parser/expression.lisp): Untyped nodes from parsing
;;;; 2. Codegen AST (this module): Typed nodes for code generation
;;;;

(in-package #:coalton-impl/codegen/ast)

;;;
;;; Compiler Backend IR
;;;


(defstruct (node (:conc-name %node-)
                 (:constructor nil)
                 (:copier %copy-node))
  ;; The `type` slot can be accessed by the exported function
  ;; `node-type`.
  (type (util:required 'type) :type tc:ty))

(defun copy-node (node &optional (new-type nil supplied-p))
  "Make a copy of `node`, optionally with a `new-type`."
  (declare (type node node)
           (type (or null tc:ty) new-type)
           (values node &optional))
  (let ((result (%copy-node node)))
    (when supplied-p
      (setf (%node-type result) new-type))
    result))

(defun node-type (node)
  "Get the stored type of `node`."
  (declare (type node node)
           (values tc:ty &optional))
  (%node-type node))

(defmethod make-load-form ((self node) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-p x)))

(deftype node-list ()
  '(satisfies node-list-p))

(defun binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons parser:identifier node))) x)))

(deftype binding-list ()
  '(satisfies binding-list-p))

(defun lisp-coalton-var-alist-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol parser:identifier))) x)))

(deftype lisp-coalton-var-alist ()
  "An association list of cons cells pairing lisp symbols (`symbol`) with
coalton symbols (`parser:identifier`)"
  '(satisfies lisp-coalton-var-alist-p))

(defstruct (node-literal (:include node))
  "Literal values like 1 or \"hello\""
  (value (util:required 'value) :type util:literal-value :read-only t))

(defstruct (node-variable (:include node))
  "Variables like x or y"
  (value (util:required 'value) :type parser:identifier :read-only t))

(defstruct keyword-param
  "A keyword parameter in a compiled lambda list."
  (keyword        (util:required 'keyword)        :type keyword           :read-only t)
  (var            (util:required 'var)            :type parser:identifier :read-only t)
  (supplied-p-var (util:required 'supplied-p-var) :type parser:identifier :read-only t))

(defmethod make-load-form ((self keyword-param) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun keyword-param-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'keyword-param-p x)))

(deftype keyword-param-list ()
  '(satisfies keyword-param-list-p))

(defstruct node-application-keyword-arg
  "A keyword argument in a compiled call."
  (keyword    (util:required 'keyword)    :type keyword            :read-only t)
  (value      (util:required 'value)      :type node               :read-only t)
  (supplied-p nil                         :type (or null node)     :read-only t))

(defmethod make-load-form ((self node-application-keyword-arg) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun keyword-arg-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-application-keyword-arg-p x)))

(deftype keyword-arg-list ()
  '(satisfies keyword-arg-list-p))

(defstruct (node-application (:include node))
  "Function application (f x)"
  ;; Extra information for use in optimizer can be stored here.
  ;; Currently its only valid keys are `:inline' and `:noinline'
  (properties (util:required 'properties) :type list      :read-only t)
  (rator      (util:required 'rator)      :type node      :read-only t)
  (rands      (util:required 'rands)      :type node-list :read-only t)
  (keyword-rands nil                      :type keyword-arg-list :read-only t))

(defstruct (node-direct-application (:include node))
  "Fully saturated function application of a known function"
  ;; Extra information for use in optimizer can be stored here.
  ;; Currently its only valid keys are `:inline' and `:noinline'
  (properties (util:required 'properties) :type list              :read-only t)
  (rator-type (util:required 'rator-type) :type tc:ty             :read-only t)
  (rator      (util:required 'rator)      :type parser:identifier :read-only t)
  (rands      (util:required 'rands)      :type node-list         :read-only t)
  (keyword-rands nil                      :type keyword-arg-list  :read-only t))

(defstruct (node-abstraction (:include node))
  "Lambda literals (fn (x) x)"
  (vars           (util:required 'vars) :type parser:identifier-list :read-only t)
  (keyword-params nil                    :type keyword-param-list    :read-only t)
  (subexpr        (util:required 'subexpr) :type node               :read-only t))

(defstruct (node-let (:include node))
  "Introduction of local mutually-recursive bindings (let ((x 2)) (+ x x))"
  (bindings (util:required 'bindings) :type binding-list :read-only t)
  (subexpr  (util:required 'subexpr)  :type node         :read-only t))

(defstruct node-dynamic-binding
  "A special-variable binding used by dynamic-bind."
  (name  (util:required 'name)  :type parser:identifier :read-only t)
  (value (util:required 'value) :type node              :read-only t))

(defmethod make-load-form ((self node-dynamic-binding) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun node-dynamic-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-dynamic-binding-p x)))

(deftype node-dynamic-binding-list ()
  '(satisfies node-dynamic-binding-list-p))

(defstruct (node-dynamic-let (:include node))
  "A dynamic scope wrapper implemented with Common Lisp special bindings."
  (bindings (util:required 'bindings) :type node-dynamic-binding-list :read-only t)
  (subexpr  (util:required 'subexpr)  :type node                      :read-only t))

(defstruct (node-lisp (:include node))
  "An embedded lisp form"
  (vars (util:required 'vars) :type lisp-coalton-var-alist    :read-only t)
  (form (util:required 'form) :type t                         :read-only t))

(defstruct (node-locally (:include node))
  "Node for the optimizer to use, similar to `cl:locally'."
  (noinline-functions (util:required 'noinline-functions) :type parser:identifier-list :read-only t)
  (type-check nil :type (or null (integer 0 3)) :read-only t)
  (subexpr            (util:required 'subexpr)            :type node                   :read-only t))

(defstruct match-branch
  "A branch of a match expression"
  (pattern (util:required 'pattern) :type pattern :read-only t)
  (body    (util:required 'body)    :type node    :read-only t))

(defmethod make-load-form ((self match-branch) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'match-branch-p x)))

(deftype branch-list ()
  '(satisfies branch-list-p))

(defstruct (node-match (:include node))
  "A pattern matching construct. Uses MATCH-BRANCH to represent branches"
  (expr     (util:required 'expr)     :type node        :read-only t)
  (branches (util:required 'branches) :type branch-list :read-only t))

(defstruct catch-branch
  "A branch of a catch expression."
  (pattern (util:required 'pattern) :type pattern :read-only t)
  (body    (util:required 'body)    :type node    :read-only t))

(defmethod make-load-form ((self catch-branch) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun catch-branch-list-p (xs)
  (and (alexandria:proper-list-p xs)
       (every #'catch-branch-p xs)))

(deftype catch-branch-list ()
  '(satisfies catch-branch-list-p))

(defstruct (node-catch (:include node))
  "An exception-catching construct. Uses CATCH-BRANCH to represent branches"
  (expr     (util:required 'expr)     :type node              :read-only t)
  (branches (util:required 'branches) :type catch-branch-list :read-only t))

(defstruct resumable-branch
  "A branch of a resumable expression."
  (pattern (util:required 'pattern) :type pattern :read-only t)
  (body    (util:required 'body)    :type node    :read-only t))

(defmethod make-load-form ((self resumable-branch) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun resumable-branch-list-p (xs)
  (and (alexandria:proper-list-p xs)
       (every #'resumable-branch-p xs)))

(deftype resumable-branch-list ()
  '(satisfies resumable-branch-list-p))

(defstruct (node-resumable (:include node))
  "A construct for continuing from a non-stack-unwinding transfer of control. 
   Uses RESUMABLE-BRANCH to represent branches"
  (expr     (util:required 'expr)     :type node                  :read-only t)
  (branches (util:required 'branches) :type resumable-branch-list :read-only t))


(defstruct node-for-binding
  "A single `for` variable with an initializer and optional step expression."
  (name (util:required 'name) :type parser:identifier :read-only t)
  (type (util:required 'type) :type tc:ty             :read-only t)
  (init (util:required 'init) :type node              :read-only t)
  (step nil                   :type (or null node)    :read-only t))

(defmethod make-load-form ((self node-for-binding) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun node-for-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-for-binding-p x)))

(deftype node-for-binding-list ()
  '(satisfies node-for-binding-list-p))

(defstruct (node-for (:include node))
  "A labelled imperative `for` with explicit bindings and step expressions."
  (label            (util:required 'label)            :type keyword                         :read-only t)
  (bindings         (util:required 'bindings)         :type node-for-binding-list          :read-only t)
  (sequential-p     nil                               :type boolean                         :read-only t)
  (returns          nil                               :type (or null node)                  :read-only t)
  (termination-kind nil                               :type (member nil :while :until :repeat) :read-only t)
  (termination-expr nil                               :type (or null node)                  :read-only t)
  (body             (util:required 'body)             :type node                            :read-only t))

(defstruct (node-break (:include node))
  "A break statement used to exit a `for`."
  (label (util:required 'label) :type keyword :read-only t))

(defstruct (node-continue (:include node))
  "A continue statement used to skip to the next iteration of a `for`."
  (label (util:required 'label) :type keyword :read-only t))

(defstruct (node-seq (:include node))
  "A series of statements to be executed sequentially"
  (nodes (util:required 'nodes) :type node-list :read-only t))

(defstruct (node-return-from (:include node))
  "A return statement, used for explicit returns in functions"
  (name (util:required 'name) :type symbol :read-only t)
  (expr (util:required 'expr) :type node   :read-only t))

(defstruct (node-throw (:include node))
  "A node that throws an exception, its argument."
  (expr (util:required 'expr) :type node :read-only t))

(defstruct (node-resume-to (:include node))
  "A node that invokes a resumption, if any exists."
  (expr (util:required 'expr) :type node :read-only t))

(defstruct (node-block (:include node))
  "A return target, used for explicit returns in functions"
  (name (util:required 'node) :type symbol :read-only t)
  (body (util:required 'body) :type node   :read-only t))

(defstruct (node-field (:include node))
  "Accessing a superclass on a typeclass dictionary"
  (name (util:required 'name) :type parser:identifier :read-only t)
  (dict (util:required 'dict) :type node              :read-only t))

(defstruct (node-dynamic-extent (:include node))
  "A single stack allocated binding"
  (name (util:required 'name) :type parser:identifier :read-only t)
  (node (util:required 'node) :type node              :read-only t)
  (body (util:required 'body) :type node              :read-only t))

(defstruct (node-bind (:include node))
  "A single non-recursive binding"
  (name (util:required 'name) :type parser:identifier :read-only t)
  (expr (util:required 'expr) :type node              :read-only t)
  (body (util:required 'body) :type node              :read-only t))

(defstruct (node-values (:include node))
  "Produce multiple values."
  (nodes (util:required 'nodes) :type node-list :read-only t))

(defstruct (node-values-bind (:include node))
  "Bind multiple values and evaluate body."
  (vars (util:required 'vars) :type parser:identifier-list :read-only t)
  (expr (util:required 'expr) :type node                   :read-only t)
  (body (util:required 'body) :type node                   :read-only t))

;;;
;;; Functions
;;;

(defun node-binding-sccs (bindings)
  "Returns a list of SCCs ordered from least to most depended on."
  (declare (type binding-list bindings))

  (let ((binding-names (mapcar #'car bindings)))
    (algo:tarjan-scc
     (loop :for (name . node) :in bindings
           :collect (cons name (intersection binding-names (node-variables node)))))))

(defun node-rands (node)
  (declare (type (or node-application node-direct-application))
           (values node-list &optional))

  (etypecase node
    (node-direct-application
     (node-direct-application-rands node))

    (node-application
     (node-application-rands node))))

(defun node-rator-name (node)
  "Returns the name of the function being called if it is known"
  (declare (type (or node-application node-direct-application))
           (values (or null parser:identifier) &optional))

  (etypecase node
    (node-direct-application
     (node-direct-application-rator node))

    (node-application
     (when (node-variable-p (node-application-rator node))
       (node-variable-value (node-application-rator node))))))

(defun node-rator-type (node)
  (declare (type (or node-application node-direct-application))
           (values tc:ty &optional))

  (etypecase node
    (node-direct-application
     (node-direct-application-rator-type node))

    (node-application
     (node-type (node-application-rator node)))))

(defun node-properties (node)
  (declare (type (or node-application node-direct-application))
           (values list &optional))

  (etypecase node
    (node-direct-application
     (node-direct-application-properties node))

    (node-application
     (node-application-properties node))))
