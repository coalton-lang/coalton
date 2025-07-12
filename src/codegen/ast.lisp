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
   #:node-application-rator             ; READER
   #:node-application-rands             ; READER
   #:node-direct-application            ; STRUCT
   #:make-node-direct-application       ; CONSTRUCTOR
   #:node-direct-application-rator-type ; READER
   #:node-direct-application-rator      ; READER
   #:node-direct-application-rands      ; READER
   #:node-direct-application-p          ; FUNCTION
   #:node-abstraction                   ; STRUCT
   #:make-node-abstraction              ; CONSTRUCTOR
   #:node-abstraction-vars              ; READER
   #:node-abstraction-subexpr           ; READER
   #:node-abstraction-p                 ; FUNCTION
   #:node-inline-call                   ; STRUCT
   #:make-node-inline-call              ; CONSTRUCTOR
   #:node-inline-call-p                 ; FUNCTION
   #:node-inline-call-rator-type        ; READER
   #:node-inline-call-rator             ; READER
   #:node-inline-call-rands             ; READER
   #:node-let                           ; STRUCT
   #:make-node-let                      ; CONSTRUCTOR
   #:node-let-p                         ; FUNCTION
   #:node-let-bindings                  ; READER
   #:node-let-subexpr                   ; READER
   #:node-lisp                          ; STRUCT
   #:make-node-lisp                     ; CONSTRUCTOR
   #:node-lisp-p                        ; FUNCTION
   #:node-lisp-vars                     ; READER
   #:node-lisp-form                     ; READER
   #:node-locally                       ; STRUCT
   #:make-node-locally                  ; CONSTRUCTOR
   #:node-locally-p                     ; FUNCTION
   #:node-locally-noinline-functions    ; READER
   #:node-locally-subexpr               ; READER
   #:match-branch                       ; STRUCT
   #:make-match-branch                  ; CONSTRUCTOR
   #:match-branch-pattern               ; READER
   #:match-branch-bindings              ; READER
   #:match-branch-body                  ; READER
   #:branch-list                        ; TYPE
   #:node-match                         ; STRUCT
   #:make-node-match                    ; CONSTRUCTOR
   #:node-match-expr                    ; READER
   #:node-match-branches                ; READER
   #:node-while                         ; STRUCT
   #:make-node-while                    ; CONSTRUCTOR
   #:node-while-label                   ; READER
   #:node-while-expr                    ; READER
   #:node-while-body                    ; ACESSOR
   #:node-while-let                     ; STRUCT
   #:make-node-while-let                ; CONSTRUCTOR
   #:node-while-let-label               ; ACESSOR
   #:node-while-let-pattern             ; READER
   #:node-while-let-expr                ; READER
   #:node-while-let-body                ; ACESSOR
   #:node-loop                          ; STRUCT
   #:make-node-loop                     ; CONSTRUCTOR
   #:node-loop-body                     ; READER
   #:node-loop-label                    ; READER
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
   #:node-variables                     ; FUNCTION
   #:node-binding-sccs                  ; FUNCTION
   #:node-free-p                        ; FUNCTION
   #:node-application-symbol-rator      ; FUNCTION
   #:node-rands                         ; FUNCTION
   #:node-rator-name                    ; FUNCTION
   #:node-rator-type                    ; FUNCTION
   ))

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

(defstruct (node-application (:include node))
  "Function application (f x)"
  (rator (util:required 'rator) :type node      :read-only t)
  (rands (util:required 'rands) :type node-list :read-only t))

(defstruct (node-direct-application (:include node))
  "Fully saturated function application of a known function"
  (rator-type (util:required 'rator-type) :type tc:ty             :read-only t)
  (rator      (util:required 'rator)      :type parser:identifier :read-only t)
  (rands      (util:required 'rands)      :type node-list         :read-only t))

(defstruct (node-abstraction (:include node))
  "Lambda literals (fn (x) x)"
  (vars    (util:required 'vars)    :type parser:identifier-list :read-only t)
  (subexpr (util:required 'subexpr) :type node                   :read-only t))

(defstruct (node-inline-call (:include node))
  "Application that forces inlining"
  (rator (util:required 'rator) :type node      :read-only t)
  (rands (util:required 'rands) :type node-list :read-only t))

(defstruct (node-let (:include node))
  "Introduction of local mutually-recursive bindings (let ((x 2)) (+ x x))"
  (bindings (util:required 'bindings) :type binding-list :read-only t)
  (subexpr  (util:required 'subexpr)  :type node         :read-only t))

(defstruct (node-lisp (:include node))
  "An embedded lisp form"
  (vars (util:required 'vars) :type lisp-coalton-var-alist :read-only t)
  (form (util:required 'form) :type t                      :read-only t))

(defstruct (node-locally (:include node))
  "Node for the optimizer to use, similar to `cl:locally'."
  (noinline-functions (util:required 'noinline-functions) :type parser:identifier-list :read-only t)
  (subexpr            (util:required 'subexpr)            :type node                   :read-only t))

(defstruct match-branch
  "A branch of a match statement"
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

(defstruct (node-while (:include node))
  "A looping construct. Executes a body until an expression is false."
  (label (util:required 'label) :type keyword :read-only t)
  (expr  (util:required 'expr)  :type node    :read-only t)
  (body  (util:required 'body)  :type node    :read-only t))

(defstruct (node-while-let (:include node))
  "A looping construct. Executes a body until a pattern match fails."
  (label   (util:required 'label)   :type keyword :read-only t)
  (pattern (util:required 'pattern) :type pattern :read-only t)
  (expr    (util:required 'expr)    :type node    :read-only t)
  (body    (util:required 'body)    :type node    :read-only t))

(defstruct (node-loop (:include node))
  "A labelled looping construct. Loops forever until broken out of by a
call to (break)."
  (label (util:required 'label) :type keyword :read-only t)
  (body  (util:required 'body)  :type node    :read-only t))

(defstruct (node-break (:include node))
  "A break statment used to exit a loop."
  (label (util:required 'label) :type keyword :read-only t))

(defstruct (node-continue (:include node))
  "A continue statment used to skip to the next iteration of a loop."
  (label (util:required 'label) :type keyword :read-only t))

(defstruct (node-seq (:include node))
  "A series of statements to be executed sequentially"
  (nodes (util:required 'nodes) :type node-list :read-only t))

(defstruct (node-return-from (:include node))
  "A return statement, used for explicit returns in functions"
  (name (util:required 'name) :type symbol :read-only t)
  (expr (util:required 'expr) :type node   :read-only t))

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

;;;
;;; Functions
;;;

(defun node-binding-sccs (bindings)
  "Returns a list of SCCs ordered from least to most depended on."
  (declare (type binding-list bindings))
  (let ((binding-names (mapcar #'car bindings)))
    (reverse
     (algo:tarjan-scc
      (loop :for (name . node) :in bindings
            :collect (cons name (intersection binding-names (node-variables node))))))))

(defun node-rands (node)
  (declare (type (or node-application node-direct-application))
           (values node-list))
  (etypecase node
    (node-direct-application
     (node-direct-application-rands node))

    (node-application
     (node-application-rands node))

    (node-inline-call
     (node-inline-call-rands node))))

(defun node-rator-name (node)
  "Returns the name of the function being called if it is known"
  (declare (type (or node-application node-direct-application))
           (values (or null parser:identifier)))
  (etypecase node
    (node-direct-application
     (node-direct-application-rator node))

    (node-application
     (when (node-variable-p (node-application-rator node))
       (node-variable-value (node-application-rator node))))

    (node-inline-call
     (node-variable-value (node-inline-call-rator node)))))

(defun node-rator-type (node)
  (declare (type (or node-application node-direct-application))
           (values tc:ty))
  (etypecase node
    (node-direct-application
     (node-direct-application-rator-type node))

    (node-application
     (node-type (node-application-rator node)))

    (node-inline-call
     (node-type (node-inline-call-rator node)))))
