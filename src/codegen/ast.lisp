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
   #:node-type                          ; ACCESSOR
   #:node-list                          ; TYPE
   #:binding-list                       ; TYPE
   #:node-literal                       ; STRUCT
   #:make-node-literal                  ; CONSTRUCTOR
   #:node-literal-value                 ; ACCESSOR
   #:node-variable                      ; STRUCT
   #:make-node-variable                 ; CONSTRUCTOR
   #:node-variable-p                    ; FUNCTION
   #:node-variable-value                ; ACCESSOR
   #:node-application                   ; STRUCT
   #:make-node-application              ; CONSTRUCTOR
   #:node-application-p                 ; FUNCTION
   #:node-application-rator             ; ACCESSOR
   #:node-application-rands             ; ACCESSOR
   #:node-direct-application            ; STRUCT
   #:make-node-direct-application       ; CONSTRUCTOR
   #:node-direct-application-rator-type ; ACCESSOR
   #:node-direct-application-rator      ; ACCESSOR
   #:node-direct-application-rands      ; ACCESSOR
   #:node-direct-application-p          ; FUNCTION
   #:node-abstraction                   ; STRUCT
   #:make-node-abstraction              ; CONSTRUCTOR
   #:node-abstraction-vars              ; ACCESSOR
   #:node-abstraction-subexpr           ; ACCESSOR
   #:node-abstraction-p                 ; FUNCTION
   #:node-let                           ; STRUCT
   #:make-node-let                      ; CONSTRUCTOR
   #:node-let-p                         ; FUNCTION
   #:node-let-bindings                  ; ACCESSOR
   #:node-let-subexpr                   ; ACCESSOR
   #:node-lisp                          ; STRUCT
   #:make-node-lisp                     ; CONSTRUCTOR
   #:node-lisp-vars                     ; ACCESSOR
   #:node-lisp-form                     ; ACCESSOR
   #:match-branch                       ; STRUCT
   #:make-match-branch                  ; CONSTRUCTOR
   #:match-branch-pattern               ; ACCESSOR
   #:match-branch-bindings              ; ACCESSOR
   #:match-branch-body                  ; ACCESSOR
   #:branch-list                        ; TYPE
   #:node-match                         ; STRUCT
   #:make-node-match                    ; CONSTRUCTOR
   #:node-match-expr                    ; ACCESSOR
   #:node-match-branches                ; ACCESSOR
   #:node-while                         ; STRUCT
   #:make-node-while                    ; CONSTRUCTOR
   #:node-while-label                   ; ACCESSOR
   #:node-while-expr                    ; ACCESSOR
   #:node-while-body                    ; ACESSOR
   #:node-while-let                     ; STRUCT
   #:make-node-while-let                ; CONSTRUCTOR
   #:node-while-let-label               ; ACESSOR
   #:node-while-let-pattern             ; ACCESSOR
   #:node-while-let-expr                ; ACCESSOR
   #:node-while-let-body                ; ACESSOR
   #:node-loop                          ; STRUCT
   #:make-node-loop                     ; CONSTRUCTOR
   #:node-loop-body                     ; ACCESSOR
   #:node-loop-label                    ; ACCESSOR
   #:node-break                         ; STRUCT
   #:make-node-break                    ; CONSTRUCTOR
   #:node-break-label                   ; ACCESSOR
   #:node-continue                      ; STRUCT
   #:make-node-continue                 ; CONSTRUCTOR
   #:node-continue-label                ; ACCESSOR
   #:node-seq                           ; STRUCT
   #:make-node-seq                      ; CONSTRUCTOR
   #:node-seq-nodes                     ; ACCESSOR
   #:node-return                        ; STRUCT
   #:make-node-return                   ; CONSTRUCTOR
   #:node-return-expr                   ; ACCESSOR
   #:node-field                         ; STRUCT
   #:make-node-field                    ; CONSTRUCTOR
   #:node-field-name                    ; ACCESSOR
   #:node-field-dict                    ; ACCESSOR
   #:node-field-p                       ; FUNCTION
   #:node-dynamic-extent                ; STRUCT
   #:make-node-dynamic-extent           ; CONSTRUCTOR
   #:node-dynamic-extent-name           ; ACCESSOR
   #:node-dynamic-extent-node           ; ACCESSOR
   #:node-dynamic-extent-body           ; ACCESSOR
   #:node-bind                          ; STRUCT
   #:make-node-bind                     ; CONSTRUCTOR
   #:node-bind-name                     ; ACCESSOR
   #:node-bind-expr                     ; ACCESSOR
   #:node-bind-body                     ; ACCESSOR
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


(defstruct (node (:constructor nil))
  (type (util:required 'type) :type tc:ty :read-only t))

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

(defstruct (node-let (:include node))
  "Introduction of local mutually-recursive bindings (let ((x 2)) (+ x x))"
  (bindings (util:required 'bindings) :type binding-list :read-only t)
  (subexpr  (util:required 'subexpr)  :type node         :read-only t))

(defstruct (node-lisp (:include node))
  "An embedded lisp form"
  (vars (util:required 'vars) :type list :read-only t)
  (form (util:required 'form) :type t    :read-only t))

(defstruct match-branch
  "A branch of a match statement"
  (pattern  (util:required 'pattern)  :type pattern            :read-only t)
  (body     (util:required 'body)     :type node               :read-only t))

(defmethod make-load-form ((self match-branch) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'match-branch-p x)))

(deftype branch-list ()
  '(satisfies branch-list-p))

(defstruct (node-match (:include node))
  "A pattern matching construct. Uses MATCH-BRANCH to represent branches"
  (expr (util:required 'expr) :type node :read-only t)
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

(defstruct (node-return (:include node))
  "A return statement, used for early returns in functions"
  (expr (util:required 'expr) :type node :read-only t))

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

(defun node-variables (node &key variable-namespace-only)
  "Returns a deduplicated list of identifiers representing variables in
both CL namespaces appearing in NODE"
  (declare (type node node)
           (type boolean variable-namespace-only)
           (values parser:identifier-list &optional))
  (remove-duplicates (node-variables-g node :variable-namespace-only variable-namespace-only) :test #'equalp))

(defun node-binding-sccs (bindings)
  "Returns a list of SCCs ordered from least to most depended on."
  (declare (type binding-list bindings))
  (let ((binding-names (mapcar #'car bindings)))
    (reverse
     (algo:tarjan-scc
      (loop :for (name . node) :in bindings
            :collect (cons name (intersection binding-names (node-variables node))))))))

(defun node-free-p (node bound-variables)
  "Returns true if every variable in NODE is free with respect to BOUND-VARIABLES"
  (declare (type node node)
           (type parser:identifier-list bound-variables)
           (values boolean))
  (null (intersection (node-variables node) bound-variables)))

(defun node-application-symbol-rator (node)
  "Returns the name of the function being called if it is known"
  (declare (type (or node-application node-direct-application) node)
           (values (or null parser:identifier)))
  (etypecase node
    (node-direct-application
     (node-direct-application-rator node))

    (node-application
     (unless (node-variable-p (node-application-rator node))
       (return-from node-application-symbol-rator))

     (node-variable-value (node-application-rator node)))))

(defun node-rands (node)
  (declare (type (or node-application node-direct-application))
           (values node-list))
  (etypecase node
    (node-direct-application
     (node-direct-application-rands node))

    (node-application
     (node-application-rands node))))

(defun node-rator-name (node)
  (declare (type (or node-application node-direct-application))
           (values (or null parser:identifier)))
  (etypecase node
    (node-direct-application
     (node-direct-application-rator node))

    (node-application
     (when (node-variable-p (node-application-rator node))
       (node-variable-value (node-application-rator node))))))

(defun node-rator-type (node)
  (declare (type (or node-application node-direct-application))
           (values tc:ty))
  (etypecase node
    (node-direct-application
     (node-direct-application-rator-type node))

    (node-application
     (node-type (node-application-rator node)))))

;;;
;;; Methods
;;;


(defgeneric node-variables-g (node &key variable-namespace-only)
  (:method ((node node-literal) &key variable-namespace-only)
    (declare (ignore node variable-namespace-only)
             (values parser:identifier-list))
    nil)

  (:method ((node node-variable) &key variable-namespace-only)
    (declare (ignore variable-namespace-only)
             (values parser:identifier-list &optional))
    (list (node-variable-value node)))

  (:method ((node node-application) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (append
     (node-variables-g (node-application-rator node) :variable-namespace-only variable-namespace-only)
     (mapcan
      (lambda (node)
        (node-variables-g node :variable-namespace-only variable-namespace-only))
      (node-application-rands node))))

  (:method ((node node-abstraction) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (node-variables-g (node-abstraction-subexpr node) :variable-namespace-only variable-namespace-only))

  (:method ((node node-direct-application) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (if variable-namespace-only
        (mapcan
         (lambda (node)
           (node-variables-g node :variable-namespace-only variable-namespace-only))
         (node-direct-application-rands node))
        (cons (node-direct-application-rator node)
              (mapcan
               (lambda (node)
                 (node-variables-g node :variable-namespace-only variable-namespace-only))
               (node-direct-application-rands node)))))

  (:method ((node node-let) &key variable-namespace-only)
    (declare (values parser:identifier-list))
    (append
     (loop :for (name . node) :in (node-let-bindings node)
           :nconc (node-variables-g node :variable-namespace-only variable-namespace-only))
     (node-variables-g (node-let-subexpr node) :variable-namespace-only variable-namespace-only)))

  (:method ((node node-lisp) &key variable-namespace-only)
    (declare (ignore variable-namespace-only))
    (mapcar #'cdr (node-lisp-vars node)))

  (:method ((node match-branch) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (node-variables-g (match-branch-body node) :variable-namespace-only variable-namespace-only))

  (:method ((node node-match) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (nconc
     (node-variables-g (node-match-expr node) :variable-namespace-only variable-namespace-only)
     (mapcan
      (lambda (node)
        (node-variables-g node :variable-namespace-only variable-namespace-only))
      (node-match-branches node))))

  (:method ((node node-while) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (nconc (node-variables-g (node-while-expr node) :variable-namespace-only variable-namespace-only)
           (node-variables-g (node-while-body node) :variable-namespace-only variable-namespace-only)))

  (:method ((node node-while-let) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (nconc (node-variables-g (node-while-let-expr node) :variable-namespace-only variable-namespace-only)
           (node-variables-g (node-while-let-body node) :variable-namespace-only variable-namespace-only)))

  (:method ((node node-loop) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (node-variables-g (node-loop-body node) :variable-namespace-only variable-namespace-only))

  (:method ((node node-break) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (declare (ignore variable-namespace-only))
    nil)

  (:method ((node node-continue) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (declare (ignore variable-namespace-only))
    nil)
  
  (:method ((node node-seq) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (mapcan
     (lambda (node)
       (node-variables-g node :variable-namespace-only variable-namespace-only))
     (node-seq-nodes node)))

  (:method ((node node-return) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (node-variables-g (node-return-expr node) :variable-namespace-only variable-namespace-only))

  (:method ((node node-field) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (node-variables-g (node-field-dict node) :variable-namespace-only variable-namespace-only))

  (:method ((node node-dynamic-extent) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (nconc
     (node-variables-g (node-dynamic-extent-node node) :variable-namespace-only variable-namespace-only)
     (node-variables-g (node-dynamic-extent-body node) :variable-namespace-only variable-namespace-only)))

  (:method ((node node-bind) &key variable-namespace-only)
    (declare (values parser:identifier-list &optional))
    (nconc
     (node-variables-g (node-bind-expr node) :variable-namespace-only variable-namespace-only)
     (node-variables-g (node-bind-body node) :variable-namespace-only variable-namespace-only))))
