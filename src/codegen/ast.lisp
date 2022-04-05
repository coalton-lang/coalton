(defpackage #:coalton-impl/codegen/ast
  (:use
   #:cl
   #:coalton-impl/util)
  (:local-nicknames
   (#:tc #:coalton-impl/typechecker))
  (:import-from
   #:coalton-impl/ast
   #:pattern)
  (:import-from
   #:coalton-impl/algorithm
   #:tarjan-scc)
  (:export
   #:node                               ; STRUCT
   #:node-type                          ; ACCESSOR
   #:node-list                          ; TYPE
   #:binding-list                       ; TYPE
   #:node-literal                       ; STRUCT
   #:node-literal-value                 ; ACCESSOR
   #:node-variable                      ; STRUCT
   #:node-variable-p                    ; FUNCTION
   #:node-variable-value                ; ACCESSOR
   #:node-application                   ; STRUCT
   #:node-application-p                 ; FUNCTION
   #:node-application-rator             ; ACCESSOR
   #:node-application-rands             ; ACCESSOR
   #:node-application-pure              ; ACCESSOR
   #:node-direct-application            ; STRUCT
   #:node-direct-application-rator-type ; ACCESSOR
   #:node-direct-application-rator      ; ACCESSOR
   #:node-direct-application-rands      ; ACCESSOR
   #:node-abstraction                   ; STRUCT
   #:node-abstraction-vars              ; ACCESSOR
   #:node-abstraction-subexpr           ; ACCESSOR
   #:node-abstraction-p                 ; FUNCTION
   #:node-bare-abstraction              ; STRUCT
   #:node-bare-abstraction-vars         ; ACCESSOR
   #:node-bare-abstraction-subexpr      ; ACCESSOR
   #:node-let                           ; STRUCT
   #:node-let-p                         ; FUNCTION
   #:node-let-bindings                  ; ACCESSOR
   #:node-let-subexpr                   ; ACCESSOR
   #:node-lisp                          ; STRUCT
   #:node-lisp-vars                     ; ACCESOR
   #:node-lisp-form                     ; ACCESOR
   #:match-branch                       ; STRUCT
   #:match-branch-pattern               ; ACCESSOR
   #:match-branch-bindings              ; ACCESSOR
   #:match-branch-body                  ; ACCESSOR
   #:branch-list                        ; TYPE
   #:node-match                         ; STRUCT
   #:node-match-expr                    ; ACCESSOR
   #:node-match-branches                ; ACCESSOR
   #:node-seq                           ; STRUCT
   #:node-seq-nodes                     ; ACCESSOR
   #:node-variables                     ; FUNCTION
   #:node-binding-sccs                  ; FUNCTION
   #:node-return                        ; STRUCT
   #:node-return-expr                   ; ACCESSOR
   ))

(in-package #:coalton-impl/codegen/ast)

(defstruct (node (:constructor nil))
  (type (required 'type) :type (or null tc:ty) :read-only t))

(defun node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-p x)))

(deftype node-list ()
  '(satisfies node-list-p))

(defun binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol node))) x)))

(deftype binding-list ()
  '(satisfies binding-list-p))

(defstruct (node-literal
            (:include node)
            (:constructor node-literal (type value)))
  "Literal values like 1 or \"hello\""
  (value (required 'value) :type literal-value :read-only t))

(defstruct (node-variable
            (:include node)
            (:constructor node-variable (type value)))
  "Variables like x or y"
  (value (required 'value) :type symbol :read-only t))

(defstruct (node-application
            (:include node)
            (:constructor node-application (type rator rands &key (pure nil))))
  "Function application (f x)"
  (rator (required 'rator) :type node      :read-only t)
  (rands (required 'rands) :type node-list :read-only t)
  (pure  (required 'pure)  :type boolean   :read-only t))

(defstruct (node-direct-application
            (:include node)
            (:constructor node-direct-application (type rator-type rator rands)))
  "Fully saturated function application of a known function"
  (rator-type (required 'rator-type) :type tc:ty     :read-only t)
  (rator      (required 'rator)      :type symbol    :read-only t)
  (rands      (required 'rands)      :type node-list :read-only t))

(defstruct (node-abstraction
            (:include node)
            (:constructor node-abstraction (type vars subexpr)))
  "Lambda literals (fn (x) x)"
  (vars    (required 'vars)    :type symbol-list        :read-only t)
  (subexpr (required 'subexpr) :type node               :read-only t))

(defstruct (node-bare-abstraction
            (:include node)
            (:constructor node-bare-abstraction (type vars subexpr)))
  "Lambda literals which do not need be wrapped in function-entries.
This is used to speedup method calls. This can be done because
although method calls are to an unknown function, they should always
be a fully saturated call."
  (vars    (required 'vars)    :type symbol-list :read-only t)
  (subexpr (required 'subexpr) :type node        :read-only t))


(defstruct (node-let
            (:include node)
            (:constructor node-let (type bindings subexpr)))
  "Introduction of local mutually-recursive bindings (let ((x 2)) (+ x x))"
  (bindings (requried 'bindings) :type binding-list :read-only t)
  (subexpr  (required 'subexpr)  :type node         :read-only t))

(defstruct (node-lisp
            (:include node)
            (:constructor node-lisp (type vars form)))
  "An embedded lisp form"
  (vars (required 'vars) :type list :read-only t)
  (form (required 'form) :type t    :read-only t))

(defstruct (match-branch
            (:constructor match-branch (pattern bindings body)))
  "A branch of a match statement"
  (pattern  (required 'pattern)  :type pattern            :read-only t)
  (bindings (required 'bindings) :type tc:ty-binding-list :read-only t)
  (body     (required 'body)     :type node               :read-only t))

(defun branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'match-branch-p x)))

(deftype branch-list ()
  '(satisfies branch-list-p))

(defstruct (node-match
            (:include node)
            (:constructor node-match (type expr branches)))
  "A pattern matching construct. Uses MATCH-BRANCH to represent branches"
  (expr (required 'expr) :type node :read-only t)
  (branches (required 'branches) :type branch-list :read-only t))

(defstruct (node-seq
            (:include node)
            (:constructor node-seq (type nodes)))
  "A series of statements to be executed sequentially"
  (nodes (required 'nodes) :type node-list :read-only t))

(defstruct (node-return
            (:include node)
            (:constructor node-return (type expr)))
  "A return statement, used for early returns in functions"
  (expr (required 'expr) :type node :read-only t))

(defun node-variables (node &key variable-namespace-only)
  "Returns a deduplicated list of symbols representing variables in
both CL namespaces appearing in NODE"
  (declare (type node node)
           (type boolean variable-namespace-only)
           (values symbol-list &optional))
  (remove-duplicates (node-variables-g node :variable-namespace-only variable-namespace-only) :test #'equalp))

(defgeneric node-variables-g (node &key variable-namespace-only)
  (:method ((node node-literal) &key variable-namespace-only)
    (declare (ignore node variable-namespace-only)
             (values symbol-list))
    nil)

  (:method ((node node-variable) &key variable-namespace-only)
    (declare (ignore variable-namespace-only)
             (values symbol-list &optional))
    (list (node-variable-value node)))

  (:method ((node node-application) &key variable-namespace-only)
    (declare (values symbol-list &optional))
    (append
     (node-variables-g (node-application-rator node) :variable-namespace-only variable-namespace-only)
     (mapcan
      (lambda (node)
        (node-variables-g node :variable-namespace-only variable-namespace-only))
      (node-application-rands node))))

  (:method ((node node-abstraction) &key variable-namespace-only)
    (declare (values symbol-list &optional))
    (node-variables-g (node-abstraction-subexpr node) :variable-namespace-only variable-namespace-only))

  (:method ((node node-bare-abstraction) &key variable-namespace-only)
    (declare (values symbol-list &optional))
    (node-variables-g (node-bare-abstraction-subexpr node) :variable-namespace-only variable-namespace-only))

  (:method ((node node-direct-application) &key variable-namespace-only)
    (declare (values symbol-list &optional))
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
    (declare (values symbol-list))
    (append
     (loop :for (name . node) :in (node-let-bindings node)
           :append (node-variables-g node :variable-namespace-only variable-namespace-only))
     (node-variables-g (node-let-subexpr node) :variable-namespace-only variable-namespace-only)))

  (:method ((node node-lisp) &key variable-namespace-only)
    (declare (ignore variable-namespace-only))
    (mapcar #'cdr (node-lisp-vars node)))

  (:method ((node match-branch) &key variable-namespace-only)
    (declare (values symbol-list &optional))
    (node-variables-g (match-branch-body node) :variable-namespace-only variable-namespace-only))

  (:method ((node node-match) &key variable-namespace-only)
    (declare (values symbol-list &optional))
    (append
     (node-variables-g (node-match-expr node) :variable-namespace-only variable-namespace-only)
     (mapcan
      (lambda (node)
        (node-variables-g node :variable-namespace-only variable-namespace-only))
      (node-match-branches node))))

  (:method ((node node-seq) &key variable-namespace-only)
    (declare (values symbol-list &optional))
    (mapcan
     (lambda (node)
       (node-variables-g node :variable-namespace-only variable-namespace-only))
     (node-seq-nodes node)))

  (:method ((node node-return) &key variable-namespace-only)
    (declare (values symbol-list &optional))
    (node-variables-g (node-return-expr node) :variable-namespace-only variable-namespace-only)))


(defun node-binding-sccs (bindings)
  "Returns a list of SCCs ordered from least to most depended on."
  (declare (type binding-list bindings))
  (let ((binding-names (mapcar #'car bindings)))
    (reverse
     (tarjan-scc
      (loop :for (name . node) :in bindings
            :collect (cons name (intersection binding-names (node-variables node))))))))
