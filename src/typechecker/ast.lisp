(defpackage #:coalton-impl/typechecker/ast
  (:use #:cl)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1)
   (#:util #:coalton-impl/util))
  (:export
   #:pattern                            ; STRUCT
   #:pattern-type                       ; ACCESSOR
   #:pattern-source                     ; ACCESSOR
   #:pattern-list                       ; TYPE
   #:pattern-var                        ; STRUCT
   #:make-pattern-var                   ; ACCESSOR
   #:pattern-var-name                   ; ACCESSOR
   #:pattern-literal                    ; STRUCT
   #:make-pattern-literal               ; CONSTRUCTOR
   #:pattern-literal-value              ; ACCESSOR
   #:pattern-wildcard                   ; STRUCT
   #:make-pattern-wildcard              ; ACCESSOR
   #:pattern-constructor                ; STRUCT
   #:make-pattern-constructor           ; CONSTRUCTOR
   #:pattern-constructor-name           ; ACCESSOR
   #:pattern-constructor-patterns       ; ACCESSOR
   #:parse-pattern                      ; FUNCTION
   #:pattern-variables                  ; FUNCTION
   
   #:node                               ; STRUCT
   #:node-type                          ; ACCESSOR
   #:node-source                        ; ACCESSOR
   #:node-list                          ; TYPE
   #:node-variable                      ; STRUCT
   #:make-node-variable                 ; CONSTRUCTOR
   #:node-variable-name                 ; ACCESSOR
   #:node-variable-list                 ; TYPE
   #:node-literal                       ; STRUCT
   #:make-node-literal                  ; CONSTRUCTOR
   #:node-literal-value                 ; ACCESSOR
   #:node-integer-literal               ; STRUCT
   #:make-node-integer-literal          ; CONSTRUCTOR
   #:node-integer-literal-value         ; ACCESSOR
   #:node-bind                          ; STRUCT
   #:make-node-bind                     ; CONSTRUCTOR
   #:node-bind-pattern                  ; ACCESSOR
   #:node-bind-expr                     ; ACCESSOR
   #:node-bind-source                   ; ACCESSOR
   #:node-body-element                  ; TYPE
   #:node-body-element-list             ; TYPE
   #:node-body                          ; STRUCT
   #:make-node-body                     ; CONSTRUCTOR
   #:node-body-nodes                    ; ACCESSOR
   #:node-body-last-node                ; ACCESSOR
   #:node-abstraction                   ; STRUCT
   #:make-node-abstraction              ; CONSTRUCTOR
   #:node-abstraction-vars              ; ACCESSOR
   #:node-abstraction-body              ; ACCESSOR
   #:node-abstraction-p                 ; FUNCTION
   #:node-let-binding                   ; STRUCT
   #:make-node-let-binding              ; CONSTRUCTOR
   #:node-let-binding-name              ; ACCESSOR
   #:node-let-binding-value             ; ACCESSOR
   #:node-let-binding-explicit-type     ; ACCESSOR
   #:node-let-binding-source            ; ACCESSOR
   #:node-let-binding-list              ; TYPE
   #:node-let-declare                   ; STRUCT
   #:make-node-let-declare              ; CONSTRUCTOR
   #:node-let-declare-name              ; ACCESSOR
   #:node-let-declare-type              ; ACCESSOR
   #:node-let-declare-source            ; ACCESSOR
   #:node-let-declare-list              ; TYPE
   #:node-let                           ; STRUCT
   #:make-node-let                      ; CONSTRUCTOR
   #:node-let-bindings                  ; ACCESSOR
   #:node-let-declares                  ; ACCESSOR
   #:node-let-body                      ; ACCESSOR
   #:node-lisp                          ; STRUCT
   #:make-node-lisp                     ; CONSTRUCTOR
   #:node-lisp-type                     ; ACCESSOR
   #:node-lisp-vars                     ; ACCESSOR
   #:node-lisp-body                     ; ACCESSOR
   #:node-match-branch                  ; STRUCT
   #:make-node-match-branch             ; CONSTRUCTOR
   #:node-match-branch-pattern          ; ACCESSOR
   #:node-match-branch-body             ; ACCESSOR
   #:node-match-branch-source           ; ACCESSOR
   #:node-match-branch-list             ; TYPE
   #:node-match                         ; STRUCT
   #:make-node-match                    ; CONSTRUCTOR
   #:node-match-expr                    ; ACCESSOR
   #:node-match-branches                ; ACCESSOR
   #:node-progn                         ; STRUCT
   #:make-node-progn                    ; CONSTRUCTOR
   #:node-progn-body                    ; ACCESSOR
   #:node-the                           ; STRUCT
   #:make-node-the                      ; CONSTRUCTOR
   #:node-the-type                      ; ACCESSOR
   #:node-the-expr                      ; ACCESSOR
   #:node-return                        ; STRUCT
   #:make-node-return                   ; CONSTRUCTOR
   #:node-return-expr                   ; ACCESSOR
   #:node-application                   ; STRUCT
   #:make-node-application              ; CONSTRUCTOR
   #:node-application-rator             ; ACCESSOR
   #:node-application-rands             ; ACCESSOR
   #:node-or                            ; STRUCT
   #:make-node-or                       ; CONSTRUCTOR
   #:node-or-nodes                      ; ACCESSOR
   #:node-and                           ; STRUCT
   #:make-node-and                      ; CONSTRUCTOR
   #:node-and-nodes                     ; ACCESSOR
   #:node-if                            ; STRUCT
   #:make-node-if                       ; CONSTRUCTOR
   #:node-if-expr                       ; ACCESSOR
   #:node-if-then                       ; ACCESSOR
   #:node-if-else                       ; ACCESSOR
   #:node-when                          ; STRUCT
   #:make-node-when                     ; CONSTRUCTOR
   #:node-when-expr                     ; ACCESSOR
   #:node-when-body                     ; ACCESSOR
   #:node-unless                        ; STRUCT
   #:make-node-unless                   ; CONSTRUCTOR
   #:node-unless-expr                   ; ACCESSOR
   #:node-unless-body                   ; ACCESSOR
   #:node-cond-clause                   ; STRUCT
   #:make-node-cond-clause              ; CONSTRUCTOR
   #:node-cond-clause-expr              ; ACCESSOR
   #:node-cond-clause-body              ; ACCESSOR
   #:node-cond-clause-source            ; ACCESSOR
   #:node-cond-clause-list              ; TYPE
   #:node-cond                          ; STRUCT
   #:make-node-cond                     ; CONSTRUCTOR
   #:node-cond-clauses                  ; ACCESSOR
   #:node-do-bind                       ; STRUCT
   #:make-node-do-bind                  ; CONSTRUCTOR
   #:node-do-bind-name                  ; ACCESSOR
   #:node-do-bind-expr                  ; ACCESSOR
   #:node-do-bind-source                ; ACCESSOR
   #:node-do-body-element               ; TYPE
   #:node-body-element-list             ; TYPE
   #:node-do                            ; STRUCT
   #:make-node-do                       ; CONSTRUCTOR
   #:node-do-nodes                      ; ACCESSOR
   #:node-do-last-node                  ; ACCESSOR
   ))

(in-package #:coalton-impl/typechecker/ast)

;;;
;;; Typed nodes
;;;
;;; This is a mirror of the parser AST with types attached to all nodes.
;;;


(defstruct (pattern
            (:constructor nil)
            (:copier nil))
  (type   (util:required 'type)   :type tc:qualified-ty :read-only t)
  (source (util:required 'source) :type cons            :read-only t))

(defun pattern-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'pattern-p x)))

(deftype pattern-list ()
  '(satisfies pattern-list-p))

(defstruct (pattern-var
            (:include pattern)
            (:copier nil))
  (name (util:required 'name) :type parser:identifier :read-only t))

(defstruct (pattern-literal
            (:include pattern)
            (:copier nil))
  (value (util:required 'value) :type util:literal-value :read-only t))

(defstruct (pattern-wildcard
            (:include pattern)
            (:copier nil)))

(defstruct (pattern-constructor
            (:include pattern)
            (:copier nil))
  (name     (util:required 'name)     :type parser:identifier :read-only t)
  (patterns (util:required 'patterns) :type pattern-list      :read-only t))

;;

(defstruct (node
            (:constructor nil)
            (:copier nil))
  (type   (util:required 'type)   :type tc:qualified-ty :read-only t)
  (source (util:required 'source) :type cons            :read-only t))

(defun node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-p x)))

(deftype node-list ()
  '(satisfies node-list-p))

(defstruct (node-variable
            (:include node)
            (:copier nil))
  (name (util:required 'name) :type parser:identifier :read-only t))

(defun node-variable-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-variable-p x)))

(deftype node-variable-list ()
  '(satisfies node-variable-list-p))

(defstruct (node-literal
            (:include node)
            (:copier nil))
  (value (util:required 'value) :type (and util:literal-value (not integer)) :read-only t))

(defstruct (node-integer-literal
            (:include node)
            (:copier nil))
  (value (util:required 'value) :type integer :read-only t))

;; Does not subclass node, can only appear in a node body
;;
(defstruct (node-bind
            (:copier nil))
  (pattern (util:required 'pattern) :type pattern         :read-only t)
  (expr    (util:required 'expr)    :type node            :read-only t)
  (source  (util:required 'source)  :type cons            :read-only t))

(deftype node-body-element ()
  '(or node node-bind))

(defun node-body-element-p (x)
  (typep x 'node-body-element))

(defun node-body-element-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-body-element-p x)))

(deftype node-body-element-list ()
  '(satisfies node-body-element-list-p))

;;
;; Does not subclass node, can only appear directly within some nodes
;;
;; - must contain at least one node
;; - cannot be terminated by a `node-bind'
;; - does not have source information (but it's children do)
;;
(defstruct (node-body
            (:copier nil))
  (nodes     (util:required 'node)      :type node-body-element-list :read-only t)
  (last-node (util:required 'last-node) :type node                   :read-only t))

(defstruct (node-abstraction
            (:include node)
            (:copier nil))
  (vars (util:required 'vars) :type node-variable-list :read-only t)
  (body (util:required 'body) :type node-body          :read-only t))

;; TODO: handle recursive construction here
(defstruct (node-let-binding
            (:copier nil))
  (name          (util:required 'name)          :type node-variable             :read-only t)
  (value         (util:required 'value)         :type node                      :read-only t)
  (explicit-type (util:required 'explicit-type) :type (or null tc:qualified-ty) :read-only t)
  (source        (util:required 'source)        :type cons                      :read-only t))

(defun node-let-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-let-binding-p x)))

(deftype node-let-binding-list ()
  '(satisfies node-let-binding-list-p))

(defstruct (node-let
            (:include node)
            (:copier nil))
  (bindings (util:required 'bindings) :type node-let-binding-list :read-only t)
  (body     (util:required 'body)     :type node-body             :read-only t))

(defstruct (node-lisp
            (:include node)
            (:copier nil))
  (vars (util:required 'vars) :type node-variable-list :read-only t)
  (body (util:required 'body) :type cst:cst            :read-only t))

(defstruct (node-match-branch
            (:copier nil))
  (pattern (util:required 'pattern) :type pattern   :read-only t)
  (body    (util:required 'body)    :type node-body :read-only t)
  (source  (util:required 'source)  :type cons      :read-only t))

(defun node-match-branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-match-branch-p x)))

(deftype node-match-branch-list ()
  '(satisfies node-match-branch-list-p))

(defstruct (node-match
            (:include node)
            (:copier nil))
  (expr     (util:required 'expr)     :type node                   :read-only t)
  (branches (util:required 'branches) :type node-match-branch-list :read-only t))

(defstruct (node-progn
            (:include node)
            (:copier nil))
  (body (util:required 'body) :type node-body :read-only t))

;; node-the does not exist in this AST!

(defstruct (node-return
            (:include node)
            (:copier nil))
  ;; Either the returned expression or null in the case of "(return)"
  (expr (util:required 'expr) :type (or null node) :read-only t))

(defstruct (node-application
            (:include node)
            (:copier nil))
  (rator (util:required 'rator) :type node      :read-only t)
  (rands (util:required 'rands) :type node-list :read-only t))

(defstruct (node-or
            (:include node)
            (:copier nil))
  (nodes (util:required 'nodes) :type node-list :read-only t))

(defstruct (node-and
            (:include node)
            (:copier nil))
  (nodes (util:required 'nodes) :type node-list :read-only t))

(defstruct (node-if
            (:include node)
            (:copier nil))
  (expr (util:required 'expr) :type node :read-only t)
  (then (util:required 'expr) :type node :read-only t)
  (else (util:required 'else) :type node :read-only t))

(defstruct (node-when
            (:include node)
            (:copier nil))
  (expr (util:required 'expr) :type node      :read-only t)
  (body (util:required 'body) :type node-body :read-only t))

(defstruct (node-unless
            (:include node)
            (:copier nil))
  (expr (util:required 'expr) :type node      :read-only t)
  (body (util:required 'body) :type node-body :read-only t))

(defstruct (node-cond-clause
            (:copier nil))
  (expr   (util:required 'expr)   :type node      :read-only t)
  (body   (util:required 'body)   :type node-body :read-only t)
  (source (util:required 'source) :type cons      :read-only t))

(defun node-cond-clause-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-cond-clause-p x)))

(deftype node-cond-clause-list ()
  '(satisfies node-cond-clause-list-p))

(defstruct (node-cond
            (:include node)
            (:copier nil))
  (clauses (util:required 'clauses) :type node-cond-clause-list :read-only t))

(defstruct (node-do-bind
            (:copier nil))
  (name   (util:required 'name)   :type node-variable :read-only t)
  (expr   (util:required 'expr)   :type node          :read-only t)
  (source (util:required 'source) :type cons          :read-only t))

(deftype node-do-body-element ()
  '(or node node-bind node-do-bind))

(defun node-do-body-element-p (x)
  (typep x 'node-do-body-element))

(defun node-do-body-element-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-do-body-element-p x)))

(deftype node-do-body-element-list ()
  '(satisfies node-do-body-element-list-p))

(defstruct (node-do
            (:include node)
            (:copier nil))
  (nodes     (util:required 'nodes)     :type node-do-body-element-list :read-only t)
  (last-node (util:required 'last-node) :type node                      :read-only t))

;;
;; apply-substitution instances for nodes
;;

(defmethod tc:apply-substitution (subs (node pattern-var))
  (declare (type tc:substitution-list subs)
           (values pattern-var &optional))
  (make-pattern-var
   :type (tc:apply-substitution subs (pattern-type node))
   :source (pattern-source node)
   :name (pattern-var-name node)))

(defmethod tc:apply-substitution (subs (node pattern-literal))
  (declare (type tc:substitution-list subs)
           (values pattern-literal &optional))
  (make-pattern-literal
   :type (tc:apply-substitution subs (pattern-type node))
   :source (pattern-source node)
   :value (pattern-literal-value node)))

(defmethod tc:apply-substitution (subs (node pattern-wildcard))
  (declare (type tc:substitution-list subs)
           (values pattern-wildcard &optional))
  (make-pattern-wildcard
   :type (tc:apply-substitution subs (pattern-type node))
   :source (pattern-source node)))

(defmethod tc:apply-substitution (subs (node pattern-constructor))
  (declare (type tc:substitution-list subs)
           (values pattern-constructor &optional))
  (make-pattern-constructor
   :type (tc:apply-substitution subs (pattern-type node))
   :source (pattern-source node)
   :name (pattern-constructor-name node)
   :patterns (tc:apply-substitution subs (pattern-constructor-patterns node))))


(defmethod tc:apply-substitution (subs (node node-variable))
  (declare (type tc:substitution-list subs)
           (values node-variable &optional))
  (make-node-variable
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :name (node-variable-name node)))

(defmethod tc:apply-substitution (subs (node node-literal))
  (declare (type tc:substitution-list subs)
           (values node-literal &optional))
  (make-node-literal
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :value (node-literal-value node)))

(defmethod tc:apply-substitution (subs (node node-integer-literal))
  (declare (type tc:substitution-list subs)
           (values node-integer-literal &optional))
  (make-node-integer-literal
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :value (node-integer-literal-value node)))

(defmethod tc:apply-substitution (subs (node node-bind))
  (declare (type tc:substitution-list subs)
           (values node-bind &optional))
  (make-node-bind
   :pattern (tc:apply-substitution subs (node-bind-pattern node))
   :expr (tc:apply-substitution subs (node-bind-expr node))
   :source (node-bind-source node)))

(defmethod tc:apply-substitution (subs (node node-body))
  (declare (type tc:substitution-list subs)
           (values node-body &optional))
  (make-node-body
   :nodes (tc:apply-substitution subs (node-body-nodes node))
   :last-node (tc:apply-substitution subs (node-body-last-node node))))

(defmethod tc:apply-substitution (subs (node node-abstraction))
  (declare (type tc:substitution-list subs)
           (values node-abstraction &optional))
  (make-node-abstraction
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :vars (tc:apply-substitution subs (node-abstraction-vars node))
   :body (tc:apply-substitution subs (node-abstraction-body node))))

(defmethod tc:apply-substitution (subs (node node-let-binding))
  (declare (type tc:substitution-list subs)
           (values node-let-binding &optional))
  (make-node-let-binding
   :name (tc:apply-substitution subs (node-let-binding-name node))
   :value (tc:apply-substitution subs (node-let-binding-value node))
   :explicit-type (tc:apply-substitution subs (node-let-binding-explicit-type node))
   :source (node-let-binding-source node)))

(defmethod tc:apply-substitution (subs (node node-let))
  (declare (type tc:substitution-list subs)
           (values node-let &optional))
  (make-node-let
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :bindings (tc:apply-substitution subs (node-let-bindings node))
   :body (tc:apply-substitution subs (node-let-body node))))

(defmethod tc:apply-substitution (subs (node node-lisp))
  (declare (type tc:substitution-list subs)
           (values node-lisp &optional))
  (make-node-lisp
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :vars (tc:apply-substitution subs (node-lisp-vars node))
   :body (node-lisp-body node)))

(defmethod tc:apply-substitution (subs (node node-match-branch))
  (declare (type tc:substitution-list subs)
           (values node-match-branch &optional))
  (make-node-match-branch
   :pattern (tc:apply-substitution subs (node-match-branch-pattern node))
   :body (tc:apply-substitution subs (node-match-branch-body node))
   :source (node-match-branch-source node)))

(defmethod tc:apply-substitution (subs (node node-match))
  (declare (type tc:substitution-list subs)
           (values node-match &optional))
  (make-node-match
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :expr (tc:apply-substitution subs (node-match-expr node))
   :branches (tc:apply-substitution subs (node-match-branches node))))

(defmethod tc:apply-substitution (subs (node node-progn))
  (declare (type tc:substitution-list subs)
           (values node-progn &optional))
  (make-node-progn
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :body (tc:apply-substitution subs (node-progn-body node))))

(defmethod tc:apply-substitution (subs (node node-return))
  (declare (type tc:substitution-list subs)
           (values node-return &optional))
  (make-node-return
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :expr (tc:apply-substitution subs (node-return-expr node))))

(defmethod tc:apply-substitution (subs (node node-application))
  (declare (type tc:substitution-list subs)
           (values node-application &optional))
  (make-node-application
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :rator (tc:apply-substitution subs (node-application-rator node))
   :rands (tc:apply-substitution subs (node-application-rands node))))

(defmethod tc:apply-substitution (subs (node node-or))
  (declare (type tc:substitution-list subs)
           (values node-or &optional))
  (make-node-or
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :nodes (tc:apply-substitution subs (node-or-nodes node))))

(defmethod tc:apply-substitution (subs (node node-and))
  (declare (type tc:substitution-list subs)
           (values node-and &optional))
  (make-node-and
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :nodes (tc:apply-substitution subs (node-and-nodes node))))

(defmethod tc:apply-substitution (subs (node node-when))
  (declare (type tc:substitution-list subs)
           (values node-when &optional))
  (make-node-when
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :expr (tc:apply-substitution subs (node-when-expr node))
   :body (tc:apply-substitution subs (node-when-body node))))

(defmethod tc:apply-substitution (subs (node node-unless))
  (declare (type tc:substitution-list subs)
           (values node-unless &optional))
  (make-node-unless
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :expr (tc:apply-substitution subs (node-unless-expr node))
   :body (tc:apply-substitution subs (node-unless-body node))))

(defmethod tc:apply-substitution (subs (node node-cond-clause))
  (declare (type tc:substitution-list subs)
           (values node-cond-clause &optional))
  (make-node-cond-clause
   :expr (tc:apply-substitution subs (node-cond-clause-expr node))
   :body (tc:apply-substitution subs (node-cond-clause-body node))
   :source (node-cond-clause-source node)))

(defmethod tc:apply-substitution (subs (node node-cond))
  (declare (type tc:substitution-list subs)
           (values node-cond &optional))
  (make-node-cond
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :clauses (tc:apply-substitution subs (node-cond-clauses node))))
