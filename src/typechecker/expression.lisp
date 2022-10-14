;;;;
;;;; Mirror of expression nodes in src/parser/expression.lisp with
;;;; types attached.
;;;;

(defpackage #:coalton-impl/typechecker/expression
  (:use
   #:cl
   #:coalton-impl/typechecker/pattern)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker/stage-1)
   (#:util #:coalton-impl/util))
  (:export
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
   #:node-lisp-var-names                ; ACCESSOR
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
   #:node-do-bind-pattern               ; ACCESSOR
   #:node-do-bind-expr                  ; ACCESSOR
   #:node-do-bind-source                ; ACCESSOR
   #:node-do-body-element               ; TYPE
   #:node-body-element-list             ; TYPE
   #:node-do                            ; STRUCT
   #:make-node-do                       ; CONSTRUCTOR
   #:node-do-nodes                      ; ACCESSOR
   #:node-do-last-node                  ; ACCESSOR
   ))

(in-package #:coalton-impl/typechecker/expression)

;;;
;;; Expression Nodes
;;;

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

(defstruct (node-body
            (:copier nil))
  (nodes     (util:required 'node)      :type node-body-element-list :read-only t)
  (last-node (util:required 'last-node) :type node                   :read-only t))

(defstruct (node-abstraction
            (:include node)
            (:copier nil))
  (vars (util:required 'vars) :type node-variable-list :read-only t)
  (body (util:required 'body) :type node-body          :read-only t))

(defstruct (node-let-binding
            (:copier nil))
  (name          (util:required 'name)          :type node-variable             :read-only t)
  (value         (util:required 'value)         :type node                      :read-only t)
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
  (vars      (util:required 'vars)      :type node-variable-list :read-only t)
  (var-names (util:required 'var-names) :type util:symbol-list   :read-only t)
  (body      (util:required 'body)      :type t                  :read-only t))

(defstruct (node-match-branch
            (:copier nil))
  (pattern    (util:required 'pattern) :type pattern   :read-only t)
  (body       (util:required 'body)    :type node-body :read-only t)
  (source     (util:required 'source)  :type cons      :read-only t))

(defun node-match-branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-match-branch-p x)))

(deftype node-match-branch-list ()
  '(satisfies node-match-branch-list-p))

(defstruct (node-match
            (:include node)
            (:copier nil))
  (expr     (util:required 'expr)         :type node                   :read-only t)
  (branches (util:required 'branches)     :type node-match-branch-list :read-only t))

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
  (pattern (util:required 'name)   :type pattern :read-only t)
  (expr    (util:required 'expr)   :type node    :read-only t)
  (source  (util:required 'source) :type cons    :read-only t))

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

;;;
;;; Methods
;;;

(defmethod tc:apply-substitution (subs (node node-variable))
  (declare (type tc:substitution-list subs)
           (values node-variable))
  (make-node-variable
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :name (node-variable-name node)))

(defmethod tc:apply-substitution (subs (node node-literal))
  (declare (type tc:substitution-list subs)
           (values node-literal))
  (make-node-literal
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :value (node-literal-value node)))

(defmethod tc:apply-substitution (subs (node node-integer-literal))
  (declare (type tc:substitution-list subs)
           (values node-integer-literal))
  (make-node-integer-literal
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :value (node-integer-literal-value node)))

(defmethod tc:apply-substitution (subs (node node-bind))
  (declare (type tc:substitution-list subs)
           (values node-bind))
  (make-node-bind
   :pattern (tc:apply-substitution subs (node-bind-pattern node))
   :expr (tc:apply-substitution subs (node-bind-expr node))
   :source (node-bind-source node)))

(defmethod tc:apply-substitution (subs (node node-body))
  (declare (type tc:substitution-list subs)
           (values node-body))
  (make-node-body
   :nodes (tc:apply-substitution subs (node-body-nodes node))
   :last-node (tc:apply-substitution subs (node-body-last-node node))))

(defmethod tc:apply-substitution (subs (node node-abstraction))
  (declare (type tc:substitution-list subs)
           (values node-abstraction))
  (make-node-abstraction
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :vars (tc:apply-substitution subs (node-abstraction-vars node))
   :body (tc:apply-substitution subs (node-abstraction-body node))))

(defmethod tc:apply-substitution (subs (node node-let-binding))
  (declare (type tc:substitution-list subs)
           (values node-let-binding))
  (make-node-let-binding
   :name (tc:apply-substitution subs (node-let-binding-name node))
   :value (tc:apply-substitution subs (node-let-binding-value node))
   :source (node-let-binding-source node)))

(defmethod tc:apply-substitution (subs (node node-let))
  (declare (type tc:substitution-list subs)
           (values node-let))
  (make-node-let
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :bindings (tc:apply-substitution subs (node-let-bindings node))
   :body (tc:apply-substitution subs (node-let-body node))))

(defmethod tc:apply-substitution (subs (node node-lisp))
  (declare (type tc:substitution-list subs)
           (values node-lisp))
  (make-node-lisp
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :vars (tc:apply-substitution subs (node-lisp-vars node))
   :var-names (node-lisp-var-names node)
   :body (node-lisp-body node)))

(defmethod tc:apply-substitution (subs (node node-match-branch))
  (declare (type tc:substitution-list subs)
           (values node-match-branch))
  (make-node-match-branch
   :pattern (tc:apply-substitution subs (node-match-branch-pattern node))
   :body (tc:apply-substitution subs (node-match-branch-body node))
   :source (node-match-branch-source node)))

(defmethod tc:apply-substitution (subs (node node-match))
  (declare (type tc:substitution-list subs)
           (values node-match))
  (make-node-match
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :expr (tc:apply-substitution subs (node-match-expr node))
   :branches (tc:apply-substitution subs (node-match-branches node))))

(defmethod tc:apply-substitution (subs (node node-progn))
  (declare (type tc:substitution-list subs)
           (values node-progn))
  (make-node-progn
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :body (tc:apply-substitution subs (node-progn-body node))))

(defmethod tc:apply-substitution (subs (node node-return))
  (declare (type tc:substitution-list subs)
           (values node-return))
  (make-node-return
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :expr (tc:apply-substitution subs (node-return-expr node))))

(defmethod tc:apply-substitution (subs (node node-application))
  (declare (type tc:substitution-list subs)
           (values node-application))
  (make-node-application
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :rator (tc:apply-substitution subs (node-application-rator node))
   :rands (tc:apply-substitution subs (node-application-rands node))))

(defmethod tc:apply-substitution (subs (node node-or))
  (declare (type tc:substitution-list subs)
           (values node-or))
  (make-node-or
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :nodes (tc:apply-substitution subs (node-or-nodes node))))

(defmethod tc:apply-substitution (subs (node node-and))
  (declare (type tc:substitution-list subs)
           (values node-and))
  (make-node-and
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :nodes (tc:apply-substitution subs (node-and-nodes node))))

(defmethod tc:apply-substitution (subs (node node-if))
  (declare (type tc:substitution-list subs)
           (values node-if))
  (make-node-if
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :expr (tc:apply-substitution subs (node-if-expr node))
   :then (tc:apply-substitution subs (node-if-then node))
   :else (tc:apply-substitution subs (node-if-else node))))

(defmethod tc:apply-substitution (subs (node node-when))
  (declare (type tc:substitution-list subs)
           (values node-when))
  (make-node-when
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :expr (tc:apply-substitution subs (node-when-expr node))
   :body (tc:apply-substitution subs (node-when-body node))))

(defmethod tc:apply-substitution (subs (node node-unless))
  (declare (type tc:substitution-list subs)
           (values node-unless))
  (make-node-unless
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :expr (tc:apply-substitution subs (node-unless-expr node))
   :body (tc:apply-substitution subs (node-unless-body node))))

(defmethod tc:apply-substitution (subs (node node-cond-clause))
  (declare (type tc:substitution-list subs)
           (values node-cond-clause))
  (make-node-cond-clause
   :expr (tc:apply-substitution subs (node-cond-clause-expr node))
   :body (tc:apply-substitution subs (node-cond-clause-body node))
   :source (node-cond-clause-source node)))

(defmethod tc:apply-substitution (subs (node node-cond))
  (declare (type tc:substitution-list subs)
           (values node-cond))
  (make-node-cond
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :clauses (tc:apply-substitution subs (node-cond-clauses node))))

(defmethod tc:apply-substitution (subs (node node-do-bind))
  (declare (type tc:substitution-list subs)
           (values node-do-bind))
  (make-node-do-bind
   :pattern (tc:apply-substitution subs (node-do-bind-pattern node))
   :expr (tc:apply-substitution subs (node-do-bind-expr node))
   :source (node-do-bind-source node)))

(defmethod tc:apply-substitution (subs (node node-do))
  (declare (type tc:substitution-list subs)
           (values node-do))
  (make-node-do
   :type (tc:apply-substitution subs (node-type node))
   :source (node-source node)
   :nodes (tc:apply-substitution subs (node-do-nodes node))
   :last-node (tc:apply-substitution subs (node-do-last-node node))))

