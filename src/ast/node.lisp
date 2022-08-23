(defpackage #:coalton-impl/ast/node
  (:use #:cl)
  (:import-from
   #:coalton-impl/ast/pattern
   #:pattern)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:node                               ; STRUCT
   #:node-unparsed                      ; ACCESSOR
   #:node-list                          ; TYPE
   #:binding-list                       ; TYPE
   #:node-literal                       ; STRUCT
   #:make-node-literal                  ; CONSTRUCTOR
   #:node-literal-value                 ; ACCESSOR
   #:node-literal-p                     ; FUNCTION
   #:node-variable                      ; STRUCT
   #:make-node-variable                 ; CONSTRUCTOR
   #:node-variable-name                 ; ACCESSOR
   #:node-variable-p                    ; FUNCTION
   #:node-application                   ; STRUCT
   #:make-node-application              ; CONSTRUCTOR
   #:node-application-rator             ; ACCESSOR
   #:node-application-rands             ; ACCESSOR
   #:node-application-p                 ; FUNCTION
   #:node-abstraction                   ; STRUCT
   #:make-node-abstraction              ; CONSTRUCTOR
   #:node-abstraction-vars              ; ACCESSOR
   #:node-abstraction-subexpr           ; ACCESSOR
   #:node-abstraction-name-map          ; ACCESSOR
   #:node-abstraction-p                 ; FUNCTION
   #:node-let                           ; STRUCT
   #:make-node-let                      ; CONSTRUCTOR
   #:node-let-bindings                  ; ACCESSOR
   #:node-let-declared-types            ; ACCESSOR
   #:node-let-subexpr                   ; ACCESSOR
   #:node-let-name-map                  ; ACCESSOR
   #:node-let-p                         ; FUNCTION
   #:node-lisp                          ; STRUCT
   #:make-node-lisp                     ; CONSTRUCTOR
   #:node-lisp-type                     ; ACCESSOR
   #:node-lisp-variables                ; ACCESSOR
   #:node-lisp-form                     ; ACCESSOR
   #:node-lisp-p                        ; FUNCTION
   #:match-branch                       ; STRUCT
   #:make-match-branch                  ; CONSTRUCTOR
   #:match-branch-unparsed              ; ACCESSOR
   #:match-branch-pattern               ; ACCESSOR
   #:match-branch-subexpr               ; ACCESSOR
   #:match-branch-name-map              ; ACCESSOR
   #:match-branch-p                     ; FUNCTION
   #:node-match                         ; STRUCT
   #:make-node-match                    ; CONSTRUCTOR
   #:node-match-expr                    ; ACCESSOR
   #:node-match-branches                ; ACCESSOR
   #:node-match-p                       ; FUNCTION
   #:node-seq                           ; STRUCT
   #:make-node-seq                      ; CONSTRUCTOR
   #:node-seq-subnodes                  ; ACCESSOR
   #:node-seq-p                         ; FUNCTION
   #:node-the                           ; STRUCT
   #:make-node-the                      ; CONSTRUCTOR
   #:node-the-type                      ; ACCESSOR
   #:node-the-subnode                   ; ACCESSOR
   #:node-the-p                         ; FUNCTION
   #:node-return                        ; STRUCT
   #:make-node-return                   ; CONSTRUCTOR
   #:node-return-expr                   ; ACCESSOR
   #:node-return-p                      ; FUNCTION
   #:node-bind                          ; STRUCT
   #:make-node-bind                     ; CONSTRUCTOR
   #:node-bind-name                     ; ACCESSOR
   #:node-bind-expr                     ; ACCESSOR
   #:node-bind-body                     ; ACCESSOR
   #:node-bind-p                        ; FUNCTION
   #:bindings-to-dag                    ; FUNCTION
   ))

(in-package #:coalton-impl/ast/node)

(defstruct (node (:constructor nil))
  (unparsed (util:required 'unparsed) :type t :read-only t))

(defun node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-p x)))

(deftype node-list ()
  '(satisfies node-list-p))

(defun binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol node))) x)))

(deftype binding-list ()
  `(satisfies binding-list-p))

(defstruct (node-literal (:include node))
  "A literal value. These include things like integers and strings."
  (value (util:required 'value) :type util:literal-value :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-literal))

(defstruct (node-variable (:include node))
  (name (util:required 'name) :type symbol :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-variable))

(defstruct (node-application (:include node))
  (rator (util:required 'rator) :type node      :read-only t)
  (rands (util:required 'rands) :type node-list :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-application))

(defstruct (node-abstraction (:include node))
  (vars     (util:required 'vars)     :type t           :read-only t)
  (subexpr  (util:required 'subexpr)  :type node        :read-only t)
  (name-map (util:required 'name-map) :type list        :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-abstraction))

(defstruct (node-let (:include node))
  (bindings       (util:required 'bindings)      :type binding-list :read-only t)
  (declared-types (util:required 'declare-types) :type list         :read-only t)
  (subexpr        (util:required 'subexpr)       :type node         :read-only t)
  (name-map       (util:required 'name-map)      :type list         :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-let))

(defstruct (node-lisp (:include node))
  (type      (util:required 'type)      :type t :read-only t)
  (variables (util:required 'variables) :type t :read-only t)
  (form      (util:required 'form)      :type t :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-lisp))

(defstruct match-branch
  (unparsed  (util:required 'unparsed) :type t       :read-only t)
  (pattern   (util:required 'pattern)  :type pattern :read-only t)
  (subexpr   (util:required 'subexpr)  :type node    :read-only t)
  (name-map  (util:required 'name-map) :type list    :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type match-branch))

(defstruct (node-match (:include node))
  (expr     (util:required 'expr)     :type node :read-only t)
  (branches (util:required 'branches) :type list :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-match))

(defstruct (node-seq (:include node))
  (subnodes (util:required 'subnodes) :type node-list :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-seq))

(defstruct (node-the (:include node))
  (type    (util:required 'type)       :type t    :read-only t)
  (subnode (util:required 'subnode)    :type node :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-the))

(defstruct (node-return (:include node))
  (expr (util:required 'expr) :type node :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-return))

(defstruct (node-bind (:include node))
  (name (util:required 'name) :type symbol :read-only t)
  (expr (util:required 'expr) :type node   :read-only t)
  (body (util:required 'body) :type node   :read-only t))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node-bind))

#+(and sbcl coalton-release)
(declaim (sb-ext:freeze-type node))

(defun expr-variables (value)
  "Compute a list of variables referenced in the VALUE expression.

NOTE: Just because a variable shows up in the list does *NOT* mean the
variable is free in the expression."
  (declare (type node value)
           (values list))
  (let ((fv nil))
    (labels ((analyze (expr)
               (etypecase expr
                 (node-literal
                  nil)

                 (node-variable
                  (let ((name (node-variable-name expr)))
                    (push name fv)))

                 (node-abstraction
                   (analyze (node-abstraction-subexpr expr)))

                 (node-let
                  (let* ((bindings (node-let-bindings expr))
                         (subexpr (node-let-subexpr expr))
                         (vals (mapcar #'cdr bindings)))
                    (mapc #'analyze (cons subexpr vals))))

                 (node-lisp
                  nil)

                 (node-application
                  (let ((rator (node-application-rator expr))
                        (rands (node-application-rands expr)))
                    (mapc #'analyze (cons rator rands))))

                 (node-match
                  (let ((value (node-match-expr expr))
                        (branches (node-match-branches expr)))
                    (analyze value)
                    (dolist (branch branches)
                      (analyze (match-branch-subexpr branch)))))

                 (node-seq
                  (dolist (subnode (node-seq-subnodes expr))
                    (analyze subnode)))

                 (node-the
                  (analyze (node-the-subnode expr)))

                 (node-return
                  (analyze (node-return-expr expr)))

                 (node-bind
                  (analyze (node-bind-expr expr))
                  (analyze (node-bind-body expr))))))

      (analyze value)
      fv)))

(defun bindings-to-dag (bindings)
  (let ((vars (mapcar #'car bindings)))
    (loop :for (var . val) :in bindings
          :collect (cons var (copy-list (intersection vars (expr-variables val)))))))
