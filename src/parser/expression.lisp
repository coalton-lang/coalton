(defpackage #:coalton-impl/parser/expression
  (:use
   #:cl
   #:coalton-impl/parser/base
   #:coalton-impl/parser/types
   #:coalton-impl/parser/pattern
   #:coalton-impl/parser/macro)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:reader #:coalton-impl/parser/reader)
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:const #:coalton-impl/constants))
  (:export
   #:node                               ; STRUCT
   #:node-list                          ; TYPE
   #:node-variable                      ; STRUCT
   #:make-node-variable                 ; CONSTRUCTOR
   #:node-variable-name                 ; ACCESSOR
   #:node-variable-list                 ; TYPE
   #:node-accessor                      ; STRUCT
   #:make-node-accessor                 ; CONSTRUCTOR
   #:node-accessor-name                 ; ACCESSOR
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
   #:node-values-bind                   ; STRUCT
   #:make-node-values-bind              ; CONSTRUCTOR
   #:node-values-bind-patterns          ; ACCESSOR
   #:node-values-bind-expr              ; ACCESSOR
   #:node-body-element                  ; TYPE
   #:node-body-element-list             ; TYPE
   #:node-body                          ; STRUCT
   #:make-node-body                     ; CONSTRUCTOR
   #:node-body-nodes                    ; ACCESSOR
   #:node-body-last-node                ; ACCESSOR
   #:node-abstraction                   ; STRUCT
   #:make-node-abstraction              ; CONSTRUCTOR
   #:node-abstraction-params            ; ACCESSOR
   #:node-abstraction-keyword-params    ; ACCESSOR
   #:node-abstraction-body              ; ACCESSOR
   #:node-abstraction-introduces-return-scope-p ; ACCESSOR
   #:node-abstraction-p                 ; FUNCTION
   #:keyword-param                      ; STRUCT
   #:make-keyword-param                 ; CONSTRUCTOR
   #:keyword-param-keyword              ; ACCESSOR
   #:keyword-param-binder               ; ACCESSOR
   #:keyword-param-default              ; ACCESSOR
   #:keyword-param-list                 ; TYPE
   #:parse-fn-argument-list             ; FUNCTION
   #:node-let-binding                   ; STRUCT
   #:make-node-let-binding              ; CONSTRUCTOR
   #:node-let-binding-name              ; ACCESSOR
   #:node-let-binding-value             ; ACCESSOR
   #:node-let-binding-list              ; TYPE
   #:node-dynamic-binding               ; STRUCT
   #:make-node-dynamic-binding          ; CONSTRUCTOR
   #:node-dynamic-binding-name          ; ACCESSOR
   #:node-dynamic-binding-value         ; ACCESSOR
   #:node-dynamic-binding-list          ; TYPE
   #:node-for-binding                  ; STRUCT
   #:make-node-for-binding             ; CONSTRUCTOR
   #:node-for-binding-name             ; ACCESSOR
   #:node-for-binding-init             ; ACCESSOR
   #:node-for-binding-step             ; ACCESSOR
   #:node-for-binding-list             ; TYPE
   #:node-let-declare                   ; STRUCT
   #:make-node-let-declare              ; CONSTRUCTOR
   #:node-let-declare-name              ; ACCESSOR
   #:node-let-declare-type              ; ACCESSOR
   #:node-let-declare-list              ; TYPE
   #:node-let                           ; STRUCT
   #:make-node-let                      ; CONSTRUCTOR
   #:node-let-bindings                  ; ACCESSOR
   #:node-let-declares                  ; ACCESSOR
   #:node-let-body                      ; ACCESSOR
   #:node-let-sequential-p              ; ACCESSOR
   #:node-rec                           ; STRUCT
   #:make-node-rec                      ; CONSTRUCTOR
   #:node-rec-name                      ; ACCESSOR
   #:node-rec-bindings                  ; ACCESSOR
   #:node-rec-declares                  ; ACCESSOR
   #:node-rec-params                    ; ACCESSOR
   #:node-rec-call-args                 ; ACCESSOR
   #:node-rec-body                      ; ACCESSOR
   #:node-dynamic-let                   ; STRUCT
   #:make-node-dynamic-let              ; CONSTRUCTOR
   #:node-dynamic-let-bindings          ; ACCESSOR
   #:node-dynamic-let-subexpr           ; ACCESSOR
   #:node-lisp                          ; STRUCT
   #:make-node-lisp                     ; CONSTRUCTOR
   #:node-lisp-output-types             ; ACCESSOR
   #:node-lisp-vars                     ; ACCESSOR
   #:node-lisp-var-names                ; ACCESSOR
   #:node-lisp-body                     ; ACCESSOR
   #:node-match-branch                  ; STRUCT
   #:make-node-match-branch             ; CONSTRUCTOR
   #:node-match-branch-pattern          ; ACCESSOR
   #:node-match-branch-body             ; ACCESSOR
   #:node-match-branch-list             ; TYPE
   #:node-match                         ; STRUCT
   #:make-node-match                    ; CONSTRUCTOR
   #:node-match-expr                    ; ACCESSOR
   #:node-match-branches                ; ACCESSOR
   #:node-catch-branch                  ; STRUCT
   #:make-node-catch-branch             ; CONSTRUCTOR
   #:node-catch-branch-pattern          ; ACCESSOR
   #:node-catch-branch-body             ; ACCESSOR
   #:node-catch-branch-list             ; TYPE
   #:node-catch                         ; STRUCT
   #:make-node-catch                    ; CONSTRUCTOR
   #:node-catch-expr                    ; ACCESSOR
   #:node-catch-branches                ; ACCESSOR
   #:node-resumable-branch              ; STRUCT
   #:make-node-resumable-branch         ; CONSTRUCTOR
   #:node-resumable-branch-pattern      ; ACCESSOR
   #:node-resumable-branch-body         ; ACCESSOR
   #:node-resumable-branch-list         ; TYPE
   #:node-resumable                     ; STRUCT
   #:make-node-resumable                ; CONSTRUCTOR
   #:node-resumable-expr                ; ACCESSOR
   #:node-resumable-branches            ; ACCESSOR
   #:node-progn                         ; STRUCT
   #:make-node-progn                    ; CONSTRUCTOR
   #:node-progn-body                    ; ACCESSOR
   #:node-type-of                       ; STRUCT
   #:make-node-type-of                  ; CONSTRUCTOR
   #:node-type-of-expr                  ; ACCESSOR
   #:node-unsafe                        ; STRUCT
   #:make-node-unsafe                   ; CONSTRUCTOR
   #:node-unsafe-body                   ; ACCESSOR
   #:node-the                           ; STRUCT
   #:make-node-the                      ; CONSTRUCTOR
   #:node-the-type                      ; ACCESSOR
   #:node-the-expr                      ; ACCESSOR
   #:node-collection-builder            ; STRUCT
   #:make-node-collection-builder       ; CONSTRUCTOR
   #:node-collection-builder-elements   ; ACCESSOR
   #:association-entry                  ; STRUCT
   #:make-association-entry             ; CONSTRUCTOR
   #:association-entry-key              ; ACCESSOR
   #:association-entry-value            ; ACCESSOR
   #:association-entry-list             ; TYPE
   #:node-association-builder           ; STRUCT
   #:make-node-association-builder      ; CONSTRUCTOR
   #:node-association-builder-entries   ; ACCESSOR
   #:builder-clause                     ; STRUCT
   #:builder-with-clause                ; STRUCT
   #:make-builder-with-clause           ; CONSTRUCTOR
   #:builder-with-clause-binder         ; ACCESSOR
   #:builder-with-clause-expr           ; ACCESSOR
   #:builder-for-clause                 ; STRUCT
   #:make-builder-for-clause            ; CONSTRUCTOR
   #:builder-for-clause-binder          ; ACCESSOR
   #:builder-for-clause-expr            ; ACCESSOR
   #:builder-below-clause               ; STRUCT
   #:make-builder-below-clause          ; CONSTRUCTOR
   #:builder-below-clause-binder        ; ACCESSOR
   #:builder-below-clause-expr          ; ACCESSOR
   #:builder-when-clause                ; STRUCT
   #:make-builder-when-clause           ; CONSTRUCTOR
   #:builder-when-clause-expr           ; ACCESSOR
   #:builder-clause-list                ; TYPE
   #:node-collection-comprehension      ; STRUCT
   #:make-node-collection-comprehension ; CONSTRUCTOR
   #:node-collection-comprehension-head ; ACCESSOR
   #:node-collection-comprehension-clauses ; ACCESSOR
   #:node-association-comprehension     ; STRUCT
   #:make-node-association-comprehension ; CONSTRUCTOR
   #:node-association-comprehension-key ; ACCESSOR
   #:node-association-comprehension-value ; ACCESSOR
   #:node-association-comprehension-clauses ; ACCESSOR
   #:node-block                         ; STRUCT
   #:make-node-block                    ; CONSTRUCTOR
   #:node-block-name                    ; ACCESSOR
   #:node-block-body                    ; ACCESSOR
   #:node-return                        ; STRUCT
   #:make-node-return                   ; CONSTRUCTOR
   #:node-return-expr                   ; ACCESSOR
   #:node-return-from                   ; STRUCT
   #:make-node-return-from              ; CONSTRUCTOR
   #:node-return-from-name              ; ACCESSOR
   #:node-return-from-expr              ; ACCESSOR
   #:node-values                        ; STRUCT
   #:make-node-values                   ; CONSTRUCTOR
   #:node-values-nodes                  ; ACCESSOR
   #:node-throw                         ; STRUCT
   #:make-node-throw                    ; CONSTRUCTOR
   #:node-throw-expr                    ; ACCESSOR
   #:node-resume-to                     ; STRUCT
   #:make-node-resume-to                ; CONSTRUCTOR
   #:node-resume-to-expr                ; ACCESSOR
   #:node-application                   ; STRUCT
   #:make-node-application              ; CONSTRUCTOR
   #:node-application-rator             ; ACCESSOR
   #:node-application-rands             ; ACCESSOR
   #:node-application-keyword-rands     ; ACCESSOR
   #:node-application-keyword-arg       ; STRUCT
   #:make-node-application-keyword-arg  ; CONSTRUCTOR
   #:node-application-keyword-arg-keyword ; ACCESSOR
   #:node-application-keyword-arg-value ; ACCESSOR
   #:node-application-keyword-arg-list  ; TYPE
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
   #:node-cond-clause-list              ; TYPE
   #:node-cond                          ; STRUCT
   #:make-node-cond                     ; CONSTRUCTOR
   #:node-cond-clauses                  ; ACCESSOR
   #:node-do-bind                       ; STRUCT
   #:make-node-do-bind                  ; CONSTRUCTOR
   #:node-do-bind-pattern               ; ACCESSOR
   #:node-do-bind-expr                  ; ACCESSOR
   #:node-do-body-element               ; TYPE
   #:node-body-element-list             ; TYPE
   #:node-do                            ; STRUCT
   #:node-for                          ; STRUCT
   #:make-node-for                     ; CONSTRUCTOR
   #:node-for-bindings                 ; ACCESSOR
   #:node-for-declares                 ; ACCESSOR
   #:node-for-returns                  ; ACCESSOR
   #:node-for-termination-kind         ; ACCESSOR
   #:node-for-termination-expr         ; ACCESSOR
   #:node-for-body                     ; ACCESSOR
   #:node-for-label                    ; ACCESSOR
   #:node-for-sequential-p             ; ACCESSOR
   #:node-break                         ; STRUCT
   #:make-node-break                    ; CONSTRUCTOR
   #:node-break-label                   ; ACCESSOR
   #:node-continue                      ; STRUCT
   #:make-node-continue                 ; CONSTRUCTOR
   #:node-continue-label                ; ACCESSOR
   #:make-node-do                       ; CONSTRUCTOR
   #:node-do-nodes                      ; ACCESSOR
   #:node-do-last-node                  ; ACCESSOR
   #:parse-expression                   ; FUNCTION
   #:parse-expressions                  ; FUNCTION
   #:parse-body                         ; FUNCTION
   #:parse-variable                     ; FUNCTION
   #:parse-ordinary-variable            ; FUNCTION
   #:parse-dynamic-variable             ; FUNCTION
   ))

;;;;
;;;; Parser AST - Untyped Expression Nodes
;;;;
;;;; This module defines the Abstract Syntax Tree structures used during parsing,
;;;; before type checking. These nodes represent the syntactic structure of Coalton
;;;; code as parsed from source text.
;;;;
;;;; This is the first of two AST systems in the Coalton compiler:
;;;; 1. Parser AST (this module): Untyped nodes used during parsing/syntax analysis
;;;; 2. Codegen AST (codegen/ast.lisp): Typed nodes used during code generation
;;;;

(in-package #:coalton-impl/parser/expression)

(defvar *macro-expansion-count* 0)

(declaim (type util:symbol-list *loop-label-context*))
(defvar *loop-label-context* nil
  "A list of known labels encountered during parse. 

Parsing (BREAK label) and (CONTINUE label) forms fails unless the label is found in
this list.

Rebound to NIL parsing an anonymous FN.")

(defconstant +macro-expansion-max+ 500)

;;;; # Expression Parsing
;;;;
;;;; Note that "expression" in the EBNF corresponds to the struct "node" in the lisp code.
;;;;
;;;; node-literal := <a lisp literal value>
;;;;
;;;; node-variable := <a lisp symbol not including "_" or starting with ".">
;;;;
;;;; node-accessor := <a lisp symbol starting with ".">
;;;;
;;;; ty := <defined in src/parser/types.lisp>
;;;;
;;;; qualified-ty := <defined in src/parser/types.lisp>
;;;;
;;;; pattern := <defined in src/parser/pattern.lisp>
;;;;
;;;; lisp-form := <an arbitrary lisp form>
;;;;
;;;; expression := node-variable
;;;;             | node-accessor
;;;;             | node-literal
;;;;             | node-abstraction
;;;;             | node-let
;;;;             | node-rec
;;;;             | node-lisp
;;;;             | node-match
;;;;             | node-progn
;;;;             | node-type-of
;;;;             | node-unsafe
;;;;             | node-the
;;;;             | node-return
;;;;             | node-application
;;;;             | node-or
;;;;             | node-and
;;;;             | node-if
;;;;             | node-when
;;;;             | node-unless
;;;;             | node-cond
;;;;             | node-do
;;;;             | node-for
;;;;             | node-break
;;;;             | node-continue
;;;;
;;;; node-bind := "(" "let" pattern "=" expression ")"
;;;;
;;;; node-body-element := expression | shorthand-let
;;;;
;;;; node-body := node-body-element* expression
;;;;
;;;; node-keyword-param := "(" identifier expression ")"
;;;; node-abstraction := "(" "fn" [":transparent-to-return"] "(" pattern* ["&key" node-keyword-param*] ")" node-body ")"
;;;;
;;;; node-let-binding := "(" identifier expression ")"
;;;;
;;;; node-for-binding := "(" identifier expression expression ")"
;;;;
;;;; node-let-declare := "(" "declare" identifier qualified-ty ")"
;;;;
;;;; node-let := "(" ("let" | "let*") "(" (node-let-binding | node-let-declare)+ ")" body ")"
;;;;
;;;; node-rec := "(" "rec" identifier "(" (node-let-binding | node-let-declare)+ ")" body ")"
;;;;
;;;; node-lisp := "(" "lisp" type "(" lisp-variable* ")" lisp-form+ ")"
;;;;
;;;; lisp-variable := variable | "(" identifier variable ")"
;;;;
;;;; node-match-branch := "(" pattern body ")"
;;;;
;;;; node-match := "(" "match" pattern match-branch* ")"
;;;;
;;;; node-progn := "(" "progn" body ")"
;;;;
;;;; node-type-of := "(" "type-of" expression ")"
;;;; node-unsafe := "(" "unsafe" body ")"
;;;;
;;;; node-the := "(" "the" qualified-ty expression ")"
;;;;
;;;; node-return := "(" "return" expression? ")"
;;;;
;;;; node-keyword-arg := keyword expression
;;;; node-application := "(" expression expression* [node-keyword-arg*] ")"
;;;;
;;;; node-or := "(" "or" expression+ ")"
;;;;
;;;; node-and := "(" "and" expression+ ")"
;;;;
;;;; node-if := "(" "if" expression expression expression ")"
;;;;
;;;; node-when := "(" "when" expression body ")"
;;;;
;;;; node-unless := "(" "unless" expression body ")"
;;;;
;;;; node-cond-clause := "(" expression body ")"
;;;;
;;;; node-cond := "(" "cond" cond-clause+ ")"
;;;;
;;;; node-do-bind "(" pattern "<-" expression ")"
;;;;
;;;; node-do-body-element := expression
;;;;                       | node-bind
;;;;                       | node-do-bind
;;;;
;;;; node-do := node-do-body-element* expression
;;;;
;;;; label := <a keyword symbol>
;;;;
;;;; node-for := "(" ("for" | "for*") label? "(" (node-for-binding | node-let-declare)* ")"
;;;;                    [":returns" expression]
;;;;                    [(":while" | ":until" | ":repeat") expression]
;;;;                    node-body-element* ")"
;;;;
;;;; node-break := "(" "break" label? ")"
;;;;
;;;; node-continue := "(" "continue" label? ")"

(defstruct (node
            (:constructor nil)
            (:copier nil))
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self node))
  (node-location self))

(defun node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-p x)))

(deftype node-list ()
  '(satisfies node-list-p))

(defstruct (node-variable
            (:include node)
            (:copier nil))
  (name (util:required 'name) :type identifier :read-only t))

(defun node-variable-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-variable-p x)))

(deftype node-variable-list ()
  '(satisfies node-variable-list-p))

(defstruct (node-accessor
            (:include node)
            (:copier nil))
  (name (util:required 'name) :type string :read-only t))

(defstruct (node-literal
            (:include node)
            (:copier nil))
  (value (util:required 'value) :type (and util:literal-value (not integer)) :read-only t))

(defstruct (node-integer-literal
            (:include node)
            (:copier nil))
  (value (util:required 'value) :type integer :read-only t))

;;
;; Does not subclass node, can only appear in a node body
;;
(defstruct (node-bind
            (:copier nil))
  (pattern  (util:required 'pattern)   :type pattern  :read-only t)
  (expr     (util:required 'expr)      :type node     :read-only t)
  (location (util:required 'location)  :type source:location :read-only t))

(defmethod source:location ((self node-bind))
  (node-bind-location self))

(defstruct (node-values-bind
            (:copier nil))
  (patterns (util:required 'patterns)  :type pattern-list    :read-only t)
  (expr     (util:required 'expr)      :type node            :read-only t)
  (location (util:required 'location)  :type source:location :read-only t))

(defmethod source:location ((self node-values-bind))
  (node-values-bind-location self))

(deftype node-body-element ()
  '(or node node-bind node-values-bind))

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
;; - does not have source information (but its children do)
;;
(defstruct (node-body
            (:copier nil))
  (nodes     (util:required 'nodes)     :type node-body-element-list :read-only t)
  (last-node (util:required 'last-node) :type node                   :read-only t))

(defstruct (node-abstraction
            (:include node)
            (:copier nil))
  (params                    (util:required 'params) :type pattern-list       :read-only t)
  (keyword-params            nil                     :type keyword-param-list :read-only t)
  (body                      (util:required 'body)   :type node-body          :read-only t)
  ;; Internal-only marker for compiler-generated lambdas that should
  ;; leave RETURN bound to the next enclosing returnable context.
  ;; This is so we can implement the `:TRANSPARENT-TO-RETURN` option
  ;; for `fn`. It is a band-aid fix so that one can write macros which
  ;; use `fn` internally, but don't capture any enclosing `return`.
  ;; This should not be relied upon or used extensively.
  (introduces-return-scope-p t                       :type boolean            :read-only t))

(defstruct (keyword-param
            (:copier nil))
  (keyword  (util:required 'keyword)  :type keyword-src     :read-only t)
  (binder   (util:required 'binder)   :type node-variable   :read-only t)
  (default  (util:required 'default)  :type node            :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self keyword-param))
  (keyword-param-location self))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun keyword-param-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'keyword-param-p x))))

(deftype keyword-param-list ()
  '(satisfies keyword-param-list-p))

(defstruct (node-let-binding
            (:copier nil))
  (name     (util:required 'name)     :type node-variable   :read-only t)
  (value    (util:required 'value)    :type node            :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self node-let-binding))
  (node-let-binding-location self))

(defun node-let-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-let-binding-p x)))

(deftype node-let-binding-list ()
  '(satisfies node-let-binding-list-p))

(defstruct (node-dynamic-binding
            (:copier nil))
  (name     (util:required 'name)     :type node-variable   :read-only t)
  (value    (util:required 'value)    :type node            :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self node-dynamic-binding))
  (node-dynamic-binding-location self))

(defun node-dynamic-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-dynamic-binding-p x)))

(deftype node-dynamic-binding-list ()
  '(satisfies node-dynamic-binding-list-p))

(defstruct (node-for-binding
            (:copier nil))
  (name     (util:required 'name)     :type node-variable   :read-only t)
  (init     (util:required 'init)     :type node            :read-only t)
  (step     nil                       :type (or null node)  :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self node-for-binding))
  (node-for-binding-location self))

(defun node-for-binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-for-binding-p x)))

(deftype node-for-binding-list ()
  '(satisfies node-for-binding-list-p))

(defstruct (node-let-declare
            (:copier nil))
  (name     (util:required 'name)     :type node-variable   :read-only t)
  (type     (util:required 'type)     :type qualified-ty    :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self node-let-declare))
  (node-let-declare-location self))

(defun node-let-declare-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-let-declare-p x)))

(deftype node-let-declare-list ()
  '(satisfies node-let-declare-list-p))

(defstruct (node-let
            (:include node)
            (:copier nil))
  (bindings     (util:required 'bindings) :type node-let-binding-list :read-only t)
  (declares     (util:required 'declares) :type node-let-declare-list :read-only t)
  (body         (util:required 'body)     :type node-body             :read-only t)
  ;; T when parsed from LET*, so later bindings can see earlier ones.
  (sequential-p nil                       :type boolean               :read-only t))

(defstruct (node-rec
            (:include node)
            (:copier nil))
  ;; Name of the recursive function introduced by `rec`.
  (name      (util:required 'name)      :type node-variable         :read-only t)
  ;; Non-self init bindings that seed the immediate recursive call.
  (bindings  (util:required 'bindings)  :type node-let-binding-list :read-only t)
  ;; Declarations for the init bindings.
  (declares  (util:required 'declares)  :type node-let-declare-list :read-only t)
  ;; Parameters of the recursive function, in source order.
  (params    (util:required 'params)    :type pattern-list          :read-only t)
  ;; Arguments passed in the implicit immediate call, in source order.
  (call-args (util:required 'call-args) :type node-variable-list    :read-only t)
  ;; Body of the recursive function itself.
  (body      (util:required 'body)      :type node-body             :read-only t))

(defstruct (node-dynamic-let
            (:include node)
            (:copier nil))
  (bindings (util:required 'bindings) :type node-dynamic-binding-list :read-only t)
  (subexpr  (util:required 'subexpr)  :type node                      :read-only t))

(defstruct (node-lisp
            (:include node)
            (:copier nil))
  (output-types (util:required 'output-types) :type (or null ty-list)  :read-only t)
  (vars      (util:required 'vars)      :type node-variable-list :read-only t)
  (var-names (util:required 'var-names) :type util:symbol-list   :read-only t)
  (body      (util:required 'body)      :type t                  :read-only t))

(defstruct (node-match-branch
            (:copier nil))
  (pattern  (util:required 'pattern)  :type pattern         :read-only t)
  (body     (util:required 'body)     :type node-body       :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self node-match-branch))
  (node-match-branch-location self))

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

(defstruct (node-type-of
            (:include node)
            (:copier nil))
  (expr (util:required 'expr) :type node :read-only t))
(defstruct (node-unsafe
            (:include node)
            (:copier nil))
  (body (util:required 'body) :type node-body :read-only t))

(defstruct (node-the
            (:include node)
            (:copier nil))
  (type (util:required 'type) :type qualified-ty :read-only t)
  (expr (util:required 'expr) :type node         :read-only t))

(defstruct (node-collection-builder
            (:include node)
            (:copier nil))
  "AST node for collection builder syntax such as `[a b c]`."
  (elements (util:required 'elements) :type node-list :read-only t))

(defstruct association-entry
  "AST node for one evaluated key/value entry inside an association builder."
  (key      (util:required 'key)      :type node             :read-only t)
  (value    (util:required 'value)    :type node             :read-only t)
  (location (util:required 'location) :type source:location  :read-only t))

(defmethod source:location ((self association-entry))
  (association-entry-location self))

(defun association-entry-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'association-entry-p x)))

(deftype association-entry-list ()
  '(satisfies association-entry-list-p))

(defstruct (node-association-builder
            (:include node)
            (:copier nil))
  "AST node for association builder syntax such as `[a => b c => d]`."
  (entries (util:required 'entries) :type association-entry-list :read-only t))

(defstruct (builder-clause
            (:constructor nil)
            (:copier nil))
  "Base AST type for builder comprehension clauses."
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self builder-clause))
  (builder-clause-location self))

(defstruct (builder-with-clause
            (:include builder-clause)
            (:copier nil))
  "AST node for a `:with` clause inside builder comprehension syntax."
  (binder (util:required 'binder) :type node-variable :read-only t)
  (expr   (util:required 'expr)   :type node          :read-only t))

(defstruct (builder-for-clause
            (:include builder-clause)
            (:copier nil))
  "AST node for a `:for ... :in ...` clause inside builder comprehension syntax."
  (binder (util:required 'binder) :type node-variable :read-only t)
  (expr   (util:required 'expr)   :type node          :read-only t))

(defstruct (builder-below-clause
            (:include builder-clause)
            (:copier nil))
  "AST node for a `:for ... :below ...` clause inside builder comprehension syntax."
  (binder (util:required 'binder) :type node-variable :read-only t)
  (expr   (util:required 'expr)   :type node          :read-only t))

(defstruct (builder-when-clause
            (:include builder-clause)
            (:copier nil))
  "AST node for a `:when` filter clause inside builder comprehension syntax."
  (expr (util:required 'expr) :type node :read-only t))

(defun builder-clause-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'builder-clause-p x)))

(deftype builder-clause-list ()
  '(satisfies builder-clause-list-p))

(defstruct (node-collection-comprehension
            (:include node)
            (:copier nil))
  "AST node for collection builder comprehension syntax."
  (head    (util:required 'head)    :type node                :read-only t)
  (clauses (util:required 'clauses) :type builder-clause-list :read-only t))

(defstruct (node-association-comprehension
            (:include node)
            (:copier nil))
  "AST node for association builder comprehension syntax."
  (key     (util:required 'key)     :type node                :read-only t)
  (value   (util:required 'value)   :type node                :read-only t)
  (clauses (util:required 'clauses) :type builder-clause-list :read-only t))

(defstruct (node-block
            (:include node)
            (:copier nil))
  "Internal control-flow node introducing a named return target."
  (name (util:required 'name) :type symbol    :read-only t)
  (body (util:required 'body) :type node-body :read-only t))

(defstruct (node-return
            (:include node)
            (:copier nil))
  "A Coalton `return` as written by the user.

This node is rewritten to NODE-RETURN-FROM by TC:RESOLVE-CONTROL-FLOW
after variable renaming and before type inference."
  (expr (util:required 'expr) :type (or null node) :read-only t))

(defstruct (node-return-from
            (:include node)
            (:copier nil))
  "Internal control-flow node returning from a named enclosing block."
  (name (util:required 'name) :type symbol :read-only t)
  (expr (util:required 'expr) :type node   :read-only t))

(defstruct (node-values
            (:include node)
            (:copier nil))
  (nodes (util:required 'nodes) :type node-list :read-only t))

(defstruct (node-application
            (:include node)
            (:copier nil))
  (rator         (util:required 'rator) :type node                           :read-only t)
  (rands         (util:required 'rands) :type node-list                      :read-only t)
  (keyword-rands nil                    :type node-application-keyword-arg-list :read-only t))

(defstruct (node-application-keyword-arg
            (:copier nil))
  (keyword  (util:required 'keyword)  :type keyword-src     :read-only t)
  (value    (util:required 'value)    :type node            :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self node-application-keyword-arg))
  (node-application-keyword-arg-location self))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun node-application-keyword-arg-list-p (x)
    (and (alexandria:proper-list-p x)
         (every #'node-application-keyword-arg-p x))))

(deftype node-application-keyword-arg-list ()
  '(satisfies node-application-keyword-arg-list-p))

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
  (expr   (util:required 'expr)   :type node            :read-only t)
  (body   (util:required 'body)   :type node-body       :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self node-cond-clause))
  (node-cond-clause-location self))

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
  (pattern (util:required 'name)   :type pattern         :read-only t)
  (expr    (util:required 'expr)   :type node            :read-only t)
  (location  (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self node-do-bind))
  (node-do-bind-location self))

(deftype node-do-body-element ()
  '(or node node-bind node-values-bind node-do-bind))

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

(defstruct (node-break
            (:include node)
            (:copier nil))
  (label (util:required 'label) :type keyword :read-only t))

(defstruct (node-continue
            (:include node)
            (:copier nil))
  (label (util:required 'label) :type keyword :read-only t))

(defstruct (node-for
            (:include node)
            (:copier nil))
  (label            (util:required 'label)            :type keyword                    :read-only t)
  (bindings         (util:required 'bindings)         :type node-for-binding-list     :read-only t)
  (declares         (util:required 'declares)         :type node-let-declare-list      :read-only t)
  (returns          nil                               :type (or null node)             :read-only t)
  (termination-kind nil                               :type (member nil :while :until :repeat) :read-only t)
  (termination-expr nil                               :type (or null node)             :read-only t)
  (body             (util:required 'body)             :type node-body                  :read-only t)
  ;; T when parsed from FOR*, so init and step bindings are sequential.
  (sequential-p     nil                               :type boolean                    :read-only t))

(defun check-sequential-binding-duplicates (bindings source context)
  (declare (type list bindings)
           (type string context))
  (let ((seen (make-hash-table :test #'eq)))
    (dolist (binding bindings)
      (let* ((name-node
               (etypecase binding
                 (node-let-binding (node-let-binding-name binding))
                 (node-for-binding (node-for-binding-name binding))))
             (name (node-variable-name name-node))
             (previous (gethash name seen)))
        (when previous
          (parse-error (format nil "Duplicate binding in ~A" context)
                       (secondary-note source
                                       (source:location-span (source:location previous))
                                       "first binding here")
                       (note source
                             (source:location-span (source:location name-node))
                             "second binding here")))
        (setf (gethash name seen) name-node)))))

(defstruct (node-throw
            (:include node)
            (:copier nil))
  (expr (util:required 'expr) :type node :read-only t))

(defstruct (node-resume-to
            (:include node)
            (:copier nil))
  (expr (util:required 'expr) :type node :read-only t))

(defstruct (node-resumable-branch
            (:copier nil))
  (pattern  (util:required 'pattern)  :type pattern         :read-only t)
  (body     (util:required 'body)     :type node-body       :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self node-resumable-branch))
  (node-resumable-branch-location self))

(defun node-resumable-branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-resumable-branch-p x)))

(deftype node-resumable-branch-list ()
  '(satisfies node-resumable-branch-list-p))

(defstruct (node-resumable
            (:include node)
            (:copier nil))
  (expr     (util:required 'expr)     :type node                         :read-only t)
  (branches (util:required 'branches) :type node-resumable-branch-list :read-only t))

(defstruct (node-catch-branch
            (:copier nil))
  (pattern  (util:required 'pattern)  :type pattern         :read-only t)
  (body     (util:required 'body)     :type node-body       :read-only t)
  (location (util:required 'location) :type source:location :read-only t))

(defmethod source:location ((self node-catch-branch))
  (node-catch-branch-location self))

(defun node-catch-branch-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-catch-branch-p x)))

(deftype node-catch-branch-list ()
  '(satisfies node-catch-branch-list-p))

(defstruct (node-catch
            (:include node)
            (:copier nil))
  (expr     (util:required 'expr)     :type node                   :read-only t)
  (branches (util:required 'branches) :type node-catch-branch-list :read-only t))

(defun values-symbol-p (symbol)
  (declare (type t symbol)
           (values boolean))
  (eq 'coalton:values symbol))

(defun values-pattern-form-p (form)
  (declare (type cst:cst form)
           (values boolean))
  (and (cst:consp form)
       (cst:atom (cst:first form))
       (values-symbol-p (cst:raw (cst:first form)))))

(defun keyword-name-from-binder (binder-name)
  (declare (type identifier binder-name)
           (values keyword &optional))
  (nth-value 0
             (intern (symbol-name binder-name) util:+keyword-package+)))

(defun dynamic-variable-form-p (form)
  (declare (type cst:cst form)
           (values boolean &optional))
  (and (cst:atom form)
       (symbolp (cst:raw form))
       (util:dynamic-variable-name-p (cst:raw form))))

(defun parse-keyword-param (form source)
  (declare (type cst:cst form)
           (values keyword-param &optional))
  (unless (and (cst:consp form)
               (cst:proper-list-p form))
    (parse-error "Malformed function keyword parameter"
                 (note source form "expected `(name init-expr)`")))
  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed function keyword parameter"
                 (note source form "expected identifier and default expression")))
  (unless (cst:atom (cst:first form))
    (parse-error "Malformed function keyword parameter"
                 (note source (cst:first form) "expected identifier")))
  (unless (identifierp (cst:raw (cst:first form)))
    (parse-error "Malformed function keyword parameter"
                 (note source (cst:first form) "expected identifier")))
  (when (cst:consp (cst:rest (cst:rest form)))
    (parse-error "Malformed function keyword parameter"
                 (note source (cst:third form) "unexpected trailing form")))
  (let* ((binder-form (cst:first form))
         (binder (parse-ordinary-variable binder-form source))
         (keyword-name (keyword-name-from-binder (node-variable-name binder))))
    (make-keyword-param
     :keyword (make-keyword-src
               :name keyword-name
               :location (form-location source binder-form))
     :binder binder
     :default (parse-expression (cst:second form) source)
     :location (form-location source form))))

(defun check-duplicate-keyword-parameters (keyword-params source)
  (declare (type keyword-param-list keyword-params))
  (let ((seen (make-hash-table :test #'eq)))
    (dolist (param keyword-params)
      (let* ((keyword (keyword-param-keyword param))
             (name (keyword-src-name keyword))
             (prev (gethash name seen)))
        (when prev
          (parse-error "Duplicate keyword parameter"
                       (secondary-note source (source:location-span (source:location prev))
                                       "first definition here")
                       (note source (source:location-span (source:location keyword))
                             "second definition here")))
        (setf (gethash name seen) keyword)))))

(defun keyword-marker-p (form)
  (and (cst:atom form)
       (eq 'coalton:&key (cst:raw form))))

(defun parse-fn-argument-list (form source)
  (declare (type cst:cst form)
           (values pattern-list keyword-param-list))
  (let ((arg-forms form)
        (params nil)
        (keyword-params nil)
        (in-keyword-section nil))
    (loop :while (cst:consp arg-forms)
          :for current := (cst:first arg-forms)
          :do
             (cond
               ((keyword-marker-p current)
                (when in-keyword-section
                  (parse-error "Malformed function"
                               (note source current "invalid `&key` placement")))
                (setf in-keyword-section t))
               (in-keyword-section
                (push (parse-keyword-param current source) keyword-params))
               (t
                (push (parse-pattern current source) params)))
             (setf arg-forms (cst:rest arg-forms)))
    (unless (cst:null arg-forms)
      (parse-error "Malformed function"
                   (note source arg-forms "unexpected dotted list")))
    (setf params (nreverse params)
          keyword-params (nreverse keyword-params))
    (check-duplicate-keyword-parameters keyword-params source)
    (values params keyword-params)))

(defun check-duplicate-call-keywords (keyword-rands source)
  (declare (type node-application-keyword-arg-list keyword-rands))
  (let ((seen (make-hash-table :test #'eq)))
    (dolist (arg keyword-rands)
      (let* ((keyword (node-application-keyword-arg-keyword arg))
             (name (keyword-src-name keyword))
             (prev (gethash name seen)))
        (when prev
          (parse-error "Duplicate keyword argument"
                       (secondary-note source (source:location-span (source:location prev))
                                       "first argument here")
                       (note source (source:location-span (source:location keyword))
                             "second argument here")))
        (setf (gethash name seen) keyword)))))

(defun parse-application-arguments (forms source)
  (declare (type cst:cst forms)
           (values node-list node-application-keyword-arg-list))
  (unless (cst:proper-list-p forms)
    (parse-error "Malformed function application"
                 (note source forms "unexpected dotted list")))
  (let ((rands nil)
        (keyword-rands nil)
        (in-keyword-section nil))
    (loop :for rest := forms :then (cst:rest rest)
          :while (cst:consp rest)
          :for current := (cst:first rest)
          :do
             (cond
               ((and (cst:atom current)
                     (keywordp (cst:raw current)))
                (unless (cst:consp (cst:rest rest))
                  (parse-error "Malformed function application"
                               (note-end source current "keyword argument value is missing")))
                (setf in-keyword-section t)
                (push (make-node-application-keyword-arg
                       :keyword (make-keyword-src
                                 :name (cst:raw current)
                                 :location (form-location source current))
                       :value (parse-expression (cst:second rest) source)
                       :location (form-location source current))
                      keyword-rands)
                (setf rest (cst:rest rest)))
               (in-keyword-section
                (parse-error "Malformed function application"
                             (note source current "positional argument after keyword argument")))
               (t
                (push (parse-expression current source) rands))))
    (setf rands (nreverse rands)
          keyword-rands (nreverse keyword-rands))
    (check-duplicate-call-keywords keyword-rands source)
    (values rands keyword-rands)))

(defun builder-marker-p (form marker)
  (and (cst:atom form)
       (eq (cst:raw form) marker)))

(defun parse-builder-binder (form source context)
  (unless (and (cst:atom form)
               (identifierp (cst:raw form)))
    (parse-error context
                 (note source form "expected identifier binder")))
  (if (string= "_" (symbol-name (cst:raw form)))
      (make-node-variable
       :name (cst:raw form)
       :location (form-location source form))
      (parse-ordinary-variable form source)))

(defun parse-association-entry (form source)
  (unless (cst:proper-list-p form)
    (parse-error "Malformed association builder"
                 (note source form "expected association entry")))
  (unless (builder-marker-p (cst:first form) (reader:association-entry-marker))
    (parse-error "Malformed association builder"
                 (note source form "expected association entry")))
  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed association builder"
                 (note-end source (cst:first form) "expected entry key")))
  (unless (cst:consp (cst:rest (cst:rest form)))
    (parse-error "Malformed association builder"
                 (note-end source (cst:second form) "expected entry value")))
  (when (cst:consp (cst:nthrest 3 form))
    (parse-error "Malformed association builder"
                 (note source (cst:fourth form) "unexpected trailing form")))
  (make-association-entry
   :key (parse-expression (cst:second form) source)
   :value (parse-expression (cst:third form) source)
   :location (form-location source form)))

(defun parse-binary-builder-clause (form source)
  "Validate and parse a builder clause that takes a binder and an expression."
  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed builder clause"
                 (note-end source (cst:first form) "expected binder")))
  (unless (cst:consp (cst:rest (cst:rest form)))
    (parse-error "Malformed builder clause"
                 (note-end source (cst:second form) "expected expression")))
  (when (cst:consp (cst:nthrest 3 form))
    (parse-error "Malformed builder clause"
                 (note source (cst:fourth form) "unexpected trailing form")))
  (values (parse-builder-binder (cst:second form) source "Malformed builder clause")
          (parse-expression (cst:third form) source)
          (form-location source form)))

(defun parse-builder-clause (form source)
  (unless (cst:proper-list-p form)
    (parse-error "Malformed builder clause"
                 (note source form "expected builder clause")))
  (cond
    ((builder-marker-p (cst:first form) (reader:builder-with-marker))
     (multiple-value-bind (binder expr location)
         (parse-binary-builder-clause form source)
       (make-builder-with-clause
        :binder binder :expr expr :location location)))
    ((builder-marker-p (cst:first form) (reader:builder-for-marker))
     (multiple-value-bind (binder expr location)
         (parse-binary-builder-clause form source)
       (make-builder-for-clause
        :binder binder :expr expr :location location)))
    ((builder-marker-p (cst:first form) (reader:builder-below-marker))
     (multiple-value-bind (binder expr location)
         (parse-binary-builder-clause form source)
       (make-builder-below-clause
        :binder binder :expr expr :location location)))
    ((builder-marker-p (cst:first form) (reader:builder-when-marker))
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed builder clause"
                    (note-end source (cst:first form) "expected predicate")))
     (when (cst:consp (cst:nthrest 2 form))
       (parse-error "Malformed builder clause"
                    (note source (cst:third form) "unexpected trailing form")))
     (make-builder-when-clause
      :expr (parse-expression (cst:second form) source)
      :location (form-location source form)))
    (t
     (parse-error "Malformed builder clause"
                  (note source form "unknown builder clause")))))

(defun parse-builder-clauses (forms source)
  (loop :for clauses := forms :then (cst:rest clauses)
        :while (cst:consp clauses)
        :collect (parse-builder-clause (cst:first clauses) source)))

(defun show-symbol (name)
  (declare (type string name)
           (values (or null symbol) &optional))
  (let ((package (find-package "COALTON/SHOW")))
    (and package
         (find-symbol name package))))

(defun synthetic-node-variable (name location)
  (declare (type symbol name)
           (type source:location location)
           (values node-variable &optional))
  (make-node-variable
   :location location
   :name name))

(defun synthetic-application (rator rands location)
  (declare (type symbol rator)
           (type node-list rands)
           (type source:location location)
           (values node-application &optional))
  (make-node-application
   :location location
   :rator (synthetic-node-variable rator location)
   :rands rands
   :keyword-rands nil))

(defun parse-application-expression (form source)
  (declare (type cst:cst form)
           (values node-application &optional))
  (multiple-value-bind (rands keyword-rands)
      (parse-application-arguments (cst:rest form) source)
    (make-node-application
     :rator (parse-expression (cst:first form) source)
     :rands rands
     :keyword-rands keyword-rands
     :location (form-location source form))))

(defun parse-expression (form source)
  (declare (type cst:cst form)
           (values node &optional))

  (cond
    ;;
    ;; Atoms
    ;;

    ((cst:atom form)
     (typecase (cst:raw form)
       (null
        (parse-error "Malformed expression"
                     (note source form "unexpected `nil` or `()`")))

       (symbol
        (if (char= #\. (aref (symbol-name (cst:raw form)) 0))
            (parse-accessor form source)
            (parse-variable form source)))

       (t
        (parse-literal form source))))

    ;;
    ;; Dotted Lists
    ;;

    ((not (cst:proper-list-p form))
     (parse-error "Malformed expression"
                  (note source form "unexpected dotted list")))

    ;;
    ;; Keywords
    ;;

    ((and (cst:atom (cst:first form))
          (eq (reader:collection-builder-marker) (cst:raw (cst:first form))))
     (make-node-collection-builder
      :location (form-location source form)
      :elements (loop :for items := (cst:rest form) :then (cst:rest items)
                      :while (cst:consp items)
                      :collect (parse-expression (cst:first items) source))))

    ((and (cst:atom (cst:first form))
          (eq (reader:association-builder-marker) (cst:raw (cst:first form))))
     (make-node-association-builder
      :location (form-location source form)
      :entries (loop :for items := (cst:rest form) :then (cst:rest items)
                     :while (cst:consp items)
                     :collect (parse-association-entry (cst:first items) source))))

    ((and (cst:atom (cst:first form))
          (eq (reader:collection-comprehension-marker) (cst:raw (cst:first form))))
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed collection comprehension"
                    (note-end source (cst:first form) "expected head expression")))
     (make-node-collection-comprehension
      :location (form-location source form)
      :head (parse-expression (cst:second form) source)
      :clauses (parse-builder-clauses (cst:nthrest 2 form) source)))

    ((and (cst:atom (cst:first form))
          (eq (reader:association-comprehension-marker) (cst:raw (cst:first form))))
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed association comprehension"
                    (note-end source (cst:first form) "expected key expression")))
     (unless (cst:consp (cst:rest (cst:rest form)))
       (parse-error "Malformed association comprehension"
                    (note-end source (cst:second form) "expected value expression")))
     (make-node-association-comprehension
      :location (form-location source form)
      :key (parse-expression (cst:second form) source)
      :value (parse-expression (cst:third form) source)
      :clauses (parse-builder-clauses (cst:nthrest 3 form) source)))

    ((and (cst:atom (cst:first form))
          (values-symbol-p (cst:raw (cst:first form))))
     (make-node-values
      :location (form-location source form)
      :nodes (loop :for values := (cst:rest form) :then (cst:rest values)
                   :while (cst:consp values)
                   :collect (parse-expression (cst:first values) source))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:fn (cst:raw (cst:first form))))
     (let ((params)
           (keyword-params)
           (body)
           (introduces-return-scope-p t)
           (argument-form)
           (body-forms)
           (tail (cst:rest form)))

       (when (and (cst:consp tail)
                  (cst:atom (cst:first tail))
                  (eq ':transparent-to-return (cst:raw (cst:first tail))))
         (setf introduces-return-scope-p nil)
         (setf tail (cst:rest tail)))

       ;; (fn)
       (unless (cst:consp tail)
         (parse-error "Malformed function"
                      (note-end source (if introduces-return-scope-p
                                           (cst:first form)
                                           (cst:second form))
                                "expected function arguments")))

       (setf argument-form (cst:first tail))
       (setf body-forms (cst:rest tail))

       ;; (fn (...))
       (unless (cst:consp body-forms)
         (parse-error "Malformed function"
                      (note-end source argument-form "expected function body")))

       ;; (fn x ...)
       ;;
       ;; NOTE: (fn () ...) is allowed
       (when (and (cst:atom argument-form)
                  (not (null (cst:raw argument-form))))
         (parse-error "Malformed function"
                      (note source argument-form
                            "malformed argument list")
                      (help source argument-form
                            (lambda (existing)
                              (concatenate 'string "(" existing ")"))
                            "add parentheses")))
       ;; Bind *LOOP-LABEL-CONTEXT* to NIL to disallow BREAKing from
       ;; or CONTINUING with loops that enclose the FN form.
       (let ((*loop-label-context* nil))
         (multiple-value-setq (params keyword-params)
           (parse-fn-argument-list argument-form source))
         (setf body (parse-body body-forms form source))
         (make-node-abstraction
          :params params
          :keyword-params keyword-params
          :body body
          :introduces-return-scope-p introduces-return-scope-p
          :location (form-location source form)))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:throw (cst:raw (cst:first form))))
     (let (expr)

       ;; (throw)
       (unless (cst:consp (cst:rest form))
         (parse-error "Malformed throw expression"
                      (note source (cst:first form) "expression expected")))

       (setf expr (parse-expression (cst:second form) source))

       ;; (throw a b ...)
       (when (cst:consp (cst:rest (cst:rest form)))
         (parse-error "Malformed throw expression"
                      (note source (cst:first (cst:rest (cst:rest form)))
                            "unexpected trailing form")))

       (make-node-throw
        :expr expr
        :location (form-location source form))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:resume-to (cst:raw (cst:first form))))
     (let (expr)

       ;; (resume-to)
       (unless (cst:consp (cst:rest form))

         (parse-error "Malformed resume-to expression"
                      (note-end source (cst:first form) "expression expected")))

       (setf expr (parse-expression (cst:second form) source))
       
       ;; (resume-to a b ...)
       (when (cst:consp (cst:rest (cst:rest form)))
         (parse-error "Malformed resume-to expression"
                      (note source (cst:first (cst:rest (cst:rest form)))
                            "unexpected trailing form")))

       (make-node-resume-to
        :expr expr
        :location (form-location source form))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:resumable (cst:raw (cst:first form))))

     ;; (resumable)
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed resumable expression"
                    (note-end source (cst:first form) "expected expression")))

     ;; (resumable expr)
     (unless (cst:consp (cst:rest (cst:rest form)))
       (parse-error "Malformed resumable expression"
                    (note-end source (cst:second form) "expected resumeable cases")))

     (make-node-resumable
      :expr (parse-expression (cst:second form) source)
      :branches (loop :for branches := (cst:nthrest 2 form) :then (cst:rest branches)
                      :while (cst:consp branches)
                      :collect (parse-resumable-branch (cst:first branches) source))
      :location (form-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:catch (cst:raw (cst:first form))))

     ;; (catch)
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed catch expression"
                    (note-end source (cst:first form) "expected expression")))
     
     ;; (catch expr)
     (unless (cst:consp (cst:rest (cst:rest form)))
       (parse-error "Malformed catch expression"
                    (note-end source (cst:second form) "expected catch cases")))
     

     (make-node-catch
      :expr (parse-expression (cst:second form) source)
      :branches (loop :for branches := (cst:nthrest 2 form) :then (cst:rest branches)
                      :while (cst:consp branches)
                      :collect (parse-catch-branch (cst:first branches) source))
      :location (form-location source form)))


    ((and (cst:atom (cst:first form))
          (member (cst:raw (cst:first form))
                  '(coalton:let coalton:let*)
                  :test #'eq))
     (let ((sequential-p (eq 'coalton:let* (cst:raw (cst:first form)))))

     ;; (let)
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed let"
                    (note-end source (cst:first form) "expected let binding list")))

     ;; (let (...))
     (unless (cst:consp (cst:rest (cst:rest form)))
       (parse-error "Malformed let"
                    (note-end source (cst:second form) "expected let body")))

     (unless (cst:proper-list-p (cst:second form))
       (parse-error "Malformed let"
                    (note source (cst:second form) "expected binding list")))

     (let* (declares
            
            (bindings (loop :for bindings := (cst:second form) :then (cst:rest bindings)
                            :while (cst:consp bindings)
                            :for binding := (cst:first bindings)
                            ;; if binding is in the form (declare x y+)
                            :if (and (cst:consp binding)
                                     (cst:consp (cst:rest form))
                                     (cst:consp (cst:rest (cst:rest form)))
                                     (cst:atom (cst:first binding))
                                     (eq (cst:raw (cst:first binding)) 'coalton:declare))
                              :do (push (parse-let-declare binding source) declares)
                            :else
                              :collect (parse-let-binding binding source))))

       (when sequential-p
         (check-sequential-binding-duplicates bindings source "let*"))

       (make-node-let
        :bindings bindings
        :declares (nreverse declares)
        :body (parse-body (cst:nthrest 2 form) form source)
        :sequential-p sequential-p
        :location (form-location source form)))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:dynamic-bind (cst:raw (cst:first form))))

     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed dynamic-bind"
                    (note-end source (cst:first form) "expected dynamic binding list")))

     (unless (cst:consp (cst:rest (cst:rest form)))
       (parse-error "Malformed dynamic-bind"
                    (note-end source (cst:second form) "expected dynamic-bind body")))

     (unless (cst:proper-list-p (cst:second form))
       (parse-error "Malformed dynamic-bind"
                    (note source (cst:second form) "expected binding list")))

     (make-node-dynamic-let
      :bindings (loop :for bindings := (cst:second form) :then (cst:rest bindings)
                      :while (cst:consp bindings)
                      :collect (parse-dynamic-binding (cst:first bindings)
                                                      source
                                                      :context "Malformed dynamic binding"))
      :subexpr (body-forms->node (cst:nthrest 2 form) form source)
      :location (form-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:rec (cst:raw (cst:first form))))

     (multiple-value-bind (name rec-bindings body)
         (cond
           ;; (rec)
           ((not (cst:consp (cst:rest form)))
            (parse-error "Malformed rec"
                         (note-end source (cst:first form)
                                   "expected function name")))
           ;; (rec name)
           ((not (cst:consp (cst:nthrest 2 form)))
            (parse-error "Malformed rec"
                         (note-end source (cst:second form)
                                   "expected binding list")))
           ;; (rec name bindings)
           ((cst:null (cst:nthrest 3 form))
            (parse-error "Malformed rec"
                         (note-end source (cst:third form)
                                   "expected rec body")))
           ;; (rec () ...)
           ((cst:null (cst:second form))
            (parse-error "Malformed rec"
                         (note source (cst:second form)
                               "expected function name")))
           ;; (rec <non-symbol> ...)
           ((not (cst:atom (cst:second form)))
            (parse-error "Malformed rec"
                         (note source (cst:second form)
                               "expected function name")))
           ;; (rec name bindings ...)
           (t
            (values (cst:second form)
                    (cst:third form)
                    (cst:nthrest 3 form))))

       (unless (cst:proper-list-p rec-bindings)
         (parse-error "Malformed rec"
                      (note source rec-bindings
                            "expected binding list")))

       (multiple-value-bind (declares bindings vars)
           (loop :for bindings := rec-bindings :then (cst:rest bindings)
                 :while (cst:consp bindings)
                 :for binding := (cst:first bindings)
                 ;; if binding is in the form (declare x y+)
                 :if (and (cst:consp binding)
                          (cst:atom (cst:first binding))
                          (eq (cst:raw (cst:first binding)) 'coalton:declare))
                   :collect (parse-let-declare binding source)
                     :into declares
                 :else
                   :collect (parse-rec-binding binding source) :into binding-list
                   :and :collect (cst:first binding) :into vars
                 :finally
                    (return (values declares binding-list vars)))

         (make-node-rec
          :name (parse-variable name source)
          :declares declares
          :bindings
          ;; Remove trivial self-bindings from the synthetic outer let; their
          ;; argument comes from the surrounding scope.
          (remove-if
           (lambda (binding)
             (and (typep (node-let-binding-value binding)
                         'node-variable)
                  (eq (node-variable-name (node-let-binding-name binding))
                      (node-variable-name (node-let-binding-value binding)))))
           bindings)
          :params (mapcar (lambda (var)
                            (parse-pattern var source))
                          vars)
          :call-args (mapcar #'node-let-binding-name bindings)
          :body (parse-body body form source)
          :location (form-location source form)))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:lisp (cst:raw (cst:first form))))
     (let ((args (cst:rest form)))

       ;; (lisp)
       (unless (cst:consp args)
         (parse-error "Malformed lisp expression"
                      (note-end source
                                (cst:first form)
                                "expected expression type")))

       (let ((type-form (cst:first args)))
         (let ((output-types (parse-lisp-return-type type-form source)))
           ;; (lisp (-> ...))
         (unless (cst:consp (cst:rest args))
           (parse-error "Malformed lisp expression"
                        (note-end source type-form "expected binding list")))

         (let* ((vars-form (cst:second args))
                (body-form (cst:nthrest 2 args)))
           ;; (lisp (-> ...) (...))
           (unless (cst:consp body-form)
             (parse-error "Malformed lisp expression"
                          (note source form "expected body")))

           (let ((vars nil)
                 (var-names nil))
             (loop :for vars-cst := vars-form :then (cst:rest vars-cst)
                   :while (cst:consp vars-cst)
                   :do (multiple-value-bind (var var-name)
                           (parse-lisp-variable-binding (cst:first vars-cst) source)
                         (push var vars)
                         (push var-name var-names)))
             (make-node-lisp
              :output-types output-types
              :vars (nreverse vars)
              :var-names (nreverse var-names)
              :body (cst:raw body-form)
              :location (form-location source form))))))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:match (cst:raw (cst:first form))))

     ;; (match)
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed match expression"
                    (note-end source (cst:first form) "expected expression")))

     (make-node-match
      :expr (parse-expression (cst:second form) source)
      :branches (loop :for branches := (cst:nthrest 2 form) :then (cst:rest branches)
                      :while (cst:consp branches)
                      :collect (parse-match-branch (cst:first branches) source))
      :location (form-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:progn (cst:raw (cst:first form))))
     (make-node-progn
      :body (parse-body (cst:rest form) (cst:first form) source)
      :location (form-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:type-of (cst:raw (cst:first form))))
     ;; (type-of)
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed type-of expression"
                    (note-end source (cst:first form) "expected expression")))

     ;; (type-of a b ...)
     (when (cst:consp (cst:rest (cst:rest form)))
       (parse-error "Malformed type-of expression"
                    (note source (cst:first (cst:rest (cst:rest form)))
                          "unexpected trailing form")))

     (make-node-type-of
      :expr (parse-expression (cst:second form) source)
      :location (form-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton++:unsafe (cst:raw (cst:first form))))
     (make-node-unsafe
      :body (parse-body (cst:rest form) (cst:first form) source)
      :location (form-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:the (cst:raw (cst:first form))))
     ;; (the)
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed the expression"
                    (note-end source (cst:first form) "expected type")))

     ;; (the T)
     (unless (cst:consp (cst:rest (cst:rest form)))
       (parse-error "Malformed the expression"
                    (note-end source (cst:second form) "expected value")))

     ;; (the a b c)
     (when (cst:consp (cst:rest (cst:rest (cst:rest form))))
       (parse-error "Malformed the expression"
                    (note source (cst:first (cst:rest (cst:rest (cst:rest form))))
                          "unexpected trailing form")))

     (make-node-the
      :type (parse-qualified-type (cst:second form) source)
      :expr (parse-expression (cst:third form) source)
      :location (form-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:return (cst:raw (cst:first form))))
     (let (expr)

       ;; (return ...)
       (when (cst:consp (cst:rest form))
         ;; (return a b ...)
         (when (cst:consp (cst:rest (cst:rest form)))
           (parse-error "Malformed return expression"
                        (note source (cst:first (cst:rest (cst:rest form)))
                              "unexpected trailing form")))

         (setf expr (parse-expression (cst:second form) source)))

       (make-node-return
        :expr expr
        :location (form-location source form))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:or (cst:raw (cst:first form))))
     (make-node-or
      :nodes (loop :for args := (cst:rest form) :then (cst:rest args)
                   :while (cst:consp args)
                   :for arg := (cst:first args)
                   :collect (parse-expression arg source))
      :location (form-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:and (cst:raw (cst:first form))))

     (make-node-and
      :nodes (loop :for args := (cst:rest form) :then (cst:rest args)
                   :while (cst:consp args)
                   :for arg := (cst:first args)
                   :collect (parse-expression arg source))
      :location (form-location source form)))
    ((and (cst:atom (cst:first form))
          (eq 'coalton:if (cst:raw (cst:first form))))
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed if expression"
                    (note-end source (cst:first form) "expected a predicate")))

     (unless (cst:consp (cst:rest (cst:rest form)))
       (parse-error "Malformed if expression"
                    (note-end source (cst:second form) "expected a form")))

     (unless (cst:consp (cst:rest (cst:rest (cst:rest form))))
       (parse-error "Malformed if expression"
                    (note-end source (cst:third form) "expected a form")))

     (when (cst:consp (cst:rest (cst:rest (cst:rest (cst:rest form)))))
       (parse-error "Malformed if expression"
                    (note source (cst:first (cst:rest (cst:rest (cst:rest (cst:rest form)))))
                          "unexpected trailing form")))

     (make-node-if
      :expr (parse-expression (cst:second form) source)
      :then (parse-expression (cst:third form) source)
      :else (parse-expression (cst:fourth form) source)
      :location (form-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:when (cst:raw (cst:first form))))
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed when expression"
                    (note-end source (cst:first form) "expected a predicate")))

     (make-node-when
      :expr (parse-expression (cst:second form) source)
      :body (parse-body (cst:rest (cst:rest form)) (cst:second form) source)
      :location (form-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:unless (cst:raw (cst:first form))))
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed unless expression"
                    (note-end source (cst:first form) "expected a predicate")))

     (make-node-unless
      :expr (parse-expression (cst:second form) source)
      :body (parse-body (cst:rest (cst:rest form)) (cst:second form) source)
      :location (form-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:cond (cst:raw (cst:first form))))
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed cond expression"
                    (note-end source (cst:first form) "expected one or more clauses")))

     (make-node-cond
      :clauses (loop :for clauses := (cst:rest form) :then (cst:rest clauses)
                     :while (cst:consp clauses)
                     :for clause := (cst:first clauses)
                     :collect (parse-cond-clause clause source))
      :location (form-location source form)))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:do (cst:raw (cst:first form))))
     (parse-do form source))

    ((and (cst:atom (cst:first form))
          (member (cst:raw (cst:first form))
                  '(coalton:for coalton:for*)
                  :test #'eq))
     (let ((sequential-p (eq 'coalton:for* (cst:raw (cst:first form)))))
       (multiple-value-bind (label labelled-body label-cst) (take-label form)
         (unless (cst:consp labelled-body)
           (parse-error "Malformed for expression"
                        (note-end source (or label-cst (cst:first form)) "expected binding list")))
         (unless (loop-binding-list-form-p (cst:first labelled-body))
           (return-from parse-expression
             (parse-application-expression form source)))
         (let* ((outer-loop-context *loop-label-context*)
                (loop-context
                  (if label
                      (list* label const:+default-loop-label+ outer-loop-context)
                      (cons const:+default-loop-label+ outer-loop-context))))

           (let* ((binding-list-form (cst:first labelled-body))
                  (post-bindings (cst:rest labelled-body))
                  (declares nil)
                  (bindings
                    (loop :for clauses := binding-list-form :then (cst:rest clauses)
                          :while (cst:consp clauses)
                          :for clause := (cst:first clauses)
                          :if (and (cst:consp clause)
                                   (cst:atom (cst:first clause))
                                   (eq (cst:raw (cst:first clause)) 'coalton:declare))
                            :do (push (parse-let-declare clause source) declares)
                          :else
                            :collect (let ((*loop-label-context* outer-loop-context))
                                       (parse-loop-binding clause source))))
                  (returns nil)
                  (termination-kind nil)
                  (termination-expr nil))

             (when sequential-p
               (check-sequential-binding-duplicates bindings source "for*"))

             (when (and (cst:consp post-bindings)
                        (loop-clause-keyword-p (cst:first post-bindings) "RETURNS"))
               (unless (cst:consp (cst:rest post-bindings))
                 (parse-error "Malformed for expression"
                              (note-end source (cst:first post-bindings) "expected expression after :returns")))
               (let ((*loop-label-context* outer-loop-context))
                 (setf returns (parse-expression (cst:second post-bindings) source)))
               (setf post-bindings (cst:nthrest 2 post-bindings)))

             (cond
               ((and (cst:consp post-bindings)
                     (loop-clause-keyword-p (cst:first post-bindings) "WHILE"))
                (unless (cst:consp (cst:rest post-bindings))
                  (parse-error "Malformed for expression"
                               (note-end source (cst:first post-bindings) "expected expression after :while")))
                (let ((*loop-label-context* outer-loop-context))
                  (setf termination-expr (parse-expression (cst:second post-bindings) source)))
                (setf termination-kind :while)
                (setf post-bindings (cst:nthrest 2 post-bindings)))
               ((and (cst:consp post-bindings)
                     (loop-clause-keyword-p (cst:first post-bindings) "UNTIL"))
                (unless (cst:consp (cst:rest post-bindings))
                  (parse-error "Malformed for expression"
                               (note-end source (cst:first post-bindings) "expected expression after :until")))
                (let ((*loop-label-context* outer-loop-context))
                  (setf termination-expr (parse-expression (cst:second post-bindings) source)))
                (setf termination-kind :until)
                (setf post-bindings (cst:nthrest 2 post-bindings)))
               ((and (cst:consp post-bindings)
                     (loop-clause-keyword-p (cst:first post-bindings) "REPEAT"))
                (unless (cst:consp (cst:rest post-bindings))
                  (parse-error "Malformed for expression"
                               (note-end source (cst:first post-bindings) "expected expression after :repeat")))
                (let ((*loop-label-context* outer-loop-context))
                  (setf termination-expr (parse-expression (cst:second post-bindings) source)))
                (setf termination-kind :repeat)
                (setf post-bindings (cst:nthrest 2 post-bindings))))

             (loop :for remaining := post-bindings :then (cst:rest remaining)
                   :while (cst:consp remaining)
                   :for clause := (cst:first remaining)
                   :when (loop-clause-keyword-p clause "RETURNS")
                     :do (parse-error "Malformed for expression"
                                      (note source clause
                                            ":returns clause must appear immediately after the binding list")))

             (let ((*loop-label-context* loop-context))
               (make-node-for
                :location (form-location source form)
                :label (or label const:+default-loop-label+)
                :bindings bindings
                :declares (nreverse declares)
                :returns returns
                :termination-kind termination-kind
                :termination-expr termination-expr
                :sequential-p sequential-p
                :body (parse-loop-body post-bindings source (form-location source form)))))))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:break (cst:raw (cst:first form))))

     (multiple-value-bind (label postlabel) (take-label form)
       (unless (cst:null postlabel)
         (parse-error "Invalid argument in break"
                      (note-end source form
                                (if label
                                    "unexpected argument after label"
                                    "expected a keyword"))))

       (if label
           (unless (member label *loop-label-context*)
             (parse-error "Invalid label in break"
                          (note source (cst:second form)
                                "label not found in any enclosing for")))
           (unless *loop-label-context*
             (parse-error "Invalid break"
                          (note source form
                                "break does not appear in an enclosing for"))))

       (make-node-break :location (form-location source form)
                        :label (or label (car *loop-label-context*)))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:continue (cst:raw (cst:first form))))

     (multiple-value-bind (label postlabel) (take-label form)
       (unless (cst:null postlabel)
         (parse-error "Invalid argument in continue"
                      (note source form
                            (if label
                                "unexpected argument after label"
                                "expected a keyword"))))

       (if label
           (unless (member label *loop-label-context*)
             (parse-error "Invalid label in continue"
                          (note source (cst:second form)
                                "label not found in any enclosing for")))
           (unless *loop-label-context*
             (parse-error "Invalid continue"
                          (note source form
                                "continue does not appear in an enclosing for"))))

       (make-node-continue :location (form-location source form)
                           :label (or label (car *loop-label-context*)))))


    ;;
    ;; Macros
    ;;

    ((and (cst:atom (cst:first form))
          (symbolp (cst:raw (cst:first form)))
          (macro-function (cst:raw (cst:first form))))

     (let ((*macro-expansion-count* (+ 1 *macro-expansion-count*)))

       (when (= *macro-expansion-count* +macro-expansion-max+)
         (parse-error "Invalid macro expansion"
                      (note source form "macro expansion limit hit")))

       (source:with-context
           (:macro "Error occurs within macro context. Source locations may be imprecise")
         (parse-expression (expand-macro form source) source))))

    ;;
    ;; Function Application
    ;;

    (t
     (parse-application-expression form source))))

(defun parse-expressions (forms source)
  (declare (type list forms)
           (values node &optional))
  (let ((nodes (mapcar (lambda (form) (parse-expression form source)) forms)))
    (make-node-progn
     :body (make-node-body
            :nodes (butlast nodes)
            :last-node (first (last nodes)))
     :location (source:make-location
                source
                (cons (source:span-start (cst:source (first forms)))
                      (source:span-end (cst:source (first (last forms)))))))))

(defun parse-variable (form source)
  (declare (type cst:cst form)
           (values node-variable &optional))

  (unless (and (cst:atom form)
               (identifierp (cst:raw form)))
    (parse-error "Invalid variable"
                 (note source form "expected identifier")))

  (when (string= "_" (symbol-name (cst:raw form)))
    (parse-error "Invalid variable"
                 (note source form "invalid variable name '_'")))

  (when (char= #\. (aref (symbol-name (cst:raw form)) 0))
    (parse-error "Invalid variable"
                 (note source form "variables cannot start with '.'")))

  (make-node-variable
   :name (cst:raw form)
   :location (form-location source form)))

(defun parse-ordinary-variable (form source)
  (declare (type cst:cst form)
           (values node-variable &optional))
  (let ((var (parse-variable form source)))
    (when (util:dynamic-variable-name-p (node-variable-name var))
      (parse-error "Invalid variable"
                   (note source form
                         "ordinary variables cannot use dynamic-variable earmuffs")))
    var))

(defun parse-dynamic-variable (form source)
  (declare (type cst:cst form)
           (values node-variable &optional))
  (let ((var (parse-variable form source)))
    (unless (util:dynamic-variable-name-p (node-variable-name var))
      (parse-error "Invalid dynamic variable"
                   (note source form
                         "dynamic variables must begin and end with '*' and contain at least one non-'*' character")))
    var))

(defun parse-lisp-variable-binding (form source)
  (declare (type cst:cst form)
           (values node-variable symbol &optional))
  (cond
    ((and (cst:atom form)
          (identifierp (cst:raw form)))
     (let ((var (parse-variable form source)))
       (values var (node-variable-name var))))
    ((cst:proper-list-p form)
     (let ((items (cst:listify form)))
       (unless (= 2 (length items))
         (parse-error "Invalid lisp binding"
                      (note source form "expected identifier or (identifier identifier)")))
       (let ((lisp-name-form (first items))
             (coalton-name-form (second items)))
         (unless (and (cst:atom lisp-name-form)
                      (identifierp (cst:raw lisp-name-form)))
           (parse-error "Invalid lisp binding"
                        (note source lisp-name-form "expected identifier")))
         (values (parse-variable coalton-name-form source)
                 (cst:raw lisp-name-form)))))
    (t
     (parse-error "Invalid lisp binding"
                  (note source form "expected identifier or (identifier identifier)")))))

(defun parse-accessor (form source)
  (declare (type cst:cst form)
           (values node-accessor))

  (assert (cst:atom form))
  (assert (symbolp (cst:raw form)))
  (assert (char= #\. (aref (symbol-name (cst:raw form)) 0)))

  (make-node-accessor
   :name (subseq (symbol-name (cst:raw form)) 1)
   :location (form-location source form)))

(defun parse-literal (form source)
  (declare (type cst:cst form)
           (values node &optional))

  (assert (cst:atom form))

  (typecase (cst:raw form)
    (integer
     (make-node-integer-literal
      :value (cst:raw form)
      :location (form-location source form)))

    (util:literal-value
     (make-node-literal
      :value (cst:raw form)
      :location (form-location source form)))

    (t
     (parse-error "Invalid literal"
                  (note source form "unknown literal type")))))

(defun parse-body (form preceding-form source)
  (declare (type cst:cst form)
           (values node-body &optional))

  (when (cst:atom form)
    (parse-error "Malformed function"
                 (note-end source preceding-form "expected body")))

  (assert (cst:proper-list-p form))

  (let ((nodes nil)
        (last-node nil)
        (remaining form))
    (loop :while (cst:consp remaining)
          :for current := (cst:first remaining)
          :do
             (cond
               ((cst:consp (cst:rest remaining))
                (cond
                  ((dynamic-shorthand-let-p current)
                   (setf last-node
                         (make-node-dynamic-let
                          :bindings (list (parse-dynamic-shorthand-binding current source))
                          :subexpr (body-forms->node (cst:rest remaining) preceding-form source)
                          :location (form-location source current)))
                   (return))
                  (t
                   (push (parse-body-element current source) nodes))))
               (t
                (setf last-node (parse-body-last-node current source))))
             (setf remaining (cst:rest remaining)))
    (make-node-body
     :nodes (nreverse nodes)
     :last-node last-node)))

(defun shorthand-let-p (form)
  "Returns t if FORM is in the form of (let x = y+)"
  (declare (type cst:cst form)
           (values boolean))

  (cond
    ((cst:atom form)
     nil)

    ;; (let)
    ((not (cst:consp (cst:rest form)))
     nil)

    ;; (let x)
    ((not (cst:consp (cst:rest (cst:rest form))))
     nil)

    ;; (let x =)
    ((not (cst:consp (cst:rest (cst:rest (cst:rest form)))))
     nil)

    (t
     (and (cst:atom (cst:first form))
          (eq (cst:raw (cst:first form)) 'coalton:let)
          (cst:atom (cst:third form))
          (eq (cst:raw (cst:third form)) 'coalton:=)))))

(defun dynamic-shorthand-let-p (form)
  (declare (type cst:cst form)
           (values boolean &optional))
  (and (shorthand-let-p form)
       (dynamic-variable-form-p (cst:second form))))

(defun parse-dynamic-binding (form source &key (context "Malformed dynamic binding"))
  (declare (type cst:cst form)
           (type string context)
           (values node-dynamic-binding &optional))
  (when (cst:atom form)
    (parse-error context
                 (note source form "expected list")))
  (unless (cst:proper-list-p form)
    (parse-error context
                 (note source form "unexpected dotted list")))
  (unless (cst:consp (cst:rest form))
    (parse-error context
                 (note-end source (cst:first form)
                           "dynamic bindings must have a value")))
  (when (cst:consp (cst:rest (cst:rest form)))
    (parse-error context
                 (note source (cst:first (cst:rest (cst:rest form)))
                       "unexpected trailing form")))
  (make-node-dynamic-binding
   :name (parse-dynamic-variable (cst:first form) source)
   :value (parse-expression (cst:second form) source)
   :location (form-location source form)))

(defun parse-dynamic-shorthand-binding (form source)
  (declare (type cst:cst form)
           (values node-dynamic-binding &optional))
  (when (cst:consp (cst:rest (cst:rest (cst:rest (cst:rest form)))))
    (parse-error "Malformed shorthand let"
                 (note source (cst:first (cst:rest (cst:rest (cst:rest (cst:rest form)))))
                       "unexpected trailing form")))
  (make-node-dynamic-binding
   :name (parse-dynamic-variable (cst:second form) source)
   :value (parse-expression (cst:fourth form) source)
   :location (form-location source form)))

(defun body-forms->node (forms preceding-form source)
  (declare (type cst:cst forms)
           (values node &optional))
  (make-node-progn
   :body (parse-body forms preceding-form source)
   :location (source:make-location source (util:cst-source-range (cst:listify forms)))))

;; Forms passed to parse-node-bind must be previously verified by `shorthand-let-p'
(defun parse-node-bind (form source)
  (declare (type cst:cst form)
           (values node-body-element))

  (when (cst:consp (cst:rest (cst:rest (cst:rest (cst:rest form)))))
    (parse-error "Malformed shorthand let"
                 (note source (cst:first (cst:rest (cst:rest (cst:rest (cst:rest form)))))
                       "unexpected trailing form")))

  (let ((pattern-form (cst:second form))
        (expr (parse-expression (cst:fourth form) source))
        (location (form-location source form)))
    (if (values-pattern-form-p pattern-form)
        (make-node-values-bind
         :patterns (loop :for patterns := (cst:rest pattern-form) :then (cst:rest patterns)
                         :while (cst:consp patterns)
                         :collect (parse-pattern (cst:first patterns) source))
         :expr expr
         :location location)
        (make-node-bind
         :pattern (parse-pattern pattern-form source)
         :expr expr
         :location location))))

(defun parse-body-element (form source)
  (declare (type cst:cst form)
           (values node-body-element &optional))

  (when (cst:atom form)
    (return-from parse-body-element
      (parse-expression form source)))

  (unless (cst:proper-list-p form)
    (parse-error "Malformed body expression"
                 (note source form "unexpected dotted list")))


  (if (shorthand-let-p form)
      (parse-node-bind form source)
      (parse-expression form source)))

(defun parse-body-last-node (form source)
  (declare (type cst:cst form)
           (values node &optional))

  (when (shorthand-let-p form)
    (parse-error "Malformed body expression"
                 (note source form
                       "body forms cannot be terminated by a shorthand let")))

  (parse-expression form source))

(defun parse-let-binding (form source)
  (declare (type cst:cst form)
           (values node-let-binding &optional))

  (when (cst:atom form)
    (parse-error "Malformed let binding"
                 (note source form "expected list")))

  (unless (cst:proper-list-p form)
    (parse-error "Malformed let binding"
                 (note source form "unexpected dotted list")))

  ;; (x)
  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed let binding"
                 (note-end source (cst:first form)
                           "let bindings must have a value")))

  ;; (a b c ...)
  (when (cst:consp (cst:rest (cst:rest form)))
    (parse-error "Malformed let binding"
                 (note source (cst:first (cst:rest (cst:rest form)))
                       "unexpected trailing form")))

  (make-node-let-binding
   :name (parse-ordinary-variable (cst:first form) source)
   :value (parse-expression (cst:second form) source)
   :location (form-location source form)))

(defun parse-loop-binding (form source)
  (declare (type cst:cst form)
           (values node-for-binding &optional))

  (when (cst:atom form)
    (parse-error "Malformed for binding"
                 (note source form "expected list")))

  (unless (cst:proper-list-p form)
    (parse-error "Malformed for binding"
                 (note source form "unexpected dotted list")))

  ;; (x)
  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed for binding"
                 (note-end source (cst:first form)
                           "for bindings must have an initializer")))

  ;; (x init)
  (unless (cst:consp (cst:rest (cst:rest form)))
    (parse-error "Malformed for binding"
                 (note-end source (cst:second form)
                           "for bindings must have a step expression")))

  (let ((rest2 (cst:rest (cst:rest form))))
    ;; (x init step ...)
    (when (cst:consp (cst:rest rest2))
      (parse-error "Malformed for binding"
                   (note source (cst:first (cst:rest rest2))
                         "unexpected trailing form")))

    (make-node-for-binding
     :name (parse-ordinary-variable (cst:first form) source)
     :init (parse-expression (cst:second form) source)
     :step (parse-expression (cst:third form) source)
     :location (form-location source form))))

(defun parse-rec-binding (form source)
  (declare (type cst:cst form)
           (values node-let-binding &optional))

  (when (cst:atom form)
    (parse-error "Malformed rec binding"
                 (note source form "expected list")))

  (unless (cst:proper-list-p form)
    (parse-error "Malformed rec binding"
                 (note source form "unexpected dotted list")))

  ;; (x)
  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed rec binding"
                 (note-end source (cst:first form)
                           "rec bindings must have a value")))

  ;; (a b c ...)
  (when (cst:consp (cst:rest (cst:rest form)))
    (parse-error "Malformed rec binding"
                 (note source (cst:first (cst:rest (cst:rest form)))
                       "unexpected trailing form")))

  (make-node-let-binding
   :name (parse-ordinary-variable (cst:first form) source)
   :value (parse-expression (cst:second form) source)
   :location (form-location source form)))

(defun parse-match-branch (form source)
  (declare (type cst:cst form)
           (values node-match-branch &optional))

  (when (cst:atom form)
    (parse-error "Malformed match branch"
                 (note source form "expected list")))

  (unless (cst:proper-list-p form)
    (parse-error "Malformed match branch"
                 (note source form "unexpected dotted list")))

  ;; (P)
  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed match branch"
                 (note-end source (cst:first form) "expected body")))

  (make-node-match-branch
   :pattern (parse-pattern (cst:first form) source)
   :body (parse-body (cst:rest form) form source)
   :location (form-location source form)))

(defun parse-catch-branch (form source)
  (declare (type cst:cst form)
           (values node-catch-branch &optional))

  (when (cst:atom form)
    (parse-error "Malformed catch branch"
                 (note source form "expected list")))

  (unless (cst:proper-list-p form)
    (parse-error "Malformed catch branch"
                 (note source form "unexpected dotted list")))

  ;; (P)
  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed catch branch"
                 (note-end source (cst:first form) "expected body")))

  (let ((pattern (parse-pattern (cst:first form) source)))
    (when (pattern-var-p pattern)
      (parse-error
       "Malformed catch branch"
       (note source (cst:first form)
             "Not Yet Allowed: Catching an exception with a pattern variable")))

    (unless (typep pattern '(or pattern-constructor pattern-wildcard))
      (parse-error
       "Malformed catch branch"
       (note source (cst:first form)
             "branch must be either an exception type constructor or a wildcard.")))

    (make-node-catch-branch
     :pattern pattern
     :body (parse-body (cst:rest form) form source)
     :location (form-location source form))))

(defun parse-resumable-branch (form source)
  (declare (type cst:cst form)
           (values node-resumable-branch &optional))

  (when (cst:atom form)
    (parse-error "Malformed resumable branch"
                 (note source form "expected list")))

  (unless (cst:proper-list-p form)
    (parse-error "Malformed resumable branch"
                 (note source form "unexpected dotted list")))

  ;; (P)
  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed resumable branch"
                 (note-end source (cst:first form) "expected body")))

  (let ((pattern (parse-pattern (cst:first form) source)))
    (unless (typep pattern 'pattern-constructor)
      (parse-error "Malformed resumable branch"
                   (note
                    source
                    (cst:first form)
                    "pattern must match a resumption constructor.")))
    (make-node-resumable-branch
     :pattern  pattern
     :body (parse-body (cst:rest form) form source)
     :location (form-location source form))))

(defun parse-cond-clause (form source)
  (declare (type cst:cst form)
           (values node-cond-clause))

  (when (cst:atom form)
    (parse-error "Malformed cond clause"
                 (note source form "expected list")))

  (unless (cst:proper-list-p form)
    (parse-error "Malformed cond clause"
                 (note source form "unexpected dotted list")))

  (make-node-cond-clause
   :expr (parse-expression (cst:first form) source)
   :body (parse-body (cst:rest form) (cst:first form) source)
   :location (form-location source form)))

(defun parse-do-forms (forms parent-form source location)
  (declare (type cst:cst forms parent-form)
           (type source:location location)
           (values node-do &optional))
  (let ((nodes nil)
        (last-node nil)
        (remaining forms))
    (loop :while (cst:consp remaining)
          :for current := (cst:first remaining)
          :do
             (cond
               ((cst:consp (cst:rest remaining))
                (cond
                  ((dynamic-shorthand-let-p current)
                   (setf last-node
                         (make-node-dynamic-let
                          :bindings (list (parse-dynamic-shorthand-binding current source))
                          :subexpr (parse-do-forms
                                    (cst:rest remaining)
                                    parent-form
                                    source
                                    (source:make-location
                                     source
                                     (util:cst-source-range
                                      (cst:listify (cst:rest remaining)))))
                          :location (form-location source current)))
                   (return))
                  (t
                   (push (parse-do-body-element current source) nodes))))
               (t
                (setf last-node (parse-do-body-last-node current parent-form source))))
             (setf remaining (cst:rest remaining)))
    (make-node-do
     :nodes (nreverse nodes)
     :last-node last-node
     :location location)))

(defun parse-do (form source)
  (declare (type cst:cst form))

  (assert (cst:consp form))

  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed do expression"
                 (note-end source (cst:first form) "expected one or more forms")))

  (parse-do-forms (cst:rest form)
                  (cst:first form)
                  source
                  (form-location source form)))

(defun do-bind-p (form)
  "Returns t if FORM is in the form of (x <- y+)"
  (declare (type cst:cst form)
           (values boolean))

  (cond
    ((not (cst:consp form))
     nil)

    ;; (x)
    ((not (cst:consp (cst:rest form)))
     nil)

    ;; (x y)
    ((not (cst:consp (cst:rest (cst:rest form))))
     nil)

    ;; (x (y) ...)
    ((not (cst:atom (cst:second form)))
     nil)

    (t
     (eq 'coalton:<- (cst:raw (cst:second form))))))

;; Forms passed to this function must first be validated with `do-bind-p'
(defun parse-node-do-bind (form source)
  (declare (type cst:cst form)
           (values node-do-bind))

  (when (cst:consp (cst:rest (cst:rest (cst:rest form))))
    (parse-error "Malformed bind form"
                 (note source (cst:first (cst:rest (cst:rest (cst:rest form))))
                       "unexpected trailing form")))

  (make-node-do-bind
   :pattern (parse-pattern (cst:first form) source)
   :expr (parse-expression (cst:third form) source)
   :location (form-location source form)))

(defun parse-do-body-element (form source)
  (declare (type cst:cst form)
           (values node-do-body-element &optional))

  (cond
    ((shorthand-let-p form)
     (parse-node-bind form source))

    ((do-bind-p form)
     (parse-node-do-bind form source))

    (t
     (parse-expression form source))))

(defun parse-do-body-last-node (form parent-form source)
  (declare (type cst:cst form)
           (type cst:cst parent-form)
           (values node &optional))

  (when (shorthand-let-p form)
    (parse-error "Malformed do expression"
                 (note source form
                       "do expressions cannot be terminated by a shorthand let")
                 (secondary-note source parent-form "when parsing do expression")))

  (when (do-bind-p form)
    (parse-error "Malformed do expression"
                 (note source form
                       "do expression cannot be terminated by a bind")
                 (secondary-note source parent-form "when parsing do expression")))

  (parse-expression form source))

(defun parse-let-declare (form source)
  (declare (type cst:cst form)
           (values node-let-declare))

  (assert (cst:consp form))
  (assert (cst:consp (cst:rest form)))
  (assert (cst:consp (cst:rest (cst:rest form))))

  (assert (cst:atom (cst:first form)))
  (assert (eq (cst:raw (cst:first form)) 'coalton:declare))

  (when (cst:consp (cst:rest (cst:rest (cst:rest form))))
    (parse-error "Malformed declare"
                 (note source (cst:fourth form) "unexpected form")))

  (let ((qualified-type (parse-qualified-type (cst:third form) source)))
    (when (typep (qualified-ty-type qualified-type) 'result-ty)
      (parse-error "Malformed declaration"
                   (note source (cst:third form)
                         "cannot declare a binding as ~A; declarations must have a single-value or function type"
                         (if (null (result-ty-output-types
                                    (qualified-ty-type qualified-type)))
                             "Void"
                             "a multi-value type"))))
    (make-node-let-declare
     :name (parse-ordinary-variable (cst:second form) source)
     :type qualified-type
     :location (form-location source form))))

(defun loop-clause-keyword-p (form name)
  (declare (type cst:cst form)
           (type string name)
           (values boolean &optional))
  (and (cst:atom form)
       (keywordp (cst:raw form))
       (string= (symbol-name (cst:raw form)) name)))

(defun loop-binding-list-form-p (form)
  (declare (type cst:cst form)
           (values boolean &optional))
  (and (cst:proper-list-p form)
       (loop :for clauses := form :then (cst:rest clauses)
             :while (cst:consp clauses)
             :always (let ((clause (cst:first clauses)))
                       (and (cst:consp clause)
                            (cst:proper-list-p clause))))))

(defun parse-loop-body (forms source location)
  (declare (type cst:cst forms)
           (type source:location location)
           (values node-body &optional))
  (when (and (not (cst:null forms))
             (not (cst:consp forms)))
    (parse-error "Malformed for expression"
                 (note source forms "unexpected dotted list")))
  (make-node-body
   :nodes (loop :for rest := forms :then (cst:rest rest)
                :while (cst:consp rest)
                :collect (parse-body-element (cst:first rest) source))
   ;; Loop bodies discard every user form, so we append an internal empty
   ;; values node to reuse the normal body sequencing/scoping machinery.
   :last-node (make-node-values
               :nodes nil
               :location location)))

(defun take-label (form)
  "Takes form (HEAD . (MAYBEKEYWORD . REST)) and returns three values,
either

MAYBEKEYWORD REST MAYBECST

if MAYBEKEYWORD is a keyword, or else

NIL (MAYBEKEYWORD . REST) NIL

if (CST:SECOND FORM) is not a keyword."
  (declare (type cst:cst form)
           (values (or keyword null) cst:cst))
  (if (and (cst:consp (cst:rest form))
           (cst:atom (cst:second form))
           (keywordp (cst:raw (cst:second form))))
      (values (cst:raw (cst:second form))
              (cst:nthrest 2 form)
              (cst:second form))
      (values nil (cst:rest form) nil)))
