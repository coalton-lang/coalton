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
   #:node-body-element                  ; TYPE
   #:node-body-element-list             ; TYPE
   #:node-body                          ; STRUCT
   #:make-node-body                     ; CONSTRUCTOR
   #:node-body-nodes                    ; ACCESSOR
   #:node-body-last-node                ; ACCESSOR
   #:node-abstraction                   ; STRUCT
   #:make-node-abstraction              ; CONSTRUCTOR
   #:node-abstraction-params            ; ACCESSOR
   #:node-abstraction-body              ; ACCESSOR
   #:node-abstraction-p                 ; FUNCTION
   #:node-let-binding                   ; STRUCT
   #:make-node-let-binding              ; CONSTRUCTOR
   #:node-let-binding-name              ; ACCESSOR
   #:node-let-binding-value             ; ACCESSOR
   #:node-let-binding-list              ; TYPE
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
   #:node-the                           ; STRUCT
   #:make-node-the                      ; CONSTRUCTOR
   #:node-the-type                      ; ACCESSOR
   #:node-the-expr                      ; ACCESSOR
   #:node-return                        ; STRUCT
   #:make-node-return                   ; CONSTRUCTOR
   #:node-return-expr                   ; ACCESSOR
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
   #:node-while                         ; STRUCT
   #:make-node-while                    ; CONSTRUCTOR
   #:node-while-label                   ; ACCESSOR
   #:node-while-expr                    ; ACCESSOR
   #:node-while-body                    ; ACCESSOR
   #:node-while-let                     ; STRUCT
   #:make-node-while-let                ; CONSTRUCTOR
   #:node-while-let-label               ; ACCESSOR
   #:node-while-let-pattern             ; ACCESSOR
   #:node-while-let-expr                ; ACCESSOR
   #:node-while-let-body                ; ACCESSOR
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
   #:node-for                           ; STRUCT
   #:make-node-for                      ; CONSTRUCTOR
   #:node-for-label                     ; ACCESSOR
   #:node-for-pattern                   ; ACCESSOR
   #:node-for-expr                      ; ACCESSOR
   #:node-for-body                      ; ACCESSOR
   #:make-node-do                       ; CONSTRUCTOR
   #:node-do-nodes                      ; ACCESSOR
   #:node-do-last-node                  ; ACCESSOR
   #:parse-expression                   ; FUNCTION
   #:parse-expressions                  ; FUNCTION
   #:parse-body                         ; FUNCTION
   #:parse-variable                     ; FUNCTION
   ))

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
;;;;
;;;; node-bind := "(" "let" pattern "=" expression ")"
;;;;
;;;; node-body-element := expression | shorthand-let
;;;;
;;;; node-body := node-body-element* expression
;;;;
;;;; node-abstraction := "(" "fn" "(" pattern* ")" node-body ")"
;;;;
;;;; node-let-binding := "(" identifier expression ")"
;;;;
;;;; node-let-declare := "(" "declare" identifier qualified-ty ")"
;;;;
;;;; node-let := "(" "let" "(" (node-let-binding | node-let-declare)+ ")" body ")"
;;;;
;;;; node-rec := "(" "rec" (identifier | "(" identifier [qualified-ty] ")" ) "(" (node-let-binding | node-let-declare)+ ")" body ")"
;;;;
;;;; node-lisp := "(" "lisp" type "(" variable* ")" lisp-form+ ")"
;;;;
;;;; node-match-branch := "(" pattern body ")"
;;;;
;;;; node-match := "(" "match" pattern match-branch* ")"
;;;;
;;;; node-progn := "(" "progn" body ")"
;;;;
;;;; node-the := "(" "the" type expression ")"
;;;;
;;;; node-return := "(" "return" expression? ")"
;;;;
;;;; node-application := "(" expression expression* ")"
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
;; - does not have source information (but its children do)
;;
(defstruct (node-body
            (:copier nil))
  (nodes     (util:required 'nodes)     :type node-body-element-list :read-only t)
  (last-node (util:required 'last-node) :type node                   :read-only t))

(defstruct (node-abstraction
            (:include node)
            (:copier nil))
  (params (util:required 'params) :type pattern-list :read-only t)
  (body   (util:required 'body)   :type node-body    :read-only t))

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
  (bindings (util:required 'bindings) :type node-let-binding-list :read-only t)
  (declares (util:required 'declares) :type node-let-declare-list :read-only t)
  (body     (util:required 'body)     :type node-body             :read-only t))

(defstruct (node-lisp
            (:include node)
            (:copier nil))
  (type      (util:required 'type)      :type ty                 :read-only t)
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

(defstruct (node-the
            (:include node)
            (:copier nil))
  (type (util:required 'type) :type ty   :read-only t)
  (expr (util:required 'expr) :type node :read-only t))

(defstruct (node-return
            (:include node)
            (:copier nil))
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

(defstruct (node-while
            (:include node)
            (:copier nil))
  (label (util:required 'label) :type keyword   :read-only t)
  (expr  (util:required 'expr)  :type node      :read-only t)
  (body  (util:required 'body)  :type node-body :read-only t))

(defstruct (node-while-let
            (:include node)
            (:copier nil))
  (label   (util:required 'label)   :type keyword   :read-only t)
  (pattern (util:required 'pattern) :type pattern   :read-only t)
  (expr    (util:required 'expr)    :type node      :read-only t)
  (body    (util:required 'body)    :type node-body :read-only t))

(defstruct (node-break
            (:include node)
            (:copier nil))
  (label (util:required 'label) :type keyword :read-only t))

(defstruct (node-continue
            (:include node)
            (:copier nil))
  (label (util:required 'label) :type keyword :read-only t))

(defstruct (node-loop
            (:include node)
            (:copier nil))
  (label (util:required 'label) :type keyword   :read-only t)
  (body  (util:required 'body)  :type node-body :read-only t))

(defstruct (node-for
            (:include node)
            (:copier nil))
  (label   (util:required 'label)   :type keyword   :read-only t)
  (pattern (util:required 'pattern) :type pattern   :read-only t)
  (expr    (util:required 'expr)    :type node      :read-only t)
  (body    (util:required 'body)    :type node-body :read-only t))

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
          (eq 'coalton:fn (cst:raw (cst:first form))))
     (let ((params)
           (body))

       ;; (fn)
       (unless (cst:consp (cst:rest form))
         (parse-error "Malformed function"
                      (note-end source (cst:first form) "expected function arguments")))

       ;; (fn (...))
       (unless (cst:consp (cst:rest (cst:rest form)))
         (parse-error "Malformed function"
                      (note-end source (cst:second form) "expected function body")))

       ;; (fn x ...)
       ;;
       ;; NOTE: (fn () ...) is allowed
       (when (and (cst:atom (cst:second form))
                  (not (null (cst:raw (cst:second form)))))
         (parse-error "Malformed function"
                      (note source (cst:second form)
                            "malformed argument list")
                      (help source (cst:second form)
                            (lambda (existing)
                              (concatenate 'string "(" existing ")"))
                            "add parentheses")))
       ;; Bind *LOOP-LABEL-CONTEXT* to NIL to disallow BREAKing from
       ;; or CONTINUING with loops that enclose the FN form.
       (let ((*loop-label-context* nil))
         (setf params
               (loop :for vars := (cst:second form) :then (cst:rest vars)
                     :while (cst:consp vars)
                     :collect (parse-pattern (cst:first vars) source)))
         (setf body (parse-body (cst:nthrest 2 form) form source))
         (make-node-abstraction
          :params params
          :body body
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
          (eq 'coalton:let (cst:raw (cst:first form))))

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

       (make-node-let
        :bindings bindings
        :declares (nreverse declares)
        :body (parse-body (cst:nthrest 2 form) form source)
        :location (form-location source form))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:rec (cst:raw (cst:first form))))

     (multiple-value-bind (name type rec-bindings body)
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
           ((cst:null (cst:second form))
            (parse-error "Malformed rec"
                         (note source (cst:second form)
                               "unexpected empty list")))
           ;; (rec name bindings ...)
           ((cst:atom (cst:second form))
            (values (cst:second form)
                    nil
                    (cst:third form)
                    (cst:nthrest 3 form)))
           ;; (rec (name qual-ty) bindings ...)
           (t
            (cond
              ((cst:null (cst:rest (cst:second form)))
               (values (cst:first (cst:second form))
                       nil
                       (cst:third form)
                       (cst:nthrest 3 form)))
              ((cst:consp (cst:rest (cst:second form)))
               (when (cst:consp (cst:nthrest 2 (cst:second form)))
                 (parse-error "Malformed rec"
                              (note-end source (cst:second (cst:second form))
                                        "unexpected trailing form(s)")))
               (values (cst:first (cst:second form))
                       (cst:second (cst:second form))
                       (cst:third form)
                       (cst:nthrest 3 form)))
              (t
               (parse-error "Malformed rec"
                            (note source (cst:rest (cst:second form))
                                  "unexpected dotted list"))))))

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

         (make-node-let
          :declares declares
          :bindings
          ;; Remove recursive bindings
          (remove-if
           (lambda (binding)
             (and (typep (node-let-binding-value binding)
                         'node-variable)

                  (eq (node-variable-name (node-let-binding-name binding))
                      (node-variable-name (node-let-binding-value binding)))))
           bindings)
          :body
          (make-node-body
           :nodes nil
           :last-node
           (make-node-let
            :declares (if type
                          (list
                           (make-node-let-declare
                            :name (parse-variable name source)
                            :type (parse-qualified-type type source)
                            :location (form-location source type)))
                          nil)
            :bindings (list (make-node-let-binding
                             :name (parse-variable name source)
                             :value
                             (make-node-abstraction
                              :params (mapcar (lambda (var)
                                                (parse-pattern var source))
                                              vars)
                              :body (parse-body body form source)
                              :location (form-location source form))
                             :location (form-location source form)))
            :body
            (make-node-body
             :nodes nil
             :last-node
             (make-node-application
              :rator (parse-expression name source)
              :rands (mapcar #'node-let-binding-name bindings)
              :location (form-location source form)))
            :location (form-location source form)))
          :location (form-location source form)))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:lisp (cst:raw (cst:first form))))
     ;; (lisp)
     (unless (cst:consp (cst:rest form))
       (parse-error "Malformed lisp expression"
                    (note-end source (cst:first form) "expected expression type")))

     ;; (lisp T)
     (unless (cst:consp (cst:rest (cst:rest form)))
       (parse-error "Malformed lisp expression"
                    (note-end source (cst:second form) "expected binding list")))

     ;; (lisp T (...))
     (unless (cst:consp (cst:rest (cst:rest (cst:rest form))))
       (parse-error "Malformed lisp expression"
                    (note source form "expected body")))

     (let ((vars (loop :for vars := (cst:third form) :then (cst:rest vars)
                       :while (cst:consp vars)
                       :collect (parse-variable (cst:first vars) source))))
       (make-node-lisp
        :type (parse-type (cst:second form) source)
        :vars vars
        :var-names (mapcar #'node-variable-name vars)
        :body (cst:raw (cst:nthrest 3 form))
        :location (form-location source form))))

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
      :type (parse-type (cst:second form) source)
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
          (eq 'coalton:while (cst:raw (cst:first form))))
     (multiple-value-bind (label labelled-body label-cst) (take-label form)
       ;; (while [label])
       (unless (cst:consp labelled-body)
         (parse-error "Malformed while expression"
                      (note-end source (or label-cst (cst:first form)) "expected condition")))
       ;; (while [label] condition)
       (unless (cst:consp (cst:rest labelled-body))
         (parse-error "Malformed while expression"
                      (note-end source (cst:first labelled-body) "expected body")))
       (let ((*loop-label-context*
               (if label
                   (list* label const:+default-loop-label+ *loop-label-context*)
                   (cons const:+default-loop-label+ *loop-label-context*))))

         (make-node-while
          :location (form-location source form)
          :label (or label const:+default-loop-label+)
          :expr (parse-expression (cst:first labelled-body) source)
          :body (parse-body (cst:rest labelled-body) form source)))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:while-let (cst:raw (cst:first form))))

     (multiple-value-bind (label labelled-body label-cst) (take-label form)
       ;; (while-let [label])
       (unless (cst:consp labelled-body)
         (parse-error "Malformed while-let expression"
                      (note-end source (or label-cst (cst:first form)) "expected pattern")))

       ;; (while-let [label] pattern)
       (unless (and (cst:consp (cst:rest labelled-body))
                    (eq 'coalton:= (cst:raw (cst:second labelled-body))))
         (parse-error "Malformed while-let expression"
                      (if (cst:consp (cst:rest labelled-body))
                          (note source (cst:second labelled-body) "expected =")
                          (note-end source (cst:first labelled-body) "expected ="))))

       ;; (when-let [label] pattern =)
       (unless (cst:consp (cst:nthrest 2 labelled-body))
         (parse-error "Malformed while-let expression"
                      (note-end source (cst:second labelled-body) "expected expression")))

       ;; (when-let pattern = expr)
       (unless (cst:consp (cst:nthrest 3 labelled-body))
         (parse-error "Malformed while-let expression"
                      (note-end source (cst:third labelled-body) "expected body")))
       (let* ((*loop-label-context*
                (if label
                    (list* label const:+default-loop-label+ *loop-label-context*)
                    (cons const:+default-loop-label+ *loop-label-context*))))
         (make-node-while-let
          :location (form-location source form)
          :label (or label const:+default-loop-label+)
          :pattern (parse-pattern (cst:first labelled-body) source)
          :expr (parse-expression (cst:third labelled-body) source)
          :body (parse-body (cst:nthrest 3 labelled-body) form source)))))

    ((and (cst:atom (cst:first form))
          (eq 'coalton:loop (cst:raw (cst:first form))))
     (multiple-value-bind (label labelled-body label-cst) (take-label form)
       (unless (cst:consp labelled-body)
         (parse-error "Malformed loop expression"
                      (note-end source (or label-cst (cst:first form)) "expected a loop body")))

       (let* ((*loop-label-context*
                (if label
                    (list* label const:+default-loop-label+ *loop-label-context*)
                    (cons const:+default-loop-label+ *loop-label-context*))))
         (make-node-loop
          :location (form-location source form)
          :label (or label const:+default-loop-label+)
          :body (parse-body labelled-body form source)))))

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
                                "label not found in any enclosing loop")))
           (unless *loop-label-context*
             (parse-error "Invalid break"
                          (note source form
                                "break does not appear in an enclosing loop"))))

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
                                "label not found in any enclosing loop")))
           (unless *loop-label-context*
             (parse-error "Invalid continue"
                          (note source form
                                "continue does not appear in an enclosing loop"))))

       (make-node-continue :location (form-location source form)
                           :label (or label (car *loop-label-context*)))))


    ((and (cst:atom (cst:first form))
          (eq 'coalton:for (cst:raw (cst:first form))))

     (multiple-value-bind (label labelled-body label-cst) (take-label form)
       ;; (for [label])
       (unless (cst:consp labelled-body)
         (parse-error "Malformed for expression"
                      (note-end source (or label-cst (cst:first form)) "expected pattern")))

       ;; (for [label] pattern)
       (unless (and (cst:consp (cst:rest labelled-body))
                    (cst:atom (cst:second labelled-body))
                    (eq 'coalton:in (cst:raw (cst:second labelled-body))))
         (parse-error "Malformed for expression"
                      (if (and (cst:consp (cst:rest labelled-body))
                               (cst:second labelled-body))
                          (note source (cst:second labelled-body) "expected in")
                          (note-end source (cst:first labelled-body) "expected in"))))

       ;; (for [label] pattern in)
       (unless (cst:consp (cst:nthrest 2 labelled-body))
         (parse-error "Malformed for expression"
                      (note-end source form "expected expression")))

       ;; (for [label] pattern in expr)
       (unless (cst:consp (cst:nthrest 3 labelled-body))
         (parse-error "Malformed for expression"
                      (note-end source (cst:third labelled-body) "expected body")))

       (let ((*loop-label-context*
               (if label
                   (list* label const:+default-loop-label+ *loop-label-context*)
                   (cons const:+default-loop-label+ *loop-label-context*))))
         (make-node-for
          :location (form-location source form)
          :label (or label const:+default-loop-label+)
          :pattern (parse-pattern (cst:first labelled-body) source)
          :expr (parse-expression (cst:third labelled-body) source)
          :body (parse-body (cst:nthrest 3 labelled-body) form  source)))))

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
     (make-node-application
      :rator (parse-expression (cst:first form) source)
      :rands (loop :for rands := (cst:rest form) :then (cst:rest rands)
                   :while (cst:consp rands)
                   :for rand := (cst:first rands)
                   :collect (parse-expression rand source))
      :location (form-location source form)))))

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

  (let* (last-node

         (nodes (loop :for nodes := form :then (cst:rest nodes)
                      :while (cst:consp nodes)

                      ;; Not the last node
                      :if (cst:consp (cst:rest nodes))
                        :collect (parse-body-element (cst:first nodes) source)

                      ;; The last node
                      :else
                        :do (setf last-node (parse-body-last-node (cst:first nodes) source)))))

    
    (make-node-body
     :nodes nodes
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

;; Forms passed to parse-node-bind must be previously verified by `shorthand-let-p'
(defun parse-node-bind (form source)
  (declare (type cst:cst form)
           (values node-bind))

  (when (cst:consp (cst:rest (cst:rest (cst:rest (cst:rest form)))))
    (parse-error "Malformed shorthand let"
                 (note source (cst:first (cst:rest (cst:rest (cst:rest (cst:rest form)))))
                       "unexpected trailing form")))

  (make-node-bind
   :pattern (parse-pattern (cst:second form) source)
   :expr (parse-expression (cst:fourth form) source)
   :location (form-location source form)))

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
   :name (parse-variable (cst:first form) source)
   :value (parse-expression (cst:second form) source)
   :location (form-location source form)))

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
   :name (parse-variable (cst:first form) source)
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

(defun parse-do (form source)
  (declare (type cst:cst form))

  (assert (cst:consp form))

  (unless (cst:consp (cst:rest form))
    (parse-error "Malformed do expression"
                 (note-end source (cst:first form) "expected one or more forms")))

  (let* (last-node

         (nodes (loop :for nodes := (cst:rest form) :then (cst:rest nodes)
                      :while (cst:consp nodes)
                      :for node := (cst:first nodes)

                      ;; Not the last node
                      :if (cst:consp (cst:rest nodes))
                        :collect (parse-do-body-element node source)

                      :else
                        :do (setf last-node (parse-do-body-last-node node (cst:first form) source)))))

    (make-node-do
     :nodes nodes
     :last-node last-node
     :location (form-location source form))))

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

  (make-node-let-declare
   :name (parse-variable (cst:second form) source)
   :type (parse-qualified-type (cst:third form) source)
   :location (form-location source form)))

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
