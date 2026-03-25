;;;; faux-macros.lisp
;;;;
;;;; Some macro definitions that are only valid inside of a COALTON (&
;;;; co.) macro, that aren't actually valid as a toplevel form in
;;;; Lisp.

(defpackage #:coalton-impl/faux-macros
  (:use
   #:cl)
  (:local-nicknames
   (#:rt #:coalton-impl/runtime)))

(in-package #:coalton-impl/faux-macros)

(defun error-coalton-toplevel-only (name)
  (error "The operator ~S is only valid in a Coalton toplevel." name))

(defun error-coalton-only (name)
  (error "The operator ~S is only valid in a Coalton expression." name))

(defmacro define-coalton-toplevel-editor-macro (name lambda-list &optional (docstring ""))
  "Define a macro so that Emacs and SLIME see it nicely, and so the forms indent properly. Not intended for actual use in Lisp code."
  (check-type docstring string)
  `(defmacro ,name ,lambda-list
     ,docstring
     (declare (ignore ,@(remove-if (lambda (sym) (char= #\& (char (symbol-name sym) 0)))
                                   lambda-list)))
     (error-coalton-toplevel-only ',name)))

(defmacro define-coalton-editor-macro (name lambda-list &optional (docstring ""))
  "Define a macro so that Emacs and SLIME see it nicely, and so the forms indent properly. Not intended for actual use in Lisp code."
  (check-type docstring string)
  `(defmacro ,name ,lambda-list
     ,docstring
     (declare (ignore ,@(remove-if (lambda (sym) (char= #\& (char (symbol-name sym) 0)))
                                   lambda-list)))
     (error-coalton-only ',name)))


;;; Top-Level Forms

(define-coalton-toplevel-editor-macro coalton:define (var-or-fun &body body)
  "Define a variable or function. (Coalton top-level operator.)")

(define-coalton-toplevel-editor-macro coalton:define-type (name &body definition)
  "Create a new algebraic data type named NAME. (Coalton top-level operator.)")

(define-coalton-toplevel-editor-macro coalton:define-struct (name &body definition)
  "Create a new struct named NAME. (Coalton top-level operator.)")

(define-coalton-toplevel-editor-macro coalton:define-type-alias (name &body definition)
  "Create a new type alias named NAME. (Coalton top-level operator.)")

(define-coalton-toplevel-editor-macro coalton:define-exception (name &body definition)
    "Create a new exception algebraic data type. (Coalton top-level operator.)")

(define-coalton-toplevel-editor-macro coalton:define-resumption (name constructor)
    "Create a new resumption with its single constructor. (Coalton top-level operator.)")

(define-coalton-toplevel-editor-macro coalton:declare (var type)
    "Declare the type of a variable. (Coalton top-level operator.)")

(define-coalton-toplevel-editor-macro coalton:define-class (class &body method-signatures)
  "Define a new type class. (Coalton top-level operator.")

(define-coalton-toplevel-editor-macro coalton:define-instance (instance &body method-definitions)
  "Define an instance of a type class. (Coalton top-level operator.)")

(define-coalton-toplevel-editor-macro coalton:lisp-toplevel (options &body lisp-toplevel-forms)
  "Include lisp forms. (Coalton top-level operator.)")

(define-coalton-toplevel-editor-macro coalton:specialize
    (generic-fun specialized-fun specialized-ty)
  "Declare a specialization for a function. (Coalton top-level operator.)")

;;; Attributes

(define-coalton-editor-macro coalton:repr (type &optional arg)
  "Annote a type definition with a runtime representation.")

(define-coalton-editor-macro coalton:derive (&rest classes)
  "Derive class instances for structs or types.")

(define-coalton-editor-macro coalton:monomorphize ()
  "Mark a definition for monomorphization.")


;;; Other Constructions

(defmacro coalton:fn (vars &body form)
  "A lambda abstraction callable within coalton."
  (rt:construct-function-entry `(lambda ,vars ,@form) (length vars)))

(define-coalton-editor-macro coalton:throw (exception)
    "Throw an exception.")

(define-coalton-editor-macro coalton:catch (expr &body handler-patterns)
    "Exception handling expression.")

(define-coalton-editor-macro coalton:dynamic-bind (bindings &body body)
    "Dynamically rebind Coalton dynamic variables.")

(define-coalton-editor-macro coalton:resumable (expr &body handler-patterns)
    "Resumption handling expression.")

(define-coalton-editor-macro coalton:resume-to (resumption)
    "Transfer control to a resumption.")

(define-coalton-editor-macro coalton:let (bindings &body form)
    "A lexical LET binding.")

(define-coalton-editor-macro coalton:let* (bindings &body form)
    "A lexical LET* binding with sequential initializers.")

(define-coalton-editor-macro coalton:rec (name bindings &body body)
  "A lexical recursive function call.")

(define-coalton-editor-macro coalton:lisp (type vars &body lisp-expr)
  "An escape from Coalton into the Lisp world.")

(define-coalton-editor-macro coalton++:unsafe (&body body)
  "Lexically disable generated Lisp type checks within BODY.")

(define-coalton-editor-macro coalton:match (expr &body patterns)
  "Pattern matching construct.")

(define-coalton-editor-macro coalton:progn (&body body)
  "A sequence of expressions.")

(define-coalton-editor-macro coalton:the (type expr)
  "Declare that EXPR is of type TYPE.")

(define-coalton-editor-macro coalton:or (&rest args)
  "Short-circuit logical disjunction. Evaluates arguments left-to-right, returning the first truthy value or False if all are False.")

(define-coalton-editor-macro coalton:and (&rest args)
  "Short-circuit logical conjunction. Evaluates arguments left-to-right, returning False on the first False value, or the last value if all are truthy.")

(define-coalton-editor-macro coalton:if (expr then else)
  "Conditional expression. Evaluates EXPR; if True, evaluates and returns THEN, otherwise evaluates and returns ELSE. Both branches must have the same type.")

(define-coalton-editor-macro coalton:when (expr &body body)
  "Evaluate BODY for side effects when EXPR is True. Returns Unit.")

(define-coalton-editor-macro coalton:unless (expr &body body)
  "Evaluate BODY for side effects when EXPR is False. Returns Unit.")

(define-coalton-editor-macro coalton:cond (&rest clauses)
  "Multi-way conditional. Each clause is (TEST BODY). Clauses are evaluated in order; the body of the first clause whose test is True is returned. The last clause's test is typically True as a default.")

(define-coalton-editor-macro coalton:do (&body body)
  "Monadic do notation. Binds the results of monadic computations using left-arrow syntax (x <- expr) and sequences them. The final expression determines the result type. Operates within a monadic context inferred from the bindings.")

(define-coalton-editor-macro coalton:values (&rest exprs)
  "Return multiple values from EXPRS.")

(define-coalton-editor-macro coalton:return (&optional value)
  "Monadic return. Wraps VALUE in the current monadic context. Equivalent to calling `pure`.")

(define-coalton-editor-macro coalton:for (&rest header-and-body)
  "Imperative iteration. Syntax: (for [label] (<binding-clause>*) [:returns expr] [(:while | :until | :repeat) expr] body...). Binding clauses are (declare var type) or (var init step). Use the variable itself as the step when it should remain constant. The binding list is required, but it may be empty, and any `:returns` clause must appear immediately after it.")

(define-coalton-editor-macro coalton:for* (&rest header-and-body)
  "Imperative iteration with sequential init and step bindings. Syntax matches `for`, but bindings follow `let*`/`do*`-style visibility.")

(define-coalton-editor-macro coalton:break (&optional label)
  "Terminate the enclosing `for` immediately. If LABEL is provided, terminates the `for` with that label, allowing break from nested loops. In the imperative `for` form, `break` skips the current iteration's step phase.")

(define-coalton-editor-macro coalton:continue (&optional label)
  "Skip the rest of the current `for` iteration and start the next one. If LABEL is provided, continues the `for` with that label. In the imperative `for` form, `continue` still performs the step phase before the next iteration.")

(define-coalton-editor-macro coalton:type-of (expr)
  "Return the inferred type scheme of `expr`, which can be displayed with `show`.")
