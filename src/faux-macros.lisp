;;;; faux-macros.lisp
;;;;
;;;; Some macro defintions that are only valid inside of a COALTON (&
;;;; co.) macro, that aren't actually valid as a toplevel form in
;;;; Lisp.

(defpackage #:coalton-impl/faux-macros
  (:use
   #:cl)
  (:local-nicknames
   (#:rt #:coalton-impl/runtime)))

(in-package #:coalton-impl/faux-macros)

(defun error-coalton-only (name)
  (error "The operator ~S is only valid in a Coalton expression." name))

(defmacro define-coalton-editor-macro (name lambda-list &optional (docstring ""))
  "Define a macro so that Emacs and SLIME see it nicely, and so the forms indent properly. Not intended for actual use in Lisp code."
  (check-type docstring string)
  `(defmacro ,name ,lambda-list
     ,docstring
     (declare (ignore ,@(remove-if (lambda (sym) (char= #\& (char (symbol-name sym) 0)))
                                   lambda-list)))
     (error-coalton-only ',name)))


;;; Top-Level Forms

(define-coalton-editor-macro coalton:define (var-or-fun &body body)
  "Define a variable or function. (Coalton top-level operator.)")

(define-coalton-editor-macro coalton:define-type (name &body definition)
  "Create a new algebraic data type named NAME. (Coalton top-level operator.)")

(define-coalton-editor-macro coalton:declare (var type)
  "Declare the type of a variable. (Coalton top-level operator.)")

(define-coalton-editor-macro coalton:define-class (class &body method-signatures)
  "Define a new type class. (Coalton top-level operator.")

(define-coalton-editor-macro coalton:define-instance (instance &body method-definitions)
  "Define an instance of a type class. (Coalton top-level operator.)")

(define-coalton-editor-macro coalton:specialize (name from-ty to-ty)
  "Declare a specialization for a function. (Coalton top-level operator.)")

;;; Attributes

(define-coalton-editor-macro coalton:repr (type &optional arg)
  "Annote a type definition with a runtime representation.")

(define-coalton-editor-macro coalton:monomorphize ()
  "Mark a definition for monomorphization.")


;;; Other Constructions

(defmacro coalton:fn (vars &body form)
  "A lambda abstraction callable within coalton."
  (rt:construct-function-entry `(lambda ,vars ,@form) (length vars)))

(define-coalton-editor-macro coalton:let (bindings &body form)
  "A lexical LET binding.")

(define-coalton-editor-macro coalton:lisp (type vars &body lisp-expr)
  "An escape from Coalton into the Lisp world.")

(define-coalton-editor-macro coalton:match (expr &body patterns)
  "Pattern matching construct.")

(define-coalton-editor-macro coalton:progn (&body body)
  "A sequence of expressions.")

(define-coalton-editor-macro coalton:the (type expr)
  "Declare that EXPR is of type TYPE.")

(define-coalton-editor-macro coalton:or (&rest args))

(define-coalton-editor-macro coalton:and (&rest args))

(define-coalton-editor-macro coalton:if (expr then else))

(define-coalton-editor-macro coalton:when (expr &body body))

(define-coalton-editor-macro coalton:unless (expr &body body))

(define-coalton-editor-macro coalton:cond (&rest clauses))

(define-coalton-editor-macro coalton:do (&body body))

(define-coalton-editor-macro coalton:return (&optional value))
