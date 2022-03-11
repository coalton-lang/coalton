;;;; faux-macros.lisp
;;;;
;;;; Some macro defintions that are only valid inside of a COALTON (&
;;;; co.) macro, that aren't actually valid as a toplevel form in
;;;; Lisp.

(in-package #:coalton-impl)

(defun error-coalton-only (name)
  (error "The operator ~S is only valid in a Coalton expression." name))

(defmacro define-coalton-editor-macro (name lambda-list docstring)
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


;;; Other Constructions

(defmacro coalton:fn (vars &body form)
  "A lambda abstraction callable within coalton."
  (construct-function-entry `(lambda ,vars ,@form) (length vars)))

(define-coalton-editor-macro coalton:match (expr &body patterns)
  "Pattern matching construct.")

(define-coalton-editor-macro coalton:let (bindings &body form)
  "A lexical LET binding.")

(define-coalton-editor-macro coalton:lisp (type vars &body lisp-expr)
  "An escape from Coalton into the Lisp world.")

(define-coalton-editor-macro coalton:do (&body form)
  "Do notation")
