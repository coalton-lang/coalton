(in-package #:coalton)

(cl:defun %set-coalton-macro-properties (name lambda-list context)
  (cl:unless (cl:member context '(:expression :toplevel) :test #'cl:eq)
    (cl:error "Invalid Coalton macro context ~S." context))
  (cl:setf (cl:get name ':coalton-macro) cl:t
           (cl:get name ':coalton-macro-lambda-list) lambda-list
           (cl:get name ':coalton-repl-context) context))

(cl:defmacro define-expression-macro (name lambda-list cl:&body body)
  "Define a macro that expands in Coalton expression context.

This has the syntax and macro expansion semantics of CL:DEFMACRO, but it tags
NAME as a Coalton expression macro for documentation and editor integration."
  `(cl:progn
     (%set-coalton-macro-properties ',name ',lambda-list ':expression)
     (cl:defmacro ,name ,lambda-list ,@body)))

(cl:defmacro define-toplevel-macro (name lambda-list cl:&body body)
  "Define a macro that expands in Coalton toplevel context.

Use this for macros that are valid inside COALTON-TOPLEVEL, COALTON-CODEGEN,
PPRINT-COALTON-CODEGEN, and related wrapper forms."
  `(cl:progn
     (%set-coalton-macro-properties ',name ',lambda-list ':toplevel)
     (cl:defmacro ,name ,lambda-list ,@body)))

(cl:defmacro defmacro (name lambda-list cl:&body body)
  "Compatibility alias for DEFINE-EXPRESSION-MACRO.

Use DEFINE-EXPRESSION-MACRO for new Coalton expression macros. Use
CL:DEFMACRO for ordinary Lisp macros, including Lisp macros that expand to a
COALTON-TOPLEVEL form."
  `(define-expression-macro ,name ,lambda-list ,@body))
