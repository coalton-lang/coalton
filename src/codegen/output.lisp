(defpackage #:coalton-impl/codegen/output
  (:use
   #:cl)
  (:export
   #:emit
   #:emit-ast
   #:emit-env))

;; Protocol for collecting compilation output
;;
;; Compiling a Coalton program produces a sequence of updates to an
;; initial global environment and a sequence of Lisp definitions that
;; depend on that environment.
;;
;; Lisp source is produced by rewriting environment updates so that
;; they alter the global environment (during compilation, the modified
;; environment is treated functionally: the global environment is not
;; modified, and an updated environment is returned by the
;; compiler). These rewritten forms are followed by compiled variable
;; and function definitions, also as Lisp source.
;;
;; 'emit' and related generics allow tailoring of compiler behavior to
;; support different output cases, such as compilation of toplevel
;; macros, whole files, and generation of code and ast diagnostic
;; artifacts.

(in-package #:coalton-impl/codegen/output)

(defgeneric emit (collector form)
  (:documentation
   "Emit a toplevel Lisp source form.")
  (:method (collector form)
    (declare (ignore collector form))))

(defgeneric emit-ast (collector name type value)
  (:documentation
   "Emit an AST entry.")
  (:method (collector name type value)
    (declare (ignore collector name type value))))

(defgeneric emit-env (collector name arglist)
  (:documentation
   "Emit an environment update. NAME is that of a function in coalton-impl/typechecker/environment that accepts the environment to update as its first argument; ARGLIST are the remaining arguments.")
  (:method (collector name arglist)
    (declare (ignore collector name arglist))))
