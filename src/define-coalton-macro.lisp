(in-package #:coalton)

(cl:defmacro defmacro (name lambda-list cl:&body body)
  "Define a Coalton macro. This has identical syntax and semantics to
CL:DEFMACRO.

This allows Coalton macros to be \"tagged\" for the purpose of
generating documentation."
  `(cl:progn
     (cl:setf (cl:get ',name ':coalton-macro) cl:t
              (cl:get ',name ':coalton-macro-lambda-list) ',lambda-list)
     (cl:defmacro ,name ,lambda-list ,@body)))
