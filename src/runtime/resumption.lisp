;;;; resumption.lisp
;;;;
;;;; Runtime support for invoking resumptions whose type is not known
;;;; when the `resume-to` form is compiled.

(defpackage #:coalton-impl/runtime/resumption
  (:use
   #:cl)
  (:export
   #:invoke-resumption))

(in-package #:coalton-impl/runtime/resumption)

(defgeneric invoke-resumption (resumption)
  (:documentation "Transfer control to the innermost active `resumable` handler for RESUMPTION.

The compiler defines a method for every type defined with `define-resumption`.
`resume-to` calls this function when the type of its argument is only known to
be an instance of `Resumption`."))
