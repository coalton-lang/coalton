(in-package #:coalton-impl)

(defun process-toplevel-class-definitions (defclass-forms env)
  (declare (type list defclass-forms)
	   (type environment env)
	   (values ty-class-list environment &optional))
  (parse-class-definitions defclass-forms env))
