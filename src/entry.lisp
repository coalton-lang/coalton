(defpackage #:coalton-impl/entry
  (:use
   #:cl)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)))

(in-package #:coalton-impl/entry)

(defparameter *global-environment* (tc:make-default-environment))

(defun entry-point (filename)
  (declare (type string filename)
           (values))

  (let* ((program (parser:parse-file filename))

         (*package* (parser:program-package program))

         (program (parser:rename-variables program))

         (file (parser:program-file program))

         (env *global-environment*))

    (multiple-value-bind (type-definitions env)
        (tc:toplevel-define-type (parser:program-types program) file env)
      (declare (ignore type-definitions))

      (multiple-value-bind (class-definitions env)
          (tc:toplevel-define-class (parser:program-classes program)
                                 file
                                 env)
        (declare (ignore class-definitions))

        (setf env (tc:toplevel-define-instance (parser:program-instances program) env file))

        (setf env (tc:toplevel-define (parser:program-defines program)
                                      (parser:program-declares program)
                                      file
                                      env))

        (setf *global-environment* env)

        (values)))))

;; TODO: remove this
;; Temporary hack to define Num so that integer literals can be
;; typechecked.
(entry-point "./bootstrap.coalton")
