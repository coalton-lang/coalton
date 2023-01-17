(defpackage #:coalton-impl/typechecker2/entry
  (:use
   #:cl)
  (:import-from
   #:coalton-impl/typechecker2/define-type
   #:toplevel-define-type)
  (:import-from
   #:coalton-impl/typechecker2/define
   #:toplevel-define)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)))

(in-package #:coalton-impl/typechecker2/entry)

(defun entry-point (filename)
  (declare (type string filename)
           (values))

  (let* ((program (parser:parse-file filename))

         (*package* (parser:program-package program))

         (program (parser:rename-variables program))

         (file (parser:program-file program))

         (env coalton-impl::*global-environment*))

    (multiple-value-bind (type-definitions env)
        (toplevel-define-type (parser:program-types program) file env)
      (declare (ignore type-definitions))

      (setf env (toplevel-define (parser:program-defines program)
                                 (parser:program-declares program)
                                 file
                                 env))

      (setf coalton-impl::*global-environment* env)

      (values))))
