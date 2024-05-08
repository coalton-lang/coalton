(defpackage #:coalton-impl/compiler
  (:use
   #:cl)
  (:shadow
   #:compile
   #:compile-file)
  (:local-nicknames
   (#:codegen #:coalton-impl/codegen)
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:parser #:coalton-impl/parser)
   (#:entry #:coalton-impl/entry))
  (:export
   #:codegen-ast-result
   #:codegen-result
   #:compile
   #:compile-toplevel
   #:generate-ast
   #:generate-code
   #:toplevel-result))

;;; Compiler backends and utility entry points
;;;
;;; The classes defined below implement the code generator's backend
;;; protocol (codegen:emit, codegen:emit-env, etc.) in order to
;;; provide output modes supporting:
;;;
;;; - The coalton-toplevel macro
;;; - Direct compilation of .coalton files
;;; - Raw code generation
;;; - AST printing

(in-package #:coalton-impl/compiler)

;;; class: compile-toplevel

;; Emit the environment and code created by the coalton-toplevel
;; macro.
;;
;; The Coalton environment is serialized by quoting arguments that
;; implement make-load-form: see implementations of make-load-form for
;; the structures defined in typechecker/environment, codegen/types
;; and elsewhere
;;
;; When files are loaded after compilation, the environment
;; definitions are immediately replayed -- harmless, but doesn't need
;; to happen and might justify making load behavior configurable with
;; a dynamic variable.

(defun %adjustable-array ()
  "Return a zero length extensible array."
  (make-array 0 :adjustable t :fill-pointer 0))

(defclass compile-toplevel ()
  ((env :initform (%adjustable-array))
   (code :initform (%adjustable-array)))
  (:documentation
   "Compiler backend that generates the value of the coalton-toplevel macro."))

;; Emit a compiled lisp form

(defmethod codegen:emit ((backend compile-toplevel) form)
  (vector-push-extend form (slot-value backend 'code)))

;; Emit an environment entry. Repeated code blocks emitted before
;; and during optimization are deduplicated at output time.

(defmethod codegen:emit-env ((backend compile-toplevel) name args)
  (vector-push-extend (cons name args) (slot-value backend 'env)))

(defun code-update-eql (a b)
  "Compare environment updates, returning t for set-code updates of the same symbol."
  (and (eql (first a) 'coalton-impl/typechecker/environment:set-code)
       (eql (first b) 'coalton-impl/typechecker/environment:set-code)
       (eql (second a)
            (second b))))

(defun toplevel-result (backend)
  "Return the result of evaluating a coalton-toplevel form."
  (with-slots (env code) backend
    (let ((updates (remove-duplicates (coerce env 'list) :test #'code-update-eql)))
      `(progn
         (let ((env entry:*global-environment*))
           ,@(loop :for (name . args) :in updates
                   :collect `(setf env (,name env ,@(mapcar #'util:runtime-quote args))))
           (setf entry:*global-environment* env))
         ,@(coerce code 'list)))))

;;; class: compile-file

;; Generate lisp source from coalton source. This is similar to
;; compile-toplevel, but serializes environment updates in a completely
;; different way, by emitting source forms, rather than load forms

(defclass compile-file ()
  ((stream :initarg :stream)))

(defmethod codegen:emit ((backend compile-file) form)
  (with-slots (stream) backend
    (let ((*package* (find-package :cl))
          (*print-case* :downcase)
          (*print-circle* nil))
      (prin1 form stream)
      (terpri stream)
      (terpri stream))))

(defmethod codegen:emit-comment ((backend compile-file) string)
  (with-slots (stream) backend
    (format stream "~%;; ~A~%~%" string)))

(defun set-value-type! (name value)
  (setf entry:*global-environment*
        (coalton-impl/typechecker:set-value-type entry:*global-environment*
                                                 name value)))

(defmethod codegen:emit-env ((backend compile-file) name args)
  (case name
    (coalton-impl/typechecker::set-value-type
     (destructuring-bind (name type) args
       (codegen:emit-comment backend (format nil "env: set-value-type ~A" name))
       (break)
       (codegen:emit backend `(set-value-type! ',name ',type))))
    (t
     ;; fixme -- source-persistable representation of environment modification
     (codegen:emit-comment backend (format nil "env: ~A" name))
     (codegen:emit backend
                   `(setf entry:*global-environment*
                          (,name entry:*global-environment*
                                 ,@(mapcar #'util:runtime-quote args)))))))

;;; class: generate-code

;; Emit generated code, ignoring environment updates, comments, etc.

(defclass generate-code ()
  ((forms :initform (%adjustable-array))))

(defmethod codegen:emit ((backend generate-code) form)
  (vector-push-extend form (slot-value backend 'forms)))

(defun codegen-result (backend)
  (util:runtime-quote (coerce (slot-value backend 'forms) 'list)))

;;; class: generate-ast

;; Emit AST, by emitting printed versions to a stream. Ignores all
;; other compiler activity.

(defclass generate-ast ()
  ((stream :initarg :stream)))

(defmethod codegen:emit-ast ((backend generate-ast) name type value)
  (with-slots (stream) backend
    (format stream "~A :: ~A~%~A~%~%~%" name type value)))

;;; entry points and helpers

(defun %process-source (stream name backend)
  "Helper: read and compile Coalton source from STREAM using BACKEND.

NAME provides information on the stream's origin in error messages."
  (let* ((file (error:make-coalton-file :stream stream
                                        :name name))
         (program (parser:read-file stream file)))
    (entry:emit-prologue backend)
    (entry:entry-point program backend)))

(defun compile (input-stream output-stream)
  "Read Coalton source from INPUT-STREAM and write Lisp source to OUTPUT-STREAM."
  (parser:with-reader-context input-stream
    (let ((backend (make-instance 'compile-file :stream output-stream)))
      ;; TODO emit header comment, emit defpackage
      ;; ... Generated from XXX.coalton on YYYYMMDD
      ;; ... defpackage ...
      (setf entry:*global-environment* (%process-source input-stream "string" backend))
      (values))))

(defun compile-file (input-file output-file)
  (with-open-file (input-stream input-file
                          :direction :input
                          :element-type 'character)
    (with-open-file (output-stream output-file
                                   :direction :output
                                   :element-type 'character
                                   :if-exists :supersede)
      (compile input-stream output-stream))))

(defun generate-ast (stream)
  "Read Coalton source from STREAM and return string representation of ast."
  (parser:with-reader-context stream
    (with-output-to-string (ast-stream)
      (let ((backend (make-instance 'generate-ast :stream ast-stream)))
        (%process-source stream "string" backend)))))
