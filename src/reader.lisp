(defpackage #:coalton-impl/reader
  (:use
   #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:error #:coalton-impl/error)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton-impl/reader)

(defvar *source-plist* nil)

(defun make-source-file ()
  (destructuring-bind (&key emacs-filename emacs-position &allow-other-keys) *source-plist*
    (if emacs-filename
        (source-error:make-displaced-source-file *compile-file-truename*
                                                 emacs-filename
                                                 emacs-position)
        (source-error:make-source-file *compile-file-truename*))))

(defun std-macro-char (c)
  (get-macro-character c (named-readtables:ensure-readtable :standard)))

(defvar *coalton-reader-allowed* t
  "Is the Coalton reader allowed to parse the current input?
Used to forbid reading while inside quasiquoted forms.")

(defun %coalton-toplevel (stream)
  (multiple-value-bind (program env)
      (entry:entry-point (parser:read-program stream :mode :toplevel-macro))
    (setf entry:*global-environment* env)
    program))

(defun %coalton-codegen (stream)
  (let ((settings:*coalton-skip-update* t)
        (settings:*emit-type-annotations* nil))
    (multiple-value-bind (program env)
        (entry:entry-point (parser:read-program stream :mode :toplevel-macro))
      (declare (ignore env))
      `',program)))

(defun %coalton-codegen-ast (stream)
  (let ((settings:*coalton-skip-update* t)
        (settings:*emit-type-annotations* nil)
        (settings:*coalton-dump-ast* t))
    (multiple-value-bind (program env)
        (entry:entry-point (parser:read-program stream :mode :toplevel-macro))
      (declare (ignore program env))
      nil)))

(defun %coalton-expression (stream)
  (entry:expression-entry-point (parser:read-expression stream)))

(defun read-open-paren (stream char)
  (unless *coalton-reader-allowed*
    (return-from read-open-paren
      (funcall (std-macro-char #\() stream char)))
  (parser:with-reader-context stream
    (let ((first-form
            (multiple-value-bind (form presentp)
                (parser:maybe-read-form stream)
              (unless presentp
                (return-from read-open-paren nil))
              form))
          (source-error:*source* (make-source-file)))
      (case (cst:raw first-form)
        (coalton:coalton-toplevel
          (%coalton-toplevel stream))
        (coalton:coalton-codegen
          (%coalton-codegen stream))
        (coalton:coalton-codegen-ast
          (%coalton-codegen-ast stream))
        (coalton:coalton
          (%coalton-expression stream))
        ;; Fall back to reading the list manually
        (t
         (%coalton-forms stream first-form))))))

(defun %coalton-forms (stream first-form)
  (let ((collected-forms (list (cst:raw first-form)))
        (dotted-context nil))
    (loop :do
      (handler-case
          (multiple-value-bind (form presentp)
              (parser:maybe-read-form stream)
            (cond ((and (not presentp)
                        dotted-context)
                   (error "Invalid dotted list"))

                  ((not presentp)
                   (return-from %coalton-forms
                     (nreverse collected-forms)))

                  (dotted-context
                   (when (nth-value 1 (parser:maybe-read-form stream))
                     (error "Invalid dotted list"))
                   (return-from %coalton-forms
                     (nreconc collected-forms (cst:raw form))))

                  (t
                   (push (cst:raw form) collected-forms))))
        (eclector.reader:invalid-context-for-consing-dot (c)
          (when dotted-context
            (error "Invalid dotted list"))
          (setf dotted-context t)
          (eclector.reader:recover c))))))

(defun read-backtick (s c)
  (let ((*coalton-reader-allowed* nil))
    (funcall (std-macro-char #\`) s c)))

(defun read-comma (s c)
  (let ((*coalton-reader-allowed* t))
    (funcall (std-macro-char #\,) s c)))

(named-readtables:defreadtable coalton:coalton
  (:merge :standard)
  (:macro-char #\( 'read-open-paren)
  (:macro-char #\` 'read-backtick)
  (:macro-char #\, 'read-comma))

(defun process-forms (forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*print-circle* t)
        (source-string (cl:format cl:nil "~S" forms)))
    (source-error:with-source-string (stream source-string)
      (cl:read stream))))

(defmacro coalton:coalton-toplevel (&body forms)
  (process-forms (cons 'coalton:coalton-toplevel forms)))

(defmacro coalton:coalton-codegen (&body forms)
  (process-forms (cons 'coalton:coalton-codegen forms)))

(defmacro coalton:coalton-codegen-ast (&body forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*print-circle* t))
    (source-error:with-source-string (stream (cl:format cl:nil "~S" (cons 'coalton:coalton-codegen-ast forms)))
      (cl:read stream))))

(defmacro coalton:coalton (&rest forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*print-circle* t))
    (source-error:with-source-string (stream (cl:format cl:nil "~S" (cons 'coalton:coalton forms)))
      (cl:read stream))))
