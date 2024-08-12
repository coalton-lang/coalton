(defpackage #:coalton-impl/reader
  (:use
   #:cl)
  (:local-nicknames
   (#:se #:source-error)
   (#:cst #:concrete-syntax-tree)
   (#:codegen #:coalton-impl/codegen)
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton-impl/reader)

(defvar *coalton-reader-allowed* t
  "Is the Coalton reader allowed to parse the current input?
Used to forbid reading while inside quasiquoted forms.")

(defun read-coalton-toplevel-open-paren (stream char)
  (declare (optimize (debug 2)))

  (unless *coalton-reader-allowed*
    (return-from read-coalton-toplevel-open-paren
      (funcall (get-macro-character #\( (named-readtables:ensure-readtable :standard)) stream char)))

  (parser:with-reader-context stream
    (let* ((file
             (se:make-source-file *compile-file-truename*
                                  :offset (file-position stream)))
           (first-form
             (multiple-value-bind (form presentp)
                 (parser:maybe-read-form stream file)
               (unless presentp
                 (return-from read-coalton-toplevel-open-paren
                   nil))
               form)))
      (case (cst:raw first-form)
        (coalton:coalton-toplevel
          (entry:compile-coalton-toplevel (parser:read-program stream file ':macro)))

        (coalton:coalton-codegen
          (let ((settings:*emit-type-annotations* nil))
            `',(entry:entry-point (parser:read-program stream file ':macro))))

        (coalton:coalton-codegen-types
          (let ((settings:*emit-type-annotations* t))
            `',(entry:entry-point (parser:read-program stream file ':macro))))

        (coalton:coalton-codegen-ast
          (let* ((settings:*emit-type-annotations* nil)
                 (ast nil)
                 (codegen:*codegen-hook* (lambda (op &rest args)
                                           (when (eql op ':AST)
                                             (push args ast)))))
            (entry:entry-point (parser:read-program stream file ':macro))
            (loop :for (name type value) :in (nreverse ast)
                  :do (format t "~A :: ~A~%~A~%~%~%" name type value)))
          nil)

        (coalton:coalton
         (entry:expression-entry-point (parser:read-expression stream file) file))

        ;; Fall back to reading the list manually
        (t
         (let ((collected-forms (list (cst:raw first-form)))
               (dotted-context nil))
           (loop :do
             (handler-case
                 (multiple-value-bind (form presentp)
                     (parser:maybe-read-form stream file)

                   (cond
                     ((and (not presentp)
                           dotted-context)
                      (error "Invalid dotted list"))

                     ((not presentp)
                      (return-from read-coalton-toplevel-open-paren
                        (nreverse collected-forms)))

                     (dotted-context
                      (when (nth-value 1 (parser:maybe-read-form stream file))
                        (error "Invalid dotted list"))

                      (return-from read-coalton-toplevel-open-paren
                        (nreconc collected-forms (cst:raw form))))

                     (t
                      (push (cst:raw form) collected-forms))))
               (eclector.reader:invalid-context-for-consing-dot (c)
                 (when dotted-context
                   (error "Invalid dotted list"))
                 (setf dotted-context t)
                 (eclector.reader:recover c))))))))))

(named-readtables:defreadtable coalton:coalton
  (:merge :standard)
  (:macro-char #\( 'read-coalton-toplevel-open-paren)
  (:macro-char #\` (lambda (s c)
                     (let ((*coalton-reader-allowed* nil))
                       (funcall (get-macro-character #\` (named-readtables:ensure-readtable :standard)) s c))))
  (:macro-char #\, (lambda (s c)
                     (let ((*coalton-reader-allowed* t))
                       (funcall (get-macro-character #\, (named-readtables:ensure-readtable :standard)) s c)))))

(defun print-form (form)
  "Prevent truncation of a FORM that will be immediately re-read."
  (let ((*print-length* nil)
        (*print-level* nil)
        (*print-circle* t))
    (prin1-to-string form)))

(defun compile-forms (mode forms)
  "Compile FORMS as Coalton using the indicated MODE."
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*compile-file-truename*
          (pathname (format nil "COALTON-TOPLEVEL (~A)" *compile-file-truename*))))
    (with-input-from-string (stream (print-form (cons mode forms)))
      (cl:read stream))))

(defmacro coalton:coalton-toplevel (&body forms)
  "Compile Coalton FORMS."
  (compile-forms 'coalton:coalton-toplevel forms))

(defmacro coalton:coalton-codegen (&body forms)
  "Generate code for FORMS, excluding Lisp type declarations."
  (compile-forms 'coalton:coalton-codegen forms))

(defmacro coalton:coalton-codegen-types (&body forms)
  "Generate code for FORMS, including Lisp type declarations."
  (compile-forms 'coalton:coalton-codegen-types forms))

(defmacro coalton:coalton-codegen-ast (&body forms)
  "Dump the AST for toplevel definitions occurring in FORMS to *standard-out* and return NIL."
  (compile-forms 'coalton:coalton-codegen-ast forms))

(defmacro coalton:coalton (&rest forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*compile-file-truename*
          (pathname (format nil "COALTON (~A)" *compile-file-truename*))))
    (with-input-from-string (stream (print-form (cons 'coalton:coalton forms)))
      (cl:read stream))))
