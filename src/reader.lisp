(defpackage #:coalton-impl/reader
  (:use
   #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:codegen #:coalton-impl/codegen)
   (#:pmacro #:coalton-impl/parser/macro)
   (#:preader #:coalton-impl/parser/reader)
   (#:settings #:coalton-impl/settings)
   (#:source #:coalton-impl/source)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton-impl/reader)

(defvar *coalton-reader-allowed* t
  "Is the Coalton reader allowed to parse the current input?
Used to forbid reading while inside quasiquoted forms.")

(defvar *source* nil
  "The source from which program text is being read.
This symbol may be bound to a string source in the case of direct evaluation in a repl.")

;; Property-list key used on deferred form ids to cache the expansion of a
;; single readtable-driven Coalton form across repeated macroexpansion.
(defconstant +deferred-coalton-expansion+ '%deferred-coalton-expansion
  "Property key used to memoize deferred Coalton macroexpansions.")

;; Modes handled by deferred Coalton readtable expansion:
;; - `coalton-toplevel` compiles toplevel definitions
;; - `coalton-codegen` returns generated Lisp without type annotations
;; - `coalton-codegen-types` returns generated Lisp with type annotations
;; - `coalton-codegen-ast` prints AST output and expands to NIL
;; - `coalton` compiles an expression form
(deftype deferred-coalton-mode ()
  '(member coalton:coalton-toplevel
           coalton:coalton-codegen
           coalton:coalton-codegen-types
           coalton:coalton-codegen-ast
           coalton:coalton))

(defun probe-symbol (package-name symbol-name)
  "Look up SYMBOL-NAME in PACKAGE-NAME, returning its value if the package exists and the symbol is bound."
  (let ((package (find-package package-name)))
    (when package
      (let ((symbol (find-symbol symbol-name package)))
        (when (boundp symbol)
          (symbol-value symbol))))))

(defun source-filename ()
  "Return the name of the file from which program text is being compiled or loaded."
  (or *compile-file-truename*
      *load-truename*))

(defun buffer-name ()
  "Return the name of the Emacs buffer containing evaluated or compiled program text."
  (or #+:sbcl (getf (probe-symbol "SB-C" "*SOURCE-PLIST*") ':emacs-buffer)
      (source-filename)
      "repl"))

(defun read-lisp (stream source first-form)
  "Helper for MAYBE-READ-COALTON when the first form wasn't Coalton: revert to reading plain Lisp."
  (let ((collected-forms (list (cst:raw first-form)))
        (dotted-context nil))
    (loop :do
      (handler-case
          (multiple-value-bind (form presentp)
              (parser:maybe-read-form stream source)

            (cond
              ((and (not presentp)
                    dotted-context)
               (error "Invalid dotted list"))

              ((not presentp)
               (return-from read-lisp (nreverse collected-forms)))

              (dotted-context
               (when (nth-value 1 (parser:maybe-read-form stream source))
                 (error "Invalid dotted list"))

               (return-from read-lisp (nreconc collected-forms (cst:raw form))))

              (t
               (push (cst:raw form) collected-forms))))
        (eclector.reader:invalid-context-for-consing-dot (c)
          (when dotted-context
            (error "Invalid dotted list"))
          (setf dotted-context t)
          (eclector.reader:recover c))))))

(defun maybe-read-coalton (stream source)
  "If the first form on STREAM indicates that Coalton code is present, read a program, and perform the indicated operation (compile, codegen, etc.).
SOURCE provides metadata for the stream argument, for error messages."
  (parser:with-reader-context stream
    (let ((first-form
            (multiple-value-bind (form presentp)
                (parser:maybe-read-form stream source)
              (unless presentp
                (return-from maybe-read-coalton nil))
              form)))

      (case (cst:raw first-form)
        (coalton:coalton-toplevel
          (entry:compile-coalton-toplevel (parser:read-program stream source ':macro)))

        (coalton:coalton-codegen
          (let ((settings:*emit-type-annotations* nil))
            `',(entry:entry-point (parser:read-program stream source ':macro))))

        (coalton:coalton-codegen-types
          (let ((settings:*emit-type-annotations* t))
            `',(entry:entry-point (parser:read-program stream source ':macro))))

        (coalton:coalton-codegen-ast
          (let* ((settings:*emit-type-annotations* nil)
                 (ast nil)
                 (codegen:*codegen-hook* (lambda (op &rest args)
                                           (when (eql op ':AST)
                                             (push args ast)))))
            (entry:entry-point (parser:read-program stream source ':macro))
            (loop :for (name type value) :in (nreverse ast)
                  :do (format t "~A :: ~A~%~A~%~%~%" name type value)))
          nil)

        (coalton:coalton
         (entry:expression-entry-point (parser:read-expressions stream source)))

        ;; Fall back to reading the list manually
        (t
         (read-lisp stream source first-form))))))

(defun utf-8-char-width (char)
  "Return the number of UTF-8 octets required to encode CHAR."
  (let ((code (char-code char)))
    (cond
      ((<= code #x7F) 1)
      ((<= code #x7FF) 2)
      ((<= code #xFFFF) 3)
      (t 4))))

(defun file-byte-offset-to-char-offset (file byte-offset)
  "Convert BYTE-OFFSET in FILE to the corresponding character offset."
  (with-open-file (stream file
                          :direction ':input
                          :element-type 'character
                          :external-format ':utf-8)
    (loop :with bytes := 0
          :with chars := 0
          :while (< bytes byte-offset)
          :for char := (read-char stream nil nil)
          :while char
          :do (incf bytes (utf-8-char-width char))
              (incf chars)
          :finally (return chars))))

(defun source-span-matches-mode-p (source mode span)
  "Return true when SPAN in SOURCE starts with MODE."
  (declare (type deferred-coalton-mode mode))
  (with-open-stream (stream (source:source-stream source))
    (file-position stream (1+ (source:span-start span)))
    (parser:with-reader-context stream
      (multiple-value-bind (form presentp eofp)
          (parser:maybe-read-form stream source)
        (declare (ignore eofp))
        (and presentp
             (eql (cst:raw form) mode))))))

(defun normalized-source-span (source mode start end)
  "Return a source span whose offsets line up with SOURCE."
  (declare (type deferred-coalton-mode mode))
  (let ((span (cons start end)))
    (if (not (typep source 'source::source-file))
        span
        (if (source-span-matches-mode-p source mode span)
            span
            (let* ((file (source::input-name source))
                   (normalized-span
                     (cons (file-byte-offset-to-char-offset file start)
                           (file-byte-offset-to-char-offset file end))))
              (if (source-span-matches-mode-p source mode normalized-span)
                  normalized-span
                  (util:coalton-bug "Unable to recover source span for ~S in ~A"
                                    mode
                                    (source:source-name source))))))))

(defun make-deferred-coalton-form (mode source span)
  "Return a macro form that will compile the Coalton form at SPAN in SOURCE once."
  (declare (type deferred-coalton-mode mode))
  (list 'coalton-impl/reader::expand-source-coalton-form
        (gensym "COALTON-FORM-")
        mode
        source
        span))

(defun maybe-read-coalton-deferred (stream source start)
  "Read a Coalton toplevel form from STREAM and defer compilation to macroexpansion.
SOURCE provides metadata for the stream argument, and START is the offset of
the opening parenthesis that began the current form."
  (parser:with-reader-context stream
    (let ((first-form
            (multiple-value-bind (form presentp)
                (parser:maybe-read-form stream source)
              (unless presentp
                (return-from maybe-read-coalton-deferred nil))
              form)))
      (let ((mode (cst:raw first-form)))
        (case mode
          ((coalton:coalton-toplevel
            coalton:coalton-codegen
            coalton:coalton-codegen-types
            coalton:coalton-codegen-ast
            coalton:coalton)
           ;; Consume the original source form now, but compile it later from
           ;; the exact source span so repeated compiler passes do not
           ;; monomorphize or inline the same form more than once.
           (read-lisp stream source first-form)
           (make-deferred-coalton-form mode
                                       source
                                       (normalized-source-span source
                                                               mode
                                                               start
                                                               (file-position stream))))
          (t
           (read-lisp stream source first-form)))))))

(defun expand-source-coalton-form-1 (mode source span)
  "Compile the Coalton form identified by MODE from SOURCE at SPAN.

See the documentation of the type `deferred-coalton-mode` for the meaning of
each MODE."
  (declare (type deferred-coalton-mode mode))
  (with-open-stream (stream (source:source-stream source))
    (file-position stream (1+ (source:span-start span)))
    (parser:with-reader-context stream
      (multiple-value-bind (form presentp eofp)
          (parser:maybe-read-form stream source)
        (declare (ignore eofp))
        (unless presentp
          (util:coalton-bug "Missing Coalton form at ~S"
                            (source:make-location source span)))
        (unless (eql (cst:raw form) mode)
          (util:coalton-bug "Expected ~S at ~S, found ~S"
                            mode
                            (source:make-location source span)
                            (cst:raw form)))
        (ecase mode
          (coalton:coalton-toplevel
           (entry:compile-coalton-toplevel
            (parser:read-program stream source ':macro)))
          (coalton:coalton-codegen
           (let ((settings:*emit-type-annotations* nil))
             `',(entry:entry-point
                 (parser:read-program stream source ':macro))))
          (coalton:coalton-codegen-types
           (let ((settings:*emit-type-annotations* t))
             `',(entry:entry-point
                 (parser:read-program stream source ':macro))))
          (coalton:coalton-codegen-ast
           (let* ((settings:*emit-type-annotations* nil)
                  (ast nil)
                  (codegen:*codegen-hook* (lambda (op &rest args)
                                            (when (eql op ':AST)
                                              (push args ast)))))
             (entry:entry-point (parser:read-program stream source ':macro))
             (loop :for (name type value) :in (nreverse ast)
                   :do (format t "~A :: ~A~%~A~%~%~%" name type value)))
           nil)
          (coalton:coalton
           (entry:expression-entry-point
            (parser:read-expressions stream source))))))))

(defmacro expand-source-coalton-form (form-id mode source span)
  "Expand the Coalton form from SOURCE at SPAN, memoized by FORM-ID."
  (or (get form-id +deferred-coalton-expansion+)
      (setf (get form-id +deferred-coalton-expansion+)
            (expand-source-coalton-form-1 mode source span))))

(defun read-coalton-toplevel-open-paren (stream char)
  "This is the dispatch function for open paren in the Coalton readtable.
It ensures the presence of source metadata for STREAM and then calls MAYBE-READ-COALTON."
  (unless *coalton-reader-allowed*
    (return-from read-coalton-toplevel-open-paren
      (funcall (get-macro-character #\( (named-readtables:ensure-readtable :standard)) stream char)))

  (let ((start (1- (file-position stream))))
    (cond
      (*source*
       ;; source metadata exists, probably courtesy of compile-forms: do
       ;; nothing
       (maybe-read-coalton stream *source*))
      ((source-filename)
       ;; no metadata, and a compile or load operation is occurring:
       ;; bind a source-file
       (let ((*source* (coalton-impl/source:make-source-file
                        (source-filename)
                        :name (buffer-name))))
         (maybe-read-coalton-deferred stream *source* start)))
      (t
       ;; no metadata, no file operation, therefore we are in a repl:
       ;; bind a source-string containing cloned input
       (let ((*source* (coalton-impl/source:make-source-string
                        (with-output-to-string (out)
                          (write-char #\( out)
                          (alexandria:copy-stream stream out))
                        :name "repl")))
         (with-open-stream (stream (source:source-stream *source*))
           (read-char stream)
           (maybe-read-coalton-deferred stream *source* 0)))))))

(defun read-cl-bracket-form (stream char)
  "Reader macro for `[...]` builder syntax on the CL readtable.
Reads a `]`-delimited list and desugars it into builder form, mirroring the
Eclector bracket reader in parser/reader.lisp."
  (declare (ignore char))
  (preader:desugar-bracket-builder
   (read-delimited-list #\] stream t)))

(defun read-cl-close-bracket (stream char)
  "Signal an error for an unmatched `]` outside bracket syntax."
  (declare (ignore stream char))
  (error "Unmatched close bracket `]`"))

(named-readtables:defreadtable coalton:coalton
  (:merge :standard)
  (:macro-char #\( 'read-coalton-toplevel-open-paren)
  (:macro-char #\[ 'read-cl-bracket-form)
  (:macro-char #\] 'read-cl-close-bracket)
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

(defun macro-body-start (form)
  "Return the source offset of the first body form inside CST FORM."
  (if (cst:consp (cst:rest form))
      (let ((span (cst:source (cst:second form))))
        (and span
             (source:span-start span)))
      (let ((span (cst:source (cst:first form))))
        (and span
             (source:span-end span)))))

(defun compile-cst-forms (mode form source)
  "Compile Coalton MODE by re-reading the original parser CST FORM from SOURCE."
  (declare (type deferred-coalton-mode mode)
           (type cst:cst form))
  (with-open-stream (stream (source:source-stream source))
    (file-position stream (macro-body-start form))
    (parser:with-reader-context stream
      (ecase mode
        (coalton:coalton-toplevel
         (entry:compile-coalton-toplevel
          (parser:read-program stream source ':macro)))
        (coalton:coalton-codegen
         (let ((settings:*emit-type-annotations* nil))
           `',(entry:entry-point
               (parser:read-program stream source ':macro))))
        (coalton:coalton-codegen-types
         (let ((settings:*emit-type-annotations* t))
           `',(entry:entry-point
               (parser:read-program stream source ':macro))))
        (coalton:coalton-codegen-ast
         (let* ((settings:*emit-type-annotations* nil)
                (ast nil)
                (codegen:*codegen-hook* (lambda (op &rest args)
                                          (when (eql op ':AST)
                                            (push args ast)))))
           (entry:entry-point (parser:read-program stream source ':macro))
           (loop :for (name type value) :in (nreverse ast)
                 :do (format t "~A :: ~A~%~A~%~%~%" name type value)))
         nil)
        (coalton:coalton
         (entry:expression-entry-point
          (parser:read-expressions stream source)))))))

(defun compile-forms (mode forms)
  "Compile FORMS as Coalton using the indicated MODE.

When called from within a Coalton macro expansion (i.e. `*macro-expansion-form*'
and `*macro-expansion-source*' are bound), delegates to `compile-cst-forms' which
re-reads the body from the original source. This preserves precise source
locations when the original macro body still has CST spans. Otherwise falls
back to printing the forms and re-reading them, which is needed when forms are
generated programmatically outside the parser or came through a CL readtable
macro that did not preserve child source spans."
  (let ((macro-body-start
          (and pmacro:*macro-expansion-form*
               pmacro:*macro-expansion-source*
               (macro-body-start pmacro:*macro-expansion-form*))))
    (if macro-body-start
        (compile-cst-forms mode
                           pmacro:*macro-expansion-form*
                           pmacro:*macro-expansion-source*)
        (let* ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
               (string (print-form (cons mode forms)))
               (*source* (coalton-impl/source:make-source-string string
                                                                 :name "<macroexpansion>")))
          (with-input-from-string (stream string)
            (cl:read stream))))))

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

(defmacro define-pprint-codegen-macro (name codegen)
  "Generate a pretty-printing macro for codegen macros."
  `(defmacro ,name (&body forms)
     ,(format nil "Pretty print code generated by ~S." codegen)
     (let ((package (gensym "PACKAGE")))
       `(let ((,package (make-package '#:pprint-codegen-package
                                      :use '(#:cl))))
          (unwind-protect (progn
                            ;; Import all symbols in the current package
                            ;; not to print its package prefix.
                            (do-symbols (symb *package*)
                              (unless (nth-value 1
                                                 (find-symbol (symbol-name symb) ,package))
                                (import symb ,package)))
                            (let ((*package* ,package))
                              (pprint (,',codegen ,@forms))
                              (values)))
            (delete-package ,package))))))

(define-pprint-codegen-macro coalton:pprint-coalton-codegen coalton:coalton-codegen)
(define-pprint-codegen-macro coalton:pprint-coalton-codegen-types coalton:coalton-codegen-types)
(define-pprint-codegen-macro coalton:pprint-coalton-codegen-ast coalton:coalton-codegen-ast)

(defmacro coalton:coalton (&rest forms)
  (compile-forms 'coalton:coalton forms))
