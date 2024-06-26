(defpackage #:coalton-impl/reader
  (:use
   #:cl)
  (:local-nicknames
   (#:se #:source-error)
   (#:cst #:concrete-syntax-tree)
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton-impl/reader)

(defvar *coalton-reader-allowed* t
  "Is the Coalton reader allowed to parse the current input?
Used to forbid reading while inside quasiquoted forms.")



(defmacro with-coalton-file ((file stream) &body body)
  "Given a stream STREAM, bind a COALTON-IMPL/ERROR:COALTON-FILE instance to the symbol FILE and execute BODY. In the event of a Coalton compiler error or warning, render the error message (which may rely on operations on STREAM)."
  (let ((opened-streams (gensym))
        (pathname (gensym))
        (filename (gensym))
        (file-input-stream (gensym)))
    `(let ((,opened-streams nil))
       (unwind-protect
            (let* ((,pathname (or *compile-file-truename* *load-truename*))
                   (,filename (if ,pathname (namestring ,pathname) "<unknown>"))
                   ;; Ensure that coalton-file has a reference to an
                   ;; FD-STREAM, so that stream positioning works.
                   (,file-input-stream
                     (cond
                       ((or #+sbcl (sb-int:form-tracking-stream-p ,stream)
                            nil)
                        (let ((s (open (pathname ,stream))))
                          (push s ,opened-streams)
                          s))
                       (t
                        ,stream)))
                   (,file (se:make-file :stream ,file-input-stream :name ,filename)))
              (handler-bind
                  ;; Render errors and set highlights
                  ((se:source-base-error
                     (lambda (c)
                       (set-highlight-position-for-error stream (funcall (se:source-base-error-err c)))
                       (se:render-source-error c)))
                   (se:source-base-warning
                     #'se:render-source-warning))
                ,@body))
         ;; Clean up any opened file streams
         (dolist (s ,opened-streams)
           (close s))))))

(defun read-coalton-toplevel-open-paren (stream char)
  (declare (optimize (debug 2)))

  (unless *coalton-reader-allowed*
    (return-from read-coalton-toplevel-open-paren
      (funcall (get-macro-character #\( (named-readtables:ensure-readtable :standard)) stream char)))

  (parser:with-reader-context stream
    (let ((first-form
            (multiple-value-bind (form presentp)
                (parser:maybe-read-form stream)
              (unless presentp
                (return-from read-coalton-toplevel-open-paren
                  nil))
              form)))
      (case (cst:raw first-form)
        (coalton:coalton-toplevel
          (with-coalton-file (file stream)
            (entry:compile-coalton-toplevel (parser:read-program stream file :mode ':toplevel-macro))))

        (coalton:coalton-codegen
          (with-coalton-file (file stream)
            (let ((settings:*emit-type-annotations* nil))
              `',(entry:entry-point (parser:read-program stream file :mode ':toplevel-macro)))))

        (coalton:coalton-codegen-types
          (with-coalton-file (file stream)
            (let ((settings:*emit-type-annotations* t))
              `',(entry:entry-point (parser:read-program stream file :mode :toplevel-macro)))))

        (coalton:coalton-codegen-ast
          (with-coalton-file (file stream)
            (let ((settings:*emit-type-annotations* nil)
                  (settings:*coalton-dump-ast* t))
              (entry:entry-point (parser:read-program stream file :mode ':toplevel-macro))
              nil)))

        (coalton:coalton
          (with-coalton-file (file stream)
            (entry:expression-entry-point (parser:read-expression stream file) file)))

        ;; Fall back to reading the list manually
        (t
         (let ((collected-forms (list (cst:raw first-form)))
               (dotted-context nil))
           (loop :do
             (handler-case
                 (multiple-value-bind (form presentp)
                     (parser:maybe-read-form stream)

                   (cond
                     ((and (not presentp)
                           dotted-context)
                      (error "Invalid dotted list"))

                     ((not presentp)
                      (return-from read-coalton-toplevel-open-paren
                        (nreverse collected-forms)))

                     (dotted-context
                      (when (nth-value 1 (parser:maybe-read-form stream))
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

(defun set-highlight-position-for-error (stream error)
  "Set the highlight position within the editor using implementation specific magic."
  #+sbcl
  ;; We need some way of setting FILE-POSITION so that
  ;; when Slime grabs the location of the error it
  ;; highlights the correct form.
  ;;
  ;; In SBCL, we can't unread more than ~512
  ;; characters due to limitations with ANSI streams,
  ;; which breaks our old method of unreading
  ;; characters until the file position is
  ;; correct. Instead, we now patch in our own
  ;; version of FILE-POSITION.
  ;;
  ;; This is a massive hack and might start breaking
  ;; with future changes in SBCL.
  (when (typep stream 'sb-impl::form-tracking-stream)
    (let* ((file-offset
             (- (sb-impl::fd-stream-get-file-position stream)
                (file-position stream)))
           (loc (se:source-error-location error)))
      (setf (sb-impl::fd-stream-misc stream)
            (lambda (stream operation arg1)
              (if (= (sb-impl::%stream-opcode :get-file-position) operation)
                  (+ file-offset loc 1)
                  (sb-impl::tracking-stream-misc stream operation arg1)))))))

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
      (let ((stream (make-instance 'parser:position-stream :stream stream)))
        (cl:read stream)))))

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
      (let ((stream (make-instance 'parser:position-stream :stream stream)))
        (cl:read stream)))))
