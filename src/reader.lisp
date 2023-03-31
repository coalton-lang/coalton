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

(defvar *coalton-reader-allowed* t
  "Is the Coalton reader allowed to parse the current input?
Used to forbid reading while inside quasiquoted forms.")

(defun read-coalton-toplevel-open-paren (stream char)
  (declare (optimize (debug 2)))

  (unless *coalton-reader-allowed*
    (return-from read-coalton-toplevel-open-paren
      (funcall (get-macro-character #\( (named-readtables:ensure-readtable :standard)) stream char)))

  (let ((first-form
          (multiple-value-bind (form presentp)
              (util:maybe-read-form stream)
            (unless presentp
              (return-from read-coalton-toplevel-open-paren
                nil))
            form)))
    (case first-form
      (coalton:coalton-toplevel
        (let ((opened-streams nil))
          (unwind-protect
               (let* ((pathname (or *compile-file-truename* *load-truename*))
                      (filename (if pathname (namestring pathname) "<unknown>"))

                      (file-input-stream
                        (cond
                          ((or #+sbcl (sb-int:form-tracking-stream-p stream)
                               nil)
                           (let ((s (open (pathname stream))))
                             (push s opened-streams)
                             s))
                          (t
                           stream)))
                      (file (error:make-coalton-file :stream file-input-stream :name filename)))

                 (handler-bind
                     ;; Render errors and set highlights
                     ((error:coalton-base-error
                        (lambda (c)
                          (set-highlight-position-for-error stream (funcall (error:coalton-error-err c)))
                          (error:render-coalton-error c)))
                      (error:coalton-base-warning
                        (lambda (c)
                          (error:render-coalton-warning c))))
                   (multiple-value-bind (program env)
                       (entry:entry-point (parser:read-program stream file :mode :toplevel-macro))
                     (setf entry:*global-environment* env)
                     program)))
            ;; Clean up any opened file streams
            (dolist (s opened-streams)
              (close s)))))

      (coalton:coalton-codegen
       (let ((opened-streams nil))
         (unwind-protect
              (let* ((pathname (or *compile-file-truename* *load-truename*))
                     (filename (if pathname (namestring pathname) "<unknown>"))

                     (file-input-stream
                       (cond
                         ((or #+sbcl (sb-int:form-tracking-stream-p stream)
                              nil)
                          (let ((s (open (pathname stream))))
                            (push s opened-streams)
                            s))
                         (t
                          stream)))
                     (file (error:make-coalton-file :stream file-input-stream :name filename)))

                (handler-bind
                    ;; Render errors and set highlights
                    ((error:coalton-base-error
                       (lambda (c)
                         (set-highlight-position-for-error stream (funcall (error:coalton-error-err c)))
                         (error:render-coalton-error c)))
                     (error:coalton-base-warning
                       (lambda (c)
                         (error:render-coalton-warning c))))
                  (let ((settings:*coalton-skip-update* t)
                        (settings:*emit-type-annotations* nil))
                    (multiple-value-bind (program env)
                        (entry:entry-point (parser:read-program stream file :mode :toplevel-macro))
                      (declare (ignore env))
                      `',program))))
           ;; Clean up any opened file streams
           (dolist (s opened-streams)
             (close s)))))

      (coalton:coalton-codegen-ast
       (let ((opened-streams nil))
         (unwind-protect
              (let* ((pathname (or *compile-file-truename* *load-truename*))
                     (filename (if pathname (namestring pathname) "<unknown>"))

                     (file-input-stream
                       (cond
                         ((or #+sbcl (sb-int:form-tracking-stream-p stream)
                              nil)
                          (let ((s (open (pathname stream))))
                            (push s opened-streams)
                            s))
                         (t
                          stream)))
                     (file (error:make-coalton-file :stream file-input-stream :name filename)))

                (handler-bind
                    ;; Render errors and set highlights
                    ((error:coalton-base-error
                       (lambda (c)
                         (set-highlight-position-for-error stream (funcall (error:coalton-error-err c)))
                         (error:render-coalton-error c)))
                     (error:coalton-base-warning
                       (lambda (c)
                         (error:render-coalton-warning c))))
                  (let ((settings:*coalton-skip-update* t)
                        (settings:*emit-type-annotations* nil)
                        (settings:*coalton-dump-ast* t))
                    (multiple-value-bind (program env)
                        (entry:entry-point (parser:read-program stream file :mode :toplevel-macro))
                      (declare (ignore program env))
                      nil))))
           ;; Clean up any opened file streams
           (dolist (s opened-streams)
             (close s)))))

      (coalton:coalton
       (let ((opened-streams nil))
         (unwind-protect
              (let* ((pathname (or *compile-file-truename* *load-truename*))
                     (filename (if pathname (namestring pathname) "<unknown>"))

                     (file-input-stream
                       (cond
                         ((or #+sbcl (sb-int:form-tracking-stream-p stream)
                              nil)
                          (let ((s (open (pathname stream))))
                            (push s opened-streams)
                            s))
                         (t
                          stream)))
                     (file (error:make-coalton-file :stream file-input-stream :name filename)))

                (handler-bind
                    ;; Render errors and set highlights
                    ((error:coalton-base-error
                       (lambda (c)
                         (set-highlight-position-for-error stream (funcall (error:coalton-error-err c)))
                         (error:render-coalton-error c)))
                     (error:coalton-base-warning
                       (lambda (c)
                         (error:render-coalton-warning c))))
                  (entry:expression-entry-point (parser:read-expression stream file) file)))
           ;; Clean up any opened file streams
           (dolist (s opened-streams)
             (close s)))))

      ;; Fall back to reading the list manually
      (t
       (let ((collected-forms (list first-form))
             (dotted-context nil))
         (loop :do
           (handler-case
               (multiple-value-bind (form presentp)
                   (util:maybe-read-form stream)

                 (cond
                   ((and (not presentp)
                         dotted-context)
                    (error "Invalid dotted list"))

                   ((not presentp)
                    (return-from read-coalton-toplevel-open-paren
                      (nreverse collected-forms)))

                   (dotted-context
                    (when (nth-value 1 (util:maybe-read-form stream))
                      (error "Invalid dotted list"))

                    (return-from read-coalton-toplevel-open-paren
                      (nreconc collected-forms form)))

                   (t
                    (push form collected-forms))))
             (eclector.reader:invalid-context-for-consing-dot (c)
               (when dotted-context
                 (error "Invalid dotted list"))
               (setf dotted-context t)
               (eclector.reader:recover c)))))))))

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
           (loc (error:coalton-error-location error)))
      (setf (sb-impl::fd-stream-misc stream)
            (lambda (stream operation arg1)
              (if (= (sb-impl::%stream-opcode :get-file-position) operation)
                  (+ file-offset loc 1)
                  (sb-impl::tracking-stream-misc stream operation arg1)))))))

(named-readtables:defreadtable coalton:coalton
  (:merge :standard)
  (:macro-char #\( #'read-coalton-toplevel-open-paren)
  (:macro-char #\` #'(lambda (s c)
                       (let ((*coalton-reader-allowed* nil))
                         (funcall (get-macro-character #\` (named-readtables:ensure-readtable :standard)) s c))))
  (:macro-char #\, #'(lambda (s c)
                       (let ((*coalton-reader-allowed* t))
                         (funcall (get-macro-character #\, (named-readtables:ensure-readtable :standard)) s c)))))

(defmacro coalton:coalton-toplevel (&body forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*compile-file-truename*
          (pathname (format nil "COALTON-TOPLEVEL (~A)" *compile-file-truename*))))
    (with-input-from-string (stream (cl:format cl:nil "(~S ~{~S~%~})" 'coalton:coalton-toplevel forms))
      (cl:read stream))))

(defmacro coalton:coalton-codegen (&body forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*compile-file-truename*
          (pathname (format nil "COALTON-TOPLEVEL (~A)" *compile-file-truename*))))
    (with-input-from-string (stream (cl:format cl:nil "(~S ~{~S~%~})" 'coalton:coalton-codegen forms))
      (cl:read stream))))

(defmacro coalton:coalton-codegen-ast (&body forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*compile-file-truename*
          (pathname (format nil "COALTON-TOPLEVEL (~A)" *compile-file-truename*))))
    (with-input-from-string (stream (cl:format cl:nil "(~S ~{~S~%~})" 'coalton:coalton-codegen-ast forms))
      (cl:read stream))))

(defmacro coalton:coalton (&rest forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*compile-file-truename*
          (pathname (format nil "COALTON (~A)" *compile-file-truename*))))
    (with-input-from-string (stream (cl:format cl:nil "(~S~{ ~S~})" 'coalton:coalton forms))
      (cl:read stream))))
