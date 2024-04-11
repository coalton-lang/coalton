(defpackage #:coalton-impl/reader
  (:use
   #:cl)
  (:local-nicknames
   (#:codegen #:coalton-impl/codegen)
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

(defun process-toplevel (stream collector)
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
               ;; render errors and set highlights
               ((error:coalton-base-error
                  (lambda (c)
                    (set-highlight-position-for-error stream (funcall (error:coalton-error-err c)))
                    (error:render-coalton-error c)))
                (error:coalton-base-warning
                  (lambda (c)
                    (error:render-coalton-warning c))))
             (entry:entry-point (parser:read-program stream file) collector)))
      ;; clean up any opened file streams
      (dolist (s opened-streams)
        (close s)))))

;; Implementation of toplevel macros

(defgeneric collector-result (collector)
  (:method (collector)
    (declare (ignore collector))))

;; (coalton-toplevel ...)

(defclass toplevel-compiler ()
  ((forms :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod codegen:emit ((collector toplevel-compiler) form)
  (vector-push-extend form (slot-value collector 'forms)))

(defmethod codegen:emit-env ((collector toplevel-compiler) name args)
  (codegen:emit collector
                `(setf entry:*global-environment*
                       (,name entry:*global-environment*
                              ,@(mapcar #'util:runtime-quote args)))))

(defmethod collector-result ((collector toplevel-compiler))
  `(progn ,@(coerce (slot-value collector 'forms) 'list)))

;; (coalton-codegen ...)

(defclass codegen-collector ()
  ((forms :initform (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod codegen:emit ((collector codegen-collector) form)
  (vector-push-extend form (slot-value collector 'forms)))

(defmethod collector-result ((collector codegen-collector))
  (util:runtime-quote (coerce (slot-value collector 'forms) 'list)))

;; (coalton-codegen-ast ...)

(defclass ast-printer ()
  ())

(defmethod codegen:emit-ast ((collector ast-printer) name type value)
  (format t "~A :: ~A~%~A~%~%~%" name type value))

;; Entry point

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
          (let ((collector (make-instance 'toplevel-compiler)))
            (setf entry:*global-environment* (process-toplevel stream collector))
            (collector-result collector)))
        (coalton:coalton-codegen
          (let ((collector (make-instance 'codegen-collector)))
            (process-toplevel stream collector)
            (collector-result collector)))
        (coalton:coalton-codegen-ast
          (let ((collector (make-instance 'ast-printer)))
            (process-toplevel stream collector)
            (collector-result collector)))
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
           (loc (error:coalton-error-location error)))
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

(defun read-in-mode (mode-symbol forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*compile-file-truename*
          (pathname (format nil "COALTON-TOPLEVEL (~A)" *compile-file-truename*)))
        (*print-circle* t))
    (with-input-from-string (stream (cl:format cl:nil "~S" (cons mode-symbol forms)))
      (cl:read stream))))

(defmacro coalton:coalton-toplevel (&body forms)
  (read-in-mode 'coalton:coalton-toplevel forms))

(defmacro coalton:coalton-codegen (&body forms)
  (read-in-mode 'coalton:coalton-codegen forms))

(defmacro coalton:coalton-codegen-ast (&body forms)
  (read-in-mode 'coalton:coalton-codegen-ast forms))

(defmacro coalton:coalton (&rest forms)
  (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
        (*compile-file-truename*
          (pathname (format nil "COALTON (~A)" *compile-file-truename*)))
        (*print-circle* t))
    (with-input-from-string (stream (cl:format cl:nil "~S" (cons 'coalton:coalton forms)))
      (cl:read stream))))
