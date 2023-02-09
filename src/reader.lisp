(defpackage #:coalton-impl/reader
  (:use
   #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:entry #:coalton-impl/entry)))

(in-package #:coalton-impl/reader)

(defvar *coalton-reader-allowed* t
  "Is the Coalton reader allowed to parse the current input?
Used to forbid reading while inside quasiquoted forms.")

(defun read-coalton-toplevel-open-paren (stream char)
  (labels ((try-read-string (expected-string)
             (%try-read-string (coerce expected-string 'list) nil))
           (%try-read-string (expected taken)
             (cond
               ((null expected)
                ;; Check if the next character is whitespace, hackily!
                (not (eql (peek-char nil stream) (peek-char t stream))))
               ((not (char-equal (car expected) (peek-char nil stream)))
                nil)
               (t
                (read-char stream)
                (cond
                  ((%try-read-string (cdr expected) (cons (car expected) taken))
                   t)
                  (t
                   (unread-char (car expected) stream)
                   nil))))))
    (cond
      ((try-read-string "coalton-toplevel")
       (unless *coalton-reader-allowed*
         (error "COALTON-TOPLEVEL is not allowed in quasiquoted forms."))
       
       (let* ((pathname (or *compile-file-truename* *load-truename*))
              (filename (if pathname (namestring pathname) "<unknown>"))
              
              (file-input-stream
                (cond
                  ((or #+sbcl (sb-int:form-tracking-stream-p stream)
                       nil)
                   (open (pathname stream)))
                  (t
                   stream)))
              (file (parser:make-coalton-file :stream file-input-stream :name filename)))

         (handler-case
             (let ((program (parser:read-program stream file :mode :toplevel-macro)))
               (multiple-value-bind (program env)
                   (entry:entry-point program)
                 (setf entry:*global-environment* env)
                 `(progn
                    #+ignore
                    (setf entry:*global-environment* ,env)
                    ,program)))
           (parser:parse-error (c)
             (set-highlight-position-for-error stream (parser:parse-error-err c))
             (error c))
           (tc:tc-error (c)
             (set-highlight-position-for-error stream (tc:tc-error-err c))
             (error c)))))
      
      ((try-read-string "coalton")
       (unless *coalton-reader-allowed*
         (error "COALTON is not allowed in quasiquoted forms."))
       
       (let* ((pathname (or *compile-file-truename* *load-truename*))
              (filename (if pathname (namestring pathname) "<unknown>"))

              (file-input-stream
                (cond
                  ((or #+sbcl (sb-int:form-tracking-stream-p stream)
                       nil)
                   (open (pathname stream)))
                  (t
                   stream)))
              (file (parser:make-coalton-file :stream file-input-stream :name filename))

              (expression
                (handler-case
                    (parser:read-expression stream file)

                  (parser:parse-error (c)
                    (set-highlight-position-for-error stream (parser:parse-error-err c))
                    (error c))
                  (tc:tc-error (c)
                    (set-highlight-position-for-error stream (tc:tc-error-err c))
                    (error c)))))
         `(format t "~A" ,(format nil "~A" expression))))
      ;; Fall back to the default open paren reader
      (t
       (funcall #'#.(get-macro-character #\() stream char)))))

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
  (let* ((file-offset
           (- (sb-impl::fd-stream-get-file-position stream)
              (file-position stream)))
         (loc (parser:coalton-error-location error)))
    (setf (sb-impl::fd-stream-misc stream)
          (lambda (stream operation arg1)
            (if (= (sb-impl::%stream-opcode :get-file-position) operation)
                (+ file-offset loc 1)
                (sb-impl::tracking-stream-misc stream operation arg1))))))

(named-readtables:defreadtable coalton:coalton
  (:merge :standard)
  (:macro-char #\( #'read-coalton-toplevel-open-paren)
  ;; TODO: We really should be somehow tracking if we are in
  ;;       quasiquoted forms.
  (:macro-char #\` #'(lambda (s c)
                       (let ((*coalton-reader-allowed* nil))
                         (funcall #'#.(get-macro-character #\`) s c)))))
