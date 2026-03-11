(defpackage #:coalton-impl/parser/reader
  (:use #:cl)
  (:local-nicknames
   (#:source #:coalton-impl/source))
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:export
   #:*coalton-eclector-client*
   #:with-reader-context
   #:maybe-read-form))

(in-package #:coalton-impl/parser/reader)

(defclass coalton-eclector-client (eclector.concrete-syntax-tree:cst-client)
  ())

(defvar *coalton-eclector-client* (make-instance 'coalton-eclector-client))

(defmacro with-reader-context (stream &rest body)
  "Run the body in the toplevel reader context."
  `(eclector.reader:call-as-top-level-read
    *coalton-eclector-client*
    (lambda ()
      ,@body)
    ,stream
    nil
    'eof
    nil))

(defun maybe-read-form (stream source &optional (eclector-client eclector.base:*client*))
  "Read the next form or return if there is no next form.

Returns (VALUES FORM PRESENTP EOFP)"
  (let ((begin (file-position stream)))
    (handler-case
        (loop :do
          ;; On empty lists report nothing
          (when (eq #\) (peek-char t stream nil))
            (read-char stream)
            (return (values nil nil nil)))

          ;; Otherwise, try to read in the next form
          (multiple-value-call
              (lambda (form type &optional parse-result)

                ;; Return the read form when valid
                (when (eq :object type)
                  (return (values (or parse-result form) t nil)))

                (when (eq :eof type)
                  (return (values nil nil t))))

            (eclector.reader:read-maybe-nothing
             eclector-client
             stream
             nil 'eof)))
      (eclector.reader:unterminated-list ()
        (let ((end (file-position stream)))
          (parse-error "Unterminated form"
                       (source:note (source:make-location source (cons begin end))
                                    "Missing close parenthesis for form starting at offset ~a" begin))))
      (error (condition)
        (let ((end (file-position stream)))
          (parse-error "Reader error"
                       (source:note (source:make-location source (cons begin end))
                                    "reader error: ~a" condition)))))))
