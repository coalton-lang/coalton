(defpackage #:coalton-impl/parser/reader
  (:use #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree))
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:export
   #:*coalton-eclector-client*
   #:with-reader-context
   #:maybe-read-form))

(in-package #:coalton-impl/parser/reader)

(defclass coalton-eclector-client (eclector.parse-result:parse-result-client)
  ())

(defvar *coalton-eclector-client* (make-instance 'coalton-eclector-client))

;;;; Check which version of eclector is installed
(eval-when (:compile-toplevel)
  (if (uiop:version< (asdf:component-version (asdf:find-system :eclector)) "0.10.0")
      (pushnew :eclector-pre-0-10-0 *features*)
      (pushnew :eclector-post-0-10-0 *features*)))

(defmethod eclector.parse-result:make-expression-result
    ((client coalton-eclector-client) expression children source)

  ;; All of our nodes need access to source into so we will tag all
  ;; children with source info.

  ;; Hack to handle breaking change in eclector
  ;; See https://github.com/s-expressionists/Concrete-Syntax-Tree/pull/36
  ;; See https://github.com/coalton-lang/coalton/issues/887
  #+eclector-pre-0-10-0
  (cst:reconstruct expression children client :default-source source)
  #+eclector-post-0-10-0
  (cst:reconstruct client expression children :default-source source))

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
          (error 'parse-error
                 :err (source-error:source-error
                       :span (cons begin end)
                       :source source
                       :message "Unterminated form"
                       :primary-note (format nil "Missing close parenthesis for form starting at offset ~a" begin)))))
      (error (condition)
        (let ((end (file-position stream)))
          (error 'parse-error
                 :err (source-error:source-error
                       :span (cons begin end)
                       :source source
                       :message "Reader error"
                       :primary-note (format nil "reader error: ~a" condition))))))))
