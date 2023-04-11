(defpackage #:coalton-impl/parser/reader
  (:use #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree))
  (:export
   #:*coalton-eclector-client*
   #:maybe-read-form))

(in-package #:coalton-impl/parser/reader)

(defclass coalton-eclector-client (eclector.parse-result:parse-result-client)
  ())

(defvar *coalton-eclector-client* (make-instance 'coalton-eclector-client))

(defmethod eclector.parse-result:make-expression-result
    ((client coalton-eclector-client) expression children source)

  ;; All of our nodes need access to source into so we will tag all
  ;; children with source info.
  (cst:reconstruct expression children client :default-source source))

(defun maybe-read-form (stream &optional (eclector-client eclector.base:*client*))
  "Read the next form or return if there is no next form.

Returns (VALUES FORM PRESENTP EOFP)"
  (loop :do
    ;; On empty lists report nothing
    (when (eq #\) (peek-char t stream nil))
      (read-char stream)
      (return (values nil nil nil)))

    ;; Otherwise, try to read in the next form
    (eclector.reader:call-as-top-level-read
     eclector-client
     (lambda ()
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
     stream
     nil 'eof
     nil)))
