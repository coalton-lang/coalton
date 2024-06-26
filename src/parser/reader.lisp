(defpackage #:coalton-impl/parser/reader
  (:use #:cl)
  (:export
   #:position-stream
   #:with-reader-context
   #:maybe-read-form))

(in-package #:coalton-impl/parser/reader)

(defclass source-position-client (eclector.parse-result:parse-result-client)
  ())

(defvar *source-position-client* (make-instance 'source-position-client))

;; Check which version of eclector is installed

(eval-when (:compile-toplevel)
  (when (uiop:version< (asdf:component-version (asdf:find-system :eclector)) "0.10.0")
    (pushnew :eclector-pre-0-10-0 *features*)))

;; All of our nodes need access to source into so we will tag all
;; children with source info.

(defmethod eclector.parse-result:make-expression-result
    ((client source-position-client) expression children source)

  ;; Hack to handle breaking change in eclector
  ;; See https://github.com/s-expressionists/Concrete-Syntax-Tree/pull/36
  ;; See https://github.com/coalton-lang/coalton/issues/887

  (concrete-syntax-tree:reconstruct #-eclector-pre-0-10-0 client expression children
                                    #+eclector-pre-0-10-0 client :default-source source))

(defclass position-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((stream :initarg :stream
           :reader inner-stream)
   (unread :initform nil
           :accessor unread-characters)
   (position :initform 0
             :accessor character-position)))

(defmethod trivial-gray-streams:stream-read-char ((stream position-stream))
  (prog1
      (cond ((not (null (unread-characters stream)))
             (pop (unread-characters stream)))
            (t
             (read-char (inner-stream stream) nil :eof)))
    (incf (character-position stream))))

(defmethod trivial-gray-streams:stream-unread-char ((stream position-stream) char)
  (push char (unread-characters stream))
  (decf (character-position stream)))

(defmethod trivial-gray-streams:stream-file-position ((stream position-stream))
  (character-position stream))

(defmethod (setf trivial-gray-streams:stream-file-position) (position-spec (stream position-stream))
  (file-position (inner-stream stream) 0)
  (dotimes (i position-spec)
    (read-char (inner-stream stream)))
  (setf (character-position stream) position-spec))

(defun maybe-read-form (stream)
  "Read the next form or return if there is no next form.

Returns (values FORM PRESENTP)"
  (declare (optimize (debug 3)))
  (loop :do
    ;; On empty lists report nothing
    (when (eq #\) (peek-char t stream nil))
      (read-char stream)
      (return (values nil nil)))
    ;; Otherwise, try to read in the next form
    (multiple-value-bind (form type parse-result)
        (eclector.reader:read-maybe-nothing eclector.base:*client* stream nil 'eof)
      ;; Return the read form when valid
      (case type
        (:object
         (return (values parse-result t)))
        (:eof
         (return (values nil nil)))))))

(defmacro with-reader-context (stream &rest body)
  "Run the body in the toplevel reader context."
  `(eclector.reader:call-as-top-level-read
    *source-position-client*
    (lambda () ,@body)
    ,stream
    nil 'eof
    nil))
