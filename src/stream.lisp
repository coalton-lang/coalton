;;; coalton-impl/stream
;;;
;;; Classes for working with streams

(defpackage #:coalton-impl/stream
  (:use
   #:cl)
  (:export
   #:make-char-position-stream))

(in-package #:coalton-impl/stream)

(defclass char-position-stream (trivial-gray-streams:fundamental-character-input-stream)
  ((stream :initarg :stream
           :reader inner-stream)
   (unread :initform nil
           :accessor unread-characters)
   (position :initform 0
             :accessor character-position))
  (:documentation "A stream that exposes the character offset into an underlying character stream through #'stream-file-position."))

(defmethod trivial-gray-streams:stream-read-char ((stream char-position-stream))
  (let ((char (cond ((not (null (unread-characters stream)))
                     (pop (unread-characters stream)))
                    (t
                     (read-char (inner-stream stream) nil :eof)))))
    (unless (eq char :eof)
      (incf (character-position stream)))
    char))

(defmethod trivial-gray-streams:stream-read-char-no-hang ((stream char-position-stream))
  (trivial-gray-streams:stream-read-char stream))

(defmethod trivial-gray-streams:stream-unread-char ((stream char-position-stream) char)
  (push char (unread-characters stream))
  (decf (character-position stream)))

(defmethod trivial-gray-streams:stream-file-position ((stream char-position-stream))
  (character-position stream))

(defmethod (setf trivial-gray-streams:stream-file-position) (position-spec (stream char-position-stream))
  (file-position (inner-stream stream) 0)
  (dotimes (i position-spec)
    (read-char (inner-stream stream)))
  (setf (character-position stream) position-spec))

(defun make-char-position-stream (stream)
  (make-instance 'char-position-stream :stream stream))
