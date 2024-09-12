;;;; Classes for working with source streams and locations

(defpackage #:coalton-impl/source
  (:use
   #:cl)
  (:export
   #:char-position-stream
   #:make-source-file
   #:make-source-string
   #:location
   #:make-location
   #:location-source
   #:location-span
   #:location<
   #:docstring
   #:source-error))

(in-package #:coalton-impl/source)

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

(defmethod (setf trivial-gray-streams:stream-file-position)
    (position-spec (stream char-position-stream))
  (file-position (inner-stream stream) 0)
  (dotimes (i position-spec)
    (read-char (inner-stream stream)))
  (setf (character-position stream) position-spec))

;; source input

(defgeneric source< (a b)
  (:method (a b)
    nil)
  (:documentation "Compare two source locations, returning T if the string name of A is lexicographically earlier than that of B."))

(defclass source ()
  ((name :initarg :name
         :initform nil
         :reader original-name))
  (:documentation "An abstract base class for sources that provide error context during condition printing.

In the case of source that is copied to a different location during compilation (e.g., by emacs+slime), original file name preserves the original location."))

(defmethod source< ((a source) (b source))
  (and (original-name a)
       (original-name b)
       (string< (original-name a)
                (original-name b))))

(defclass source-file (source)
  ((file :initarg :file
         :reader input-name)
   (offset :initarg :offset
           :initform 0
           :reader file-offset))
  (:documentation "A source that supplies error context from a FILE."))

(defmethod print-object ((self source-file) stream)
  (if *print-readably*
      (format stream "#.(make-instance 'coalton-impl/source::source-file~@[ :name ~s~] :file ~s~:[~; :offset ~s~])"
              (original-name self)
              (input-name self)
              (< 0 (file-offset self))
              (file-offset self))
      (call-next-method)))

(defmethod make-load-form ((self source-file) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun ensure-namestring (file-designator)
  (when file-designator
    (etypecase file-designator
      (string file-designator)
      (pathname (namestring file-designator)))))

(defun make-source-file (file &key name (offset 0))
  "Make a source that supplies error context from a FILE.

OFFSET indicates starting character offset within the file."
  (make-instance 'source-file
    :file (ensure-namestring file)
    :name (ensure-namestring name)
    :offset offset))

(defmethod source-error:source-available-p ((self source-file))
  (not (null (input-name self))))

(defmethod source-error:source-name ((self source-file))
  (or (original-name self)
      (input-name self)))

(defmethod source-error:source-stream ((self source-file))
  (let* ((fd-stream (open (input-name self)
                          :direction ':input
                          :element-type 'character
                          :external-format ':utf-8))
         (stream (make-instance 'char-position-stream
                   :stream fd-stream)))
    (when (plusp (file-offset self))
      (file-position stream (file-offset self)))
    stream))

(defclass source-string (source)
  ((string :initarg :string
           :reader source-string))
  (:documentation "A source that supplies error context from a STRING."))

(defmethod print-object ((self source-string) stream)
  (if *print-readably*
      (format stream "#.(make-instance 'coalton-impl/source::source-string :string ~s)"
              (source-string self))
      (call-next-method)))

(defmethod make-load-form ((self source-string) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun make-source-string (string &key name)
  "Make a source that supplies error context from a string."
  (make-instance 'source-string
    :string string
    :name (ensure-namestring name)))

(defmethod source-error:source-available-p ((self source-string))
  (not (null (source-string self))))

(defmethod source-error:source-stream ((self source-string))
  (make-string-input-stream (source-string self)))

(defmethod source-error:source-name ((self source-string))
  (or (original-name self) "<string input>"))

(defgeneric docstring (object)
  (:documentation "The docstring accompanying a Coalton object's definition."))

(deftype span ()
  "A pair of offsets that indicates a range of characters in a source file."
  '(cons fixnum fixnum))

(declaim (inline span-start span-end))

(defun span-start (span)
  (car span))

(defun span-end (span)
  (car span))

(defun span< (a b)
  "Return T if span A starts before span B. If both spans start at the same offset, return T if A is shorter than B."
  (or (< (span-start a)
         (span-start b))
      (< (span-end a)
         (span-end b))))

(defstruct (location
            (:constructor %make-location))
  (source nil
   :read-only t)
  (span nil
   :type span :read-only t))

(defgeneric location (object)
  (:documentation "The location of a Coalton object's source definition."))

(defun location< (a b)
  "If locations A and B appear within the same source, return T if A's span starts before B's.
If locations appear in different sources, compare the sources by name."
  (if (eq (location-source a)
          (location-source b))
      (span< (location-span a)
             (location-span b))
      (source< a b)))

(defmethod make-load-form ((self location) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun make-location (source form)
  "Make a source location structure from a SOURCE and a form, which may be either a cons of start, end or a cst node."
  (etypecase form
    (cst:cst (%make-location :source source
                             :span (cst:source form)))
    (cons (%make-location :source source
                          :span form))))

(defun source-error (&key (type :error) location (highlight :all)
                          message primary-note notes help-notes)
  "Convenience function to unpack a LOCATION into source and span and create a source-error structure."
  (declare (type location location))
  (source-error:source-error :type type
                             :span (location-span location)
                             :source (location-source location)
                             :highlight highlight
                             :message message
                             :primary-note primary-note
                             :notes notes
                             :help-notes help-notes))
