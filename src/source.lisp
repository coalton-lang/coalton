;;;; Classes for working with source streams and locations

(defpackage #:coalton-impl/source
  (:use
   #:cl)
  (:shadow
   #:error
   #:warn)
  (:local-nicknames
   (#:se #:source-error))
  (:export
   #:char-position-stream
   #:error
   #:warn
   #:help
   #:note
   #:primary-note
   #:message
   #:make-source-error
   #:make-source-file
   #:make-source-string
   #:location
   #:end-location
   #:make-location
   #:location-source
   #:location-span
   #:location<
   #:span
   #:span-start
   #:span-end
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

(defgeneric location (object)
  (:documentation "The source location of a Coalton object's definition."))

(defgeneric docstring (object)
  (:documentation "The docstring accompanying a Coalton object's definition."))

(deftype span ()
  "A pair of offsets that indicates a range of characters in a source file."
  '(cons fixnum fixnum))

(declaim (inline span-start span-end))

(defun span-start (span)
  "Return the zero-based offset of the first character in SPAN."
  (car span))

(defun span-end (span)
  "Return the zero-based offset immediately past the last character in SPAN."
  (cdr span))

(defun end-span (span)
  "Return an empty span immediately following SPAN."
  (cons (span-end span)
        (span-end span)))

(defun span< (a b)
  "Return T if span A starts before span B. If both spans start at the same offset, return T if A is shorter than B."
  (or (< (span-start a)
         (span-start b))
      (< (span-end a)
         (span-end b))))

(defun span-empty-p (span)
  "Return T if span is zero-length."
  (= (span-start span)
     (span-end span)))

(defstruct (location
            (:constructor %make-location))
  (source nil
   :read-only t)
  (span nil
   :type span :read-only t))

(defgeneric location (object)
  (:documentation "The location of a Coalton object's source definition."))

(defun end-location (location)
  "Return a new location that points at a zero-length span immediately past tht end of LOCATION."
  (make-location (location-source location)
                 (end-span (location-span location))))

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

(defun make-location (source span)
  "Make a source location structure from a SOURCE and a SPAN."
  (declare (type cons span))
  (%make-location :source source
                  :span span))

(defgeneric message (object)
  (:documentation "The primary message associated with an object."))

(defclass note ()
  ((location :initarg :location
             :reader location)
   (message :initarg :message
            :reader message)
   (replace :initarg :replace
            :initform #'identity
            :reader replace-function)
   (type :initarg :type
         :initform ':secondary
         :reader note-type))
  (:documentation "A message describing the contents of a source location."))

(defmethod print-object ((self note) stream)
  (if *print-readably*
      (call-next-method)
      (format stream "~(~a~) note: ~a: ~a"
              (note-type self)
              (location self)
              (message self))))

(defun ensure-location (locatable)
  (typecase locatable
    (location locatable)
    (t (location locatable))))

(defun note (location format-string &rest format-args)
  "Return a note that describes a source LOCATION."
  (declare (type string format-string))
  (make-instance 'note
    :location (ensure-location location)
    :message (apply #'format nil format-string format-args)))

(defun primary-note (location format-string &rest format-args)
  "Return a note that describes a primary source LOCATION."
  (declare (type string format-string))
  (make-instance 'note
    :location (ensure-location location)
    :message (apply #'format nil format-string format-args)
    :type ':primary))

(defun help (location replace format-string &rest format-args)
  "Return a help note related to a source LOCATION.

REPLACE is a 1-argument function that accepts and returns a string to suggest an edit or fix."
  (declare (type function replace)
           (type string format-string))
  (make-instance 'note
    :location (ensure-location location)
    :message (apply #'format nil format-string format-args)
    :replace replace
    :type ':help))

;;; The following functions up to SOURCE-ERROR are for interfacing
;;; with the SOURCE-ERROR package."

(defun help-note-p (note)
  "T if NOTE is a help note."
  (eq ':help (note-type note)))

(defun note->source-note (note)
  "Convert NOTE to a source error help note."
  (se:make-source-error-note :span (location-span (location note))
                             :type (note-type note)
                             :message (message note)))

(defun note->help-note (note)
  "Convert NOTE to a source error help note."
  (se:make-source-error-help :span (location-span (location note))
                             :replacement (replace-function note)
                             :message (message note)))

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

(defun make-source-error (type message notes)
  "Build a SOURCE-ERROR:SOURCE-ERROR structure by destructuring the first note in NOTES, and translating remaining notes to SOURCE-ERROR versions."
  (destructuring-bind (primary &rest secondary) notes
    (let ((primary-span (location-span (location primary))))
      (se:source-error :type type
                       :span primary-span
                       :source (location-source (location primary))
                       :highlight (if (span-empty-p primary-span) :end :all)
                       :message message
                       :primary-note (message primary)
                       :notes (mapcar #'note->source-note
                                      (remove-if #'help-note-p secondary))
                       :help-notes (mapcar #'note->help-note
                                           (remove-if-not #'help-note-p secondary))))))

(defun error (message note &rest notes)
  "Signal an error related to one or more source locations"
  (cl:error 'se:source-base-error
            :err (make-source-error ':error message (cons note notes))))

(defun warn (message note &rest notes)
  "Signal a warning related to one or more source locations."
  (cl:warn 'se:source-base-warning
           :err (make-source-error ':warn message (cons note notes))))
