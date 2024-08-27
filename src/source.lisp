;;;; Classes for working with source streams and locations.
;;;;
;;;; char-position-stream
;;;;
;;;;   An input stream class that reports position as character
;;;;   offset, rather than octet. This allows consistent reporting of
;;;;   character ranges reported by concrete-syntax-tree, which uses
;;;;   FILE-POSITION to construct ranges.
;;;;
;;;; source protocol
;;;;
;;;;   A 'source' is anything that provides a stream containing
;;;;   Coalton program text, by implementing the source-stream and
;;;;   source-name generic functions. Definitions are provided for
;;;;   files (source-file, make-source-file) and string buffers
;;;;   (source-string, make-source-string). The two sources define a
;;;;   readable representation, so that source locations may be
;;;;   preserved in compiled files.
;;;;
;;;; location protocol
;;;;
;;;;   A 'location' consists of a source and a character range, and
;;;;   provides a means for a compiled object to point to the location
;;;;   of its definition. All parser and typechecker AST nodes
;;;;   implement coalton-impl/source:location.
;;;;
;;;; source conditions
;;;;
;;;;   Source and location-aware condition classes, for reporting
;;;;   syntax and other compiler errors, with complex references to
;;;;   multiple locations.

(defpackage #:coalton-impl/source
  (:use
   #:cl
   #:coalton-impl/settings)
  (:export
   #:char-position-stream
   #:find-line-offsets
   #:report-source-condition
   #:location
   #:location-source
   #:location-span
   #:docstring
   #:make-help
   #:make-end-location
   #:make-location
   #:make-note
   #:make-source-file
   #:make-source-string
   #:source-condition
   #:source-error
   #:source-name
   #:source-stream
   #:source-warning
   #:->
   #:=>
   #:forall
   #:with-context
   #:with-note))

(in-package #:coalton-impl/source)

;;; char-position-stream

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

;;; source protocol

(defgeneric source-stream (source)
  (:documentation "Open and return a stream from which source text may be read. The caller is responsible for closing the stream, and the stream's initial position may be greater than zero."))

(defgeneric source-name (source)
  (:documentation "The name of an error's source, suitable for reporting in errors. If the source is a file, SOURCE-NAME will be that file's absolute path."))

(defgeneric source-offset (source)
  (:documentation "The offset into a file at which source begins."))

(defclass source ()
  ((name :initarg :name
         :initform nil
         :reader source-name))
  (:documentation "An abstract base class for sources that provide error context during condition printing.

In the case of source that is copied to a different location during compilation (e.g., by emacs+slime), original file name preserves the original location."))

(defclass source-file (source)
  ((file :initarg :file
         :reader source-file)
   (offset :initarg :offset
           :initform 0
           :reader source-offset))
  (:documentation "A source that supplies error context from a FILE."))

(defmethod print-object ((self source-file) stream)
  (if *print-readably*
      (format stream "#.(make-instance 'coalton-impl/source::source-file~@[ :name ~s~] :file ~s~:[~; :offset ~s~])"
              (source-name self)
              (source-file self)
              (< 0 (source-offset self))
              (source-offset self))
      (call-next-method)))

(defmethod make-load-form ((self source-file) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun ensure-namestring (file-designator)
  "Coerce FILE-DESIGNATOR to a namestring."
  (when file-designator
    (etypecase file-designator
      (string file-designator)
      (pathname (namestring file-designator)))))

(defun make-source-file (file &key name (offset 0))
  "Make a source that supplies error context from a FILE.

OFFSET indicates starting character offset within the file."
  (make-instance 'source-file
    :file (ensure-namestring file)
    :name name
    :offset offset))

(defmethod source-stream ((self source-file))
  (let ((fd-stream (open (source-file self)
                         :direction ':input
                         :element-type 'character
                         :external-format ':utf-8)))
    (make-instance 'char-position-stream :stream fd-stream)))

(defclass source-string (source)
  ((string :initarg :string
           :reader source-string))
  (:documentation "A source that supplies error context from a STRING."))

(defun make-source-string (string &key (name "<string input>"))
  "Make a source that supplies error context from a string."
  (make-instance 'source-string
    :string string
    :name name))

(defmethod print-object ((self source-string) stream)
  (if *print-readably*
      (format stream "#.(make-instance 'coalton-impl/source::source-string :string ~s)"
              (source-string self))
      (call-next-method)))

(defmethod make-load-form ((self source-string) &optional env)
  (make-load-form-saving-slots self :environment env))

(defmethod source-stream ((self source-string))
  (make-string-input-stream (source-string self)))

(defmethod source-offset ((self source-string))
  0)

;;; location protocol

(deftype span ()
  '(cons fixnum fixnum))

(defun span-start (span)
  (car span))

(defun span-end (span)
  (cdr span))

(defun offset-span (span offset)
  "Return a new span offset by OFFSET."
  (if (zerop offset)
      span
      (cons (+ (car span) offset)
            (+ (cdr span) offset))))

(defgeneric location (object)
  (:documentation "The location of a Coalton object's definition."))

(defgeneric docstring (object)
  (:documentation "The docstring accompanying a Coalton object's definition."))

(defstruct (location (:constructor %make-location))
  (source nil
   :read-only t)
  (span nil
   :type span :read-only t))

(defmethod location ((self location))
  self)

(defmethod make-load-form ((self location) &optional env)
  (make-load-form-saving-slots self :environment env))

(defun make-location (source form)
  "Make a source location structure from a SOURCE and a form, which may be either a cons of start, end or a cst node."
  (let ((span (etypecase form
                (cst:cst (cst:source form))
                (cons form))))
    (%make-location :source source
                    :span (offset-span span (source-offset source)))))

(defun make-end-location (source form)
  "Make a source location structure from a SOURCE and a form, which may be either a cons of start, end or a cst node."
  (let ((end (cdr (etypecase form
                    (cst:cst (cst:source form))
                    (cons form)))))
    (%make-location :source source
                    :span (offset-span (cons end end)
                                       (1- (source-offset source))))))

;;; source conditions

(defvar *context* nil)

(defmacro with-context ((key message) &body body)
  `(let ((*context* (cons (cons ,key ,message) *context*)))
     ,@body))

(define-condition source-condition ()
  ((message :initarg :message
            :reader source-condition-message)
   (notes :initarg :notes
          :reader source-condition-notes)
   (context :initform *context*
            :reader source-condition-context))
  (:report report-source-condition))

(defgeneric severity (condition))

(define-condition source-error (source-condition error)
  ()
  (:documentation "A user-facing error."))

(defmethod severity ((condition source-error))
  :error)

(define-condition source-warning (source-condition style-warning)
  ()
  (:documentation "A user-facing warning."))

(defmethod severity ((condition source-warning))
  :warning)

(defun source-error (message &rest notes)
  (error 'source-error
         :message message
         :notes notes))

(defun source-warning (message &rest notes)
  (error 'source-warning
         :message message
         :notes notes))

;; notes

(defclass note ()
  ((location :initarg :location
             :initform nil
             :reader note-location)
   (message :initarg :message
            :initform "no message"
            :reader note-message)
   (primary :initarg :primary
            :initform nil
            :accessor note-primary-p)))

(defun make-note (object message)
  (make-instance 'note
                 :location (location object)
                 :message message))

(defun empty-span (note)
  (let ((span (location-span (note-location note))))
    (= (car span) (cdr span))))

(defmacro with-note ((source form message) &body body)
  `(handler-case
       (progn ,@body)
     (source-error (condition)
       (error 'source-error
              :message (source-condition-message condition)
              :notes (append (source-condition-notes condition)
                             (list (make-note (make-location ,source ,form)
                                              ,message)))))))

(defclass help (note)
  ((replacement :initarg :replacement
                :initform nil
                :reader note-replacement)))

(defun note-help-p (object)
  (typep object 'help))

(defun make-help (location message &optional replacement)
  (make-instance 'help
                 :location location
                 :message message
                 :replacement replacement))

(defmethod print-object ((self note) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (self stream :type t)
        (format stream "~a (~a)"
                (note-message self)
                (note-location self)))))

(defun forall ()
  (if *coalton-print-unicode*
      "∀"
      "FORALL"))

(defun => ()
  (if *coalton-print-unicode*
      " ⇒ "
      " => "))

(defun -> ()
  (if *coalton-print-unicode*
      " → "
      " -> "))

;;; Printer
;;;
;;; `report-source-condition`, at the bottom, is a workhorse function that
;;; takes a source condition, resolves source-relative locations
;;; to the input source, and prints annotated source code.
;;;
;;; `printer-state` maintains state during printing: current line,
;;; note depth, etc.

(defclass printer-state ()
  ((source-stream :initarg :source-stream
                  :reader source-stream
                  :documentation "The stream form which source code is to be read.")

   (notes :initarg :notes)
   (help :initarg :help)
   (context :initarg :context)

   (line-offsets :reader line-offsets)
   (offset-positions :initform (make-hash-table))

   (line-number-width)
   (current-line)
   (last-line)

   ;; We need to keep track of the current depth of multiline
   ;; notes so that we can pad the left with the correct
   ;; number of columns.

   (note-stack :accessor note-stack
               :initform nil)
   (note-max-depth :initform 0)))

(defun first-line-number (notes offset-positions)
  (unless (null notes)
    (span-start (gethash (start-offset (car notes)) offset-positions))))

(defun last-line-number (notes offset-positions)
  (let ((last-offset (apply #'max (mapcar #'end-offset notes))))
    (span-start (gethash last-offset offset-positions))))

(defmethod initialize-instance :after ((printer-state printer-state) &rest initargs)
  (declare (ignore initargs))
  (with-slots (source-stream
               line-offsets
               offset-positions
               note-max-depth
               current-line
               last-line
               line-number-width
               notes)
      printer-state
    (let ((char-offsets (char-offsets printer-state))
          (offsets (find-line-offsets source-stream)))
      (setf line-offsets (coerce offsets 'vector))
      (loop :for (char-offset line column) :in (find-column-offsets offsets char-offsets)
            :do (setf (gethash char-offset offset-positions)
                      (cons line column)))
      (setf current-line (1- (first-line-number notes offset-positions))
            last-line (last-line-number notes offset-positions)
            line-number-width (1+ (floor (log last-line 10)))
            note-max-depth (max-depth notes)))))

(defun start-offset (note)
  (span-start (location-span (note-location note))))

(defun start-position (printer-state note)
  (with-slots (offset-positions) printer-state
    (gethash (start-offset note) offset-positions)))

(defun end-offset (note)
  (cdr (location-span (note-location note))))

(defun end-position (printer-state note)
  (with-slots (offset-positions) printer-state
    (gethash (end-offset note) offset-positions)))

(defun location-lines (printer-state note)
  "Return the start and end lines of NOTE"
  (let ((start-line (car (start-position printer-state note)))
        (end-line (car (end-position printer-state note))))
    (values start-line end-line)))

(defun location-positions (printer-state note)
  "Return the start and end positions of NOTE"
  (with-slots (offset-positions) printer-state
    (destructuring-bind (start-line . start-column)
        (gethash (start-offset note) offset-positions)
      (destructuring-bind (end-line . end-column)
          (gethash (end-offset note) offset-positions)
        (values start-line start-column end-line end-column)))))

;; Mapping between character offsets and line and column positions.
;;
;; First line (zero indexed) = offset 0, etc.

(defun find-line-offsets (stream)
  "Compute the offsets of lines in STREAM."
  (file-position stream 0)
  (loop :with index := 0
        :for char := (read-char stream nil nil)
        :unless char
          :return (cons 0 offsets)
        :when (char= char #\Newline)
          :collect (1+ index) :into offsets
        :do (incf index)))

(defun find-column-offsets (line-offsets offsets)
  "Given the offsets of newlines in a stream, compute the line and
column numbers for a sequence of absolute stream offsets."
  (loop :with line := 0
        :with position := 0
        :while offsets
        :when (or (null line-offsets)
                  (< (car offsets)
                     (car line-offsets)))
          :collect (list (car offsets) line (- (car offsets) position))
          :and :do (pop offsets)
        :else
          :do (setf position (car line-offsets)
                    line (1+ line)
                    line-offsets (cdr line-offsets))))

(defun line-contents (printer-state line-number)
  (with-slots (line-offsets source-stream) printer-state
    (let ((offset (if (= 1 line-number)
                      0
                      (aref line-offsets (1- line-number)))))
      (file-position source-stream offset)
      (read-line source-stream nil ""))))

(defun positioned-annotations (printer-state)
  (with-slots (notes help) printer-state
    (concatenate 'list notes help)))

(defun char-offsets (printer-state)
  (sort (remove-duplicates
         (mapcan (lambda (note)
                   (list (start-offset note) (end-offset note)))
                 (positioned-annotations printer-state)))
        #'<))

(defun start-line (printer-state location)
  (with-slots (offset-positions) printer-state
    (car (gethash (start-offset location) offset-positions))))

(defun start-column (printer-state location)
  (with-slots (offset-positions) printer-state
    (cdr (gethash (start-offset location) offset-positions))))

(defun end-line (printer-state location)
  (with-slots (offset-positions) printer-state
    (car (gethash (end-offset location) offset-positions))))

(defun end-column (printer-state location)
  (with-slots (offset-positions) printer-state
    (cdr (gethash (end-offset location) offset-positions))))

(defun location-point< (a b)
  (destructuring-bind (offset-a . type-a) a
    (destructuring-bind (offset-b . type-b) b
      (if (= offset-a offset-b)
          (and (eql type-a :start)
               (eql type-b :end))
          (< offset-a offset-b)))))

(defun location-points (location)
  (list (cons (start-offset location) :start)
        (cons (end-offset location) :end)))

(defun max-depth (notes)
  (let ((max-depth 0)
        (depth 0))
    (dolist (op (mapcar #'cdr (sort (mapcan #'location-points notes) #'location-point<)) max-depth)
      (cond ((eql op :start)
             (incf depth)
             (when (< max-depth depth)
               (setf max-depth depth)))
            ((eql op :end)
             (decf depth))))))

(defun offset-position (printer-state location)
  (let ((location (etypecase location
                    (cons (car location))
                    (integer location))))
    (gethash location (slot-value printer-state 'offset-positions) (cons 1 0))))

(defun note-highlight-char (note)
  (if (note-primary-p note)
      #\^
      #\-))

(defun write-nchar (char n stream)
  (dotimes (n n)
    (write-char char stream)))

;;; Printer

(defun primary-note (condition)
  (first (source-condition-notes condition)))

(defun print-condition-location (stream printer-state condition)
  (let* ((note (primary-note condition))
         (source (location-source (note-location note))))
    (destructuring-bind (line . column)
        (offset-position printer-state (start-offset note))
      (format stream "~(~A~): ~A~%  --> ~A:~D:~D~%"
              (severity condition)
              (source-condition-message condition)
              (source-name source)
              line
              column))))

(defun print-line-prefix (stream printer-state &key (line-number nil))
  (with-slots (line-number-width) printer-state
    (cond (line-number
           (format stream " ~va |" line-number-width line-number))
          (t
           (write-nchar #\Space (+ 2 line-number-width) stream)
           (write-char #\| stream)))))

(defun print-line-number (stream printer-state line-number show-line-number)
  (with-slots (line-number-width note-stack) printer-state
    (print-line-prefix stream printer-state :line-number (and show-line-number line-number))
    (format stream
            " ~{~:[ ~;|~]~}"
            (mapcar
             (lambda (note)
               (>= (end-line printer-state note)
                   line-number))
             note-stack))))

(defun print-line-contents (stream printer-state line-number)
  (print-line-number stream printer-state line-number t)
  (with-slots (note-stack note-max-depth) printer-state
    (format stream "~v@{ ~}~A~%"
            (- note-max-depth (length note-stack))
            (line-contents printer-state line-number))))

(defun print-single-line-note (stream printer-state note)
  (multiple-value-bind (start-line start-column end-line end-column)
      (location-positions printer-state note)
    (declare (ignore end-line))
    (print-line-number stream printer-state start-line nil)
    (with-slots (note-max-depth note-stack) printer-state
      (write-nchar #\Space
                   (+ start-column
                      (- note-max-depth (length note-stack)))
                   stream)
      (format stream "~v{~C~:*~} ~A~%"
              (max 1 (- end-column start-column))
              (list (note-highlight-char note))
              (note-message note)))))

(defun print-note-start (stream printer-state note)
  (destructuring-bind (start-line . start-column)
      (start-position printer-state note)
    (print-line-number stream printer-state start-line nil)
    (with-slots (note-max-depth note-stack) printer-state
      (write-char #\Space stream)
      (write-nchar #\_ (+ start-column (- note-max-depth (length note-stack) 1)) stream)
      (write-char (note-highlight-char note) stream)
      (terpri stream))))

(defun print-note-end (stream printer-state note)
  (let ((start-line (start-line printer-state note))
        (end-column (end-column printer-state note)))
    (print-line-number stream printer-state start-line nil)
    (with-slots (note-max-depth note-stack) printer-state
      (write-nchar #\_ (+ end-column (- note-max-depth (length note-stack) 1)) stream)
      (format stream "~C ~A~%" (note-highlight-char note) (note-message note)))))

(defun print-finished-notes-for-line (stream printer-state line-number)
  (with-slots (note-stack) printer-state
    ;; Check if there are any multiline notes that need to be printed
    (loop :for stack-head := note-stack :then (cdr stack-head)
          :for note := (car stack-head)
          :for end-line := (and note (end-line printer-state note))
          :when (null stack-head)
            :do (return)
          :when (= line-number end-line)
            :do (print-note-end stream printer-state note)
          :when (and (eq note (car note-stack))
                     (>= line-number end-line))
            :do (pop note-stack))))

(defun print-lines-until (stream printer-state line-number)
  (with-slots (current-line note-stack) printer-state
    (cond ((= line-number current-line)
           ;; If we are on the same line then don't reprint.
           )
          ((>= 3 (- line-number current-line))
           ;; If we are within 3 lines of the previous one then just
           ;; print those lines.
           (loop :for line :from current-line :below line-number
                 :do (print-line-contents stream printer-state (1+ line))
                 :unless (= (1+ line) line-number)
                   :do (print-finished-notes-for-line stream printer-state (1+ line))))
          (t
           ;; Otherwise split the output.
           (print-line-contents stream printer-state (1+ current-line))
           ;; Print out any intermediate multiline note endings.
           (loop :for note :in note-stack
                 :for end-line := (end-line printer-state note)
                 :when (< current-line end-line line-number)
                   :do (print-lines-until stream printer-state end-line))
           (format stream " ...~%")
           (print-line-contents stream printer-state (1- line-number))
           (print-line-contents stream printer-state line-number)))
    (setf current-line line-number)))

(defun print-note (stream printer-state note)
  (multiple-value-bind (start-line end-line)
      (location-lines printer-state note)
    (print-lines-until stream printer-state start-line)
    (cond ((/= start-line end-line)
           (print-note-start stream printer-state note)
           (push note (note-stack printer-state)))
          (t
           (print-single-line-note stream printer-state note)
           (print-finished-notes-for-line stream printer-state start-line)))))

(defun print-notes (stream printer-state)
  (with-slots (notes last-line) printer-state
    (loop :for note :in notes
          :do (print-note stream printer-state note))
    (print-lines-until stream printer-state last-line)
    (print-finished-notes-for-line stream printer-state last-line)))

(defun print-help (stream printer-state help)
  (with-slots (source-stream) printer-state
    (multiple-value-bind (start-line start-column end-line end-column)
        (location-positions printer-state help)
      (unless (= start-line end-line)
        (error "multiline help messages not supported"))
      (format stream "help: ~A~%" (note-message help))
      (let ((line (line-contents printer-state start-line)))
        (print-line-prefix stream printer-state :line-number start-line)
        (format stream " ~A" (subseq line 0 start-column))
        (let ((replaced-text (funcall (or (note-replacement help) #'identity)
                                      (subseq line start-column end-column))))
          (format stream "~A~A~%" replaced-text (subseq line end-column))
          (print-line-prefix stream printer-state)
          (format stream "~v{~C~:*~}~v{~C~:*~}~%"
                  (1+ start-column) '(#\Space)
                  (length replaced-text) '(#\-)))))))

(defun print-empty-line (stream printer-state)
  (print-line-prefix stream printer-state)
  (terpri stream))

(defun %reduce-context (context)
  (mapcar #'cdr
          (reduce (lambda (acc context)
                    (if (assoc (car context) acc)
                        acc
                        (cons context acc)))
                  context
                  :initial-value nil)))

(defun make-printer-state (source-stream condition)
  (setf (note-primary-p (car (source-condition-notes condition))) t)
  (let* ((all-notes (sort (copy-list (source-condition-notes condition))
                          #'< :key #'start-offset))
         (notes (remove-if #'note-help-p all-notes))
         (help (remove-if-not #'note-help-p all-notes)))
    (make-instance 'printer-state
      :source-stream source-stream
      :notes notes
      :help help
      :context (%reduce-context (source-condition-context condition)))))

(defun condition-stream (condition)
  (source-stream (location-source (note-location (first (source-condition-notes condition))))))

(defun report-source-condition (condition stream)
  (with-open-stream (source-stream (condition-stream condition))
    (let ((state (make-printer-state source-stream condition)))
      (print-condition-location stream state condition)
      (print-empty-line stream state)
      (with-slots (help context last-line) state
        (print-notes stream state)
        (loop :for help :in help
              :do (print-help stream state help))
        (loop :for context :in context
              :do (format stream "note: ~A~%" context))))))
