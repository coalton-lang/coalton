(defpackage #:source-error
  (:use
   #:cl)
  (:export
   #:make-displaced-source-file
   #:make-help
   #:make-note
   #:make-source-file
   #:make-source-string
   #:report-source-condition
   #:*source*
   #:source
   #:source-error
   #:source-filename
   #:source-warning
   #:with-displaced-source-file
   #:with-source-context
   #:with-source-file
   #:with-source-string))

(in-package #:source-error)

;; Source-aware errors and warnings
;;
;; Error and warning conditions contain information about the source
;; of the error and optional explanatory notes and
;; messages. `report-source-condition` uses these structures to emit
;; annotated source code when these conditions are printed.

;; The generic functions `source-filename` and `source-stream` define a
;; protocol that gives the condition printer access to an input source
;; related to the condition, via the `source` slot on a
;; %source-error struct (see below). These provide, respectively,
;; the name, and contents of a source.

;; source annotation structures
;;
;; These are classes that hold for input location-associated error
;; annotations. The top-level `%source-error` structure may
;; contain sets of optional `note` and
;; `source-error-help` messages that refer to source locations, which
;; `report-source-condition` will resolve to character ranges in an input
;; source.

(defgeneric source-filename (source)
  (:documentation "Returns a string that names a source, for reporting in conditions. In
the case of files, this is the filename path."))

(defgeneric source-stream (source)
  (:documentation "A seekable stream providing access to the original contents of a
source, for reporting in condition printers. WARNING: it is the
responsibility of the caller to close the stream, if not using the
with-source-stream convenience macro."))

(defvar *source*)

(defvar *source-context* nil)

;; Support direct compilation of files and strings, and slime's method
;; of writing out a tempfile containing a form taken from within an
;; original source file.

(defclass source () ())

(defclass source-file (source)
  ((filename :initarg :filename
             :documentation "The file containing source to be compiled.")
   (original-filename :initarg :original-filename
                      :initform nil
                      :documentation "The original name of the source file, if source was copied iinto place for compilation.")
   (original-position :initarg :original-position
                      :initform 0)))

(defun make-source-file (filename)
  (make-instance 'source-file
    :filename filename))

(defun make-displaced-source-file (filename original-filename original-position)
  (make-instance 'source-file
    :filename filename
    :original-filename original-filename
    :original-position original-position))

(defmethod source-filename ((object source-file))
  (with-slots (filename original-filename) object
    (or original-filename filename)))

(defmethod source-stream ((object source-file))
  (open (slot-value object 'filename)))

(defclass source-string (source)
  ((string :initarg :string
           :reader source-string)))

(defun make-source-string (string)
  (make-instance 'source-string :string string))

(defmethod source-filename ((object source-string))
  "string input")

(defmethod source-stream ((object source-string))
  (make-string-input-stream (source-string object)))

(defmacro with-source-string ((stream string) &body body)
  `(let ((*source* (make-source-string ,string)))
     (with-open-stream (,stream (source-stream *source*))
       ,@body)))

(defmacro with-source-file ((stream filename) &body body)
  `(let ((*source* (make-source-file ,filename)))
     (with-open-stream (,stream (source-stream *source*))
       ,@body)))

(defmacro with-displaced-source-file ((stream filename original-filename position) &body body)
  `(let ((*source* (make-displaced-source-file ,filename ,original-filename ,position)))
     (with-open-stream (,stream (source-stream *source*))
       ,@body)))

(defmacro with-source-context ((key message) &body body)
  "Add MESSAGE to error context, deduplicating messages by KEY."
  `(let ((*source-context* (cons (cons ,key ,message) *source-context*)))
     ,@body))

(defun %reduce-context (context)
  (mapcar #'cdr
          (reduce (lambda (acc context)
                    (if (assoc (car context) acc)
                        acc
                        (cons context acc)))
                  context
                  :initial-value nil)))

(define-condition source-condition (condition)
  ((source :initform *source*)
   (context :initarg :context
            :initform *source-context*)
   (location :initarg :location
             :initform nil)
   (message :initarg :message
            :initform nil)
   (primary-note :initarg :primary-note
                 :initform nil)
   (notes :initarg :notes
          :initform nil
          :reader notes)
   (help :initarg :help-notes
         :initform nil))
  (:documentation "The type for user-facing errors.")
  (:report report-source-condition))

(define-condition source-error (source-condition error)
  ()
  (:documentation "The type for user-facing errors."))

(defmethod condition-severity ((condition source-error))
  :error)

(define-condition source-warning (source-condition style-warning)
  ()
  (:documentation "The type for user-facing warnings."))

(defmethod condition-severity ((condition source-warning))
  :warning)

(defclass source-annotation ()
  ((location :initarg :location
             :reader location)
   (message :initarg :message
            :reader note-message)))

(defclass note (source-annotation)
  ((type :initarg :type
         :reader note-type)))

(defun make-note (&key location type message)
  (make-instance 'note
    :location location
    :type type
    :message message))

(defclass help (source-annotation)
  ((replacement :initarg :replacement
                :reader note-replacement)))

(defun make-help (&key location replacement message)
  (make-instance 'help
    :location location
    :message message
    :replacement replacement))

;; error formatter
;;
;; `report-source-condition`, at the bottom, is a workhorse function that
;; takes a source condition, resolves source-relative locations
;; to the input source, and prints annotated source code.
;;
;; `printer-state` maintains state during printing: current line,
;; note depth, etc.

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
    (car (gethash (start-offset (car notes)) offset-positions))))

(defun last-line-number (notes offset-positions)
  (let ((last-offset (apply #'max (mapcar #'end-offset notes))))
    (car (gethash last-offset offset-positions))))

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
      (setf line-offsets (make-array (length offsets) :initial-contents offsets))
      (loop :for (char-offset line column) :in (find-column-offsets offsets char-offsets)
            :do (setf (gethash char-offset offset-positions)
                      (cons line column)))
      (setf current-line (1- (first-line-number notes offset-positions))
            last-line (last-line-number notes offset-positions)
            line-number-width (1+ (floor (log last-line 10)))
            note-max-depth (max-depth notes)))))


(defun start-offset (source-annotation)
  (with-slots (location) source-annotation
    (etypecase location
      (cons (car location))
      (integer (1- location)))))

(defun start-position (printer-state source-annotation)
  (with-slots (offset-positions) printer-state
    (gethash (start-offset source-annotation) offset-positions)))

(defun end-offset (source-annotation)
  (with-slots (location) source-annotation
    (etypecase location
      (cons (cdr location))
      (integer location))))

(defun end-position (printer-state source-annotation)
  (with-slots (offset-positions) printer-state
    (gethash (end-offset source-annotation) offset-positions)))

(defun location-lines (printer-state source-annotation)
  "Return the start and end lines of SOURCE-ANNOTATION"
  (with-slots (offset-positions) printer-state
    (values (car (start-position printer-state source-annotation))
            (car (gethash (end-offset source-annotation) offset-positions)))))

(defun location-positions (printer-state source-annotation)
  "Return the start and end positions of SOURCE-ANNOTATION"
  (with-slots (offset-positions) printer-state
    (destructuring-bind (start-line . start-column)
        (gethash (start-offset source-annotation) offset-positions)
      (destructuring-bind (end-line . end-column)
          (gethash (end-offset source-annotation) offset-positions)
        (values start-line start-column end-line end-column)))))

;; Mapping between character offsets and line and column positions.
;;
;; First line (zero indexed) = offset 0, etc.

(defun find-line-offsets (stream)
  "Compute the offsets of lines in a stream."
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
  (case (note-type note)
    (:primary #\^)
    (otherwise #\-)))

(defun write-nchar (char n stream)
  (dotimes (n n)
    (write-char char stream)))

;; Printer

(defun print-error-location (stream printer-state source-condition)
  (with-slots (source location message) source-condition
    (destructuring-bind (line . column)
        (offset-position printer-state location)
      (format stream "~(~A~): ~A~%  --> ~A:~D:~D~%"
              (condition-severity source-condition)
              message
              (source-filename source)
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

      ;; FIXME clean up the formats here and elsewhere

      (format stream "~v{~C~:*~} ~A~%"
              (- end-column start-column)
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
        (let ((replaced-text (funcall (note-replacement help)
                                      (subseq line start-column end-column))))
          (format stream "~A~A~%" replaced-text (subseq line end-column))
          (print-line-prefix stream printer-state)
          (format stream "~v{~C~:*~}~v{~C~:*~}~%"
                  (1+ start-column) '(#\Space)
                  (length replaced-text) '(#\-)))))))

(defun print-empty-line (stream printer-state)
  (print-line-prefix stream printer-state)
  (terpri stream))

(defun make-printer-state (source-stream condition)
  (with-slots (location primary-note notes help context) condition
    (make-instance 'printer-state
      :source-stream source-stream
      :notes (sort (list* (make-note :type :primary
                                     :location location
                                     :message primary-note)
                          notes)
                   #'< :key #'start-offset)
      :help help
      :context (%reduce-context context))))

(defun report-source-condition (condition stream)
  (with-slots (source) condition
    (with-open-stream (source-stream (source-stream source))
      (let ((state (make-printer-state source-stream condition)))
        (print-error-location stream state condition)
        (print-empty-line stream state)
        (with-slots (help context last-line) state
          (print-notes stream state)
          (loop :for help :in help
                :do (print-help stream state help))
          (loop :for context :in context
              :do (format stream "note: ~A~%" context)))))))
