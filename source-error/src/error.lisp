(defpackage #:source-error/error
  (:use
   #:cl)
  (:export
   #:source-base-error                  ; CONDITION
   #:source-condition-err               ; ACCESSOR
   #:source-base-warning                ; CONDITION
   #:source-available-p                 ; GENERIC
   #:source-stream                      ; GENERIC
   #:source-name                        ; GENERIC
   #:make-source-error-note             ; FUNCTION
   #:make-source-error-help             ; FUNCTION
   #:make-source-error-context          ; FUNCTION
   #:*source-error-context*             ; VARIABLE
   #:source-error                       ; MACRO
   #:source-error-location              ; ACCESSOR
   #:source-error-source                ; ACCESSOR
   #:display-source-error               ; FUNCTION
   ))

(in-package #:source-error/error)

(define-condition source-condition ()
  ((err :accessor source-condition-err
        :initarg :err))
  (:report (lambda (c s)
             (display-source-error s (source-condition-err c)))))

(define-condition source-base-error (source-condition error)
  ()
  (:documentation "The base type for user-facing errors."))

(define-condition source-base-warning (source-condition style-warning)
  ()
  (:documentation "The base type for user-facing warnings."))

(defstruct (source-error-note
            (:copier nil))
  (type    nil :type (member :primary :secondary) :read-only t)
  (span    nil :type (cons integer integer)       :read-only t)
  (message nil :type string                       :read-only t))

(defun source-error-note-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'source-error-note-p x)))

(deftype source-error-note-list ()
  '(satisfies source-error-note-list-p))

(defstruct (source-error-help
            (:copier nil))
  (span        nil :type (cons integer integer)     :read-only t)
  (replacement nil :type (function (string) string) :read-only t)
  (message     nil :type string                     :read-only t))

(defun source-error-help-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'source-error-help-p x)))

(deftype source-error-help-list ()
  '(satisfies source-error-help-list-p))

(defstruct (source-error-context
            (:copier nil))
  (message nil :type string :read-only t))

(defun source-error-context-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'source-error-context-p x)))

(deftype source-error-context-list ()
  '(satisfies source-error-context-list-p))

(defvar *source-error-context* nil)

;;; An object that implements #'source-name and #'source-stream
;;; provides access to a source-error condition's source text and
;;; metadata about its origin so that the condition printer can
;;; decorate that text with notes and line numbers.

(defgeneric source-stream (source)
  (:documentation "Open and return a stream from which source text may be read. The caller is responsible for closing the stream, and the stream's initial position may be greater than zero."))

(defgeneric source-available-p (source)
  (:documentation "Return T if a stream containing SOURCE's source text can be opened."))

(defgeneric source-name (source)
  (:documentation "The name of an error's source, suitable for reporting in errors. If the source is a file, SOURCE-NAME will be that file's absolute path."))

(defstruct (source-error
            (:constructor %make-source-error))
  (type       nil :type (member :error :warn)     :read-only t)
  (source     nil                                 :read-only t)
  (location   nil :type integer                   :read-only t)
  (message    nil :type string                    :read-only t)
  (notes      nil :type source-error-note-list    :read-only t)
  (help-notes nil :type source-error-help-list    :read-only t)
  (context    nil :type source-error-context-list :read-only t))

(defun make-source-error (&key
                            (type :error)
                            span
                            source
                            (highlight :all)
                            message
                            primary-note
                            notes
                            help-notes
                            context)
  "Construct a `SOURCE-ERROR' with a message and primary note attached to the provided form."
  (declare (type cons span)
           (type (member :all :end) highlight)
           (type string message)
           (type string primary-note)
           (type source-error-note-list notes)
           (type source-error-help-list help-notes)
           (type source-error-context-list context)
           (values source-error))
  (let ((start (car span))
        (end (cdr span)))
    (%make-source-error :type type
                        :source source
                        :location (ecase highlight
                                    (:all (car span))
                                    (:end (cdr span)))
                        :message message
                        :notes (list*
                                (ecase highlight
                                  (:all
                                   (make-source-error-note
                                    :type :primary
                                    :span (cons start end)
                                    :message primary-note))
                                  (:end
                                   (make-source-error-note
                                    :type :primary
                                    :span (cons (1- end) end)
                                    :message primary-note)))
                                notes)
                        :help-notes help-notes
                        :context context)))

(defmacro source-error (&key (type :error) span source (highlight :all) message primary-note notes help-notes)
  `(make-source-error :type ,type
                      :span ,span
                      :source ,source
                      :highlight ,highlight
                      :message ,message
                      :primary-note ,primary-note
                      :notes ,notes
                      :help-notes ,help-notes
                      :context *source-error-context*))

(defstruct (source-error-resolved-note
            (:copier nil))
  "A `SOURCE-ERROR-NOTE' with its location resolved in the file's context."
  (type         nil :type (member :primary :secondary) :read-only t)
  (start-line   nil :type integer                      :read-only t)
  (start-column nil :type integer                      :read-only t)
  (end-line     nil :type integer                      :read-only t)
  (end-column   nil :type integer                      :read-only t)
  (message      nil :type string                       :read-only t))

(defun display-source-error (stream error)
  (declare (type stream stream)
           (type source-error error))

  (let ((*print-circle* nil)
        (source (source-error-source error)))
    (unless (source-available-p source)
      (format stream
              "~(~A~): ~A~%  --> ~A (source unavailable)~%"
              (source-error-type error)
              (source-error-message error)
              (source-name (source-error-source error)))
      (return-from display-source-error nil))

    (with-open-stream (source-stream (source-stream source))

      ;; Print the error message and location
      (multiple-value-bind (line-number line-start-index)
          (get-line-from-index source-stream (source-error-location error))

        (format stream
                "~(~A~): ~A~%  --> ~A:~D:~D~%"
                (source-error-type error)
                (source-error-message error)
                (source-name (source-error-source error))
                line-number
                (- (source-error-location error) line-start-index)))

      ;; Print the error notes
      (let* (;; We need to keep track of the current depth of multiline
             ;; notes so that we can pad the left with the correct
             ;; number of columns.
             (multiline-note-stack nil)
             (multiline-note-current-depth 0)
             (multiline-note-max-depth 0)

             ;; Sort notes by start of span
             (sorted-notes
               (stable-sort
                (source-error-notes error)
                #'<
                :key (lambda (note)
                       (car (source-error-note-span note)))))

             ;; Attach line info to notes and figure out the maximum
             ;; multiline note depth we will need.
             (resolved-notes
               (mapcar
                (lambda (note)
                  (let ((start (car (source-error-note-span note)))
                        (end (cdr (source-error-note-span note))))
                    ;; Get line info for the start of the span
                    (multiple-value-bind (start-line start-line-start)
                        (get-line-from-index source-stream start)

                      ;; Get line info for the end of the span
                      (multiple-value-bind (end-line end-line-start)
                          (get-line-from-index source-stream (1- end))

                        ;; Compute column numbers
                        (let ((start-column (- start start-line-start))
                              (end-column   (- end end-line-start)))

                          ;; Ensure that spans are valid
                          (when (or (< end-line start-line)
                                    (and (= end-line start-line)
                                         (< end-column start-column)))
                            (error "Error note contains invalid span ~A:~A to ~A:~A"
                                   start-line start-column end-line end-column))

                          ;; Clear any multiline notes in the stack that we have now passed.
                          (loop :while (and (car multiline-note-stack)
                                            (> start-line
                                               (source-error-resolved-note-end-line
                                                (car multiline-note-stack))))
                                :do (pop multiline-note-stack)
                                    (decf multiline-note-current-depth))

                          (let ((resolved-note (make-source-error-resolved-note
                                                :type (source-error-note-type note)
                                                :start-line start-line
                                                :start-column start-column
                                                :end-line end-line
                                                :end-column end-column
                                                :message (source-error-note-message note))))

                            ;; If this is a multiline note then keep track of the current depth.
                            (when (/= end-line start-line)
                              (push resolved-note multiline-note-stack)
                              (incf multiline-note-current-depth)

                              (if (> multiline-note-current-depth
                                     multiline-note-max-depth)
                                  (setf multiline-note-max-depth
                                        multiline-note-current-depth)))

                            resolved-note))))))
                sorted-notes))

             ;; Get the character width of the last line mentioned.
             (first-line (source-error-resolved-note-start-line (car resolved-notes)))
             (last-line (reduce #'max resolved-notes :key #'source-error-resolved-note-end-line))
             (line-number-width
               (1+ (floor (log last-line 10)))))

        ;; Ensure the multiline stack is empty so we can reuse it when printing notes.
        (setf multiline-note-stack nil
              multiline-note-current-depth 0)

        ;; Print first empty line.
        (format stream
                " ~v{~C~:*~} |~%"
                line-number-width
                '(#\Space))

        (let (;; Keep track of which lines we have output.  We start at
              ;; one line before the beginning to ensure the first line
              ;; is printed correctly.
              (current-line (1- first-line)))
          (labels ((print-line-number (line-number show-line-number)
                     (format stream
                             " ~:[~vA~*~*~;~*~*~v{~C~:*~}~] | ~{~:[ ~;|~]~}"
                             (not show-line-number)
                             line-number-width
                             line-number
                             line-number-width
                             '(#\Space)
                             (mapcar
                              (lambda (note)
                                (>= (source-error-resolved-note-end-line note)
                                    line-number))
                              multiline-note-stack)))

                   (print-line-contents (line-number)
                     (print-line-number line-number t)
                     (format stream " ~v@{ ~}~A~%"
                             (- multiline-note-max-depth
                                 multiline-note-current-depth)
                             (get-nth-line source-stream line-number)))

                   (note-highlight-char (note)
                     (ecase (source-error-resolved-note-type note)
                       (:primary #\^)
                       (:secondary #\-)))

                   (print-singleline-note (note)
                     (print-line-number (source-error-resolved-note-start-line note) nil)
                     (format stream
                             " ~v{~C~:*~}~v{~C~:*~} ~A~%"
                             (+ (source-error-resolved-note-start-column note)
                                 (- multiline-note-max-depth
                                     multiline-note-current-depth))
                             '(#\Space)
                             (- (source-error-resolved-note-end-column note)
                                 (source-error-resolved-note-start-column note))
                             (list (note-highlight-char note))
                             (source-error-resolved-note-message note)))

                   (print-multiline-note-start (note)
                     (print-line-number (source-error-resolved-note-start-line note) nil)
                     (format stream " ~v{~C~:*~}~C~%"
                             (+ (source-error-resolved-note-start-column note)
                                 (- multiline-note-max-depth
                                     multiline-note-current-depth))
                             '(#\_)
                             (note-highlight-char note)))

                   (print-multiline-note-end (note)
                     (print-line-number (source-error-resolved-note-start-line note) nil)
                     (format stream
                             "~v{~C~:*~}~C ~A~%"
                             (+ (source-error-resolved-note-end-column note)
                                 (- multiline-note-max-depth
                                     multiline-note-current-depth))
                             '(#\_)
                             (note-highlight-char note)
                             (source-error-resolved-note-message note)))

                   (print-finished-multiline-notes-for-line (line-number)
                     ;; Check if there are any multiline notes that need to be printed
                     (loop :for stack-head := multiline-note-stack :then (cdr stack-head)
                           :for note := (car stack-head)

                           :when (null stack-head)
                             :do (return)

                           :when (= line-number (source-error-resolved-note-end-line note))
                             :do (print-multiline-note-end note)

                           :when (and (eq note (car multiline-note-stack))
                                      (>= line-number (source-error-resolved-note-end-line note)))
                             :do (pop multiline-note-stack)
                                 (decf multiline-note-current-depth)))

                   (print-lines-until (line-number)
                     (cond (;; If we are on the same line then
                            ;; don't reprint.
                            (= line-number current-line))

                           (;; If we are within 3 lines of the
                            ;; previous one then just print those
                            ;; lines.
                            (>= 3 (- line-number current-line))
                            (loop :for line :from current-line :below line-number
                                  :do (print-line-contents (1+ line))
                                  :unless (= (1+ line) line-number)
                                    :do (print-finished-multiline-notes-for-line (1+ line))))

                           (;; Otherwise split the output.
                            t
                            (print-line-contents (1+ current-line))

                            ;; Print out any intermediate multiline note endings.
                            (loop :for note :in multiline-note-stack
                                  :when (< current-line (source-error-resolved-note-end-line note) line-number)
                                    :do (print-lines-until (source-error-resolved-note-end-line note)))

                            (format stream " ...~%")
                            (print-line-contents (1- line-number))
                            (print-line-contents line-number)))
                     (setf current-line line-number)))

            ;; Walk down our notes, printing lines and notes as needed
            ;; and keeping track of multiline note depth.
            (loop :for note :in resolved-notes
                  :do (cond
                        ;; For multiline we need to add to the current stack.
                        ((/= (source-error-resolved-note-start-line note)
                             (source-error-resolved-note-end-line note))
                         ;; Print lines until this note.
                         (print-lines-until (source-error-resolved-note-start-line note))

                         ;; Print out a new row with underlines connecting
                         ;; back to the multiline position.
                         (print-multiline-note-start note)

                         ;; Push this note on to the stack for safe keeping.
                         (push note multiline-note-stack)
                         (incf multiline-note-current-depth))
                        ;; For non-multiline just print the note
                        (t
                         ;; Print lines until this note.
                         (print-lines-until (source-error-resolved-note-start-line note))

                         (print-singleline-note note)

                         (print-finished-multiline-notes-for-line (source-error-resolved-note-start-line note)))))

            ;; If there are any multiline notes that have not closed out then
            ;; print them.
            (print-lines-until last-line)
            (print-finished-multiline-notes-for-line last-line))))

      ;; Print help messages
      (loop :for help :in (source-error-help-notes error)
            :for start := (car (source-error-help-span help))
            :for end := (cdr (source-error-help-span help))
            :do (multiple-value-bind (start-line start-line-start)
                    (get-line-from-index source-stream start)
                  (multiple-value-bind (end-line end-line-start)
                      (get-line-from-index source-stream (1- end))

                    (unless (= start-line end-line)
                      (error "multiline help messages not supported yet."))

                    (let ((line-number-width (1+ (floor (log end-line 10)))))
                      (format stream "help: ~A~%"
                              (source-error-help-message help))

                      (format stream " ~vD | ~A"
                              line-number-width
                              start-line
                              (subseq (get-nth-line source-stream start-line)
                                      0 (- start start-line-start)))

                      (let ((replaced-text (funcall (source-error-help-replacement help)
                                                    (subseq (get-nth-line source-stream start-line)
                                                            (- start start-line-start)
                                                            (- end end-line-start)))))
                        (format stream "~A~A~%"
                                replaced-text
                                (subseq (get-nth-line source-stream start-line)
                                        (- end end-line-start)))

                        (format stream
                                " ~v{~C~:*~} |~v{~C~:*~}~v{~C~:*~}~%"
                                line-number-width
                                '(#\Space)
                                (1+ (- start start-line-start))
                                '(#\Space)
                                (length replaced-text)
                                '(#\-)))))))

      ;; Print error context
      (loop :for context :in (source-error-context error)
            :do (format stream "note: ~A~%" (source-error-context-message context))))))

(defun get-line-from-index (stream index)
  "Get the line number corresponding to the character offset INDEX.

Returns (VALUES LINE-NUM LINE-START-INDEX)"
  (declare (type stream stream)
           (type integer index)
           (values integer integer))
  (file-position stream 0)
  (loop :with line-num := 1
        :with line-start-index := 0
        :for char := (read-char stream nil nil)
        :for char-index :from 0
        :when (= index char-index)
          :return (values line-num line-start-index)
        :when (null char)
          :do (error "Index ~D out of bounds for stream ~A" char-index stream)
        :when (char= char #\Newline)
          :do (incf line-num)
              (setf line-start-index (1+ char-index))))

(defun get-nth-line (stream index)
  "Get the INDEXth line in STREAM. This function uses 1 based indexing."
  (declare (type stream stream)
           (type integer index)
           (values string &optional))
  (file-position stream 0)
  (loop :for i :from 1 :to index
        :for line := (read-line stream)
        :when (= i index)
          :do (return-from get-nth-line line))

  (error "Line number ~D out of bounds for stream ~A" index stream))
