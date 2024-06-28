(defpackage #:source-error/error
  (:shadow
   #:file
   #:file-stream)
  (:use
   #:cl)
  (:export
   #:source-base-error                  ; CONDITION
   #:source-base-error-err              ; ACCESSOR
   #:source-base-error-text             ; ACCESSOR
   #:source-base-warning                ; CONDITION
   #:source-base-warning-err            ; ACCESSOR
   #:source-base-warning-text           ; ACCESSOR
   #:render-source-error                ; FUNCTION
   #:render-source-warning              ; FUNCTION
   #:file                               ; TYPE
   #:make-file                          ; CONSTRUCTOR
   #:file-stream                        ; ACCESSOR
   #:file-name                          ; ACCESSOR
   #:make-source-error-note             ; FUNCTION
   #:make-source-error-help             ; FUNCTION
   #:make-source-error-context          ; FUNCTION
   #:*source-error-context*             ; VARIABLE
   #:source-error                       ; MACRO
   #:source-error-location              ; ACCESSOR
   #:source-error-file                  ; ACCESSOR
   #:display-source-error               ; FUNCTION
   #:define-source-condition            ; MACRO
   ))

(in-package #:source-error/error)

(define-condition source-base-error (error)
  ((err :accessor source-base-error-err
        :initarg :err
        :type (or null function))
   (text :accessor source-base-error-text
         :initarg :text
         :initform nil
         :type (or null string)))
  (:documentation "The base type for user-facing errors. Only ERR needs to be specified, and TEXT will be filled when `RENDER-ERROR' is called.")
  (:report (lambda (c s)
             (if (source-base-error-text c)
                 (write-string (source-base-error-text c) s)
                 (display-source-error s (source-base-error-err c))))))

(define-condition source-base-warning (style-warning)
  ((err :accessor source-base-warning-err
        :initarg :err
        :type (or null function))
   (text :accessor source-base-warning-text
         :initarg :text
         :initform nil
         :type (or null string)))
    (:documentation "The base type for user-facing warnings. Only ERR needs to be specified, and TEXT will be filled when `RENDER-WARNING' is called.")
    (:report (lambda (c s)
             (if (source-base-warning-text c)
                 (write-string (source-base-warning-text c) s)
                 (display-source-error s (source-base-warning-err c))))))

(defun render-source-error (e)
  "Render the error object within a `SOURCE-BASE-ERROR' to text, removing the need to keep source file handles open."
  (declare (type source-base-error e))
  (let ((*print-escape* nil))
    (setf (source-base-error-text e)
          (with-output-to-string (s)
            (print-object e s))
          (source-base-error-err e)
          nil)))

(defun render-source-warning (w)
  "Render the error object within a `SOURCE-BASE-ERROR' to text, removing the need to keep source file handles open."
  (declare (type source-base-warning w))
  (let ((*print-escape* nil))
    (setf (source-base-warning-text w)
          (with-output-to-string (s)
            (print-object w s))
          (source-base-warning-err w)
          nil)))

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

(defstruct (file
            (:copier nil))
  (stream nil :type stream :read-only t)
  (name   nil :type string :read-only t))

(defstruct (source-error
            (:copier nil))
  (type       nil :type (member :error :warn)     :read-only t)
  (file       nil :type file                      :read-only t)
  (location   nil :type integer                   :read-only t)
  (message    nil :type string                    :read-only t)
  (notes      nil :type source-error-note-list    :read-only t)
  (help-notes nil :type source-error-help-list    :read-only t)
  (context    nil :type source-error-context-list :read-only t))

(defmacro source-error (&key (type :error) span file (highlight :all) message primary-note notes help-notes)
  `(let ((ctx *source-error-context*))
     (lambda ()
       (source-error%
        :type ,type
        :span ,span
        :file ,file
        :highlight ,highlight
        :message ,message
        :primary-note ,primary-note
        :notes ,notes
        :help-notes ,help-notes
        :context ctx))))

(defun source-error% (&key
                        (type :error)
                        span
                        file
                        (highlight :all)
                        message
                        primary-note
                        notes
                        help-notes
                        context)
  "Construct a `SOURCE-ERROR' with a message and primary note attached to the provided form."
  (declare (type cons span)
           (type file file)
           (type (member :all :end) highlight)
           (type string message)
           (type string primary-note)
           (type source-error-note-list notes)
           (type source-error-help-list help-notes)
           (type source-error-context-list context)
           (values source-error))

  (let ((start (car span))
        (end (cdr span)))
    (make-source-error
     :type type
     :file file
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
           (type function error))

  (let* ((*print-circle* nil)

         (error (funcall error))

         (file-stream (file-stream (source-error-file error)))
         (file-stream-pos (file-position file-stream)))

    (progn

      ;; Print the error message and location
      (multiple-value-bind (line-number line-start-index)
          (get-line-from-index file-stream (source-error-location error))

        ;; get-line-from-index sometimes returns a line-start-index
        ;; that is 1 larger than (coalton-error-location error),
        ;; resulting in a column number of -1. The following lines
        ;; compensate for that and can be removed if and when that is
        ;; fixed.
        (let ((column-number (- (source-error-location error) line-start-index)))
          (when (< column-number 0)
            (decf line-number)
            (setf column-number (1- (length (get-nth-line file-stream line-number)))))

          (format stream
                  "~(~A~): ~A~%  --> ~A:~D:~D~%"
                  (source-error-type error)
                  (source-error-message error)
                  (file-name (source-error-file error))
                  line-number
                  column-number)))

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
                        (get-line-from-index file-stream start)

                      ;; Get line info for the end of the span
                      (multiple-value-bind (end-line end-line-start)
                          (get-line-from-index file-stream (1- end))

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
                             (get-nth-line file-stream line-number)))

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
                    (get-line-from-index file-stream start)
                  (multiple-value-bind (end-line end-line-start)
                      (get-line-from-index file-stream (1- end))

                    (unless (= start-line end-line)
                      (error "multiline help messages not supported yet."))

                    (let ((line-number-width (1+ (floor (log end-line 10)))))
                      (format stream "help: ~A~%"
                              (source-error-help-message help))

                      (format stream " ~vD | ~A"
                              line-number-width
                              start-line
                              (subseq (get-nth-line file-stream start-line)
                                      0 (- start start-line-start)))

                      (let ((replaced-text (funcall (source-error-help-replacement help)
                                                    (subseq (get-nth-line file-stream start-line)
                                                            (- start start-line-start)
                                                            (- end end-line-start)))))
                        (format stream "~A~A~%"
                                replaced-text
                                (subseq (get-nth-line file-stream start-line)
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
            :do (format stream "note: ~A~%" (source-error-context-message context)))

      ;; Reset our file position to avoid messing things up.
      (file-position file-stream file-stream-pos))))

(defun get-line-from-index (file index)
  "Get the line number corresponding to the character offset INDEX.

Returns (VALUES LINE-NUM LINE-START-INDEX)"
  (declare (type stream file)
           (type integer index)
           (values integer integer))
  (file-position file 0)
  (let ((line 1)
        (line-start-index 0))
    (loop :for char := (read-char file)
          :for code := (char-code char)
          ;; It is assumed that code is utf-8 code point.
          :for length := (cond ((<= code #x7f) 1)
                               ((<= code #x7ff) 2)
                               ((<= code #xffff) 3)
                               (t 4))
          :for i := 0 :then (+ i length)
          :when (char= char #\Newline)
            :do (setf line (1+ line)
                      line-start-index (1+ i))
          :when (>= i index)
            :return nil)
    (values line line-start-index)))

(defun get-nth-line (file index)
  "Get the INDEXth line FILE. This function uses 1 based indexing."
  (declare (type stream file)
           (type integer index)
           (values string &optional))
  (file-position file 0)
  (loop :for i :from 1 :to index
        :for line := (read-line file)
        :when (= i index)
          :do (return-from get-nth-line line))

  (error "Line number ~D out of bounds for file ~A" index file))
