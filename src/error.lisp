(defpackage #:coalton-impl/error
  (:use #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:coalton-internal-condition         ; CONDITION
   #:coalton-base-error                 ; CONDITION
   #:coalton-error-err                  ; ACCESSOR
   #:coalton-error-text                 ; ACCESSOR
   #:coalton-base-warning               ; CONDITION
   #:coalton-warning-err                ; ACCESSOR
   #:render-coalton-error               ; FUNCTION
   #:render-coalton-warning             ; FUNCTION
   #:coalton-internal-type-error        ; CONDITION
   #:coalton-file                       ; TYPE
   #:coalton-file-stream                ; ACCESSOR
   #:coalton-file-name                  ; ACCESSOR
   #:make-coalton-file                  ; FUNCTION
   #:make-coalton-error-note            ; FUNCTION
   #:make-coalton-error-help            ; FUNCTION
   #:make-coalton-error-context         ; FUNCTION
   #:*coalton-error-context*            ; VARIABLE
   #:coalton-error                      ; MACRO
   #:coalton-error-location             ; ACCESSOR
   #:coalton-error-file                 ; ACCESSOR
   #:display-coalton-error              ; FUNCTION
   ))

(in-package #:coalton-impl/error)


(define-condition coalton-internal-condition (error)
  ()
  (:documentation "An internal Coalton condition for use in signaling. Internal conditions should always be caught.")
  (:report (lambda (c s)
             (declare (ignore c))
             (format s "Unhandled internal condition.~%~%If you are seeing this, please file an issue on Github."))))

(define-condition coalton-base-error (error)
  ((err :accessor coalton-error-err
        :initarg :err
        :type function)
   (text :accessor coalton-error-text
         :initarg :text
         :initform nil
         :type (or null string)))
  (:documentation "The base type for user-facing errors. Only ERR needs to be specified, and TEXT will be filled when RENDER-COALTON-ERROR is called.")
  (:report (lambda (c s)
             (if (coalton-error-text c)
                 (write-string (coalton-error-text c) s)
                 (display-coalton-error s (coalton-error-err c))))))

(define-condition coalton-base-warning (style-warning)
  ((err :accessor coalton-warning-err
        :initarg :err
        :type function)
   (text :accessor coalton-warning-text
         :initarg :text
         :initform nil
         :type (or null string)))
    (:documentation "The base type for user-facing warnings. Only ERR needs to be specified, and TEXT will be filled when RENDER-COALTON-WARNING is called.")
    (:report (lambda (c s)
             (if (coalton-warning-text c)
                 (write-string (coalton-warning-text c) s)
                 (display-coalton-error s (coalton-warning-err c))))))

(defun render-coalton-error (e)
  "Render the error object within a COALTON-BASE-ERROR to text, removing the need to keep source file handles open."
  (declare (type coalton-base-error e))
  (let ((*print-escape* nil))
    (setf (coalton-error-text e)
          (with-output-to-string (s)
            (print-object e s))
          (coalton-error-err e)
          nil)))

(defun render-coalton-warning (w)
  "Render the error object within a COALTON-BASE-ERROR to text, removing the need to keep source file handles open."
  (declare (type coalton-base-warning w))
  (let ((*print-escape* nil))
    (setf (coalton-warning-text w)
          (with-output-to-string (s)
            (print-object w s))
          (coalton-warning-err w)
          nil)))


(define-condition coalton-internal-type-error (coalton-internal-condition)
  ())

;;;
;;; Error Rendering
;;;

(defstruct (coalton-error-note
            (:copier nil))
  (type    (util:required 'type)    :type (member :primary :secondary) :read-only t)
  (span    (util:required 'span)    :type (cons integer integer)       :read-only t)
  (message (util:required 'message) :type string                       :read-only t))

(defun coalton-error-note-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'coalton-error-note-p x)))

(deftype coalton-error-note-list ()
  '(satisfies coalton-error-note-list-p))


(defstruct (coalton-error-help
            (:copier nil))
  (span        (util:required 'span)        :type (cons integer integer)     :read-only t)
  (replacement (util:required 'replacement) :type (function (string) string) :read-only t)
  (message     (util:required 'message)     :type string                     :read-only t))

(defun coalton-error-help-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'coalton-error-help-p x)))

(deftype coalton-error-help-list ()
  '(satisfies coalton-error-help-list-p))


(defstruct (coalton-error-context
            (:copier nil))
  (message (util:required 'message) :type string :read-only t))

(defvar *coalton-error-context* nil)

(defstruct (coalton-file
            (:copier nil))
  (stream (util:required 'stream) :type stream :read-only t)
  (name   (util:required 'file)   :type string :read-only t))

(defstruct (coalton-error
            (:copier nil))
  (type            (util:required 'type)     :type (member :error :warn)   :read-only t)
  (file            (util:required 'file)     :type coalton-file            :read-only t)
  (location        (util:required 'location) :type integer                 :read-only t)
  (message         (util:required 'message)  :type string                  :read-only t)
  (notes           (util:required 'notes)    :type coalton-error-note-list :read-only t)
  (help-notes      nil                       :type coalton-error-help-list :read-only t)
  (context         *coalton-error-context*   :type list                    :read-only t))

(defmacro coalton-error (&key (type :error) span file (highlight :all) message primary-note notes help-notes)
  `(lambda ()
     (coalton-error_
      :type ,type
      :span ,span
      :file ,file
      :highlight ,highlight
      :message ,message
      :primary-note ,primary-note
      :notes ,notes
      :help-notes ,help-notes)))

(defun coalton-error_ (&key
                         (type :error)
                         span
                         file
                         (highlight :all)
                         message
                         primary-note
                         notes
                         help-notes)
  "Construct a COALTON-ERROR with a message and primary note attached to the provided form.

MESSAGE and PRIMARY-NOTE must be supplied string arguments.
NOTES and HELP-NOTES may optionally be supplied notes and help messages."
  (declare (type cons span)
           (type coalton-file file)
           (type (member :all :end) highlight)
           (type string message primary-note)
           (type list notes help-notes)
           (values coalton-error &optional))

  (let ((start (car span))
        (end (cdr span)))
    (make-coalton-error
     :type type
     :file file
     :location (ecase highlight
                 (:all (car span))
                 (:end (cdr span)))
     :message message
     :notes (list*
             (ecase highlight
               (:all
                (make-coalton-error-note
                 :type :primary
                 :span (cons start end)
                 :message primary-note))
               (:end
                (make-coalton-error-note
                 :type :primary
                 :span (cons (1- end) end)
                 :message primary-note)))
             notes)
     :help-notes help-notes)))

(defstruct (coalton-error-resolved-note
            (:copier nil))
  "A COALTON-ERROR-NOTE with its location resolved in the file's context."
  (type         (util:required 'type)         :type (member :primary :secondary) :read-only t)
  (start-line   (util:required 'start-line)   :type integer                      :read-only t)
  (start-column (util:required 'start-column) :type integer                      :read-only t)
  (end-line     (util:required 'end-line)     :type integer                      :read-only t)
  (end-column   (util:required 'end-column)   :type integer                      :read-only t)
  (message      (util:required 'message)      :type string                       :read-only t))

(defun display-coalton-error (stream error)
  (declare (type stream stream)
           (type function error))

  (let* ((*print-circle* nil)

         (error (funcall error))

         (file-stream (coalton-file-stream (coalton-error-file error))))

    (progn

      ;; Print the error message and location
      (multiple-value-bind (line-number line-start-index)
          (get-line-from-index file-stream (coalton-error-location error))
        (format stream
                "~(~A~): ~A~%  --> ~A:~D:~D~%"
                (coalton-error-type error)
                (coalton-error-message error)
                (coalton-file-name (coalton-error-file error))
                line-number
                (- (coalton-error-location error) line-start-index)))

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
                (coalton-error-notes error)
                #'<
                :key (lambda (note)
                       (car (coalton-error-note-span note)))))

             ;; Attach line info to notes and figure out the maximum
             ;; multiline note depth we will need.
             (resolved-notes
               (mapcar
                (lambda (note)
                  (let ((start (car (coalton-error-note-span note)))
                        (end (cdr (coalton-error-note-span note))))
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
                            (util:coalton-bug "Error note contains invalid span ~A:~A to ~A:~A"
                                              start-line start-column end-line end-column))

                          ;; Clear any multiline notes in the stack that we have now passed.
                          (loop :while (and (car multiline-note-stack)
                                            (> start-line
                                               (coalton-error-resolved-note-end-line
                                                (car multiline-note-stack))))
                                :do (pop multiline-note-stack)
                                    (decf multiline-note-current-depth))

                          (let ((resolved-note (make-coalton-error-resolved-note
                                                :type (coalton-error-note-type note)
                                                :start-line start-line
                                                :start-column start-column
                                                :end-line end-line
                                                :end-column end-column
                                                :message (coalton-error-note-message note))))

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
             (first-line (coalton-error-resolved-note-start-line (car resolved-notes)))
             (last-line (reduce #'max resolved-notes :key #'coalton-error-resolved-note-end-line))
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
                                (>= (coalton-error-resolved-note-end-line note)
                                    line-number))
                              multiline-note-stack)))

                   (print-line-contents (line-number)
                     (print-line-number line-number t)
                     (format stream " ~v@{ ~}~A~%"
                             (- multiline-note-max-depth
                                multiline-note-current-depth)
                             (get-nth-line file-stream line-number)))

                   (note-highlight-char (note)
                     (ecase (coalton-error-resolved-note-type note)
                       (:primary #\^)
                       (:secondary #\-)))

                   (print-singleline-note (note)
                     (print-line-number (coalton-error-resolved-note-start-line note) nil)
                     (format stream
                             " ~v{~C~:*~}~v{~C~:*~} ~A~%"
                             (+ (coalton-error-resolved-note-start-column note)
                                (- multiline-note-max-depth
                                   multiline-note-current-depth))
                             '(#\Space)
                             (- (coalton-error-resolved-note-end-column note)
                                (coalton-error-resolved-note-start-column note))
                             (list (note-highlight-char note))
                             (coalton-error-resolved-note-message note)))

                   (print-multiline-note-start (note)
                     (print-line-number (coalton-error-resolved-note-start-line note) nil)
                     (format stream " ~v{~C~:*~}~C~%"
                             (+ (coalton-error-resolved-note-start-column note)
                                (- multiline-note-max-depth
                                   multiline-note-current-depth))
                             '(#\_)
                             (note-highlight-char note)))

                   (print-multiline-note-end (note)
                     (print-line-number (coalton-error-resolved-note-start-line note) nil)
                     (format stream
                             "~v{~C~:*~}~C ~A~%"
                             (+ (coalton-error-resolved-note-end-column note)
                                (- multiline-note-max-depth
                                   multiline-note-current-depth))
                             '(#\_)
                             (note-highlight-char note)
                             (coalton-error-resolved-note-message note)))

                   (print-finished-multiline-notes-for-line (line-number)
                     ;; Check if there are any multiline notes that need to be printed
                     (loop :for stack-head := multiline-note-stack :then (cdr stack-head)
                           :for note := (car stack-head)

                           :when (null stack-head)
                             :do (return)

                           :when (= line-number (coalton-error-resolved-note-end-line note))
                             :do (print-multiline-note-end note)

                           :when (and (eq note (car multiline-note-stack))
                                      (>= line-number (coalton-error-resolved-note-end-line note)))
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
                                  :when (< current-line (coalton-error-resolved-note-end-line note) line-number)
                                    :do (print-lines-until (coalton-error-resolved-note-end-line note)))

                            (format stream " ...~%")
                            (print-line-contents (1- line-number))
                            (print-line-contents line-number)))
                     (setf current-line line-number)))

            ;; Walk down our notes, printing lines and notes as needed
            ;; and keeping track of multiline note depth.
            (loop :for note :in resolved-notes
                  :do (cond
                        ;; For multiline we need to add to the current stack.
                        ((/= (coalton-error-resolved-note-start-line note)
                             (coalton-error-resolved-note-end-line note))
                         ;; Print lines until this note.
                         (print-lines-until (coalton-error-resolved-note-start-line note))

                         ;; Print out a new row with underlines connecting
                         ;; back to the multiline position.
                         (print-multiline-note-start note)

                         ;; Push this note on to the stack for safe keeping.
                         (push note multiline-note-stack)
                         (incf multiline-note-current-depth))
                        ;; For non-multiline just print the note
                        (t
                         ;; Print lines until this note.
                         (print-lines-until (coalton-error-resolved-note-start-line note))

                         (print-singleline-note note)

                         (print-finished-multiline-notes-for-line (coalton-error-resolved-note-start-line note)))))

            ;; If there are any multline notes that have not closed out then
            ;; print them.
            (print-lines-until last-line)
            (print-finished-multiline-notes-for-line last-line))))

      ;; Print help messages
      (loop :for help :in (coalton-error-help-notes error)
            :for start := (car (coalton-error-help-span help))
            :for end := (cdr (coalton-error-help-span help))
            :do (multiple-value-bind (start-line start-line-start)
                    (get-line-from-index file-stream start)
                  (multiple-value-bind (end-line end-line-start)
                      (get-line-from-index file-stream end)

                    (unless (= start-line end-line)
                      (util:coalton-bug "multiline help messages not supported yet."))

                    (let ((line-number-width (1+ (floor (log end-line 10)))))
                      (format stream "help: ~A~%"
                              (coalton-error-help-message help))

                      (format stream " ~vD | ~A"
                              line-number-width
                              start-line
                              (subseq (get-nth-line file-stream start-line)
                                      0 (- start start-line-start)))

                      (let ((replaced-text (funcall (coalton-error-help-replacement help)
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
      (loop :for context :in (coalton-error-context error)
            :do (format stream "note: ~A~%" (coalton-error-context-message context))))))

(defun get-line-from-index (file index)
  "Get the line number corresponding to the character offset INDEX.

Returns (VALUES LINE-NUM LINE-START-INDEX)"
  (declare (type stream file)
           (type integer index)
           (values integer integer))
  (file-position file 0)
  (let ((line 1)
        (line-start-index 0))
    (loop :for i :to index
          :for char := (read-char file)
          :when (char= char #\Newline)
            :do (setf line (1+ line)
                      line-start-index (1+ i)))
    (values line line-start-index)))

(defun get-source-line-info (file form)
  "Get source information about FORM which can be used in errors.

Returns (VALUES LINE-NUM LINE-START-INDEX LINE-END-INDEX)"
  (declare (type stream file)
           (type cst:cst form))
  (let ((start-index (car (cst:source form)))
        (end-index   (cdr (cst:source form))))
    (multiple-value-bind (line-number line-start-index)
        (get-line-from-index file start-index)
      (values line-number
              (- start-index line-start-index)
              (- end-index   line-start-index)))))

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

  (util:coalton-bug "Line number ~D out of bounds for file ~A" index file))
