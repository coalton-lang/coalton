(defpackage #:coalton-impl/parser/base
  (:use
   #:cl)
  (:shadow
   #:parse-error)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:identifier                         ; TYPE
   #:identifierp                        ; FUNCTION
   #:get-line-from-index                ; FUNCTION
   #:get-source-line-info               ; FUNCTION
   #:get-nth-line                       ; FUNCTION
   #:coalton-file                       ; TYPE
   #:coalton-file-stream
   #:make-coalton-file                  ; FUNCTION
   #:make-coalton-error-note            ; FUNCTION
   #:make-coalton-error-help            ; FUNCTION
   #:make-coalton-error-context         ; FUNCTION
   #:make-coalton-error                 ; FUNCTION
   #:*coalton-error-context*            ; VARIABLE
   #:coalton-error                      ; FUNCTION
   #:coalton-error-location
   #:coalton-error-file
   #:display-coalton-error              ; FUNCTION
   #:parse-error                        ; CONDITION
   #:parse-error-err
   ))

(in-package #:coalton-impl/parser/base)

(deftype identifier ()
  '(and symbol (not boolean) (not keyword)))

(defun identifierp (x)
  (typep x 'identifier))

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

;; TODO: names here are chaotic
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

(defstruct (coalton-error-note
            (:copier nil))
  (type    (util:required 'type)    :type (member :primary :secondary) :read-only t)
  (span    (util:required 'span)    :type (cons integer integer)       :read-only t)
  (message (util:required 'message) :type string                       :read-only t))

(defstruct (coalton-error-help
            (:copier nil))
  (span        (util:required 'span)        :type (cons integer integer)     :read-only t)
  (replacement (util:required 'replacement) :type (function (string) string) :read-only t)
  (message     (util:required 'message)     :type string                     :read-only t))

(defstruct (coalton-error-context
            (:copier nil))
  (message (util:required 'message) :type string :read-only t))

(defvar *coalton-error-context* nil)

(defstruct (coalton-file
            (:copier nil))
  (stream (util:required 'stream) :type stream :read-only t)
  (name   (util:required 'file)   :type string :read-only t))

;; TODO: specify list type
(defstruct (coalton-error
            (:copier nil))
  (type            (util:required 'type)     :type (member :error :warn) :read-only t)
  (file            (util:required 'file)     :type coalton-file          :read-only t)
  (location        (util:required 'location) :type integer               :read-only t)
  (message         (util:required 'message)  :type string                :read-only t)
  (notes           (util:required 'notes)    :type list                  :read-only t)
  (help-notes      nil                       :type list                  :read-only t)
  (context         *coalton-error-context*   :type list                  :read-only t))

(defun coalton-error (&key
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
     :type :error
     :file file
     ;; TODO: Do we want to change this based on HIGHLIGHT?
     :location (car span) 
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

(defun display-coalton-error (stream error)
  (declare (type stream stream)
           (type coalton-error error))

  (let ((file-stream (coalton-file-stream (coalton-error-file error))))

    ;; Print the error message and location
    (multiple-value-bind (line-number line-offset)
        (get-line-from-index file-stream (coalton-error-location error))
      (format stream
              "~(~A~): ~A~%  --> ~A:~D:~D~%"
              (coalton-error-type error)
              (coalton-error-message error)
              (coalton-file-name (coalton-error-file error))
              line-number
              line-offset))

    ;; Print the error notes
    (let* ((contains-multiline-note nil)

           ;; Sort notes, erroring if there are overlapping regions
           (notes-with-line-info
             (stable-sort
              (coalton-error-notes error)
              (lambda (a b)
                (when (or (< (car (coalton-error-note-span a))
                             (car (coalton-error-note-span b))
                             (cdr (coalton-error-note-span a)))
                          (< (car (coalton-error-note-span b))
                             (car (coalton-error-note-span a))
                             (cdr (coalton-error-note-span b))))
                  (util:coalton-bug "Error notes contain overlapping spans ~A-~A and ~A-~A"
                                    (car (coalton-error-note-span a))
                                    (cdr (coalton-error-note-span a))
                                    (car (coalton-error-note-span b))
                                    (cdr (coalton-error-note-span b))))
                ;; TODO: We should attach line info before ordering so
                ;; we can print rightmost in a line first.
                (< (car (coalton-error-note-span a))
                   (car (coalton-error-note-span b))))))

           (notes-with-line-info
             (mapcar
              (lambda (note)
                (let ((start (car (coalton-error-note-span note)))
                      (end (cdr (coalton-error-note-span note))))
                  (multiple-value-bind (start-line start-line-start)
                      (get-line-from-index file-stream start)
                    (multiple-value-bind (end-line end-line-start)
                        (get-line-from-index file-stream (1- end))
                      (let ((start-offset (- start start-line-start))
                            (end-offset (- end end-line-start)))
                        ;; Ensure that spans are valid
                        (when (or (< end-line start-line)
                                  (and (= end-line start-line)
                                       (< end-offset start-offset)))
                          (util:coalton-bug "Error note contains invalid span ~A:~A to ~A:~A"
                                            start-line start-offset end-line end-offset))

                        (when (/= end-line start-line)
                          (setf contains-multiline-note t))

                        (list note start-line start-offset end-line end-offset))))))
              notes-with-line-info))

           ;; Get the last line mentioned
           (line-number-width
             (1+ (floor (log (fourth (car (last notes-with-line-info))) 10)))))
      ;; Print first empty line
      (format stream
              " ~v{~C~:*~} |~%"
              line-number-width
              '(#\Space))

      (loop :with last-line := (1- (second (first notes-with-line-info))) ; HACK
            :for (note start-line start-offset end-line end-offset) :in notes-with-line-info
            :for is-multiline := (/= start-line end-line)
            :for in-multiline := nil
            :for note-char
              := (ecase (coalton-error-note-type note)
                   (:primary #\^)
                   (:secondary #\-))
            :do (labels ((print-column-number (&optional line-number)
                           (format stream
                                   " ~:[~vA~*~*~;~*~*~v{~C~:*~}~] |~A"
                                   (null line-number)
                                   line-number-width
                                   line-number
                                   line-number-width
                                   '(#\Space)
                                   (cond
                                     (in-multiline
                                      " |")
                                     (contains-multiline-note
                                      "  ")
                                     (t ""))))
                         (print-line-contents (line-number)
                           (print-column-number line-number)
                           (format stream " ~:[~; ~]~A~%"
                                   contains-multiline-note
                                   (get-nth-line file-stream line-number))))
                  (cond (;; If we are on the same line then don't reprint.
                         ;; TODO: It would be nice to merge these together if they don't overlap.
                         (= start-line last-line)
                         nil)
                        ;; If the last linen was part of the output then just print a
                        ;; new line and the message.
                        ((= 1 (- start-line last-line))
                         (print-line-contents start-line))
                        ;; If there was a gap of one line then print that line and the new one.
                        ((= 2 (- start-line last-line))
                         (print-line-contents (1- start-line))
                         (print-line-contents start-line))
                        (t
                         (format stream " ...~%")
                         (print-line-contents start-line)))

                  (setf last-line start-line)

                  (when is-multiline
                    (print-column-number nil)
                    (format stream "~v{~C~:*~}~C~%"
                            (+ 2 start-offset)
                            '(#\_)
                            note-char)

                    (setf in-multiline t)

                    (cond
                      ;; Print spans up to 10 lines without splitting
                      ((>= 7 (- end-line last-line))
                       (loop :for line :from (1+ last-line) :to end-line
                             :do (print-line-contents line)))
                      ;; Otherwise include more context
                      (t
                       (print-line-contents (1+ last-line))
                       (format stream " ...~v{~C~:*~}|~%"
                               line-number-width
                               '(#\Space))
                       (print-line-contents (1- end-line))
                       (print-line-contents end-line)))
                    (setf last-line end-line))

                  (print-column-number nil)
                  (cond
                    (is-multiline
                     (format stream
                             "~v{~C~:*~}~C ~A~%"
                             (1+ end-offset)
                             '(#\_)
                             note-char
                             (coalton-error-note-message note)))
                    (t
                     (format stream
                             "~v{~C~:*~}~v{~C~:*~} ~A~%"
                             (1+ start-offset)
                             '(#\Space)
                             (- end-offset start-offset)
                             (list note-char)
                             (coalton-error-note-message note)))))))

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
          :do (format stream "note: ~A~%" (coalton-error-context-message context)))))

(define-condition parse-error (error)
  ((err :reader parse-error-err
        :initarg :err
        :type coalton-error))
  (:report (lambda (c s)
             (display-coalton-error s (parse-error-err c)))))
