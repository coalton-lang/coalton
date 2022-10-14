(coalton-library/utils:defstdlib-package #:coalton-library/char-stream
  (:use #:coalton
        #:coalton-library/classes)
  (:local-nicknames (#:gray #:trivial-gray-streams))
  (:import-from #:coalton-impl/runtime
                #:function-entry)
  (:export
   #:StreamError
   #:StreamErrorSimple
   #:StreamErrorClosed
   #:StreamErrorEndOfFile
   #:StreamErrorDecoding
   #:StreamErrorEncoding
   #:StreamErrorTimeout
   #:StreamErrorReader

   #:Stream
   #:open?
   #:close!

   #:Input
   #:read-char!
   #:read-line!

   #:FlushOperation
   #:FlushBlocking
   #:FlushAsync

   #:Output
   #:write-char!
   #:write-string!
   #:flush-output!

   #:write-line!

   ;; common lisp condition classes
   #:coalton-stream-error
   #:coalton-stream-error-simple
   #:coalton-stream-error-simple-message
   #:coalton-stream-error-closed
   #:coalton-stream-error-end-of-file
   #:coalton-stream-error-decoding
   #:coalton-stream-error-encoding
   #:coalton-stream-error-timeout
   #:coalton-stream-error-reader))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(cl:in-package #:coalton-library/char-stream)

(cl:defmacro %define-into-as-unsafe-coerce (from-type to-type)
  (alexandria:with-gensyms (instance)
    `(define-instance (Into ,from-type ,to-type)
       (define (into ,instance)
         (lisp ,to-type (,instance)
           ,instance)))))

(coalton-toplevel
  ;; users should interact with streams via the classes `Input' and `Output', rather than explicitly
  ;; referencing these types
  (repr :native cl:stream)
  (define-type %AbstractStream)
  
  (repr :native (cl:and cl:stream (cl:satisfies cl:input-stream-p)))
  (define-type %InputStream)

  (%define-into-as-unsafe-coerce %InputStream %AbstractStream)

  (repr :native (cl:and cl:stream (cl:satisfies cl:output-stream-p)))
  (define-type %OutputStream)

  (%define-into-as-unsafe-coerce %OutputStream %AbstractStream)

  (repr :native (cl:and cl:stream (cl:satisfies cl:input-stream-p) (cl:satisfies cl:output-stream-p)))
  (define-type %TwoWayStream)

  (%define-into-as-unsafe-coerce %TwoWayStream %AbstractStream)
  (%define-into-as-unsafe-coerce %TwoWayStream %InputStream)
  (%define-into-as-unsafe-coerce %TwoWayStream %OutputStream)
  
  ;; variants of this type correspond 1:1 with subclasses of `cl:stream-error' on SBCL
  (define-type StreamError
    (StreamErrorSimple String) ; encodes SBCL's `sb-int:simple-stream-error'
    StreamErrorClosed
    StreamErrorEndOfFile
    StreamErrorDecoding
    StreamErrorEncoding
    StreamErrorTimeout
    StreamErrorReader)

  (define-class (Stream :stream)
    (open? (:stream -> Boolean))
    (close! (:stream -> Result StreamError Unit)))

  (define-class (Stream :stream => Input :stream)
    (read-char! (:stream -> Result StreamError Char))
    (read-line! (:stream -> Result StreamError String)))

  (define-type FlushOperation
    FlushBlocking
    FlushAsync)

  (define-class (Stream :stream => Output :stream)
    (write-char! (:stream -> Char -> Result StreamError Unit))
    (write-string! (:stream -> String -> Result StreamError Unit))
    (flush-output! (FlushOperation -> :stream -> Result StreamError Unit)))

  (declare %abstract-stream-open? (%AbstractStream -> Boolean))
  (define (%abstract-stream-open? stream)
    (lisp Boolean (stream)
      (cl:open-stream-p stream)))

  (declare %abstract-stream-close! (%AbstractStream -> Result StreamError Unit))
  (define (%abstract-stream-close! stream)
    (if (%abstract-stream-open? stream)
        (Ok (lisp Unit (stream)
              (cl:close stream)))
        (Err StreamErrorClosed))))

(cl:defmacro %define-stream-by-into-abstract-stream (stream-type)
  (alexandria:with-gensyms (instance)
    `(define-instance (Stream ,stream-type)
       (define (open? ,instance)
         (%abstract-stream-open? (into ,instance)))
       (define (close! ,instance)
         (%abstract-stream-close! (into ,instance))))))

(coalton-toplevel
  (%define-stream-by-into-abstract-stream %InputStream)
  (%define-stream-by-into-abstract-stream %OutputStream)
  (%define-stream-by-into-abstract-stream %TwoWayStream)

  (declare %with-stream-errors (Stream :stream => :stream -> (:stream -> :success) -> Result StreamError :success))
  (define (%with-stream-errors stream thunk)
    (if (open? stream)
        (lisp (Result StreamError :success) (stream thunk)
          (cl:handler-case (call-coalton-function thunk stream)
            (cl:reader-error () StreamErrorReader)
            (cl:end-of-file () StreamErrorEndOfFile)
            #+sbcl
            (sb-int:stream-decoding-error () StreamErrorDecoding)
            #+sbcl
            (sb-int:stream-encoding-error () StreamErrorEncoding)
            #+sbcl
            (sb-int:simple-stream-error (e)
              (StreamErrorSimple (cl:prin1-to-string e)))
            #+sbcl
            (sb-int:closed-stream-error () StreamErrorClosed)
            #+sbcl
            (sb-sys:io-timeout () (StreamErrorTimeout))
            (:no-error (success) (Ok success))))
        (Err StreamErrorClosed)))

  (define-instance (Input %InputStream)
    (define (read-char! stream)
      (%with-stream-errors
       stream
       (fn (stream)
         (lisp Char (stream)
           (cl:read-char stream)))))
    (define (read-line! stream)
      (%with-stream-errors
       stream
       (fn (stream)
         (lisp String (stream)
           (cl:values (cl:read-line stream)))))))

  (define-instance (Input %TwoWayStream)
    (define (read-char! stream)
      (read-char! (the %InputStream (into stream))))
    (define (read-line! stream)
      (read-line! (the %InputStream (into stream)))))

  (define-instance (Output %OutputStream)
    (define (write-char! stream char)
      (%with-stream-errors
       stream
       (fn (stream)
         (lisp Unit (stream char)
           (cl:write-char char stream)
           Unit))))
    (define (write-string! stream string)
      (%with-stream-errors
       stream
       (fn (stream)
         (lisp Unit (stream string)
           (cl:write-string string stream)
           Unit))))
    (define (flush-output! flush-op stream)
      (%with-stream-errors
       stream
       (fn (stream)
         (match flush-op
           ((FlushBlocking) (lisp Unit (stream)
                              (cl:finish-output stream)
                              Unit))
           ((FlushAsync) (lisp Unit (stream)
                           (cl:force-output stream)
                           Unit)))))))

  (define-instance (Output %TwoWayStream)
    (define (write-char! stream char)
      (write-char! (the %OutputStream (into stream))
                   char))
    (define (write-string! stream string)
      (write-string! (the %OutputStream (into stream))
                     string))
    (define (flush-output! flush-op stream)
      (flush-output! flush-op (the %OutputStream (into stream)))))

  (declare write-line! (Output :stream => :stream -> String -> Result StreamError Unit))
  (define (write-line! stream string)
    "Write STRING followed by a newline to STREAM."
    (match (write-string! stream string)
      ((Err e) (Err e))
      ((Ok _) (write-char! stream #\newline)))))

(cl:deftype coalton-function ()
  '(cl:or cl:function function-entry))

(cl:defclass coalton-char-stream (gray:fundamental-character-stream)
  ((%open?-function :type coalton-function
                    :initarg :open?-function
                    :accessor coalton-open?-function)
   (%close!-function :type coalton-function
                     :initarg :close!-function
                     :accessor coalton-close!-function)))

(cl:defclass coalton-char-input-stream (gray:fundamental-character-input-stream coalton-char-stream)
  ((%read-char!-function :type coalton-function
                         :initarg :read-char!-function
                         :accessor coalton-read-char!-function)
   (%read-line!-function :type coalton-function
                         :initarg :read-line!-function
                         :accessor coalton-read-line!-function)))

(cl:defclass coalton-char-output-stream (gray:fundamental-character-output-stream coalton-char-stream)
  ((%write-char!-function :type coalton-function
                          :initarg :write-char!-function
                          :accessor coalton-write-char!-function)
   (%write-string!-function :type coalton-function
                            :initarg :write-string!-function
                            :accessor coalton-write-string!-function)
   (%finish-output-function :type coalton-function
                            :initarg :finish-output-function
                            :accessor coalton-finish-output-function)
   (%force-output-function :type coalton-function
                           :initarg :force-output-function
                           :accessor coalton-force-output-function)))

(cl:defclass coalton-char-two-way-stream (coalton-char-input-stream coalton-char-output-stream)
  ())

(cl:define-condition coalton-stream-error (cl:stream-error) ())

(cl:define-condition coalton-stream-error-simple (coalton-stream-error)
  ((%message :type string
             :initarg :message
             :accessor coalton-stream-error-simple-message)))
(cl:define-condition coalton-stream-error-closed (coalton-stream-error) ())
(cl:define-condition coalton-stream-error-end-of-file (coalton-stream-error) ())
(cl:define-condition coalton-stream-error-decoding (coalton-stream-error) ())
(cl:define-condition coalton-stream-error-encoding (coalton-stream-error) ())
(cl:define-condition coalton-stream-error-timeout (coalton-stream-error) ())
(cl:define-condition coalton-stream-error-reader (coalton-stream-error) ())

(coalton-toplevel
  (declare throw-stream-error (StreamError -> :any))
  (define (throw-stream-error error)
    (match error
      ((StreamErrorSimple msg) (lisp :any (msg)
                                 (cl:error 'coalton-stream-error-simple
                                           :message msg)))
      ((StreamErrorClosed) (lisp :any ()
                             (cl:error 'coalton-stream-error-closed)))
      ((StreamErrorEndOfFile) (lisp :any ()
                                (cl:error 'coalton-stream-error-end-of-file)))
      ((StreamErrorDecoding) (lisp :any ()
                               (cl:error 'coalton-stream-error-decoding)))
      ((StreamErrorEncoding) (lisp :any ()
                               (cl:error 'coalton-stream-error-encoding)))
      ((StreamErrorTimeout) (lisp :any ()
                              (cl:error 'coalton-stream-error-timeout)))
      ((StreamErrorReader) (lisp :any ()
                             (cl:error 'coalton-stream-error-reader)))))

  (declare unwrap-stream-result (Result StreamError :success -> :success))
  (define (unwrap-stream-result res)
    (match res
      ((Ok success) success)
      ((Err error) (throw-stream-error error))))
  
  (declare get-open?-function (Stream :stream => :stream -> Unit -> Boolean))
  (define (get-open?-function stream)
    (fn ()
      (open? stream)))

  (declare get-close!-function (Stream :stream => :stream -> Unit -> Unit))
  (define (get-close!-function stream)
    (fn ()
      (unwrap-stream-result (close! stream))))

  (declare get-read-char!-function (Input :stream => :stream -> Unit -> Char))
  (define (get-read-char!-function stream)
    (fn ()
      (unwrap-stream-result (read-char! stream))))

  (declare get-read-line!-function (Input :stream => :stream -> Unit -> String))
  (define (get-read-line!-function stream)
    (fn ()
      (unwrap-stream-result (read-line! stream))))

  (declare get-write-char!-function (Output :stream => :stream -> Char -> Unit))
  (define (get-write-char!-function stream)
    (fn (char)
      (unwrap-stream-result (write-char! stream char))))

  (declare get-write-string!-function (Output :stream => :stream -> String -> Unit))
  (define (get-write-string!-function stream)
    (fn (string)
      (unwrap-stream-result (write-string! stream string))))

  (declare get-flush-output!-function (Output :stream => FlushOperation -> :stream -> Unit -> Unit))
  (define (get-flush-output!-function flush-op stream)
    (fn ()
      (unwrap-stream-result (flush-output! flush-op stream))))

  (declare wrap-input-stream-for-lisp (Input :stream => :stream -> %InputStream))
  (define (wrap-input-stream-for-lisp stream)
    (let open?-function = (get-open?-function stream))
    (let close!-function = (get-close!-function stream))
    (let read-char!-function = (get-read-char!-function stream))
    (let read-line!-function = (get-read-line!-function stream))
    (lisp %InputStream (open?-function close!-function read-char!-function read-line!-function)
      (cl:make-instance 'coalton-char-input-stream
                        :open?-function open?-function
                        :close!-function close!-function
                        :read-char!-function read-char!-function
                        :read-line!-function read-line!-function)))

  (declare wrap-output-stream-for-lisp (Output :stream => :stream -> %OutputStream))
  (define (wrap-output-stream-for-lisp stream)
    (let open?-function = (get-open?-function stream))
    (let close!-function = (get-close!-function stream))
    (let write-char!-function = (get-write-char!-function stream))
    (let write-string!-function = (get-write-string!-function stream))
    (let finish-output-function = (get-flush-output!-function FlushBlocking stream))
    (let force-output-function = (get-flush-output!-function FlushAsync stream))
    (lisp %OutputStream (open?-function
                         close!-function
                         write-char!-function
                         write-string!-function
                         finish-output-function
                         force-output-function)
      (cl:make-instance 'coalton-char-output-stream
                        :open?-function open?-function
                        :close!-function close!-function
                        :write-char!-function write-char!-function
                        :write-string!-function write-string!-function
                        :finish-output-function finish-output-function
                        :force-output-function force-output-function)))

  (declare wrap-two-way-stream-for-lisp ((Input :stream) (Output :stream) => :stream -> %TwoWayStream))
  (define (wrap-two-way-stream-for-lisp stream)
    (let open?-function = (get-open?-function stream))
    (let close!-function = (get-close!-function stream))
    (let read-char!-function = (get-read-char!-function stream))
    (let read-line!-function = (get-read-line!-function stream))
    (let write-char!-function = (get-write-char!-function stream))
    (let write-string!-function = (get-write-string!-function stream))
    (let finish-output-function = (get-flush-output!-function FlushBlocking stream))
    (let force-output-function = (get-flush-output!-function FlushAsync stream))
    (lisp %TwoWayStream (open?-function
                         close!-function
                         read-char!-function
                         read-line!-function
                         write-char!-function
                         write-string!-function
                         finish-output-function
                         force-output-function)
      (cl:make-instance 'coalton-char-input-stream
                        :open?-function open?-function
                        :close!-function close!-function
                        :read-char!-function read-char!-function
                        :read-line!-function read-line!-function
                        :write-char!-function write-char!-function
                        :write-string!-function write-string!-function
                        :finish-output-function finish-output-function
                        :force-output-function force-output-function))))

(cl:defmethod cl:open-stream-p ((stream coalton-char-stream))
  (call-coalton-function (coalton-open?-function stream) Unit))

(cl:defmethod cl:close ((stream coalton-char-stream) cl:&key abort)
  (cl:declare (cl:ignore abort))
  (call-coalton-function (coalton-close!-function stream) Unit))

(cl:defmethod gray:stream-read-char ((stream coalton-char-input-stream))
  (call-coalton-function (coalton-read-char!-function stream) Unit))

(cl:defmethod gray:stream-read-line ((stream coalton-char-input-stream))
  (cl:values (call-coalton-function (coalton-read-line!-function stream) Unit)
             cl:nil))

(cl:defmethod gray:stream-write-char ((stream coalton-char-output-stream) (char cl:character))
  (call-coalton-function (coalton-write-char!-function stream)
                         char))

(cl:defmethod gray:stream-write-string ((stream coalton-char-output-stream) (string cl:string)
                                        cl:&optional start end)
  (cl:assert (cl:not start) ()
             "COALTON-CHAR-OUTPUT-STREAMs do not support START or END arguments to STREAM-WRITE-STRING")
  (cl:assert (cl:not end) ()
             "COALTON-CHAR-OUTPUT-STREAMs do not support START or END arguments to STREAM-WRITE-STRING")
  (call-coalton-function (coalton-write-string!-function stream)
                         string))

(cl:defmethod gray:stream-finish-output ((stream coalton-char-output-stream))
  (call-coalton-function (coalton-finish-output-function stream) Unit))

(cl:defmethod gray:stream-force-output ((stream coalton-char-output-stream))
  (call-coalton-function (coalton-force-output-function stream) Unit))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/CHAR-STREAM")

