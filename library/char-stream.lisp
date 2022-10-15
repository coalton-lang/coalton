(coalton-library/utils:defstdlib-package #:coalton-library/char-stream
  (:use #:coalton
        #:coalton-library/classes
        #:coalton-library/builtin)
  (:local-nicknames (#:gray #:trivial-gray-streams)
                    (#:iter #:coalton-library/iterator))
  (:import-from #:coalton-impl/runtime
                #:function-entry)
  (:export

   ;; error type and its variants
   #:StreamError
   #:StreamErrorSimple
   #:StreamErrorClosed
   #:StreamErrorEndOfFile
   #:StreamErrorDecoding
   #:StreamErrorEncoding
   #:StreamErrorTimeout
   #:StreamErrorReader

   ;; Stream class and its methods
   #:Stream
   #:open?
   #:close!

   ;; Input class and its methods
   #:Input
   #:read-char!
   #:read-line!

   ;; additional Input non-method operations
   #:input-chars!
   #:input-lines!

   ;; flush operations on Output streams
   #:FlushOperation
   #:FlushBlocking
   #:FlushAsync

   ;; Output class and its methods
   #:Output
   #:write-char!
   #:write-string!
   #:flush-output!

   ;; additional Output non-method operations
   #:newline!
   #:write-line!
   #:write-chars!
   #:write-lines!
   #:write-strings!
   #:finish-output!
   #:force-output!

   ;; opening files

   ;; paths
   #:Path

   ;; encodings
   #:Encoding
   #:ASCII
   #:UTF-8
   #:UTF-16
   #:LATIN-1
   #:default-encoding

   ;; what to do if opening a file that already exists
   #:IfExists
   #:IfExistsError
   #:IfExistsRename
   #:IfExistsAppend
   #:IfExistsOverwrite
   #:IfExistsSupersede

   ;; what to do if opening a file that does not exist
   #:IfDoesNotExist
   #:IfDoesNotExistError
   #:IfDoesNotExistCreate

   ;; options for opening files, incl. encoding, if-exists and if-does-not-exist
   #:FileOptions
   #:default-file-options
   #:with-encoding
   #:if-exists
   #:if-does-not-exist

   ;; errors while opening files
   #:FileError

   ;; actually opening files
   #:open-input-file!
   #:open-output-file!
   #:open-two-way-file!

   ;; passing streams to lisp
   #:wrap-input-stream-for-lisp
   #:wrap-output-stream-for-lisp
   #:wrap-two-way-stream-for-lisp

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
  (define-type %AbstractStream
    "A Common Lisp `cl:stream' object, with no claims made as to its direction.

Users should interact with streams via the `Stream' class, rather than explicitly referencing this type.")

  ;; it would be nice if this type could be `(and stream (satisfies input-stream-p))', but on SBCL
  ;; `input-stream-p' and `output-stream-p' return nil for closed input- and output-streams, so that breaks
  ;; after you close a stream.
  (repr :native cl:stream)
  (define-type %InputStream
    "A Common Lisp `cl:stream' object which is an input stream, i.e. which can be read from.

Users should interact with input streams via the `Input' class, rather than explicitly referencing this type.")

  (%define-into-as-unsafe-coerce %InputStream %AbstractStream)

  ;; it would be nice if this type could be `(and stream (satisfies output-stream-p))', but on SBCL
  ;; `input-stream-p' and `output-stream-p' return nil for closed input- and output-streams, so that breaks
  ;; after you close a stream.
  (repr :native cl:stream)
  (define-type %OutputStream
    "A Common Lisp `cl:stream' object which is an output stream, i.e. which can be written to.

Users should interact without output streams via the `Output' class, rather than explicitly referencing this
type.")

  (%define-into-as-unsafe-coerce %OutputStream %AbstractStream)

  ;; it would be nice if this type could be
  ;; `(and stream (satisfies input-stream-p) (satisfies output-stream-p))',
  ;; but on SBCL `input-stream-p' and `output-stream-p' return nil for closed input- and output-streams, so
  ;; that breaks after you close a stream.
  (repr :native cl:stream)
  (define-type %TwoWayStream
    "A Common Lisp `cl:stream' object which is a bidirectional stream, i.e. which can be both read from and written to.

Users should interact with two-way streams via the `Input' and `Output' classes, rather than explicitly
referencing this type.")

  (%define-into-as-unsafe-coerce %TwoWayStream %AbstractStream)
  (%define-into-as-unsafe-coerce %TwoWayStream %InputStream)
  (%define-into-as-unsafe-coerce %TwoWayStream %OutputStream)

  ;;; class definitions
  
  ;; variants of this type correspond 1:1 with subclasses of `cl:stream-error' on SBCL
  ;;
  ;; TODO: add members to variants
  (define-type StreamError
    (StreamErrorSimple String) ; encodes SBCL's `sb-int:simple-stream-error'
    StreamErrorClosed
    StreamErrorEndOfFile
    StreamErrorDecoding
    StreamErrorEncoding
    StreamErrorTimeout
    StreamErrorReader)

  (define-class (Stream :stream)
    "A character stream which, while open, is potentially readable and/or writeable.

(open? STREAM) should return true if STREAM has not yet been `close!'d.

(close! STREAM) should cause STREAM to no longer be `open?', and free any resources associated with
it (buffers, fds, etc.)."
    (open? (:stream -> Boolean))
    (close! (:stream -> Result StreamError Unit)))

  (define-class (Stream :stream => Input :stream)
    "An input stream from which characters can be read.

(read-char! STREAM) will return the first available character from STREAM, blocking until data is available.

(read-line! STREAM) will return a string containing all the characters from STREAM up to the first newline or
end-of-file, blocking until data is available. The terminating newline is not included in the returned
string."
    (read-char! (:stream -> Result StreamError Char))
    (read-line! (:stream -> Result StreamError String)))

  (define-type FlushOperation
    "A variant of the `flush-output!' operation on `Output' streams.

(flush-output! FlushBlocking STREAM) causes all pending output on STREAM to be written, and waits until the
data is written before returing. Equivalent to `cl:finish-output'.

(flush-output! FlushAsync STREAM) causes all pending output on STREAM to be written, but returns immediately
rather than waiting for the flush to complete. Equivalent to `cl:force-output'."
    FlushBlocking
    FlushAsync)

  (define-class (Stream :stream => Output :stream)
    "An output stream to which characters can be written.

(write-char! STREAM C) writes C to STREAM, blocking if necessary until the stream is ready to accept data.

(write-string! STREAM STRING) writes all the characters of STRING to STREAM, blocking if necessary until the
stream is ready to accept data.

(flush-output! FLUSH-OP STREAM) causes previously written data on STREAM to be made visible to a consumer. See
`FlushOperation' for available FLUSH-OPs and their meanings."
    (write-char! (:stream -> Char -> Result StreamError Unit))
    (write-string! (:stream -> String -> Result StreamError Unit))
    (flush-output! (FlushOperation -> :stream -> Result StreamError Unit)))

  ;;; `Stream', `Input' and `Output' implementations for CL streams

  (declare %abstract-stream-open? (%AbstractStream -> Boolean))
  (define (%abstract-stream-open? stream)
    (lisp Boolean (stream)
      (cl:open-stream-p stream)))

  (declare %abstract-stream-close! (%AbstractStream -> Result StreamError Unit))
  (define (%abstract-stream-close! stream)
    (if (%abstract-stream-open? stream)
        (Ok (lisp Unit (stream)
              (cl:close stream)
              Unit))
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
    "Invoke (THUNK STREAM) in a dynamic context where Common Lisp stream-related conditions are handled and converted into `StreamError's."
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

  ;;; additional `Input' operations

  (declare input-iterator! (Input :stream =>
                                  (:stream -> Result StreamError :elt)
                                  -> :stream
                                  -> iter:Iterator (Result StreamError :elt)))
  (define (input-iterator! read-one! in)
    "Repeatedly read elements from IN by READ-ONE!, stopping when it hits end-of-file.

IN will not be closed after the iterator stops."
    (iter:new (fn ()
                (if (not (open? in))
                    None
                    (match (read-one! in)
                      ((Ok elt) (Some (Ok elt)))
                      ((Err (StreamErrorEndOfFile))
                       None)
                      ((Err e) (Some (Err e))))))))

  (declare input-chars! (Input :stream => :stream -> iter:Iterator (Result StreamError Char)))
  (define input-chars!
    "Read characters from an input stream, stopping when it hits end-of-file.

The stream will not be closed after the iterator stops."
    (input-iterator! read-char!))

  (declare input-lines! (Input :stream => :stream -> iter:Iterator (Result StreamError String)))
  (define input-lines!
    "Read lines from an input stream, stopping when it hits end-of-file.

The stream will not be closed after the iterator stops."
    (input-iterator! read-line!))

  ;;; additional `Output' operations

  (declare newline! (Output :stream => :stream -> Result StreamError Unit))
  (define (newline! stream)
    "Write a newline character to STREAM."
    (write-char! stream #\newline))

  (declare write-line! (Output :stream => :stream -> String -> Result StreamError Unit))
  (define (write-line! stream string)
    "Write STRING followed by a newline to STREAM."
    (match (write-string! stream string)
      ((Err e) (Err e))
      ((Ok _) (newline! stream))))

  (declare write-all! (Output :stream =>
                              (:stream -> :elt -> Result StreamError Unit)
                              -> :stream
                              -> iter:Iterator :elt
                              -> Result StreamError Unit))
  (define (write-all! write-one! out elts)
    "Write each of the ELTS to OUT via WRITE-ONE!, stopping as soon as a write returns an error."
    (match (iter:next! elts)
      ((None) (Ok Unit))
      ((Some elt) (match (write-one! out elt)
                    ((Err e) (Err e))
                    ((Ok _) (write-all! write-one! out elts))))))

  (declare write-chars! (Output :stream => :stream -> iter:Iterator Char -> Result StreamError Unit))
  (define write-chars!
    "Write all the characters of an iterator to a stream, stopping as soon as a write returns an error."
    (write-all! write-char!))

  (declare write-lines! (Output :stream => :stream -> iter:Iterator String -> Result StreamError Unit))
  (define write-lines!
    "Write all the strings of an iterator to a stream with a newline after each, stopping as soon as a write returns an error."
    (write-all! write-line!))

  (declare write-strings! (Output :stream => :stream -> iter:Iterator String -> Result StreamError Unit))
  (define write-strings!
    "Write all the characters of an iterator to a stream, stopping as soon as a write returns an error."
    (write-all! write-string!))

  (declare finish-output! (Output :stream => :stream -> Result StreamError Unit))
  (define finish-output!
    "Causes all pending output on a stream to be written, and waits until the data is written before returing."
    (flush-output! FlushBlocking))

  (declare force-output! (Output :stream => :stream -> Result StreamError Unit))
  (define force-output!
    "Causes all pending output on a stream to be written, but returns immediately rather than waiting for the flush to complete."
    (flush-output! FlushAsync))

  ;;;; file streams

  ;;; paths

  ;; as of now, i'm not convinced there's a useful way to represent paths as anything other than strings. if
  ;; it turns out in the future that there is, using an opaque wrapper here allows us to use it without
  ;; breaking the API.
  (define-type Path
    (%Path String))

  (define-instance (Into String Path)
    (define into %Path))

  (define-instance (Into Path String)
    (define (into p)
      (let (%Path str) = p)
      str)))

;;; options for opening a file

(cl:defmacro define-cl-enum (type-name docstring cl:&body pairs)
  "Define a type TYPE-NAME to represent a set of keywords, with Coalton constants defined to hold those keywords.
Each of the PAIRS should be a list (COALTON-NAME KEYWORD), where COALTON-NAME is a name that will be defined
to hold an instance of TYPE-NAME, and KEYWORD is a literal Common Lisp keyword.
For example,
(define-cl-enum Foo \"docstring for Foo\"
  (Bar :bar)
  (Baz :baz))
Will define:
- a type named `Foo' with an appropriate `repr :native' to hold the keywords `:bar' and `:baz'
- a constant `Bar' of type `Foo' bound to the keyword `:bar'
- a constant `Baz' of type `Foo' bound to the keyword `:baz'"
  `(cl:progn
     (coalton-toplevel
       (repr :native (cl:member ,@(cl:mapcar #'cl:second pairs)))
       (define-type ,type-name ,docstring))
     ,@(cl:mapcar (cl:lambda (pair)
                    (cl:destructuring-bind (name keyword) pair
                      `(coalton-toplevel
                         (declare ,name ,type-name)
                         (define ,name (lisp ,type-name () ,keyword)))))
                  pairs)))

;;; encodings, aka external-formats

(define-cl-enum Encoding
    "A text encoding; CL calls this an \"external format\".
Others are allowed; SBCL supports a wealth, listed at http://www.sbcl.org/manual/#Supported-External-Formats .
To add an external format supported by SBCL as an `Encoding', add a pair (COALTON-NAME LISP-NAME) to the
`define-encodings' form in coalton/library/file.lisp where LISP-NAME is a keyword which names an encoding, and
add the same COALTON-NAME to the `:exports' clause in that file's `defstdlib-package' form."
  (ASCII :ascii)
  (UTF-8 :utf-8)
  (UTF-16 :utf-16)
  (LATIN-1 :latin-1))

(coalton-toplevel
  (define default-encoding UTF-8))

;;; actions taken if a file exists

(define-cl-enum IfExists
    "Behavior taken if a file to be opened for writing exists.
Supported are a subset of the operators allowed by `cl:open'; see the Hyperspec for more detailed descriptions.
`IfExistsError' - return an error from `open!'. The default.
`IfExistsRename' - rename the existing file, and create a new file with the intended name.
`IfExistsAppend' - open the existing file for writing, with its file pointer initially at the end.
`IfExistsOverwrite' - open the file anyway, with the initial file pointer at 0. A footgun for write-only
                      streams, but sensible for two-way streams.
`IfExistsSupersede' - create a new file with the intended name, deleting the existing file."
  (IfExistsError :error)
  (IfExistsRename :rename)
  (IfExistsAppend :append)
  (IfExistsOverwrite :overwrite)
  (IfExistsSupersede :supersede))

;;; actions taken if a file does not exist

(define-cl-enum IfDoesNotExist
    "Behavior taken if a file to be opened does not exist.
Applies to files opened for both reading or writing, but with different defaults.
`IfDoesNotExistError' - return an error from `open!'. The default for files opened only for reading.
`IfDoesNotExistCreate' - create a new, empty file. The default for files opened for writing."
  (IfDoesNotExistError :error)
  (IfDoesNotExistCreate :create))

(coalton-toplevel
  (define-type FileOptions
    (%FileOptions Encoding
                  (Optional IfExists)
                  (Optional IfDoesNotExist)))

  (declare default-file-options FileOptions)
  (define default-file-options
    (%FileOptions default-encoding None None))

  (declare with-encoding (Encoding -> FileOptions -> FileOptions))
  (define (with-encoding enc opts)
    (let (%FileOptions _ if-so if-not) = opts)
    (%FileOptions enc if-so if-not))

  (declare if-exists (IfExists -> FileOptions -> FileOptions))
  (define (if-exists then opts)
    (let (%FileOptions enc _ if-not) = opts)
    (%FileOptions enc (Some then) if-not))

  (declare if-does-not-exist (IfDoesNotExist -> FileOptions -> FileOptions))
  (define (if-does-not-exist then opts)
    (let (%FileOptions enc if-so _) = opts)
    (%FileOptions enc if-so (Some then)))

  (define-instance (Into Encoding FileOptions)
    (define (into enc) (with-encoding enc default-file-options)))

  (define-instance (Into IfExists FileOptions)
    (define (into if-so) (if-exists if-so default-file-options)))

  (define-instance (Into IfDoesNotExist FileOptions)
    (define (into if-not) (if-does-not-exist if-not default-file-options)))

  ;; sbcl has only simple-file-error (and pathname-unparse-error, but... eugh)
  (define-type FileError
    (FileError String))

  ;;; opening files
  (declare open-input-file! (FileOptions -> Path -> (Result FileError %InputStream)))
  (define (open-input-file! opts path)
    (let (%FileOptions enc _ if-does-not-exist) = opts)
    (let if-does-not-exist = (match if-does-not-exist
                               ((Some if-not) if-not)
                               ((None) IfDoesNotExistError)))
    (let (%Path pathname) = path)
    (lisp (Result FileError %InputStream) (enc if-does-not-exist pathname)
      (cl:handler-case (cl:open pathname
                                :direction :input
                                :element-type 'cl:character
                                :external-format enc
                                :if-does-not-exist if-does-not-exist)
        (cl:file-error (e) (Err (cl:prin1-to-string e)))
        (:no-error (file) (Ok file)))))

  (declare open-output-file! (FileOptions -> Path -> (Result FileError %OutputStream)))
  (define (open-output-file! opts path)
    (let (%FileOptions enc if-exists if-does-not-exist) = opts)
    (let if-exists = (match if-exists
                       ((Some if-so) if-so)
                       ((None) IfExistsError)))
    (let if-does-not-exist = (match if-does-not-exist
                               ((Some if-not) if-not)
                               ((None) IfDoesNotExistCreate)))
    (let (%Path pathname) = path)
    (lisp (Result FileError %OutputStream) (enc if-exists if-does-not-exist pathname)
      (cl:handler-case (cl:open pathname
                                :direction :output
                                :element-type 'cl:character
                                :external-format enc
                                :if-exists if-exists
                                :if-does-not-exist if-does-not-exist)
        (cl:file-error (e) (Err (cl:prin1-to-string e)))
        (:no-error (file) (Ok file)))))

  (declare open-two-way-file! (FileOptions -> Path -> Result FileError %TwoWayStream))
  (define (open-two-way-file! opts path)
    (let (%FileOptions enc if-exists if-does-not-exist) = opts)
    (let if-exists = (match if-exists
                       ((Some if-so) if-so)
                       ((None) IfExistsOverwrite)))
    (let if-does-not-exist = (match if-does-not-exist
                               ((Some if-not) if-not)
                               ((None) IfDoesNotExistCreate)))
    (let (%Path pathname) = path)
    (lisp (Result FileError %TwoWayStream) (enc if-exists if-does-not-exist pathname)
      (cl:handler-case (cl:open pathname
                                :direction :io
                                :element-type 'cl:character
                                :external-format enc
                                :if-exists if-exists
                                :if-does-not-exist if-does-not-exist)
        (cl:file-error (e) (Err (cl:prin1-to-string e)))
        (:no-error (file) (Ok file))))))

;;; gray-streams based interface for passing Coalton streams to Common Lisp

(cl:deftype coalton-function ()
  '(cl:or cl:function function-entry))

(cl:defclass coalton-char-stream (gray:fundamental-character-stream)
  ((%open?-function :type coalton-function
                    :initarg :open?-function
                    :accessor coalton-open?-function)
   (%close!-function :type coalton-function
                     :initarg :close!-function
                     :accessor coalton-close!-function))
  (:documentation "A Coalton implementor of `Stream' wrapped in a CLOS object as a Gray character-stream."))

(cl:defclass coalton-char-input-stream (gray:fundamental-character-input-stream coalton-char-stream)
  ((%read-char!-function :type coalton-function
                         :initarg :read-char!-function
                         :accessor coalton-read-char!-function)
   (%read-line!-function :type coalton-function
                         :initarg :read-line!-function
                         :accessor coalton-read-line!-function))
  (:documentation "A Coalton implementor of `Input' wrapped in a CLOS object as a Gray character-input-stream."))

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
                           :accessor coalton-force-output-function))
  (:documentation "A Coalton implementor of `Output' wrapped in a CLOS object as a Gray character-output-stream."))

(cl:defclass coalton-char-two-way-stream (coalton-char-input-stream coalton-char-output-stream)
  ()
  (:documentation "A Coalton implementor of both `Input' and `Output' wrapped in a CLOS object as a Gray character-bidirectional-stream."))

;; TODO: as with `StreamError', add members
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

