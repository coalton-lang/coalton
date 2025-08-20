(coalton-library/utils:defstdlib-package #:coalton-library/stream
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/builtin
   #:coalton-library/functions)
  (:import-from #:coalton-library/system #:LispCondition)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:char #:coalton-library/char)
   (#:list #:coalton-library/list))
  (:export
   #:InputStream #:OutputStream #:IOStream
   #:Readable #:Writable #:Closable
   #:read #:read-vector #:unread #:peek
   #:write #:write-vector #:flush
   #:close #:abort
   #:ReaderErr #:EOF
   #:ReaderPredicate #:Inclusive #:Exclusive
   #:drop-to #:read-to
   #:read-word #:read-line #:read-all
   #:stdout #:stderr #:stdin))

(in-package #:coalton-library/stream)

(named-readtables:in-readtable coalton:coalton)

;;
;; Utilities
;;

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro lisp-result (type capture cl:&body body)
    "Lisp FFI call that catches errors and returns a Result."
    `(lisp (Result LispCondition ,type) ,capture
       (cl:handler-case (Ok (cl:progn ,@body)) (cl:error (c) (Err c)))))

  (cl:defmacro define-instances ((class cl:&rest types) cl:&body methods)
    "Define instances in batches."
    `(progn ,@(cl:mapcar
               (cl:lambda (type)
                 `(define-instance (,class ,@type) ,@methods))
               types))))

;;
;; Tokens
;;

(coalton-toplevel
  (define whitespaces
    (lisp (List UFix) ()
      (cl:mapcar #'cl:char-code '(#\Space #\Tab #\Linefeed #\Return #\Page))))

  (define-class (Newline :elt)
    (newline? (:elt -> Boolean)))

  (define-instance (Newline Char)
    (define (newline? obj) (== obj #\Newline)))

  (define-instance (Newline U8)
    (define (newline? obj) (== obj 10)))

  (define-class (Whitespace :elt)
    (whitespace? (:elt -> Boolean)))

  (define-instance (Whitespace Char)
    (define (whitespace? obj) (list:member (char:char-code obj) whitespaces)))

  (define-instance (Whitespace U8)
    (define (whitespace? obj) (list:member (into obj) whitespaces))))

;;
;; Stream Types
;;

(coalton-toplevel
  (repr :native (cl:or flex:flexi-input-stream flex::vector-input-stream))
  (define-type (InputStream :elt)
    "A stream that can be read from.")

  (repr :native (cl:or flex:flexi-output-stream flex::vector-output-stream))
  (define-type (OutputStream :elt)
    "A stream that can be written to.")

  (repr :native flex:flexi-io-stream)
  (define-type (IOStream :elt)
    "A stream that can be read from or written to.")

  (define-class (Closable :stream :elt)
    "A stream that can be closed."
    (close        (:stream :elt -> Unit))
    (abort        (:stream :elt -> Unit)))

  (define-class (Readable :stream :elt)
    "An input or IO stream."
    (read         (:stream :elt -> (Result LispCondition :elt)))
    (read-vector  (:stream :elt -> UFix -> (Result LispCondition (vec:Vector :elt))))
    (unread       (:stream :elt -> :elt -> :stream :elt))
    (peek         (:stream :elt -> (Result LispCondition :elt))))

  (define-class (Writable :stream :elt)
    "An output or IO stream."
    (write        (:stream :elt -> :elt -> (Result LispCondition Unit)))
    (write-vector (:stream :elt -> vec:Vector :elt -> (Result LispCondition Unit)))
    (flush        (:stream :elt -> Unit))))

;;
;; Class Instances
;;

(coalton-toplevel
  (define-instances (Closable (InputStream Char) (OutputStream Char) (IOStream Char)
                              (InputStream U8) (OutputStream U8) (IOStream U8))
    (define (close stream)
      (lisp Unit (stream)
        (cl:close stream)
        Unit))
    (define (abort stream)
      (lisp Unit (stream)
        (cl:close stream :abort cl:t)
        Unit)))

  (define-instances (Readable (InputStream Char) (IOStream Char))
    (define (read stream)
      (lisp-result Char (stream)
        (cl:read-char stream)))
    (define (read-vector stream n)
      (lisp-result (vec:Vector Char) (stream n)
        (cl:let ((vec (cl:make-array n :adjustable cl:t)))
          (cl:read-sequence vec stream)
          vec)))
    (define (unread stream elt)
      (lisp :a (stream elt)
        (cl:unread-char elt stream)
        stream))
    (define (peek stream)
      (lisp-result Char (stream)
        (cl:peek-char nil stream))))
  (define-instances (Writable (OutputStream Char) (IOStream Char))
    (define (write stream elt)
      (lisp-result Unit (stream elt)
        (cl:write-char elt stream)
        Unit))
    (define (write-vector stream vec)
      (lisp-result Unit (stream vec)
        (cl:write-sequence vec stream)
        Unit))
    (define (flush stream)
      (lisp Unit (stream)
        (cl:finish-output stream)
        Unit)))

  (define-instances (Readable (InputStream U8) (IOStream U8))
    (define (read stream)
      (lisp-result U8 (stream)
        (cl:read-byte stream)))
    (define (read-vector stream n)
      (lisp-result (vec:Vector U8) (stream n)
        (cl:let ((vec (cl:make-array n :adjustable cl:t)))
          (cl:read-sequence vec stream)
          vec)))
    (define (unread stream elt)
      (lisp :a (stream elt)
        (flex:unread-byte elt stream)
        stream))
    (define (peek stream)
      (lisp-result U8 (stream)
        (flex:peek-byte stream))))
  (define-instances (Writable (OutputStream U8) (IOStream U8))
    (define (write stream elt)
      (lisp-result Unit (stream elt)
        (cl:write-byte elt stream)
        Unit))
    (define (write-vector stream vec)
      (lisp-result Unit (stream vec)
        (cl:write-sequence vec stream)
        Unit))
    (define (flush stream)
      (lisp Unit (stream)
        (cl:finish-output stream)
        Unit))))

;;
;; High Level Functions
;;

(coalton-toplevel
  (define-type (ReaderErr :elt)
    (EOF (vec:Vector :elt)))

  (define-type (ReaderPredicate :elt)
    (Inclusive (:elt -> Boolean))
    (Exclusive (:elt -> Boolean)))

  (declare read-to ((Readable :stream :elt) => :stream :elt -> ReaderPredicate :elt -> (Result (ReaderErr :elt) (vec:Vector :elt))))
  (define (read-to stream pred)
    "Consume elements from a stream, collecting them into a vector."
    (let vec = (vec:make))
    (while-let (Ok elt) = (read stream)
      (match pred
        ((Inclusive f)
         (progn (vec:push! elt vec) Unit)
         (when (f elt) (return (Ok vec))))
        ((Exclusive f)
         (if (f elt)
             (progn (unread stream elt) (return (Ok vec)))
             (progn (vec:push! elt vec) Unit)))))
    (Err (EOF vec)))

  (declare drop-to ((Readable :stream :elt) => :stream :elt -> ReaderPredicate :elt -> (Result (ReaderErr :elt) (:stream :elt))))
  (define (drop-to stream pred)
    "Consume elements from a stream without collecting them."
    (while-let (Ok elt) = (read stream)
      (match pred
        ((Inclusive f)
         (when (f elt)
           (return (Ok stream))))
        ((Exclusive f)
         (when (f elt)
           (unread stream elt)
           (return (Ok stream))))))
    (Err (EOF (vec:make))))

  (declare read-word ((Readable :stream :elt) (Whitespace :elt) => :stream :elt -> (Result (ReaderErr :elt) (vec:Vector :elt))))
  (define (read-word stream)
    "Read to the next whitespace token (Exclusive)."
    (drop-to stream (Exclusive (fn (elt) (not (whitespace? elt)))))
    (read-to stream (Exclusive whitespace?)))

  (declare read-line ((Readable :stream :elt) (Newline :elt) => :stream :elt -> (Result (ReaderErr :elt) (vec:Vector :elt))))
  (define (read-line stream)
    "Read to the next newline token (Exclusive)."
    (match (peek stream)
      ((Ok elt) (when (newline? elt) (read stream) Unit))
      ((Err _) (return (Err (EOF mempty)))))
    (read-to stream (Exclusive newline?)))

  (declare read-all ((Readable :stream :elt) => :stream :elt -> (vec:Vector :elt)))
  (define (read-all stream)
    "Consume elements from a stream until EOF, collecting them into a vector."
    (match (read-to stream (Inclusive (fn (_) False)))
      ((Ok result) result)
      ((Err (EOF result)) result))))

;;
;; System Streams
;;

(coalton-toplevel
  (declare stdout (Unit -> OutputStream :elt))
  (define (stdout)
    "Equivalent to `cl:*standard-output*`."
    (lisp (OutputStream :elt) ()
      (flex:make-flexi-stream cl:*standard-output*)))

  (declare stderr (Unit -> OutputStream :elt))
  (define (stderr)
    "Equivalent to `cl:*error-output*`."
    (lisp (OutputStream :elt) ()
      (flex:make-flexi-stream cl:*error-output*)))

  (declare stdin (Unit -> InputStream :elt))
  (define (stdin)
    "Equivalent to `cl:*standard-input*`."
    (lisp (InputStream :elt) ()
      (flex:make-flexi-stream cl:*standard-input*))))
