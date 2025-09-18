(coalton-library/utils:defstdlib-package #:coalton-library/stream
    (:use
     #:coalton
     #:coalton-library/classes
     #:coalton-library/builtin
     #:coalton-library/functions)
  (:import-from #:coalton-library/system #:LispCondition)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:array #:coalton-library/lisparray)
   (#:char #:coalton-library/char)
   (#:list #:coalton-library/list))
  (:export
   #:InputStream #:OutputStream #:IOStream
   #:Readable #:Writable #:Closable
   #:read #:read-unchecked #:read-array
   #:write #:write-array #:write-unchecked #:write-array-unchecked
   #:unread #:peek-unchecked #:peek
   #:make-peekable
   #:flush #:close #:abort
   #:ReaderErr #:EOF
   #:ReaderPredicate #:Inclusive #:Exclusive
   #:drop-to #:read-to
   #:stdout #:stderr #:stdin))

(in-package #:coalton-library/stream)

(named-readtables:in-readtable coalton:coalton)

;;
;; Utilities
;;

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
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
  (repr :native cl:stream)
  (define-type (InputStream :elt)
    "A stream that can be read from.")

  (repr :native cl:stream)
  (define-type (OutputStream :elt)
    "A stream that can be written to.")

  (repr :native cl:stream)
  (define-type (IOStream :elt)
    "A stream that can be read from or written to.")

  (define-struct (PeekableInputStream :elt)
    "A peekable stream with an unread buffer that can be read from."
    (stream (InputStream :elt))
    (buffer (vec:Vector  :elt)))

  (define-struct (PeekableIOStream :elt)
    "A peekable stream with an unread buffer that can be read from or written to."
    (stream (IOStream   :elt))
    (buffer (vec:Vector :elt)))

  (define-class (Closable :stream :elt)
    "A stream that can be closed."
    (close
     "Mark the end of the stream's use as a source or sink of data."
     (:stream :elt -> Unit))
    (abort
     "Closes a stream and aborts all operations."
     (:stream :elt -> Unit)))

  (define-class (Readable :stream :elt)
    "An input or IO stream."
    (read-unchecked
     "Consume 1 element from a stream.

Signals a condition on error."
     (:stream :elt -> :elt))
    (read-array
     "Consume `n` elements from a stream and destructively fill array."
     (:stream :elt -> array:LispArray :elt -> UFix)))

  (define-class (Writable :stream :elt)
    "An output or IO stream."
    (write-unchecked
     "Write 1 element to a stream.

Signals a condition on error."
     (:stream :elt -> :elt -> Unit))
    (write-array-unchecked
     "Write `n` elements to a stream.

Signals a condition on error."
     (:stream :elt -> array:LispArray :elt -> Unit))
    (flush
     "Flush buffered output."
     (:stream :elt -> Unit)))

  (define-class (Readable :stream :elt => Peekable :stream :elt)
    (unread
     "Push an element back onto a stream."
     (:stream :elt -> :elt -> Unit))
    (peek-unchecked
     "Get the next element in the stream without consuming it.

Signals a condition on error."
     (:stream :elt -> :elt)))

  (define-class (IntoPeekable :stream :elt :peekablestream (:stream -> :peekablestream))
    (make-peekable
     "Turn a stream into one impementing `Peekable`."
     (:stream :elt -> :peekablestream :elt))))

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
    (define (read-unchecked stream)
      (lisp Char (stream)
        (cl:read-char stream)))
    (define (read-array stream arr)
      (lisp UFix (stream arr)
        (cl:read-sequence arr stream))))

  (define-instances (Writable (OutputStream Char) (IOStream Char))
    (define (write-unchecked stream elt)
      (lisp Unit (stream elt)
        (cl:write-char elt stream)
        Unit))
    (define (write-array-unchecked stream arr)
      (lisp Unit (stream arr)
        (cl:write-sequence arr stream)
        Unit))
    (define (flush stream)
      (lisp Unit (stream)
        (cl:finish-output stream)
        Unit)))

  (define-instances (Readable (InputStream U8) (IOStream U8))
    (define (read-unchecked stream)
      (lisp U8 (stream)
        (cl:read-byte stream)))
    (define (read-array stream arr)
      (lisp UFix (stream arr)
        (cl:read-sequence arr stream))))

  (define-instances (Writable (OutputStream U8) (IOStream U8))
    (define (write-unchecked stream elt)
      (lisp Unit (stream elt)
        (cl:write-byte elt stream)
        Unit))
    (define (write-array-unchecked stream arr)
      (lisp Unit (stream arr)
        (cl:write-sequence arr stream)
        Unit))
    (define (flush stream)
      (lisp Unit (stream)
        (cl:finish-output stream)
        Unit)))

  (define-instances (Peekable (InputStream Char) (IOStream Char))
    (define (unread stream elt)
      (lisp Unit (stream elt)
        (cl:unread-char elt stream)
        Unit))
    (define (peek-unchecked stream)
      (lisp Char (stream)
        (cl:peek-char nil stream))))

  (define-instances (Peekable (PeekableInputStream U8) (PeekableIOStream U8))
    (define (unread stream elt)
      (vec:push! elt (.buffer stream))
      Unit)
    (define (peek-unchecked stream)
      (if (not (vec:empty? (.buffer stream)))
          (vec:last-unsafe (.buffer stream))
          (let ((elt (read-unchecked (.stream stream))))
            (vec:push! elt (.buffer stream))
            elt))))

  (define-instances (Readable (PeekableInputStream U8) (PeekableIOStream U8))
    (define (read-unchecked stream)
      (match (vec:pop! (.buffer stream))
        ((None) (read-unchecked (.stream stream)))
        ((Some elt) elt)))
    (define (read-array stream arr)
      (let length = (vec:length (.buffer stream)))
      (match (<= (array:length arr) (vec:length (.buffer stream)))
        ((True)
         (let b = (.buffer stream))
         (lisp Unit (arr b) (cl:replace arr b) Unit)
         (vec:kill! b 0 length)
         length)
        ((False)
         (let s = (.stream stream))
         (let b = (.buffer stream))
         (lisp Unit (arr b) (cl:replace arr b) Unit)
         (let result = 
           (lisp UFix (arr s length)
             (cl:read-sequence arr s :start length)))
         (vec:clear! (.buffer stream))
         result))))

  (define-instance (Writable PeekableIOStream U8)
    (define (write-unchecked stream elt)
      (write-unchecked (.stream stream) elt))
    (define (write-array-unchecked stream arr)
      (write-array-unchecked (.stream stream) arr))
    (define (flush stream)
      (flush (.stream stream))))

  (define-instance (Closable PeekableIOStream U8)
    (define (close stream)
      (close (.stream stream)))
    (define (abort stream)
      (abort (.stream stream))))

  (define-instance (IntoPeekable InputStream U8 PeekableInputStream)
    (define (make-peekable stream)
      (PeekableInputStream stream (vec:new)))))

;;
;; Safe Stream Operations
;;

(coalton-toplevel
  (declare read (Readable :stream :elt => :stream :elt -> Optional :elt))
  (define (read stream)
    "Consume 1 element from a stream."
    (catch (Some (read-unchecked stream))
      (_ None)))

  (declare peek (Peekable :stream :elt => :stream :elt -> Optional :elt))
  (define (peek stream)
    "Get the next element in the stream without consuming it."
    (catch (Some (peek-unchecked stream))
      (_ None)))

  (declare write (Writable :stream :elt => :stream :elt -> :elt -> Boolean))
  (define (write stream elt)
    "Write an element to a `Writable` stream.

Returns `True` on success, `False` on error."
    (catch (progn (write-unchecked stream elt) True)
      (_ False)))

  (declare write-array (Writable :stream :elt => :stream :elt -> array:LispArray :elt -> Boolean))
  (define (write-array stream vec)
    "Write a vector of elements to a `Writable` stream.

Returns `True` on success, `False` on error."
    (catch (progn (write-array-unchecked stream vec) True)
      (_ False))))


;;
;; High Level Functions
;;

(coalton-toplevel
  (define-type (ReaderErr :elt)
    (EOF (vec:Vector :elt)))

  (define-type (ReaderPredicate :elt)
    (Inclusive (:elt -> Boolean))
    (Exclusive (:elt -> Boolean)))

  (declare read-to ((Peekable :stream :elt) => :stream :elt -> ReaderPredicate :elt -> (Result (ReaderErr :elt) (vec:Vector :elt))))
  (define (read-to stream pred)
    "Consume elements from a stream, collecting them into a vector."
    (let vec = (vec:make))
    (while-let (Some elt) = (read stream)
      (match pred
        ((Inclusive f)
         (vec:push! elt vec)
         (when (f elt)
           (return (Ok vec))))
        ((Exclusive f)
         (if (f elt)
             (progn (unread stream elt) (return (Ok vec)))
             (progn (vec:push! elt vec) Unit)))))
    (Err (EOF vec)))

  (declare drop-to ((Peekable :stream :elt) => :stream :elt -> ReaderPredicate :elt -> (Result (ReaderErr :elt) (:stream :elt))))
  (define (drop-to stream pred)
    "Consume elements from a stream without collecting them."
    (while-let (Some elt) = (read stream)
      (match pred
        ((Inclusive f)
         (when (f elt)
           (return (Ok stream))))
        ((Exclusive f)
         (when (f elt)
           (unread stream elt)
           (return (Ok stream))))))
    (Err (EOF (vec:make)))))

;;
;; System Streams
;;

(coalton-toplevel
  (declare stdout (Unit -> OutputStream :elt))
  (define (stdout)
    "Equivalent to `cl:*standard-output*`."
    (lisp (OutputStream :elt) ()
      cl:*standard-output*))

  (declare stderr (Unit -> OutputStream :elt))
  (define (stderr)
    "Equivalent to `cl:*error-output*`."
    (lisp (OutputStream :elt) ()
      cl:*error-output*))

  (declare stdin (Unit -> InputStream :elt))
  (define (stdin)
    "Equivalent to `cl:*standard-input*`."
    (lisp (InputStream :elt) ()
      cl:*standard-input*)))
