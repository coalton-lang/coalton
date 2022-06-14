(coalton-library/utils:defstdlib-package #:coalton-library/char-io
    (:use
     #:coalton
     #:coalton-library/classes
     #:coalton-library/builtin)
  (:local-nicknames (#:iter #:coalton-library/iterator))
  (:export

   ;; error handling
   #:UnknownStreamError #:UnknownFileError
   #:StreamError #:Closed #:EndOfFile #:StreamError #:FileError

   ;; input files
   #:Input
   #:close-input!
   #:open-input?
   #:read-char!
   #:read-line!
   #:iterator!
   #:chars!
   #:lines!
   #:standard-input

   ;; output files
   #:Output
   #:close-output!
   #:open-output?
   #:write-char!
   #:write-string!
   #:write-line!
   #:newline!
   #:standard-output

   ;; internal
   #:%with-converting-stream-errors))
(cl:in-package #:coalton-library/char-io)

;; errors

(coalton-toplevel
  (repr :native cl:stream-error)
  (define-type UnknownStreamError)

  (repr :native cl:file-error)
  (define-type UnknownFileError)

  (define-type StreamError
    Closed
    EndOfFile
    (StreamError UnknownStreamError)
    (FileError UnknownFileError)))

(cl:defmacro %with-converting-stream-errors (cl:&body forms)
  "Intended for internal use only. Evaluate the FORMS with handlers in place for stream-related errors, and return a (Result StreamError :result)."
  `(cl:handler-case
       (cl:progn ,@forms)
     (cl:end-of-file () (Err EndOfFile))
     #+sbcl (sb-int:closed-stream-error () (Err Closed))
     (cl:stream-error (e) (Err (StreamError e)))
     (cl:file-error (e) (Err (FileError e)))
     (:no-error (res cl:&rest ignored) ; `cl:read-line' returns an extra value that we don't care about, so ignore it
       (cl:declare (cl:ignore ignored))
       (Ok res))))

;; char input streams

(coalton-toplevel
  (repr :native (cl:and cl:stream (cl:satisfies cl:input-stream-p)))
  (define-type Input
    "An input stream, from which characters can be read.")

  (declare open-input? (Input -> Boolean))
  (define (open-input? in)
    "Is IN open? Returns `False' if `close-input!' has been called on IN in the past.

If `False', reads from IN will fail."
    (lisp Boolean (in)
      (cl:open-stream-p in)))

  (declare close-input! (Input -> (Result StreamError Unit)))
  (define (close-input! in)
    "Close IN, freeing any corresponding resources. Future reads from IN will fail.

Closing an `Input' derived from a `file:File' will close the underlying file. If an `Output' is also
associated with the same `file:File', it will also be closed."
    (if (open-input? in)
        (lisp (Result StreamError Unit) (in)
          (%with-converting-stream-errors
            (cl:close in)
            Unit))
        (Err Closed)))

  (declare read-char! (Input -> (Result StreamError Char)))
  (define (read-char! in)
    "Read a character from IN.

Blocks if no data is available from IN."
    (lisp (Result StreamError Char) (in)
      (%with-converting-stream-errors
        (cl:read-char in cl:t cl:nil cl:nil))))

  (declare read-line! (Input -> (Result StreamError String)))
  (define (read-line! in)
    "Read a line up to a newline from IN, discard the newline and return the line.

Blocks if no data is available from IN."
    (lisp (Result StreamError String) (in)
      (%with-converting-stream-errors
        (cl:read-line in cl:t cl:nil cl:nil))))

  (declare iterator! ((Input -> (Result StreamError :elt))
                      -> Input
                      -> (iter:Iterator (Result StreamError :elt))))
  (define (iterator! read-elt! in)
    "Returns an iterator over elements of IN by READ-ELT!

If multiple iterators are derived from the same `Input', advancing any iterator will cause all of them to
advance."
    (iter:new
     (fn ()
       (if (not (open-input? in))
           None
           (match (read-elt! in)
             ((Ok elt) (Some (Ok elt)))
             ((Err (EndOfFile)) None)
             ((Err e) (Some (Err e))))))))

  (declare chars! (Input -> (iter:Iterator (Result StreamError Char))))
  (define (chars! in)
    "Returns an iterator over the characters of IN, calling `read-char!' successively.

If multiple iterators are derived from the same `Input', advancing any iterator will cause all of them to
advance.

Advancing the iterator may block if no data is available on IN."
    (iterator! read-char! in))

  (declare lines! (Input -> (iter:Iterator (Result StreamError String))))
  (define (lines! in)
    "Returns an iterator over the lines of IN, calling `read-line!' successively.

If multiple iterators are derived from the same `Input', advancing any iterator will cause all of them to
advance.

Advancing the iterator may block if no data is available on IN."
    (iterator! read-line! in))

  (declare standard-input (Unit -> Input))
  (define (standard-input)
    "Returns the current standard input stream, i.e. the value of `cl:*standard-input*'."
    (lisp Input () cl:*standard-input*)))

;; output files

(coalton-toplevel
  (repr :native (cl:and cl:stream (cl:satisfies cl:output-stream-p)))
  (define-type Output
    "An output file, to which characters can be written.")

  (declare open-output? (Output -> Boolean))
  (define (open-output? out)
    "Is OUT open? Returns `False' if `close-output!' has been called on OUT in the past.

If `False', writes to OUT will fail."
    (lisp Boolean (out)
      (cl:open-stream-p out)))

  (declare close-output! (Output -> (Result StreamError Unit)))
  (define (close-output! out)
    "Close OUT, forcing its writes to complete and then freeing corresponding resources.

Future writes to OUT will fail.

Closing an `Output' derived from a `file:File' will close the underlying file. If an `Input' is also
associated with the same `file:File', it will also be closed."
    (if (open-output? out)
        (lisp (Result StreamError Unit) (out)
          (%with-converting-stream-errors
            (cl:close out)
            Unit))
        (Err Closed)))

  (declare write-char! (Output -> Char -> (Result StreamError Unit)))
  (define (write-char! out c)
    "Write C to OUT."
    (lisp (Result StreamError Unit) (out c)
      (%with-converting-stream-errors
        (cl:write-char c out)
        Unit)))

  (declare write-string! (Output -> String -> (Result StreamError Unit)))
  (define (write-string! out str)
    "Write STR to OUT."
    (lisp (Result StreamError Unit) (out str)
      (%with-converting-stream-errors
        (cl:write-string str out)
        Unit)))

  (declare write-line! (Output -> String -> (Result StreamError Unit)))
  (define (write-line! out str)
    "Write STR to OUT with a trailing newline."
    (lisp (Result StreamError Unit) (out str)
      (%with-converting-stream-errors
        (cl:write-line str out)
        Unit)))

  (declare newline! (Output -> (Result StreamError Unit)))
  (define (newline! out)
    "Write a newline to OUT."
    (lisp (Result StreamError Unit) (out)
      (%with-converting-stream-errors
        (cl:terpri out)
        Unit)))

  (declare standard-output (Unit -> Output))
  (define (standard-output)
    "Returns the current standard output stream, i.e. the value of `cl:*standard-output*'."
    (lisp Output () cl:*standard-output*)))
