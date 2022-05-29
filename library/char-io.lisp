(coalton-library/utils:defstdlib-package #:coalton-library/char-io
    (:use
     #:coalton
     #:coalton-library/classes
     #:coalton-library/builtin)
  (:local-nicknames (#:iter #:coalton-library/iterator))
  (:export

   ;; error handling
   #:UnknownStreamError
   #:StreamError #:Closed #:EndOfFile #:Unknown

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
   #:standard-output))
(cl:in-package #:coalton-library/char-io)

;; errors

(coalton-toplevel
  (repr :native cl:stream-error)
  (define-type UnknownStreamError)
  
  (define-type StreamError
    Closed
    EndOfFile
    (Unknown UnknownStreamError)))

(cl:defmacro with-converting-stream-errors (cl:&body forms)
  `(cl:handler-case
       (cl:progn ,@forms)
     (cl:end-of-file () (Err EndOfFile))
     #+sbcl (sb-int:closed-stream-error () (Err Closed))
     (cl:stream-error (e) (Err (Unknown e)))
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

  (declare close-input! (Input -> Unit))
  (define (close-input! in)
    "Close IN, freeing any corresponding resources. Future reads from IN will fail.

It is safe to call `close-input!' on an `Input' that is already closed."
    (when (open-input? in)
      (lisp :any (in)
        (cl:close in))
      Unit))

  (declare try-close-input! (Input -> (Result StreamError Unit)))
  (define (try-close-input! in)
    (if (open-input? in)
        (Ok (close-input! in))
        (Err Closed)))

  (declare read-char! (Input -> (Result StreamError Char)))
  (define (read-char! in)
    "Read a character from IN.

Blocks if no data is available from IN."
    (lisp (Result StreamError Char) (in)
      (with-converting-stream-errors
        (cl:read-char in cl:t cl:nil cl:nil))))

  (declare read-line! (Input -> (Result StreamError String)))
  (define (read-line! in)
    "Read a line up to a newline from IN, discard the newline and return the line.

Blocks if no data is available from IN."
    (lisp (Result StreamError String) (in)
      (with-converting-stream-errors
          (cl:read-line in cl:t cl:nil cl:nil))))

  (declare iterator! ((Input -> (Result StreamError :elt))
                      -> Input
                      -> (iter:Iterator (Result StreamError :elt))))
  (define (iterator! read-elt! in)
    "Returns an iterator over elements of IN by READ-ELT!"
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

`next!' may block if no data is available on IN."
    (iterator! read-char! in))

  (declare lines! (Input -> (iter:Iterator (Result StreamError String))))
  (define (lines! in)
    "Returns an iterator over the lines of IN, calling `read-line!' successively.

`next!' may block if no data is available on IN."
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
  
  (declare close-output! (Output -> Unit))
  (define (close-output! out)
    "Close OUT, forcing its writes to complete and then freeing corresponding resources.

Future writes to OUT will fail.

It is safe to call `close-output!' on an `Output' that has already been closed."
    (when (open-output? out)
      (lisp :any (out)
        (cl:close out)))
    Unit)

  (declare try-close-output! (Output -> (Result StreamError Unit)))
  (define (try-close-output! out)
    "Close OUT, forcing its writes to complete and then freeing corresponding resources.

Returns (Err Closed) if OUT was already closed.

Future writes to OUT will fail."
    (if (open-output? out)
        (Ok (close-output! out))
        (Err Closed)))

  (declare write-char! (Output -> Char -> (Result StreamError Unit)))
  (define (write-char! out c)
    "Write C to OUT."
    (lisp (Result StreamError Unit) (out c)
      (with-converting-stream-errors
        (cl:write-char c out)
        Unit)))

  (declare write-string! (Output -> String -> (Result StreamError Unit)))
  (define (write-string! out str)
    "Write STR to OUT."
    (lisp (Result StreamError Unit) (out str)
      (with-converting-stream-errors
        (cl:write-string str out)
        Unit)))

  (declare write-line! (Output -> String -> (Result StreamError Unit)))
  (define (write-line! out str)
    "Write STR to OUT with a trailing newline."
    (lisp (Result StreamError Unit) (out str)
      (with-converting-stream-errors
        (cl:write-line str out)
        Unit)))

  (declare newline! (Output -> (Result StreamError Unit)))
  (define (newline! out)
    "Write a newline to OUT."
    (lisp (Result StreamError Unit) (out)
        (with-converting-stream-errors
          (cl:terpri out)
          Unit)))

  (declare standard-output (Unit -> Output))
  (define (standard-output)
    "Returns the current standard output stream, i.e. the value of `cl:*standard-output*'."
    (lisp Output () cl:*standard-output*)))
