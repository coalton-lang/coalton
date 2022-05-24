(coalton-library/utils:defstdlib-package #:coalton-library/file
    (:use
     #:coalton
     #:coalton-library/classes
     #:coalton-library/builtin)
  (:local-nicknames (#:iter #:coalton-library/iterator))
  (:export

   ;; encodings
   #:Encoding #:ASCII #:UTF-8 #:UTF-16 #:LATIN-1
   #:default-encoding

   ;; input files
   #:Input
   #:open-input! #:input-with-encoding!
   #:close-input!
   #:open-input?
   #:read-char!
   #:read-line!
   #:iterator!
   #:chars!
   #:lines!
   #:call-with-input-file! #:with-input-file!
   #:standard-input
   #:call-with-standard-input! #:with-standard-input!

   ;; output files
   #:Output
   #:open-output! #:output-with-encoding!
   #:close-output!
   #:open-output?
   #:write-char!
   #:write-string!
   #:write-line!
   #:newline!
   #:call-with-output-file! #:with-output-file!
   #:standard-output
   #:call-with-standard-output! #:with-standard-output!

   ;; formatted output
   #:Show #:show!))
(cl:in-package #:coalton-library/file)

;; encodings

(coalton-toplevel
  (define-type Encoding
    "A text encoding; CL calls this an \"external format\".

Others are allowed; SBCL supports a wealth, listed at http://www.sbcl.org/manual/#Supported-External-Formats .

To add a new external format, define a variant of `Encoding', and add a corresponding branch to `encoding-name'."
    ASCII
    UTF-8
    UTF-16
    LATIN-1)

  (declare encoding-name (Encoding -> Lisp-Object))
  (define (encoding-name enc)
    (match enc
      ((ASCII) (lisp Lisp-Object () :ascii))
      ((UTF-8) (lisp Lisp-Object () :utf-8))
      ((UTF-16) (lisp Lisp-Object () :utf-16))
      ((LATIN-1) (lisp Lisp-Object () :latin-1))))

  (define default-encoding UTF-8))

;; input files

(coalton-toplevel
  (repr :native (cl:and cl:stream (cl:satisfies cl:input-stream-p)))
  (define-type Input
    "An input stream, from which characters can be read.")

  (declare input-with-encoding! (Encoding -> String -> (Optional Input)))
  (define (input-with-encoding! enc path)
    "Open PATH as an input file using the encoding ENC."
    (let ((enc-name (encoding-name enc)))
      (lisp (Optional Input) (path enc-name)
        (cl:handler-case
            (cl:open path
                     :direction :input
                     :element-type 'cl:character
                     :if-does-not-exist :error
                     :external-format enc-name)
          (cl:file-error () None)
          (:no-error (f) (Some f))))))

  (declare open-input! (String -> (Optional Input)))
  (define (open-input! path)
    "Open PATH as an input file using the default encoding."
    (input-with-encoding! default-encoding path))

  (declare open-input? (Input -> Boolean))
  (define (open-input? file)
    "Is FILE open? Returns `False' if `close-input!' has been called on FILE in the past.

If `False', reads from FILE will fail."
    (lisp Boolean (file)
      (cl:open-stream-p file)))

  (declare close-input! (Input -> Unit))
  (define (close-input! file)
    "Close FILE, freeing any corresponding resources. Future reads from FILE will fail.

It is safe to call `close-input!' on an `Input' that is already closed."
    (when (open-input? file)
      (lisp :any (file)
        (cl:close file))
      Unit))

  (declare read-char! (Input -> (Optional Char)))
  (define (read-char! file)
    "Read a character from FILE.

Blocks if no data is available from FILE."
    (lisp (Optional Char) (file)
      (cl:handler-case
          (cl:read-char file cl:t cl:nil cl:nil)
        (cl:error () None)
        (:no-error (c) (Some c)))))

  (declare read-line! (Input -> (Optional String)))
  (define (read-line! file)
    "Read a line up to a newline from FILE, discard the newline and return the line.

Blocks if no data is available from FILE."
    (lisp (Optional String) (file)
      (cl:handler-case
          (cl:read-line file cl:t cl:nil cl:nil)
        (cl:error () None)
        (:no-error (l missing-newline-p)
          (cl:declare (cl:ignore missing-newline-p))
          (Some l)))))

  (declare iterator! ((Input -> (Optional :elt))
                      -> Input
                      -> (iter:Iterator :elt)))
  (define (iterator! read-elt! file)
    "Returns an iterator over elements of FILE by READ-ELT!, closing FILE after READ-ELT! returns `None' for the first time."
    (iter:new
     (fn ()
       (if (not (open-input? file))
           None
           (match (read-elt! file)
             ((Some elt) (Some elt))
             ((None) (progn (close-input! file)
                            None)))))))

  (declare chars! (Input -> (iter:Iterator Char)))
  (define (chars! file)
    "Returns an iterator over the characters of FILE, calling `read-char!' successively.

Closes the file when done.

`next!' may block if no data is available on FILE."
    (iterator! read-char! file))

  (declare lines! (Input -> (iter:Iterator String)))
  (define (lines! file)
    "Returns an iterator over the lines of FILE, calling `read-line!' successively.

Closes the file when done.

`next!' may block if no data is available on FILE."
    (iterator! read-line! file))

  (declare call-with-input-file! (Encoding -> String -> (Input -> :res) -> (Optional :res)))
  (define (call-with-input-file! enc path thunk)
    "Open PATH as an ENC-encoded input file, call THUNK with the file, then close the file.

If opening the file fails, THUNK will not be called, and `None' is returned."
    (match (input-with-encoding! enc path)
      ((Some file)
       (progn (let res = (thunk file))
              (close-input! file)
              (Some res)))
      ((None) None)))

  (declare standard-input (Unit -> Input))
  (define (standard-input)
    "Returns the current standard input stream, i.e. the value of `cl:*standard-input*'."
    (lisp Input () cl:*standard-input*))

  (declare call-with-standard-input! (Input -> (Unit -> :ret) -> :ret))
  (define (call-with-standard-input! inp thunk)
    "Call THUNK with INP as the standard input stream, i.e. with `cl:*standard-input*' bound to INP."
    (lisp :ret (inp thunk)
      (cl:let ((cl:*standard-input* inp))
        (coalton-impl/codegen/function-entry:a1 thunk Unit)))))

(cl:defmacro with-input-file! ((var path cl:&optional (enc 'default-encoding)) cl:&body body)
  "Evaluate BODY with VAR bound to an open `Input' file reading from PATH using ENC encoding.

Close the file after BODY.

If opening the file fails, BODY will not be evaluated, and `None' is returned."
  `(call-with-input-file! ,enc ,path (fn (,var) ,@body)))

(cl:defmacro with-standard-input! ((stream) cl:&body body)
  "Evaluate BODY with STREAM as the standard input stream, i.e. with `cl:*standard-input*' bound to STREAM."
  `(call-with-standard-input! ,stream (fn () ,@body)))

;; output files

(cl:defmacro lisp-write! (inputs cl:&body (do-write))
  `(lisp (Optional Unit) ,inputs
     (cl:handler-case ,do-write
       (cl:error () None)
       (:no-error (cl:&rest stuff) (cl:declare (cl:ignore stuff)) (Some Unit)))))

(coalton-toplevel
  (repr :native (cl:and cl:stream (cl:satisfies cl:output-stream-p)))
  (define-type Output
    "An output file, to which characters can be written.")

  (declare output-with-encoding! (Encoding -> String -> (Optional Output)))
  (define (output-with-encoding! enc path)
    "Open PATH for output using ENC as an encoding, replacing if it exists."
    (let enc-name = (encoding-name enc))
    (lisp (Optional Output) (enc-name path)
      (cl:handler-case
           (cl:open path
                    :direction :output
                    :element-type 'cl:character
                    :if-exists :supersede
                    :external-format enc-name)
         (cl:file-error () None)
         (:no-error (f) (Some f)))))

  (declare open-output! (String -> (Optional Output)))
  (define (open-output! path)
    "Open PATH for output using the default encoding, replacing if it exists."
    (output-with-encoding! default-encoding path))

  (declare open-output? (Output -> Boolean))
  (define (open-output? file)
    "Is FILE open? Returns `False' if `close-output!' has been called on FILE in the past.

If `False', writes to FILE will fail."
    (lisp Boolean (file)
      (cl:open-stream-p file)))
  
  (declare close-output! (Output -> Unit))
  (define (close-output! file)
    "Close FILE, forcing its writes to complete and then freeing corresponding resources.

Future writes to FILE will fail.

It is safe to call `close-output!' on an `Output' that has already been closed."
    (when (open-output? file)
      (lisp :any (file)
        (cl:close file)))
    Unit)

  (declare write-char! (Output -> Char -> (Optional Unit)))
  (define (write-char! file c)
    "Write C to FILE. Returns (Some Unit) if successful, or None if the write failed."
    (lisp-write! (file c)
      (cl:write-char c file)))

  (declare write-string! (Output -> String -> (Optional Unit)))
  (define (write-string! file str)
    "Write STR to FILE. Returns (Some Unit) if successful, or None if the write failed."
    (lisp-write! (file str)
      (cl:write-string str file)))

  (declare write-line! (Output -> String -> (Optional Unit)))
  (define (write-line! file str)
    "Write STR to FILE with a trailing newline. Returns (Some Unit) if successful, or None if the write failed."
    (lisp-write! (file str)
      (cl:write-line str file)))

  (declare newline! (Output -> (Optional Unit)))
  (define (newline! file)
    "Write a newline to FILE. Returns (Some Unit) if successful, or None if the write failed."
    (lisp-write! (file)
      (cl:terpri file)))

  (declare call-with-output-file! (Encoding -> String -> (Output -> :res) -> (Optional :res)))
  (define (call-with-output-file! enc path thunk)
    "Open PATH as an ENC-encoded output file, call THUNK with the file, then close the file.

If opening the file fails, THUNK will not be called, and `None' is returned."
    (match (output-with-encoding! enc path)
      ((Some file)
       (progn (let res = (thunk file))
              (close-output! file)
              (Some res)))
      ((None) None)))

  (declare standard-output (Unit -> Output))
  (define (standard-output)
    "Returns the current standard output stream, i.e. the value of `cl:*standard-output*'."
    (lisp Output () cl:*standard-output*))

  (declare call-with-standard-output! (Output -> (Unit -> :res) -> :res))
  (define (call-with-standard-output! file thunk)
    "Call THUNK with FILE as the standard output stream, i.e. with `cl:*standard-output*' bound to FILE."
    (lisp :res (file thunk)
      (cl:let ((cl:*standard-output* file))
        (coalton-impl/codegen/function-entry:a1 thunk Unit)))))

(cl:defmacro with-output-file! ((var path cl:&optional (enc 'UTF-8)) cl:&body body)
  "Evaluate BODY with VAR bound to an open `Output' file writing to PATH using ENC encoding.

If a file already exists at PATH, it will be replaced.

Close the file after BODY.

If opening the file fails, BODY will not be evaluated, and `None' is returned."
  `(call-with-output-file! ,enc ,path (fn (,var) ,@body)))

(cl:defmacro with-standard-output! ((file) cl:&body body)
  "Evaluate BODY with FILE as the standard output stream, i.e. with `cl:*standard-output*' bound to FILE."
  `(call-with-standard-output! ,file (fn () ,@body)))

;; formatted output

(coalton-toplevel
  (define-class (Show :showable)
    "Types which can be printed on an `Output' stream.

`show!' methods should return (Some Unit) if the write succeeds, or None if the write fails."
    (show! (Output -> :showable -> (Optional Unit))))

  (define-instance (Show Char)
    (define show! write-char!))

  (define-instance (Show String)
    (define show! write-string!)))

(cl:defmacro define-show-integer (type)
  `(coalton-toplevel
     (define-instance (Show ,type)
       (define (show! file int)
         (lisp-write! (file int)
           (cl:format file "~d" int))))))

(define-show-integer Integer)
(define-show-integer UFix)
(define-show-integer IFix)
(define-show-integer U8)
(define-show-integer I8)
(define-show-integer U16)
(define-show-integer I16)
(define-show-integer U32)
(define-show-integer I32)
(define-show-integer U64)
(define-show-integer I64)
