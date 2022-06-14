(coalton-library/utils:defstdlib-package #:coalton-library/file
    (:use
     #:coalton
     #:coalton-library/classes
     #:coalton-library/builtin
     #:coalton-library/functions)
  (:local-nicknames (#:char-io #:coalton-library/char-io)
                    (#:resource #:coalton-library/resource))
  (:export

   ;; encodings
   #:Encoding #:ASCII #:UTF-8 #:UTF-16 #:LATIN-1
   #:default-encoding

   ;; the type
   #:File

   ;; introspection ops
   #:open?
   #:input?
   #:output?
   #:io?

   ;; conversions into char streams
   #:get-char-input!
   #:get-char-output!

   ;; open options
   #:IfExists #:IfExistsError #:IfExistsRename #:IfExistsAppend #:IfExistsSupersede
   #:IfDoesNotExist #:IfDoesNotExistError #:IfDoesNotExistCreate #:IfDoesNotExistDefault
   #:Direction #:Input #:Output #:InputOutput
   #:FileOptions #:config
   #:with-mode
   #:with-encoding
   #:if-exists
   #:if-does-not-exist

   ;; paths
   #:Path

   ;; opening and closing
   #:open!
   #:close! #:try-close!
   #:with-file!

   ;; convenience functions for input-only and output-only filess
   #:open-char-input!
   #:open-char-output!
   #:with-char-input!
   #:with-char-output!))
(cl:in-package #:coalton-library/file)

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

;; encodings

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

;; file typedef and inspection ops

(coalton-toplevel
  (repr :native cl:file-stream)
  (define-type File
    "A file which may be readable and/or writeable.")

  (declare open? (File -> Boolean))
  (define (open? file)
    (lisp Boolean (file)
      (cl:open-stream-p file)))

  (declare input? (File -> Boolean))
  (define (input? file)
    (lisp Boolean (file)
      (cl:input-stream-p file)))

  (declare output? (File -> Boolean))
  (define (output? file)
    (lisp Boolean (file)
      (cl:output-stream-p file)))

  (declare io? (File -> Boolean))
  (define (io? file)
    (and (input? file) (output? file))))

;; converting files into char streams

(coalton-toplevel
  (declare get-char-input! (File -> (Optional char-io:Input)))
  (define (get-char-input! file)
    "Returns the `char-io:Input' corresponding to FILE, if one exists.

Repeated calls to `get-char-input!' on the same FILE will return the same `char-io:Input'."
    (if (input? file)
        (Some (lisp char-io:Input (file) file))
        None))

  (declare get-char-output! (File -> (Optional char-io:Output)))
  (define (get-char-output! file)
    "Returns the `char-io:Output' corresponding to FILE, if one exists.

Repeated calls to `get-char-output!' on the same FILE will return the same `char-io:Output'."
    (if (output? file)
        (Some (lisp char-io:Output (file) file))
        None)))

;; file-opening options

(define-cl-enum IfExists
    "Behavior taken if a file to be opened for writing exists.

Supported are a subset of the operators allowed by `cl:open'; see the Hyperspec for more detailed descriptions.

`IfExistsError' - return an error from `open!'. The default.
`IfExistsRename' - rename the existing file, and create a new file with the intended name.
`IfExistsAppend' - open the existing file for writing, with its file pointer initially at the end.
`IfExistsSupersede' - create a new file with the intended name.
"
  (IfExistsError :error)
  (IfExistsRename :rename)
  (IfExistsAppend :append)
  (IfExistsSupersede :supersede))

(define-cl-enum IfDoesNotExist
    "Behavior taken if a file to be opened does not exist.

Applies to files opened for both reading or writing, but with different defaults.

`IfDoesNotExistError' - return an error from `open!'. The default for files opened only for reading.
`IfDoesNotExistCreate' - create a new, empty file. The default for files opened for writing."
  (IfDoesNotExistError :error)
  (IfDoesNotExistCreate :create))

(define-cl-enum Direction
    "A direction for opening a `File'"
  (Input :input)
  (Output :output)
  (InputOutput :io))

(coalton-toplevel
  (define-instance (Eq Direction)
    (define (== a b)
      (lisp Boolean (a b)
        (cl:eq a b))))

  (define-type FileOptions
    (%FileOptions Direction
                  Encoding
                  IfExists
                  IfDoesNotExist))

  (declare config (Direction -> FileOptions))
  (define (config dir)
    (%FileOptions dir
                  default-encoding
                  IfExistsError
                  (if (== dir Input) IfDoesNotExistError
                      IfDoesNotExistCreate)))

  (declare with-mode (Direction -> FileOptions -> FileOptions))
  (define (with-mode dir opts)
    (match opts
      ((%FileOptions _ enc if-ex if-not)
       (%FileOptions dir enc if-ex if-not))))

  (declare with-encoding (Encoding -> FileOptions -> FileOptions))
  (define (with-encoding enc opts)
    (match opts
      ((%FileOptions dir _ if-ex if-not)
       (%FileOptions dir enc if-ex if-not))))

  (declare if-exists (IfExists -> FileOptions -> FileOptions))
  (define (if-exists then opts)
    (match opts
      ((%FileOptions dir enc _ if-not)
       (%FileOptions dir enc then if-not))))

  (declare if-does-not-exist (IfDoesNotExist -> FileOptions -> FileOptions))
  (define (if-does-not-exist then opts)
    (match opts
      ((%FileOptions dir enc if-ex _)
       (%FileOptions dir enc if-ex then)))))

;; paths

(coalton-toplevel
  ;; as of now, i'm not convinced there's a useful way to represent paths as anything other than strings. if
  ;; it turns out in the future that there is, using an opaque wrapper here allows us to use it without
  ;; breaking the API.
  (define-type Path
    (%Path String))

  (define-instance (Into String Path)
    (define into %Path)))

;; opening and closing files

(coalton-toplevel
  (declare open! (FileOptions -> Path -> (Result char-io:StreamError File)))
  (define (open! opts path)
    (match opts
      ((%FileOptions dir enc if-ex if-not)
       (match path
         ((%Path pathname)
          (lisp (Result char-io:StreamError File) (dir enc if-ex if-not pathname)
            (char-io:%with-converting-stream-errors
              (cl:open pathname
                       :direction dir
                       :external-format enc
                       :element-type 'cl:character
                       :if-exists if-ex
                       :if-does-not-exist if-not))))))))

  (declare close! (File -> (Result char-io:StreamError Unit)))
  (define (close! file)
    "Close FILE, forcing any writes to complete and then freeing corresponding resources.

This will close FILE's corresponding `char-io:Input' and `char-io:Output'."
    (if (open? file)
        (lisp :any (file)
          (char-io:%with-converting-stream-errors
            (cl:close file)
            Unit))
        (Err char-io:Closed)))

  (declare with-file! (FileOptions
                       -> Path
                       -> (File -> (Result char-io:StreamError :result))
                       -> (Result char-io:StreamError :result)))
  (define (with-file! opts path thunk)
    (match (resource:with-resource
               (fn () (open! opts path))
             (fn (file) (thunk file))
             (fn (file) (close! file)))
      ((Ok res) (Ok res))
      ((Err e) (Err (resource:flatten-resource-error e))))))

;; convenience functions for input-only and output-only files

(coalton-toplevel
  (declare open-char-input! (FileOptions -> Path -> (Result char-io:StreamError char-io:Input)))
  (define (open-char-input! opts path)
    (map (compose unwrap get-char-input!)
         (open! (with-mode Input opts) path)))

  (declare open-char-output! (FileOptions -> Path -> (Result char-io:StreamError char-io:Output)))
  (define (open-char-output! opts path)
    (map (compose unwrap get-char-output!)
         (open! (with-mode Output opts) path)))

  (declare with-char-input! (FileOptions
                             -> Path
                             -> (char-io:Input -> (Result char-io:StreamError :result))
                             -> (Result char-io:StreamError :result)))
  (define (with-char-input! opts path thunk)
    (with-file! (with-mode Input opts)
      path
      (fn (file) (thunk (unwrap (get-char-input! file))))))

  (declare with-char-output! (FileOptions
                              -> Path
                              -> (char-io:Output -> (Result char-io:StreamError :result))
                              -> (Result char-io:StreamError :result)))
  (define (with-char-output! opts path thunk)
    (with-file! (with-mode Output opts)
      path
      (fn (file) (thunk (unwrap (get-char-output! file)))))))
