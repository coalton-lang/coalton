(coalton-library/utils:defstdlib-package #:coalton-library/file
    (:use
     #:coalton
     #:coalton-library/classes
     #:coalton-library/builtin
     #:coalton-library/functions)
  (:local-nicknames (#:char-io #:coalton-library/char-io))
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
   #:char-input
   #:char-output

   ;; open options
   #:IfExists #:IfExistsError #:IfExistsRename #:IfExistsAppend #:IfExistsSupersede
   #:IfDoesNotExist #:IfDoesNotExistError #:IfDoesNotExistCreate #:IfDoesNotExistDefault
   #:FileOptions
   #:default-file-options
   #:with-input
   #:with-output
   #:with-io
   #:with-encoding
   #:if-exists
   #:if-does-not-exist

   ;; error handling
   #:UnknownFileError
   #:FileError #:Unknown #:Closed

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
   #:with-char-output!
   ))
(cl:in-package #:coalton-library/file)

;; encodings

(cl:defmacro define-cl-enum (type-name docstring cl:&body pairs)
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
  (declare char-input (File -> (Optional char-io:Input)))
  (define (char-input file)
    (if (input? file)
        (Some (lisp char-io:Input (file) file))
        None))

  (declare char-output (File -> (Optional char-io:Output)))
  (define (char-output file)
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
`IfDoesNotExistCreate' - create a new, empty file. The default for files opened for writing.
`IfDoesNotExistDefault' - behaves like `IfDoesNotExistError' for input-only files, and like `IfDoesNotExistCreate' for writable files."
  (IfDoesNotExistError :error)
  (IfDoesNotExistCreate :create)
  (IfDoesNotExistDefault :default))

(cl:defun if-does-not-exist-default (out? mode)
  (cl:if (cl:eq mode :default)
         (cl:if out?
                :create
                :error)
         mode))

(cl:defun direction-spec (in? out?)
  (cl:cond ((cl:and in? out?) :io)
           (in? :input)
           (out? :output)
           (cl:t :probe)))

(coalton-toplevel
  (define-type FileOptions
    (%FileOptions Boolean ; input?
                  Boolean ; output?
                  Encoding
                  IfExists
                  IfDoesNotExist))

  (declare default-file-options FileOptions)
  (define default-file-options
    (%FileOptions False False default-encoding IfExistsError IfDoesNotExistDefault))

  (declare with-input (FileOptions -> FileOptions))
  (define (with-input opts)
    (match opts
      ((%FileOptions _ out? enc if-ex if-not)
       (%FileOptions True out? enc if-ex if-not))))

  (declare with-output (FileOptions -> FileOptions))
  (define (with-output opts)
    (match opts
      ((%FileOptions in? _ enc if-ex if-not)
       (%FileOptions in? True enc if-ex if-not))))

  (declare with-io (FileOptions -> FileOptions))
  (define with-io (compose with-input with-output))

  (declare with-encoding (Encoding -> FileOptions -> FileOptions))
  (define (with-encoding enc opts)
    (match opts
      ((%FileOptions in? out? _ if-ex if-not)
       (%FileOptions in? out? enc if-ex if-not))))

  (declare if-exists (IfExists -> FileOptions -> FileOptions))
  (define (if-exists then opts)
    (match opts
      ((%FileOptions in? out? enc _ if-not)
       (%FileOptions in? out? enc then if-not))))

  (declare if-does-not-exist (IfDoesNotExist -> FileOptions -> FileOptions))
  (define (if-does-not-exist then opts)
    (match opts
      ((%FileOptions in? out? enc if-ex _)
       (%FileOptions in? out? enc if-ex then)))))

;; error handling

(coalton-toplevel
  ;; neither the CL spec nor SBCL's extensions offer any useful information from a `cl:file-error'. they have
  ;; a single slot, `pathname', and no interesting subclasses. as a result, i'm just returning the
  ;; `cl:file-error' object.
  (repr :native cl:file-error)
  (define-type UnknownFileError)

  (define-type FileError
    (Unknown UnknownFileError)
    Closed))

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
  (declare open! (FileOptions -> Path -> (Result FileError File)))
  (define (open! opts path)
    (match opts
      ((%FileOptions in? out? enc if-ex if-not)
       (match path
         ((%Path pathname)
          (lisp (Result FileError File) (in? out? enc if-ex if-not pathname)
            (cl:handler-case
                (cl:open pathname
                         :direction (direction-spec in? out?)
                         :external-format enc
                         :element-type 'cl:character
                         :if-exists if-ex
                         :if-does-not-exist (if-does-not-exist-default out? if-not))
              (cl:file-error (e) (Err (Unknown e)))
              (:no-error (file) (Ok file)))))))))

  (declare close! (File -> Unit))
  (define (close! file)
    (when (open? file)
      (lisp :any (file)
        (cl:close file))
      Unit))

  (declare try-close! (File -> (Result FileError Unit)))
  (define (try-close! file)
    (if (open? file)
        (Ok (close! file))
        (Err Closed)))

  (declare with-resource! ((Unit -> :state) -> (:state -> :result) -> (:state -> Unit) -> :result))
  (define (with-resource! ctor thunk dtor)
    "A wrapper around `cl:unwind-protect'"
    (lisp :result (ctor thunk dtor)
      (cl:let (state)
        (cl:unwind-protect (cl:progn (cl:setf state
                                              (coalton-impl/codegen:a1 ctor Unit))
                                     (coalton-impl/codegen:a1 thunk state))
          (cl:when state
            (coalton-impl/codegen:a1 dtor state))))))

  (declare with-file! (FileOptions -> Path -> (File -> :result) -> (Result FileError :result)))
  (define (with-file! opts path thunk)
    (with-resource!
        (fn () (open! opts path))
      (fn (open-res) (map thunk open-res))
      (fn (open-res) (match open-res
                       ((Ok file) (close! file))
                       (_ Unit))))))

;; convenience functions for input-only and output-only files

(coalton-toplevel
  (declare open-char-input! (FileOptions -> Path -> (Result FileError char-io:Input)))
  (define (open-char-input! opts path)
    (map (compose unwrap char-input)
         (open! (with-input opts) path)))

  (declare open-char-output! (FileOptions -> Path -> (Result FileError char-io:Output)))
  (define (open-char-output! opts path)
    (map (compose unwrap char-output)
         (open! (with-output opts) path)))

  (declare with-char-input! (FileOptions -> Path -> (char-io:Input -> :result) -> (Result FileError :result)))
  (define (with-char-input! opts path thunk)
    (with-file! (with-input opts)
      path
      (fn (file) (thunk (unwrap (char-input file))))))

  (declare with-char-output! (FileOptions -> Path -> (char-io:Output -> :result) -> (Result FileError :result)))
  (define (with-char-output! opts path thunk)
    (with-file! (with-output opts)
      path
      (fn (file) (thunk (unwrap (char-output file)))))))
