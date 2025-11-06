(coalton-library/utils:defstdlib-package #:coalton-library/file
    (:documentation "This is Coalton's library for directory utilities and file IO.

Most functions return outputs of type `(Result FileError :a)`, ensuring that errors can be assessed and handled.

File IO is handled using stream options, for instance:

```
(with-open-file (Bidirectional file EError)
  (fn (stream)
    (write-string stream \"Hello World!\")
    (read-file-to-vector stream)
```

Common Lisp makes a distinction between file and directory paths. Directory paths are always terminated with a trailing slash, file paths must never have a trailing slash.")
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/system)
  (:local-nicknames
   (#:str #:coalton-library/string)
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell)
   (#:list #:coalton-library/list)
   (#:vec #:coalton-library/vector)
   (#:res #:coalton-library/result)
   (#:types #:coalton-library/types)
   (#:char #:coalton-library/char))
  (:export

   #:Pathname

   #:FileError
   #:PathError
   #:LispError
   #:EOF

   #:directory-pathname?
   #:file-pathname?
   #:exists?
   #:directory-exists?
   #:file-exists?

   #:merge

   #:create-directory!
   #:directory-files
   #:subdirectories
   #:empty?
   #:remove-directory!
   #:remove-directory-recursive!
   #:system-relative-pathname

   #:copy!
   #:delete-file!

   #:FileStream

   #:IfExists
   #:EError
   #:Overwrite
   #:Append
   #:Supersede

   #:StreamOptions
   #:Input
   #:Output
   #:Bidirectional

   #:close
   #:abort

   #:read-char
   #:read-line
   #:write-char

   #:flush
   #:file-position
   #:set-file-position

   #:File
   #:open
   #:read
   #:write

   #:with-open-file
   #:read-vector
   #:read-file-to-vector
   #:write-vector
   #:write-string
   #:write-line

   #:create-temp-directory!
   #:create-temp-file!
   #:with-temp-file
   #:with-temp-directory

   #:append-to-file!
   #:write-to-file!
   #:read-file-to-string
   #:read-file-lines))

(in-package #:coalton-library/file)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;;
;;; Pathnames, lisp conditions, condition handling
;;;

(coalton-toplevel

  (repr :native cl:pathname)
  (define-type Pathname
    "Pathname object. Equivalent to `cl:pathname`")

  (define-instance (Into String Pathname)
    (define (into s)
      (lisp Pathname (s)
        (cl:pathname s))))

  (define-instance (Into Pathname String)
    (define (into p)
      (lisp String (p)
        (cl:namestring p))))

  (define-instance (Eq Pathname)
    (define (== a b)
      (lisp Boolean (a b)
        (cl:equalp a b))))

  (define-instance (Ord Pathname)
    (define (<=> p q)
      (<=> (the String (into p))
           (the String (into q))))))

;;;
;;; Handling Errors
;;;

(coalton-toplevel

  (define-type FileError
    "Errors for file functions."
    (FileError String)
    (PathError String Pathname)
    (LispError LispCondition)
    EOF)

  (define-instance (Signalable FileError)
    (define (error ferr)
      (match ferr
        ((FileError str1)
         (error (lisp String (str1)
                  (cl:format cl:nil "File Error:~%~%~a" str1))))
        ((PathError str path)
         (error (lisp String (str path)
                  (cl:format cl:nil "Path Error:~%~%~a ~a" str path))))
        ((LispError c)
         (error c))
        ((EOF)
         (error #.(cl:format cl:nil "Error~%~%End of File")))))))

;;;
;;; Macro for handling lisp errors in file functions
;;;

(cl:defmacro %handle-file-function (form)
  "A macro for handling potentially erroring Lisp file operations.
Automatically returns the lisp condition if one is thrown."
  (cl:let ((c (cl:gensym "C")))
    `(cl:handler-case (Ok ,form)
       (cl:error (,c) (Err (LispError ,c))))))

;;;
;;; Handling existential path queries
;;;

(coalton-toplevel

  (declare directory-pathname? ((Into :a Pathname) => :a -> Boolean))
  (define (directory-pathname? path)
    "Returns True if a pathname has no file component."
    (let p = (the Pathname (into path)))
    (lisp Boolean (p)
      (to-boolean (uiop:directory-pathname-p p))))

  (declare file-pathname? ((Into :a Pathname) => :a -> Boolean))
  (define (file-pathname? path)
    "Returns True if a pathname has a file component."
    (let p = (the Pathname (into path)))
    (lisp Boolean (p)
      (to-boolean (uiop:file-pathname-p p))))

  (declare exists? ((Into :a Pathname) => :a -> (Result FileError Boolean)))
  (define (exists? path)
    "Returns whether a file or directory exists."
    (let p = (the Pathname (into path)))
    (lisp (Result FileError Boolean) (p)
      (%handle-file-function (to-boolean (cl:probe-file p)))))

  (declare %if-directory-path ((Into :a Pathname)
                               => (Pathname -> (Result FileError :b))
                               -> :a
                               -> (Result FileError :b)))
  (define (%if-directory-path action path)
    "Performs an operation only if the path is a valid directory pathname."
    (let p = (the Pathname (Into path)))
    (let dir = (directory-pathname? p))
    (if dir
        (action p)
        (Err (PathError "Invalid directory path." p))))

  (declare directory-exists? ((Into :a Pathname) => :a -> (Result FileError Boolean)))
  (define (directory-exists? path)
    "Returns True if a pathname names a directory that exists."
    (%if-directory-path
     (fn (p)
       (lisp (Result FileError Boolean) (p)
         (%handle-file-function (to-boolean (uiop:directory-exists-p p)))))
     path))

  (declare file-exists? ((Into :a Pathname) => :a -> (Result FileError Boolean)))
  (define (file-exists? path)
    "Returns True if a pathname names a file that exists."
    (do
     (let p = (the Pathname (Into path)))
     (let file = (file-pathname? p))
      (if file
          (lisp (Result FileError Boolean) (p)
            (%handle-file-function (to-boolean (uiop:file-exists-p p))))
          (Err (PathError "Invalid file path." p))))))

;;;
;;; Merging, semigroup and monoid
;;;

(coalton-toplevel

  (declare merge ((Into :a Pathname) (Into :b Pathname) => :a -> :b -> Pathname))
  (define (merge path1 path2)
    "Merges two pathnames together. The directory pathname should be the first argument."
    (let p1 = (the Pathname (into path1)))
    (let p2 = (the Pathname (into path2)))
    (if (not (directory-pathname? p1))
        (error (PathError "Merge: first argument must be a directory path." p1))
        (lisp Pathname (p1 p2)
          (cl:merge-pathnames p2 p1))))

  (define-instance (Semigroup Pathname)
    (define <> merge))

  (define-instance (Monoid Pathname)
    (define mempty (the Pathname (into "")))))
;;;
;;; Working with directories
;;;

(coalton-toplevel

  (declare create-directory! ((Into :a Pathname) => :a -> (Result FileError Pathname)))
  (define (create-directory! path)
    "This is equivalent to `mkdir -p`. Creates a directory and its parents. The pathname must be a valid directory pathname."
    (%if-directory-path (fn (p)
                          (lisp (Result FileError Pathname) (p)
                            (%handle-file-function (cl:ensure-directories-exist p))))
                        path))



  (declare directory-files ((Into :a Pathname) => :a -> (Result FileError (List Pathname))))
  (define (directory-files path)
    "Returns all files within the directory. Returns an error if the pathname is not a directory pathname."
    (%if-directory-path (fn (p)
                          (lisp (Result FileError (List Pathname)) (p)
                            (%handle-file-function (uiop:directory-files p))))
                        path))

  (declare subdirectories ((Into :a Pathname) => :a -> (Result FileError (List Pathname))))
  (define (subdirectories path)
    "Returns all subdirectories within the directory. Returns an error if the pathname is not a directory pathname."
    (%if-directory-path (fn (p)
                          (lisp (Result FileError (List Pathname)) (p)
                            (%handle-file-function (uiop:subdirectories p))))
                        path))

  ;;
  ;; Handling directory behavior that depends on emptiness
  ;;

  (declare empty? ((Into :a Pathname) => :a -> (Result FileError Boolean)))
  (define (empty? path)
    "Checks whether a directory is empty."
    (%if-directory-path (fn (p)
                          (pure (lisp Boolean (p)
                                  (cl:null (cl:directory (cl:merge-pathnames uiop:*wild-directory* p))))))
                        path))

  (declare remove-directory! ((Into :a Pathname) => :a -> (Result FileError :a)))
  (define (remove-directory! path)
    "Deletes an empty directory."
    (let p = (the Pathname (into path)))
    (lisp (Result FileError :a) (p)
      (%handle-file-function (uiop:delete-empty-directory p))))

  (declare remove-directory-recursive! ((Into :a Pathname) => :a -> (Result FileError Unit)))
  (define (remove-directory-recursive! path)
    "Deletes a target directory recursively. Equivalent to `rm -r`. Errors if the path is not a directory."
    (%if-directory-path (fn (p)
                          (lisp (Result FileError Unit) (p)
                            (%handle-file-function (uiop:delete-directory-tree p :validate cl:t))))
                        path))

  (declare system-relative-pathname ((Into :a String) => :a -> String -> (Result FileError Pathname)))
  (define (system-relative-pathname system-name name)
    "Generates a system-relative-pathname for a given filename or path. This is a wrapper for `asdf:system-relative-pathname`. `Name` will likely be an empty string unless a subdirectory or filename is specified."
    (lisp (Result FileError Pathname) (system-name name)
      (cl:handler-case (Ok (asdf:system-relative-pathname system-name name))
        (cl:error (c) (Err (LispError c)))))))

;;;
;;; Basic File Operations
;;;

(coalton-toplevel

  (declare copy! ((Into :a Pathname) (Into :b Pathname) => :a -> :b -> (Result FileError Unit)))
  (define (copy! input output)
    "Copies a file to a new location."
    (do
     (let in = (the Pathname (into input)))
     (let out = (the Pathname (into output)))
      (if (not (file-pathname? in))
          (Err (PathError "Invalid input for copying, path is not a file:" in))
          (lisp (Result FileError :c) (in out)
            (%handle-file-function (uiop:copy-file in out))))))

  (declare delete-file! ((Into :a Pathname) => :a -> (Result FileError Unit)))
  (define (delete-file! path)
    "Deletes a given file if the file exists."
    (do
     (let p = (the Pathname (into path)))
     (lisp (Result FileError Unit) (p)
       (%handle-file-function (uiop:delete-file-if-exists p))))))

;;;
;;; FStreams, FileStreams, and options
;;;

(coalton-toplevel

  (repr :native cl:file-stream)
  (define-type (FileStream :a)
    "Represents a file stream, using `cl:file-stream`.")

  (repr :enum)
  (define-type IfExists
    "Possible options for opening a stream when the file exists."
    EError
    Overwrite
    Append
    Supersede)

  (define-type StreamOptions
    "A type for providing parameters for opening streams. StreamOptions take strings for pathnames, but they will error if they are not proper and appropriate pathnames."
    (Input Pathname)                  "Constructor for opening an input stream"
    (Output Pathname IfExists)        "Constructor for opening an output stream."
    (Bidirectional Pathname IfExists) "Constructor for opening a bidirectional stream.")

  ;;
  ;; Opening Streams
  ;;
  (declare %open-input (Pathname -> types:lisptype -> (Result FileError (FileStream :a))))
  (define (%open-input path etype)
    "Opens an input stream for the given filepath, and for a given type."
    (lisp (Result FileError (FileStream :a)) (path etype)
      (%handle-file-function
       (cl:open path
                :direction ':input
                :element-type etype
                :if-does-not-exist ':error))))

  (declare %open-output (Pathname -> IfExists -> types:lisptype -> (Result FileError (FileStream :a))))
  (define (%open-output path if-exists etype)
    "Opens an output stream for the given filepath, and for a given type."
    (lisp (Result FileError (FileStream :a)) (path if-exists etype)
      (%handle-file-function
       (cl:open path
                :direction ':output
                :element-type etype
                :if-exists (cl:case if-exists
                             (IfExists/EError ':error)
                             (IfExists/Overwrite ':overwrite)
                             (IfExists/Append ':append)
                             (IfExists/Supersede ':supersede))
                :if-does-not-exist ':create))))

  (declare %open-bidirectional (Pathname -> IFExists -> types:lisptype -> (Result FileError (FileStream :a))))
  (define (%open-bidirectional path if-exists etype)
    "Opens a two way stream for the given filepath and for a given type."
    (lisp (Result FileError (FileStream :a)) (path if-exists etype)
      (%handle-file-function
       (cl:open path
                :direction ':io
                :element-type etype
                :if-exists (cl:case if-exists
                             (IfExists/EError ':error)
                             (IfExists/Overwrite ':overwrite)
                             (IfExists/Append ':append)
                             (IfExists/Supersede ':supersede))
                :if-does-not-exist ':create))))

  (declare %open (StreamOptions -> types:lisptype -> (Result FileError (FileStream :a))))
  (define (%open stream-options etype)
    "Opens a FileStream for a given type and StreamOptions."
    (match stream-options
      ((Input path)
       (%open-input path etype))
      ((Output path exists)
       (%open-output path exists etype))
      ((Bidirectional path exists)
       (%open-bidirectional path exists etype))))

  ;;
  ;; Other basic stream operations
  ;;

  (declare close ((FileStream :a) -> (Result FileError :b)))
  (define (close stream)
    "Closes a FileStream."
    (lisp (Result FileError :a) (stream)
      (%handle-file-function (cl:close stream))))

  (declare abort ((FileStream :a) -> (Result FileError :b)))
  (define (abort stream)
    "Closes a FileStream and aborts all operations.."
    (lisp (Result FileError :a) (stream)
      (%handle-file-function (cl:close stream :abort cl:t))))

  (declare read-char ((FileStream Char) -> (Result FileError Char)))
  (define (read-char stream)
    "Reads a character from an FileStream."
    (lisp (Result FileError Char) (stream)
      (cl:handler-case (Ok (cl:read-char stream))
        (cl:end-of-file () (Err (EOF)))
        (cl:error (c) (Err (LispError c))))))

  (declare read-line (FileStream Char -> Result FileError String))
  (define (read-line stream)
    "Reads a line of characters from a FileStream."
    (lisp (Result FileError String) (stream)
      (cl:handler-case (Ok (cl:read-line stream))
        (cl:end-of-file () (err (EOF)))
        (cl:error (c) (Err (LispError c))))))

  (declare write-char ((FileStream Char) -> Char -> (Result FileError Unit)))
  (define (write-char stream data)
    "Writes a `Char` to the stream."
    (lisp (Result FileError Unit) (stream data)
      (%handle-file-function (cl:write-char data stream))))

  (define-class (%FileByte :a)
    "A class of `byte` types that can be written to and read from files.")

  (declare %read-byte ((%FileByte :a) => (FileStream :a) -> (Result FileError :a)))
  (define (%read-byte stream)
    "Reads a byte from a FileStream"
    (lisp (Result FileError :a) (stream)
      (cl:handler-case (Ok (cl:read-byte stream))
        (cl:end-of-file () (Err (EOF)))
        (cl:error (c) (Err (LispError c))))))

  (declare %write-byte ((%FileByte :a) => (FileStream :a) -> :a -> (Result FileError Unit)))
  (define (%write-byte stream data)
    "Writes a `Char` to the stream."
    (lisp (Result FileError Unit) (stream data)
      (%handle-file-function (cl:write-byte data stream))))

  (declare flush ((FileStream :a) -> (Result FileError :b)))
  (define (flush stream)
    "Blocks until `stream` has been flushed. Calls `cl:finish-output`."
    (lisp (Result FileError :b) (stream)
      (%handle-file-function (cl:finish-output stream))))

  (declare file-position ((FileStream :a) -> (Result FileError UFix)))
  (define (file-position stream)
    "Finds the file-position of a file stream."
    (lisp (Result FileError UFix) (stream)
      (%handle-file-function (cl:file-position stream))))

  (declare set-file-position ((FileStream :a) -> UFix -> (Result FileError Unit)))
  (define (set-file-position stream i)
    "Sets the file position of a file stream."
    (lisp (Result FileError Unit) (stream i)
      (%handle-file-function (cl:file-position stream i)))))

;;;
;;; File Class
;;;

(cl:defun %read-sequence-completely (v s n)
  "Read N elements of the stream S into the sequence V."
  (cl:loop
    :with begin := 0
    :while (cl:< begin n)
    :do (cl:incf begin (cl:read-sequence v s :start begin))
    :finally (cl:return v)))

(coalton-toplevel

  (define-class (File :a)
    "A class of types which are able to be written to or read from a file."
    (open           (StreamOptions   -> (Result FileError (Filestream :a))))
    (read           ((FileStream :a) -> (Result FileError :a)))
    (write          ((FileStream :a) -> :a -> (Result FileError Unit))))

  (define-instance (File Char)
    (define (open stream-options)
      (let ((type (types:runtime-repr (the (types:Proxy Char) types:Proxy))))
        (%open stream-options type)))
    (define (read fs)
      (read-char fs))
    (define (write fs data)
      (write-char fs data))))

;;;
;;; File instances for supported Integer/Byte types
;;;

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:defmacro define-file-type (type)
    `(progn (define-instance (%FileByte ,type))
            (define-instance (File ,type)
              (define (open stream-options)
                (let ((t (types:runtime-repr (the (types:Proxy ,type) types:Proxy))))
                  (%open stream-options t)))
              (define (read fs)
                (%read-byte fs))
              (define (write fs data)
                (%write-byte fs data))))))

(coalton-toplevel
  (define-file-type IFix)
  (define-file-type UFix)
  (define-file-type I8)
  (define-file-type U8)
  (define-file-type I16)
  (define-file-type U16)
  (define-file-type I32)
  (define-file-type U32)
  (define-file-type I64)
  (define-file-type U64))

;;;
;;;
;;;

(coalton-toplevel

  (declare with-open-file ((File :a)
                           =>
                           StreamOptions
                           -> ((FileStream :a) -> (Result FileError :b))
                           -> (Result FileError :b)))
  (define (with-open-file stream-options thunk)
    "Opens a file stream, performs `thunk` on it, then closes the stream."
    (bracket (open stream-options)
             close
             thunk))

  (declare read-vector ((File :a)
                        =>
                        (FileStream :a)
                        -> UFix
                        -> (Result FileError (vec:Vector :a))))
  (define (read-vector stream chunk-size)
    "Reads a chunk of a file into a vector of type `:a`."
    (lisp (Result FileError (vec:Vector :a)) (stream chunk-size)
      (%handle-file-function
       (cl:let ((v (cl:make-array chunk-size
                                  :adjustable cl:t
                                  :element-type cl:t)))
         (%read-sequence-completely v stream chunk-size)
         v))))

  (declare read-file-to-vector ((File :a)
                                =>
                                (FileStream :a)
                                -> (Result FileError (vec:Vector :a))))
  (define (read-file-to-vector stream)
    "Reads a file into a vector of type `:a`."
    (lisp (Result FileError (vec:Vector :a)) (stream)
      (%handle-file-function
       (cl:let* ((size (cl:file-length stream))
                 (v (cl:make-array size
                                   :adjustable cl:t
                                   :element-type cl:t)))
         (%read-sequence-completely v stream size)
         v))))

  (declare write-vector ((types:RuntimeRepr :a) (File :a)
                         => (FileStream :a)
                         -> (vec:Vector :a)
                         -> (Result FileError Unit)))
  (define (write-vector stream v)
    "Writes elements of an vector of type `:a` to a stream of type `:a`."

    (lisp (Result FileError Unit) (stream v)
      (%handle-file-function (cl:write-sequence v stream))))

  (declare write-string ((FileStream Char) -> String -> (Result FileError Unit)))
  (define (write-string fs s)
    "Writes a `string` to a FileStream of type Char."
    (write-vector fs (iter:collect! (str:chars s))))

  (declare write-line ((FileStream Char) -> String -> (Result FileError Unit)))
  (define (write-line stream s)
    "Writes a string with an appended newline to a filestream of type Char."
    (write-vector stream (iter:collect! (Str:chars s)))
    (write stream #\NewLine)))

;;;
;;; Temporary files and directories
;;;

(coalton-toplevel

  (declare %temp-directory (Unit -> Pathname))
  (define (%temp-directory)
    "Returns the current temporary directory, `uiop:*temporary-directory*`."
    (lisp Pathname ()
      (uiop:temporary-directory)))

  (declare %set-temp-directory ((Into :a Pathname) => :a -> (Result FileError Unit)))
  (define (%set-temp-directory path)
    "Sets the temporary directory."
    (%if-directory-path (fn (p)
                          (lisp (Result FileError Unit) (p)
                            (%handle-file-function (cl:setf uiop:*temporary-directory* p))))
                        path))

  (declare %make-temp-dir-pathname (Unit -> Pathname))
  (define (%make-temp-dir-pathname)
    "Makes a temporary directory pathname."
    (merge (%temp-directory)
           (lisp Pathname ()
             (cl:pathname
              (cl:format cl:nil "~acoal~36R-tmp/"
                         (uiop:temporary-directory)
                         (cl:random (cl:expt 36 8)))))))

  (declare %make-temp-file-pathname (String -> Pathname))
  (define (%make-temp-file-pathname file-ext)
    "Makes a pathname of a file in the temporary directory.
File extensions need to include `.`, like \".txt\"."
    (merge (%temp-directory)
           (lisp Pathname (file-ext)
             (cl:pathname
              (cl:format cl:nil "~36R-tmp~A"
                         (cl:random (cl:expt 36 8))
                         file-ext)))))

  (declare create-temp-directory! (Unit -> (Result FileError Pathname)))
  (define (create-temp-directory!)
    "This configures a default temporary directory for use."
    (create-directory! (%make-temp-dir-pathname)))

  (declare create-temp-file! (String -> (Result FileError Pathname)))
  (define (create-temp-file! file-ext)
    "This configures a default temporary file for use."
    (let filepath = (%make-temp-file-pathname file-ext))
    (do
     (lisp (Result FileError :a) (filepath)
       (%handle-file-function (cl:open filepath :direction :output :if-does-not-exist :create)))
     (pure filepath)))

  (declare with-temp-file ((File :a)
                           => String
                           -> ((FileStream :a) -> (Result FileError :b))
                           -> (Result FileError :b)))
  (define (with-temp-file file-type thunk)
    "Performs an operation `thunk` on a temporary file. File type extensions need to include `.`, like \".txt\"."
    (let file = (%make-temp-file-pathname file-type))
    (bracket
     (open (Bidirectional file Overwrite))
     (fn (_)
       (delete-file! file))
     thunk))

  (declare with-temp-directory ((Pathname -> (Result FileError :a)) -> (Result FileError :a)))
  (define (with-temp-directory thunk)
    "Performs an operation `thunk` inside a temporary directory."
    (bracket
     (create-temp-directory!)
     remove-directory-recursive!
     thunk)))

;;;
;;; Top-level functions, i.e. those that don't require FileStream or File knowledge
;;;

(coalton-toplevel

  (declare append-to-file! ((types:Runtimerepr :a)
                            (Into :p Pathname)
                            (File :a)
                            => :p
                            -> (vec:Vector :a)
                            -> (Result FileError Unit)))
  (define (append-to-file! path data)
    "Opens and appends a file with data of type :a."
    (with-open-file (Output (into path) Append)
      (fn (stream)
        (write-vector stream data))))

  (declare write-to-file! ((types:Runtimerepr :a)
                           (Into :p Pathname)
                           (File :a)
                           => :p
                           -> (vec:Vector :a)
                           -> (Result FileError Unit)))
  (define (write-to-file! path data)
    "Opens and writes to a file with data of type :a. Supersedes existing data on the file."
    (with-open-file (Output (into path) Supersede)
      (fn (stream)
        (write-vector stream data))))

  (declare read-file-to-string ((Into :a Pathname) => :a -> (Result FileError String)))
  (define (read-file-to-string path)
    "Reads a file into a string, given a pathname string."
    (let p = (the Pathname (into path)))
    (lisp (Result FileError String) (p)
      (%handle-file-function (uiop:read-file-string p))))

  (declare read-file-lines ((Into :a Pathname) => :a -> (Result FileError (List String))))
  (define (read-file-lines path)
    "Reads a file into lines, given a pathname or string."
    (let p = (the Pathname (into path)))
    (lisp (Result FileError (List String)) (p)
      (%handle-file-function (uiop:read-file-lines p)))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/FILE")
