(coalton-library/utils:defstdlib-package #:coalton-library/file
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-library/builtin
   #:coalton-library/functions)
  (:local-nicknames
   (#:str #:coalton-library/string)
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell)
   (#:list #:coalton-library/list)
   (#:vec #:coalton-library/vector)
   (#:res #:coalton-library/result)
   (#:types #:coalton-library/types)
   (#:char #:coalton-library/char)
   (#:math #:coalton-library/math/bounded))
  (:documentation "This file library is particularly sensitive to trailing slashes or lack thereof. Directory paths should always use trailing `/`s, and file paths should be without.")
  (:export
   
   #:Pathname

   #:merge
   #:directory-pathname?
   #:file-pathname?
   #:exists?
   #:directory-exists?
   #:file-exists?

   #:create-directory
   #:directory-files
   #:subdirectories
   #:empty?
   #:if-empty
   #:remove-directory
   #:remove-directory-recursive
   #:system-relative-pathname

   #:copy
   #:delete-file

   #:FileStream
   
   #:IfExists
   #:EError
   #:NewVersion
   #:Rename
   #:RenameAndDelete
   #:Overwrite
   #:Append
   #:Supersede

   #:IfDoesNotExist
   #:DNEError
   #:Create

   #:StreamOptions
   #:Input
   #:Output
   #:TwoWay

   #:read-char
   #:read-byte
   #:write-char
   #:write-byte
  
   #:flush
   #:clear
   #:file-position
   #:set-file-position

   #:Streamable
   #:open
   #:close
   #:read
   #:write

   #:with-open-file
   #:read-sequence
   #:write-sequence
   #:write-string
   #:write-line

   #:append-to-file
   #:write-to-file
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
    "Pathname object. Uses `cl:pathname`")

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

  (repr :native cl:condition)
  (define-type LispCondition
    "Condition for lisp error handling. Uses `cl:condition`.")

  (define-instance (Signalable LispCondition)
    (define (error condition)
      (lisp :a (condition)
        (cl:error condition)))))

(cl:defmacro handle-file-function ((func cl:&rest args))
  "A macro for handling potentially erroring lisp file operations. Automatically returns the lisp condition if one is thrown."
  `(cl:handler-case (Ok (,func ,@args))
     (cl:error (c) (Err (LispError c)))))

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
         (error #. (cl:format cl:nil "Error~%~%End of File")))))))

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

  (declare merge ((Into :a Pathname) (Into :b Pathname) => :a -> :b -> (Result FileError Pathname)))
  (define (merge path1 path2)
    "Merges two pathnames together. The directory pathname should be the first argument."
    (let p1 = (the Pathname (into path1)))
    (let p2 = (the Pathname (into path2)))
    (if (not (directory-pathname? p1))
        (Err (PathError "Merge: first argument must be a directory path." p1))
        (Ok (lisp Pathname (p1 p2)
              (cl:merge-pathnames p2 p1)))))

  (declare exists? ((Into :a Pathname) => :a -> (Result FileError Boolean)))
  (define (exists? path)
    "Returns whether a file or directory exists."
    (do
     (let p = (the Pathname (into path)))
     (lisp (Result FileError Boolean) (p)
       (handle-file-function (to-boolean (cl:probe-file p))))))

  (declare if-directory-path ((Into :a Pathname) => :a -> (Pathname -> (Result FileError :b)) -> (Result FileError :b)))
  (define (if-directory-path path action)
    "Performs an operation only if the path is a valid directory pathname."
    (do
     (let p = (the Pathname (Into path)))
     (let dir = (directory-pathname? p))
      (if dir
          (action p)
          (Err (PathError "This path does not represent a directory. Please add a trailing `/` or rethink your decision." p)))))
  
  (declare directory-exists? ((Into :a Pathname) => :a -> (Result FileError Boolean)))
  (define (directory-exists? path)
    "Returns True if a pathname names a directory that exists."
    (if-directory-path path (fn (p)
                              (lisp (Result FileError Boolean) (p)
                                (handle-file-function (to-boolean (uiop:directory-exists-p p)))))))

  (declare file-exists? ((Into :a Pathname) => :a -> (Result FileError Boolean)))
  (define (file-exists? path)
    "Returns True if a pathname names a file that exists."
    (do
     (let p = (the Pathname (Into path)))
     (let file = (file-pathname? p))
      (if file
          (lisp (Result FileError Boolean) (p)
              (handle-file-function (to-boolean (uiop:file-exists-p p))))
          (Err (PathError "This path does not represent a file. Please remove the trailing `/` or rethink your decision." p))))))

;;;
;;; Working with directories
;;;

(coalton-toplevel
  
  (declare create-directory ((Into :a Pathname) => :a -> (Result FileError Pathname)))
  (define (create-directory path)
    "This is equivalent to `mkdir -p`. Creates a directory and its parents. The pathname must be a valid directory pathname."
    (if-directory-path path
                       (fn (p)
                         (lisp (Result FileError Pathname) (p)
                           (handle-file-function (cl:ensure-directories-exist p))))))

  (declare directory-files ((Into :a Pathname) => :a -> (Result FileError (List Pathname))))
  (define (directory-files path)
    "Returns all files within the directory. Returns an error if the pathname is not a directory pathname."
    (if-directory-path path
                       (fn (p)
                         (lisp (Result FileError (List Pathname)) (p)
                           (handle-file-function (uiop:directory-files p))))))

  (declare subdirectories ((Into :a Pathname) => :a -> (Result FileError (List Pathname))))
  (define (subdirectories path)
    "Returns all subdirectories within the directory. Returns an error if the pathname is not a directory pathname."
    (if-directory-path path
                       (fn (p)
                         (lisp (Result FileError (List Pathname)) (p)
                           (handle-file-function (uiop:subdirectories p))))))

  ;;
  ;; Handling directory behavior that depends on emptiness
  ;;
  
  (declare empty? ((Into :a Pathname) => :a -> (Result FileError Boolean)))
  (define (empty? path)
    "Checks whether a directory is empty."
    (if-directory-path path
                       (fn (p)
                         (pure (lisp Boolean (p)
                                 (cl:null (cl:directory (cl:format cl:nil "~a*" p))))))))

  (declare remove-directory ((Into :a Pathname) => :a -> (Result FileError :a)))
  (define (remove-directory path)
    "Deletes an empty directory."
    (let p = (the Pathname (into path)))
    (lisp (Result FileError :a) (p)
      (handle-file-function (uiop:delete-empty-directory p))))
  
  
  (declare remove-directory-recursive ((Into :a Pathname) => :a -> (Result FileError Unit)))
  (define (remove-directory-recursive path)
    "Deletes a target directory recursively. Equivalent to `rm -r`. Errors if the path is not a directory."
    (if-directory-path path
                       (fn (p)
                         (lisp (Result FileError Unit) (p)
                           (handle-file-function (uiop:delete-directory-tree p))))))
  
  (declare system-relative-pathname ((Into :a String) => :a -> String -> (Result FileError Pathname)))
  (define (system-relative-pathname system-name name)
    "Generates a system-relative-pathname for a given filename or path."
    (lisp (Result FileError Pathname) (system-name name)
      (cl:handler-case (Ok (asdf:system-relative-pathname system-name name))
        (cl:error (c) (Err (LispError c)))))))

;;;
;;; Basic File Operations
;;;

(coalton-toplevel

  (declare copy ((Into :a Pathname) (Into :b Pathname) => :a -> :b -> (Result FileError Unit)))
  (define (copy input output)
    "Copies a file to a new location."
    (do
     (let in = (the Pathname (into input)))
     (let out = (the Pathname (into output)))
      (output-exists <- (exists? out))
      (let input-is-file = (file-pathname? in))
      (if output-exists
          (Err (PathError "Invalid output for copying, path already exists:" out))
          (if (not input-is-file)
              (Err (PathError "Invalid input for copying, path is not a file:" in))
              (lisp (Result FileError :c) (in out)
                (handle-file-function (uiop:copy-file in out)))))))

  (declare delete-file ((Into :a Pathname) => :a -> (Result FileError Unit)))
  (define (delete-file path)
    "Deletes a given file if the file exists."
    (do
     (let p = (the Pathname (into path)))
     (lisp (Result FileError Unit) (p)
       (handle-file-function (uiop:delete-file-if-exists p))))))

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
    NewVersion
    Rename
    RenameAndDelete
    Overwrite
    Append
    Supersede)

  (repr :enum)
  (define-type IfDoesNotExist
    "Possible options for opening a stream when the file does not exist."
    DNEError
    Create)

  (define-type (StreamOptions :a)
    "A type for providing parameters for opening streams. StreamOptions take strings for pathnames, but they will error if they are not proper and appropriate pathnames."
    (Input Pathname)
    (Output Pathname IfExists IfDoesNotExist)
    (Bidirectional Pathname IfExists IfDoesNotExist))

  ;;
  ;; Opening Streams
  ;;
  (declare %open-input (Pathname -> types:lisptype -> (Result FileError (FileStream :a))))
  (define (%open-input path etype)
    "Opens an input stream for the given filepath, and for a given type."
    (lisp (Result FileError (FileStream :a)) (path etype)
      (handle-file-function (cl:open path
                                     :direction :input
                                     :element-type etype))))

  (declare %open-output (Pathname -> IfExists -> IfDoesNotExist -> types:lisptype -> (Result FileError (FileStream :a))))
  (define (%open-output path if-exists if-does-not-exist etype)
    "Opens an output stream for the given filepath, and for a given type."
    (lisp (Result FileError (FileStream :a)) (path if-exists if-does-not-exist etype)
      (handle-file-function (cl:open path
                                     :direction :output
                                     :element-type etype
                                     :if-exists (cl:case if-exists
                                                  (IfExists/ExistsError ':error)
                                                  (IfExists/NewVersion ':new-version)
                                                  (IfExists/Rename ':rename)
                                                  (IfExists/RenameAndDelete ':rename-and-delete)
                                                  (IfExists/Overwrite ':overwrite)
                                                  (IfExists/Append ':append)
                                                  (IfExists/Supersede ':supersede))
                                     :if-does-not-exist (cl:case if-does-not-exist
                                                          (IfDoesNotExist/DNExistError ':error)
                                                          (IfDoesNotExist/Create ':create))))))

  (declare %open-bidirectional (Pathname -> IFExists -> IfDoesNotExist -> types:lisptype -> (Result FileError (FileStream :a))))
  (define (%open-bidirectional path if-exists if-does-not-exist etype)
    "Opens a two way stream for the given filepath and for a given type."
    (lisp (Result FileError (FileStream :a)) (path if-exists if-does-not-exist etype)
      (handle-file-function (cl:open path
                                     :direction :io
                                     :element-type etype
                                     :if-exists (cl:case if-exists
                                                  (IfExists/ExistsError ':error)
                                                  (IfExists/NewVersion ':new-version)
                                                  (IfExists/Rename ':rename)
                                                  (IfExists/RenameAndDelete ':rename-and-delete)
                                                  (IfExists/Overwrite ':overwrite)
                                                  (IfExists/Append ':append)
                                                  (IfExists/Supersede ':supersede))
                                     :if-does-not-exist (cl:case if-does-not-exist
                                                          (IfDoesNotExist/DNExistError ':error)
                                                          (IfDoesNotExist/Create ':create))))))

  (declare %open ((StreamOptions :a) -> types:lisptype -> (Result FileError (FileStream :a))))
  (define (%open stream-options etype)
    "Opens a FileStream for a given type and StreamOptions."
    (match stream-options
      ((Input path)
       (%open-input path etype))
      ((Output path exists does-not-exist)
       (%open-output path exists does-not-exist etype))
      ((Bidirectional path exists does-not-exist)
       (%open-bidirectional path exists does-not-exist etype))))

  ;;
  ;; Other basic stream operations
  ;;
  (declare %close ((FileStream :a) -> (Result FileError :b)))
  (define (%close stream)
    "Closes a FileStream."
    (lisp (Result FileError :a) (stream)
      (handle-file-function (cl:close stream))))

  (declare read-char ((FileStream Char) -> (Result FileError Char)))
  (define (read-char stream)
    "Reads a character from an FileStream."
    (lisp (Result FileError Char) (stream)
      (cl:handler-case (Ok (cl:read-char stream))
        (cl:end-of-file () (Err (EOF)))
        (cl:error (c) (Err (LispError c))))))

  (declare read-byte ((math:Bounded :a) => (FileStream :a) -> (Result FileError :a)))
  (define (read-byte stream)
    "Reads a byte from a FileStream"
    (lisp (Result FileError :a) (stream)
      (cl:handler-case (Ok (cl:read-byte stream))
        (cl:end-of-file () (Err (EOF)))
        (cl:error (c) (Err (LispError c))))))

  (declare write-char ((FileStream Char) -> Char -> (Result FileError Unit)))
  (define (write-char stream data)
    "Writes a `Char` to the stream."
    (lisp (Result FileError Unit) (stream data)
      (handle-file-function (cl:write-char data stream))))

  (declare write-byte ((math:Bounded :a) => (FileStream :a) -> :a -> (Result FileError Unit)))
  (define (write-byte stream data)
    "Writes a `Char` to the stream."
    (lisp (Result FileError Unit) (stream data)
      (handle-file-function (cl:write-byte data stream))))

  ;;
  ;;
  ;;
  (declare force ((FileStream :a) -> (Result FileError :b)))
  (define (force stream)
    "Attempts to force the end of a stream."
    (lisp (Result FileError :b) (stream)
      (handle-file-function (cl:force-output stream))))

  (declare flush ((FileStream :a) -> (Result FileError :b)))
  (define (flush stream)
    "Attempts to finish a stream."
    (lisp (Result FileError :b) (stream)
      (handle-file-function (cl:finish-output stream))))

  (declare clear ((FileStream :a) -> (Result FileError :b)))
  (define (clear stream)
    "Attempts to clear a stream."
    (lisp (Result FileError :b) (stream)
      (handle-file-function (cl:clear-output stream))))

  (declare file-position ((FileStream :a) -> (Result FileError UFix)))
  (define (file-position stream)
    "Finds the file-position of a file stream."
    (lisp (Result FileError UFix) (stream)
      (handle-file-function (cl:file-position stream))))

  (declare set-file-position ((FileStream :a) -> UFix -> (Result FileError UFix)))
  (define (set-file-position stream i)
    "Sets the file position of a file stream."
    (lisp (Result FileError UFix) (stream i)
      (handle-file-function (cl:file-position stream i)))))

;;;
;;; Streamable Class
;;;

(coalton-toplevel

  (define-class (Streamable :a)
    "A class of types which are able to be written to or read from a file."
    (open           ((StreamOptions :a)   -> (Result FileError (Filestream :a))))
    (close          ((FileStream :a) -> (Result FileError :b)))
    (read           ((FileStream :a) -> (Result FileError :a)))
    (write          ((FileStream :a) -> :a -> (Result FileError Unit))))

  (define-instance (Streamable Char)
    (define (open stream-options)
      (let ((Type (types:runtime-repr (the (types:Proxy Char) types:Proxy))))
        (%open stream-options type)))
    (define (close fs)
      (%close fs))
    (define (read fs)
      (read-char fs))
    (define (write fs data)
      (write-char fs data))))

;;;
;;; Streamable instances for supported Integer/Byte types
;;;

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:defmacro define-streamable (type)
    `(define-instance (Streamable ,type)
       (define (open stream-options)
         (let ((t (types:runtime-repr (the (types:Proxy ,type) types:Proxy))))
           (%open stream-options t)))
       (define (close fs)
         (%close fs))
       (define (read fs)
         (read-byte fs))
       (define (write fs data)
         (write-byte fs data)))))

(coalton-toplevel
  (define-streamable IFix)
  (define-streamable UFix)
  (define-streamable I8)
  (define-streamable U8)
  (define-streamable I16)
  (define-streamable U16)
  (define-streamable I32)
  (define-streamable U32)
  (define-streamable I64)
  (define-streamable U64))

;;;
;;; Other useful Streamable functions
;;;

(cl:defmacro %with-open-file (stream thunk)
  (cl:let ((abortp (cl:gensym)))
    `(cl:let ((,abortp cl:t)
              (output cl:nil))
       (cl:unwind-protect (cl:progn (cl:setq output (call-coalton-function ,thunk ,stream))
                                    (cl:setq ,abortp cl:nil))
         (cl:when ,stream
           (cl:close ,stream :abort ,abortp)))
       output)))

(coalton-toplevel

  (declare with-open-file ((Streamable :a) => (StreamOptions :a) -> ((FileStream :a) -> (Result FileError :b)) -> (Result FileError :b)))
  (define (with-open-file stream-options thunk)
    "Opens a file stream, performs `thunk` on it, then closes the stream."
    (do
     (stream <- (open stream-options))
     (lisp (Result FileError :a) (stream thunk)
       (%with-open-file stream thunk))))

  (declare read-sequence ((Streamable :a) => (FileStream :a) -> (Result FileError (vec:Vector :a))))
  (define (read-sequence stream)
    "Reads a file into a vector of type `:a`."
    (let v = (vec:new))
    (let r = (cell:new (read stream)))
    (while (res:Ok? (cell:read r))
      (vec:push! (unwrap (cell:read r)) v)
      (cell:write! r (read stream)))
    (match (cell:read r)
      ((Err (EOF))
       (Ok v))
      ((Err e)
       (Err e))
      (_ (Err (FileError "Invalid read action that inexplicably failed to signal a lisp error.")))))

  (declare write-sequence ((Streamable :a) => (FileStream :a) -> (iter:Iterator :a) -> (Result FileError Unit)))
  (define (write-sequence stream data)
    "Writes elements of an iterator of type `:a` to a stream of type `:a`."
    (let written = (cell:new Nil))
    (for elt in data
         (cell:push! written (write stream elt)))
    (if (list:all res:ok? (cell:read written))
        (Ok Unit)
        (unwrap (list:find res:err? (cell:read written)))))

  (declare write-string ((FileStream Char) -> String -> (Result FileError Unit)))
  (define (write-string fs s)
    "Writes a `string` to a FileStream of type Char."
    (write-sequence fs (str:chars s)))

  (declare write-line ((FileStream Char) -> String -> (Result FileError Unit)))
  (define (write-line stream s)
    "Writes a string with an appended newline to a filestream of type Char."
    (write-sequence stream (Str:chars s))
    (write stream #\NewLine)))

;;;
;;; Top-level functions, i.e. those that don't require FileStream or Streamable knowledge
;;;

(coalton-toplevel

  (declare append-to-file ((Into :p Pathname) (Streamable :a) => :p -> (iter:Iterator :a) -> (Result FileError Unit)))
  (define (append-to-file path data)
    "Opens and appends a file with data of type :a."
    (with-open-file (Output (into path) Append DNEError)
      (fn (stream)
        (write-sequence stream data))))

  (declare write-to-file ((Into :p Pathname) (Streamable :a) => :p -> (iter:Iterator :a) -> (Result FileError Unit)))
  (define (write-to-file path data)
    "Opens and writes to a file with data of type :a. Supersedes existing data on the file."
    (with-open-file (Output (into path) Supersede DNEError)
      (fn (stream)
        (write-sequence stream data))))

  (declare read-file-to-string ((Into :a Pathname) => :a -> (Result FileError String)))
  (define (read-file-to-string path)
    "Reads a file into a string, given a pathname string."
    (with-open-file (Input (into path))
      (fn (stream)
        (do
         (chars <- (read-sequence stream))
         (pure (into (the (List Char) (into (the (vec:Vector Char) chars)))))))))

  (declare %read-file-lines ((FileStream Char) -> (Result FileError (List String))))
  (define (%read-file-lines fs)
    "Reads a filestream into a list of line strings."
    (let current-line = (cell:new Nil))
    (let lines = (cell:new Nil))
    (do
     (chars <- (read-sequence fs))
     (pure (progn
             (iter:for-each!
              (fn (c)
                (cond ((== c (unwrap (char:code-char 10)))
                       (cell:push! lines (the String (into (list:reverse (cell:read current-line)))))
                       (cell:write! current-line Nil))
                      (True
                       (cell:push! current-line c)))
                Unit)
              (iter:into-iter chars))
             (if (not (list:null? (cell:read current-line)))
                 (list:reverse (cell:push! lines (the String (into (list:reverse (cell:read current-line))))))
                 (list:reverse (cell:read lines)))))))

  (declare read-file-lines ((Into :a Pathname) => :a -> (Result FileError (List String))))
  (define (read-file-lines path)
    "Reads a file into lines, given a pathname string."
    (with-open-file (Input (into path))
      (fn (stream)
        (%read-file-lines stream)))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/FILE")
