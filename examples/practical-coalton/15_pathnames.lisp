(in-package :cl-user)
(defpackage :practical-coalton.pathnames
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
    (#:pth #:com.gigamonkeys.pathnames)
    (#:str #:coalton-library/string))
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p
   :IncludeDirs
   :ExcludeDirs))
(in-package :practical-coalton.pathnames)

(named-readtables:in-readtable coalton:coalton)

;; Port of Chapter 15 from Practical Common Lisp, by Peter Seibel,
;; to Coalton.
;;
;; In the previous chapter, we took the approach of rewriting the
;; Common Lisp functions in Coalton. In this chapter, we instead
;; write a thin wrapper around the Common Lisp functions.
;;
;; First, we start by defining a few Coalton types that provide
;; a type-safe interface to the Common Lisp pathname functions.
;;
;; Second, we define a corresponding Coalton function for each
;; function exported by the Common Lisp pathname package.

(cl:defmacro wrap-lisp-call (type function value)
  "Wrap a call to a Lisp function that returns a value of type TYPE
in a Coalton function that returns a value of type (Optional TYPE).
A nil return value is converted to None."
  `(lisp (Optional ,type) (,value)
     (cl:let ((result (,function ,value)))
       (cl:if result
         (coalton (Some (lisp ,type () result)))
         (coalton None)))))

(coalton-toplevel

  (repr :native cl:pathname)
  (define-type Pathname)

  (declare pathname->string (Pathname -> String))
  (define (pathname->string pathname)
    (lisp String (pathname)
      (cl:write-to-string pathname)))
  
  (repr :native cl:pathname)
  (define-type WildPathname)

  ;; TODO: Is this safe? I want to be able to pass any of these
  ;; straight into CL code so I don't have to wrap identical code
  ;; in three separate pattern matches every time
  (repr :native cl:t)
  (define-type Path
    (PathnamePath Pathname)
    (StringPath String)
    (WildPath WildPathname))
  
  (define-instance (Into Pathname Path)
    (define (into pathname)
      (PathnamePath pathname)))
  
  (define-instance (Into String Path)
    (define (into str)
      (StringPath str)))
  
  (define-instance (Into WildPathname Path)
    (define (into wild-pathname)
      (WildPath wild-pathname)))

  (declare error-wild! (String -> Path -> Unit))
  (define (error-wild! msg path)
    (match path
      ((WildPath _) (error (str:concat "Cannot pass a wild pathname into " msg))))))

(coalton-toplevel

  (declare list-directory (Path -> (List Pathname)))
  (define (list-directory dirname)
    "Return a list of pathnames for the files in the directory named by DIRNAME."
    (error-wild! "list-directory" dirname)
    (lisp (List Pathname) (dirname)
      (pth:list-directory dirname)))

  (declare file-exists-p (Path -> (Optional Pathname)))
  (define (file-exists-p pth)
    "Return a pathname for the file named by PTH if it exists, otherwise return None."
    (wrap-lisp-call :a pth:file-exists-p pth))

  (declare directory-pathname-p (Path -> (Optional :a)))
  (define (directory-pathname-p pth)
    (wrap-lisp-call :a pth:directory-pathname-p pth))

  (declare file-pathname-p (Path -> (Optional :a)))
  (define (file-pathname-p pth)
    (wrap-lisp-call :a pth:file-pathname-p pth))

  (declare pathname-as-directory (Path -> Pathname))
  (define (pathname-as-directory pth)
    (error-wild! "pathname-as-directory" pth)
    (lisp :a (pth)
      (pth:pathname-as-directory pth)))

  (declare pathname-as-file (Path -> Pathname))
  (define (pathname-as-file pth)
    (error-wild! "pathname-as-file" pth)
    (lisp :a (pth)
      (pth:pathname-as-file pth)))

  (declare directory-p (Path -> (Optional :a)))
  (define (directory-p pth)
    "Return a pathname for the directory named by PTH if it is an existing directory.
Otherwise return None."
    (wrap-lisp-call :a pth:directory-p pth))
  
  (declare file-p (Path -> (Optional :a)))
  (define (file-p pth)
    "Return a pathname for the file named by PTH if it is an existing file.
Otherwise return None."
    (wrap-lisp-call :a pth:file-p pth)))

(coalton-toplevel
  
  ;; Create an enum type to make the :directories argument to walk-directory
  ;; more discoverable from the type signature.
  (repr :enum)
  (define-type WalkDirectoryOptions
    IncludeDirs
    ExcludeDirs)
  
  (define-instance (Eq WalkDirectoryOptions)
    (define (== x y)
      (match (Tuple x y)
        ((Tuple (IncludeDirs) (IncludeDirs))
         True)
        ((Tuple (ExcludeDirs) (ExcludeDirs))
         True)
        (_ False))))
  
  (declare walk-directory-if (String -> (Pathname -> :b) -> WalkDirectoryOptions -> (Pathname -> Boolean) -> Unit))
  (define (walk-directory-if dirname map-fn include-dirs test-fn)
    "Walk the directory named by DIRNAME and call MAP-FN on each file. If INCLUDE-DIRS
is IncludeDirs, MAP-FN will be called on each directory as well. TEST-FN is called
on each pathname to determine if it should be included in the walk."
    (let should-include-dirs = (== include-dirs IncludeDirs))
    (lisp :t (dirname map-fn should-include-dirs test-fn)
        (pth:walk-directory dirname
                            (cl:lambda (pthnm)
                              (call-coalton-function map-fn pthnm))
                            :directories should-include-dirs
                            :test (cl:lambda (pthnm)
                                    (call-coalton-function test-fn pthnm))))))
