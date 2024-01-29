;; To load this file, you need to load Chapter 15 from the Practical
;; Common Lisp repository. The source code can be found at the link
;; below. Then you can copy the Chapter 15 directory into
;; ~/quicklisp/local-projects and run (quickload "Chapter-15").
;; https://gigamonkeys.com/book/

(in-package :cl-user)
(ql:quickload "coalton")
(ql:quickload "Chapter-15")
(defpackage :practical-coalton.pathnames
  (:use
   #:coalton
   #:coalton-prelude)
  (:local-nicknames
    (#:pth #:com.gigamonkeys.pathnames))
  (:export
   :list-directory
   :file-exists-p
   :directory-pathname-p
   :file-pathname-p
   :pathname-as-directory
   :pathname-as-file
   :walk-directory
   :directory-p
   :file-p))
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
;; For example, in several of the Common Lisp functions a wildcard
;; pathname is an invalid argument. Instead of checking for wildcard
;; paths and erroring if found, we can use the type system to ensure
;; that we never pass a wildcard path to a function that doesn't
;; accept it.
;;
;; Second, we define a corresponding Coalton function for each
;; function exported by the Common Lisp pathname package.

;; TODO: I'm open to any suggestions to make this code less verbose.
;; If you count the comments at the top of the file and the package
;; definition (which the original doesn't have), this is almost
;; as long as the Common Lisp implementation this is wrapping!

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

  (define-class (Path :a))

  (define-instance (Path String))

  (repr :native cl:pathname)
  (define-type Pathname)

  (declare pathname->string (Pathname -> String))
  (define (pathname->string pathname)
    (lisp String (pathname)
      (cl:write-to-string pathname)))

  (define-instance (Path Pathname))

  (repr :native cl:pathname)
  (define-type WildPathname)

  (define-instance (Path WildPathname)))

(coalton-toplevel

  ;; TODO: It's a little unforunate that the Either type is called
  ;; Result in Coalton, because it makes a usage to represent one of two
  ;; possible values a little confusing. Is there a more idiomatic way
  ;; to represent this?
  (declare list-directory ((Result String Pathname) -> (List Pathname)))
  (define (list-directory dirname)
    "Return a list of pathnames for the files in the directory named by DIRNAME."
    (match dirname
      ((Err str)
       (lisp (List Pathname) (str)
         (pth:list-directory str)))
      ((Ok pth)
       (lisp (List Pathname) (pth)
         (pth:list-directory pth)))))

  (declare file-exists-p ((Path :a) => :a -> (Optional Pathname)))
  (define (file-exists-p pth)
    "Return a pathname for the file named by PTH if it exists, otherwise return None."
    (wrap-lisp-call :a pth:file-exists-p pth))

  (declare directory-pathname-p ((Path :a) => :a -> (Optional :a)))
  (define (directory-pathname-p pth)
    (wrap-lisp-call :a pth:directory-pathname-p pth))

  (declare file-pathname-p ((Path :a) => :a -> (Optional :a)))
  (define (file-pathname-p pth)
    (wrap-lisp-call :a pth:file-pathname-p pth))

  (declare pathname-as-directory ((Result String Pathname) -> Pathname))
  (define (pathname-as-directory pth)
    (match pth
      ((Err str)
       (lisp :a (str)
         (pth:pathname-as-directory str)))
      ((Ok pth)
       (lisp :a (pth)
         (pth:pathname-as-directory pth)))))

  (declare pathname-as-file ((Result String Pathname) -> Pathname))
  (define (pathname-as-file pth)
    (match pth
      ((Err str)
       (lisp :a (str)
         (pth:pathname-as-file str)))
      ((Ok pth)
       (lisp :a (pth)
         (pth:pathname-as-file pth)))))

  (declare directory-p ((Path :a) => :a -> (Optional :a)))
  (define (directory-p pth)
    "Return a pathname for the directory named by PTH if it is an existing directory.
Otherwise return None."
    (wrap-lisp-call :a pth:directory-p pth))
  
  (declare file-p ((Path :a) => :a -> (Optional :a)))
  (define (file-p pth)
    "Return a pathname for the file named by PTH if it is an existing file.
Otherwise return None."
    (wrap-lisp-call :a pth:file-p pth)))

(coalton-toplevel
  
  ;; Create an enum type to make the :directories argument to walk-directory
  ;; more discoverable from the type signature.
  (define-type WalkDirectoryOptions
    IncludeDirs
    ExcludeDirs)
  
  ;; TODO: Is there a better way to be able to check if two WalkDirectoryOptions
  ;; are equal? This seems inefficient at runtime and verbose.
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
