(cl:defpackage #:coalton-library/utils
  (:use
   #:coalton)
  (:export
   #:defstdlib-package
   #:concat-string))

(cl:in-package #:coalton-library/utils)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro defstdlib-package (name cl:&rest args)
  `(cl:eval-when (:compile-toplevel :load-toplevel)
     #+sb-package-locks
     (cl:when (cl:find-package ',name)
       (sb-ext:unlock-package ',name))
     (cl:defpackage ,name ,@args)
     #+sb-package-locks
     (sb-ext:lock-package ',name)))

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;; Concatenation function that doesn't depend on the string
  ;; library. Used frequently in primitive type class implementations.
  (inline)
  (declare concat-string (String -> String -> String))
  (define (concat-string a b)
    (lisp String (a b)
      (cl:concatenate 'cl:string a b))))
