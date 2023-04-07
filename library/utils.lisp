(defpackage #:coalton-library/utils
  (:use
   #:coalton)
  (:export
   #:defstdlib-package))

(in-package #:coalton-library/utils)

(cl:defmacro defstdlib-package (name cl:&rest args)
  `(cl:eval-when (:compile-toplevel :load-toplevel)
     #+sb-package-locks
     (cl:when (cl:find-package ',name)
       (sb-ext:unlock-package ',name))
     (cl:defpackage ,name ,@args)
     #+sb-package-locks
     (sb-ext:lock-package ',name)))
