(defpackage #:coalton-cffi/utils
  (:use
   #:coalton)
  (:export
   #:define-cffi-package))

(in-package #:coalton-cffi/utils)

(cl:defmacro define-cffi-package (name cl:&body options)
  "A convenience macro for defining locked packages for the `coalton/cffi` system."
  `(cl:eval-when (:compile-toplevel :load-toplevel)
     #+sb-package-locks
     (cl:when (cl:find-package ',name)
       (sb-ext:unlock-package ',name))
     (cl:defpackage ,name ,@options)
     #+sb-package-locks
     (sb-ext:lock-package ',name)))
