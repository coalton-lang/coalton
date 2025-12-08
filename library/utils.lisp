(defpackage #:coalton-library/utils
  (:use
   #:coalton
   #:coalton-compatibility-layer)
  (:local-nicknames
   (#:compat #:coalton-compatibility-layer))
  (:export
   #:defstdlib-package))

(in-package #:coalton-library/utils)

(cl:defmacro defstdlib-package (name cl:&rest args)
  `(cl:eval-when (:compile-toplevel :load-toplevel)
     (cl:when (cl:find-package ',name)
       (compat:try-unlock-package ',name))
     (cl:defpackage ,name ,@args)
     (compat:try-lock-package ',name)))
