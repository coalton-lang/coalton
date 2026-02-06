(defpackage #:coalton/utils
  (:use
   #:coalton)
  (:nicknames
   #:coalton-library/utils)
  (:export
   #:defstdlib-package))

(in-package #:coalton/utils)

(cl:defmacro defstdlib-package (name cl:&rest args)
  (cl:let* ((name-string (cl:string-upcase (cl:string name)))
            (legacy-name (cl:and (cl:<= 8 (cl:length name-string))
                                 (cl:string= "COALTON/" name-string :end2 8)
                                 (cl:concatenate 'cl:string "COALTON-LIBRARY/"
                                                 (cl:subseq name-string 8)))))
  `(cl:eval-when (:compile-toplevel :load-toplevel)
     #+sb-package-locks
     (cl:when (cl:find-package ',name)
       (sb-ext:unlock-package ',name))
     (cl:defpackage ,name
       ,@(cl:if legacy-name
                `((:nicknames ,legacy-name))
                '())
       ,@args)
     #+sb-package-locks
     (sb-ext:lock-package ',name))))
