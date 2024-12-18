(coalton-library/utils:defstdlib-package #:coalton-library/collections/immutable/set
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/functions
   #:coalton-library/classes)
  (:local-nicknames
   (#:cln #:coalton-library/collections/classes)
   (#:opt #:coalton-library/optional)
   (#:types #:coalton-library/types)
   (#:list #:coalton-library/list)
   (#:cell #:coalton-library/cell)
   (#:iter #:coalton-library/iterator)
   (#:ram #:coalton-library/randomaccess))
  (:export))

(in-package #:coalton-library/collections/immutable/set)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;; https://michael.steindorfer.name/publications/oopsla15.pdf

; #+sb-package-locks
; (sb-ext:lock-package "COALTON-LIBRARY/COLLECTIONS/IMMUTABLE/SET")
