(uiop:define-package #:coalton-library/collections
  (:use-reexport
   #:coalton-library/collections/classes)
  ;; Don't need to re-export List because it's a builtin type
  (:import-from
   #:coalton-library/collections/mutable/vector
   #:Vector)
  (:export
   #:Vector))

;; #+sb-package-locks
;; (sb-ext:lock-package "COALTON-LIBRARY/COLLECTIONS")
