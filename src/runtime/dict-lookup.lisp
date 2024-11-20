(defpackage #:coalton-impl/runtime/dict-lookup
  (:use
   #:cl)
  (:local-nicknames
   (#:util #:coalton-impl/util))
  (:export
   #:lookup-dict))

(defun lookup-dict (obj key)
  (declare (ignore obj key))
  0)
