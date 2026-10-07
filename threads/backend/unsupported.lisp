;;;; unsupported.lisp
;;;;
;;;; Loaded, by the system coalton/threads/unsupported, instead of a
;;;; backend on Lisp implementations that coalton/threads does not
;;;; support. See "Porting" in threads/README.md.

(error "coalton/threads does not support ~A yet; it currently supports SBCL. ~
See \"Porting\" in threads/README.md."
       (lisp-implementation-type))
