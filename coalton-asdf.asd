;;; Coalton ASDF extensions
;;;
;;; Add a defsystem-depends-on clause to your .asd file:
;;;
;;;     (asdf:defsystem "xyzzy"
;;;       ...
;;;       :defsystem-depends-on ("coalton-asdf")
;;;       ...)
;;;
;;; and coalton source files using the :coalton-file keyword:
;;;
;;;       ...
;;;       :components ((:file "package")
;;;                    (:coalton-file "array")
;;;                    ...))

(asdf:defsystem "coalton-asdf"
  :description "Coalton ASDF extensions"
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :depends-on ("coalton-compiler")
  :components ((:file "coalton-asdf")))
