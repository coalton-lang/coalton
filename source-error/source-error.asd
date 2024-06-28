(asdf:defsystem #:source-error
  :license "MIT"
  :depends-on (#:alexandria)
  :pathname "src"
  :serial t
  :components ((:file "error")
               (:file "package")))
