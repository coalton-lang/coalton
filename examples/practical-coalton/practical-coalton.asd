(defsystem pcl-pathnames
  :name "pcl-pathnames"
  :author "Peter Seibel <peter@gigamonkeys.com>"
  :version "1.0"
  :maintainer "Peter Seibel <peter@gigamonkeys.com>"
  :licence "BSD"
  :description "Portable pathname manipulation functions."
  :long-description ""
  :pathname "practical-common-lisp"
  :components
  ((:module "Chapter15"
    :components ((:file "packages")
                 (:file "pathnames" :depends-on ("packages"))))))

(defsystem practical-coalton
  :name "practical-coalton"
  :author "Jason Walker <Jason0@pm.me>"
  :version "0.1"
  :description "A port of Peter Seibel's Practical Common Lisp to Coalton."
  :depends-on (#:pcl-pathnames
               #:alexandria
               #:coalton)
  :components ((:file "1_simple_database")
               (:file "15_pathnames")))
