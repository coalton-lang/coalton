;; ASDF Coalton component type
;;
;; Add an asdf component that supports compiling files with a .coal
;; extension.
;;
;; To use, add a :defsystem-depends-on clause to your system
;; definition, and refer to files using component type :coalton-file,
;; like:
;;
;;     (asdf:defsystem #:application
;;       :defsystem-depends-on (#:coalton-asdf)
;;       :depends-on (#:coalton)
;;       :pathname "src/"
;;       :serial t
;;       :components ((:coalton-file "types")
;;                    (:coalton-file "main")))
;;
;; This class is defined in the ASDF package, so that the keyword :coalton-file works.
;;
;; see: https://github.com/fare/asdf/blob/master/doc/best_practices.md#using-asdf-extensions

(in-package :asdf)

(defclass coalton-file (cl-source-file)
  ((type :initform "coal")))
