;; Support for compilation using (:coalton-file "???.coalton")
;;
;; The behavior is currently identical to that of asdf:cl-source-file.
;;
;; In the future it will support native Coalton package forms and compiler entry.
;;
;; To test:
;;
;;     1. Load this extension by adding it to #:coalton/library in coalton.asd:
;;    
;;         :defsystem-depends-on (#:coalton-asdf)
;;    
;;     2. Rename library/types.lisp to library/types.coal
;;    
;;     3. Change (:file "types") to (:coalton-file "types") in coalton.asd

(asdf:defsystem #:coalton-asdf
  :description "Coalton ASDF extensions."
  :author "Coalton contributors (https://github.com/coalton-lang/coalton)"
  :license "MIT"
  :version (:read-file-form "VERSION.txt")
  :components ((:file "coalton-asdf")))
