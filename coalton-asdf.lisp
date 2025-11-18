;;; Coalton ASDF extensions
;;;
;;; This class is defined in the ASDF package, so that the keyword :coalton-file works.
;;;
;;; see: https://github.com/fare/asdf/blob/master/doc/best_practices.md#using-asdf-extensions

(in-package :asdf)

;; .ct is a special extension for Coalton files that are written as
;; .lisp files.

(defclass ct-file (cl-source-file)
  ((type :initform "ct")))


;; .coal is a special extension for Coalton files written in native
;; Coalton syntax.
;;
;; XXX: Should we just make this "coalton" instead of "coalton-file"?

(defclass coalton-file (cl-source-file)
  ((type :initform "coal")))

(defmethod perform ((o compile-op) (c coalton-file))
  (let ((coal-file (first (input-files o c)))
        (fasl-file (first (output-files o c))))
    (call-with-around-compile-hook
     c (lambda (&rest flags)
         (declare (ignore flags))
         (coalton-impl/entry:compile (coalton-impl/source:make-source-file coal-file)
                                     :load nil
                                     :output-file fasl-file)))))
