;;;; sbcl-arm64.lisp

(in-package #:coalton-library/internal/rbit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Tell the compiler about the function, `%rbit`.
  (sb-c:defknown rbit ((unsigned-byte 64)) (unsigned-byte 64) ()
    :overwrite-fndb-silently t))

(in-package #:sb-vm) 

(define-vop (coalton-library/internal/rbit:rbit)
  (:translate    coalton-library/internal/rbit:rbit)
  (:policy       :fast-safe)
  (:args         (arg :scs (unsigned-reg) :target res))
  (:arg-types    unsigned-num)
  (:results      (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator    1 (inst rbit res arg)))

(in-package #:coalton-library/internal/rbit)

(defun rbit (x)
  (rbit x))

