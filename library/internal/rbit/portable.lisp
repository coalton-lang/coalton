;;;; portable.lisp

(in-package #:coalton-library/internal/rbit)

(defun rbit (x)
  (declare (type (unsigned-byte 64) x)
           (values (unsigned-byte 64) &optional))
  (loop :with result :of-type (unsigned-byte 64) := 0
        :for i :of-type fixnum :below 64
        :for xi :of-type bit := (ldb (byte 1 i) x)
        :do (setf result (dpb xi (byte 1 (- 63 i)) result))
        :finally (return result)))

