;;;; fixed-size-numbers.lisp
;;;;
;;;; Fixed size numberical types

(in-package #:coalton)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (repr :native (cl:unsigned-byte 8))
  (define-type U8
    "Unsigned 8-bit integer capable of storing values in `[0, 255]`. Uses `(unsigned-byte 8)`.")

  (repr :native (cl:unsigned-byte 16))
  (define-type U16
    "Unsigned 16-bit integer capable of storing values in `[0, 65535]`. Uses `(unsigned-byte 16)`.")

  (repr :native (cl:unsigned-byte 32))
  (define-type U32
    "Unsigned 32-bit integer capable of storing values in `[0, 4294967295]`. Uses `(unsigned-byte 32)`.")

  (repr :native (cl:unsigned-byte 64))
  (define-type U64
    "Unsigned 64-bit integer capable of storing values in `[0, 18446744073709551615]`. Uses `(unsigned-byte 64)`.")

  (repr :native (cl:signed-byte 8))
  (define-type I8
    "Signed 8-bit integer capable of storing values in `[-128, 127]`. Uses `(signed-byte 8)`.")

  (repr :native (cl:signed-byte 16))
  (define-type I16
    "Signed 16-bit integer capable of storing values in `[-32768, 32767]`. Uses `(signed-byte 16)`.")

  (repr :native (cl:signed-byte 32))
  (define-type I32
    "Signed 32-bit integer capable of storing values in `[-2147483648, 2147483647]`. Uses `(signed-byte 32)`.")

  (repr :native (cl:signed-byte 64))
  (define-type I64
    "Signed 64-bit integer capable of storing values in `[-9223372036854775808, 9223372036854775807]`. Uses `(signed-byte 64)`.")

  (repr :native cl:fixnum)
  (define-type IFix
    "Non-allocating tagged integer; range is platform-dependent. Does not error on overflow. Uses `fixnum`.")

  (repr :native (cl:and cl:fixnum cl:unsigned-byte))
  (define-type UFix 
    "Non-allocating tagged non-negative integer; range is platform-dependent. Uses `(and fixnum unsigned-byte)`."))
