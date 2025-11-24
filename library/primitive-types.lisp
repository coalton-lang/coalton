;;;; primitive-types.lisp
;;;;
;;;; Primitive types

(in-package #:coalton)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

;;; XXX: The "constants" True, False, Unit, and Nil are currently
;;; treated specially by codegen as being constant; their values are
;;; looked up at compile-time and emitted directly.

(coalton-toplevel
  (repr :native cl:t)
  (define-type Void)

  ;; Boolean is an early type
  (declare True Boolean)
  (define True (lisp Boolean () cl:t))

  (declare False Boolean)
  (define False (lisp Boolean () cl:nil))

  ;; Unit is an early type
  (declare Unit Unit)
  (define Unit (lisp Unit () coalton-impl/constants:+value-of-unit+))

  ;; List is an early type
  (inline)
  (declare Cons (:a -> (List :a) -> (List :a)))
  (define (Cons x xs)
    (lisp (List :a) (x xs)
      (cl:cons x xs)))

  (declare Nil (List :a))
  (define Nil
    (lisp (List :a) ()
      cl:nil))

  ;; Optional is an early type
  ;;
  ;; Defining the following is functionally
  ;; equivalent to having defined an ADT as
  ;; (define-type (Optional :a)
  ;;   (Some :a)
  ;;   None)
  (inline)
  (declare Some (:a -> Optional :a))
  (define (Some x)
    "A constructor for the type, `Optional`. This constructor can be used
like any other algebraic data type constructor, including for pattern
matching, as in the following example.

```lisp
(match x
  ((Some value)
    value)
  (_ (error \"Oh, no!\")))
```"
    (lisp (Optional :a) (x)
      (coalton-impl/runtime:cl-some x)))

  (declare None (Optional :a))
  (define None
    "A constructor for the type, `Optional`. This constructor can be used
like any other algebraic data type constructor, including for pattern
matching, as in the following example.

```lisp
(match x
  ((None)
   \"Fantastic!\")
  (_ (error \"Oh, no!\")))
```"
    (lisp (Optional :a) ()
      coalton-impl/runtime:cl-none))

  (repr :native cl:bit)
  (define-type Bit
    "A single bit, equal to 0 or 1. Uses `cl:bit`.")

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

  (repr :native coalton-impl/util:ufixnum)
  (define-type UFix
    "Non-allocating tagged non-negative integer; range is platform-dependent. Uses `(and fixnum unsigned-byte)`.")

  (define-type-alias Single-Float F32
    "Deprecated name for F32. This is provided for backward compatibility.")

  (define-type-alias Double-Float F64
    "Deprecated name for F64. This is provided for backward compatibility."))

