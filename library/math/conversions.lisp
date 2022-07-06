;;;; conversions.lisp
;;;;
;;;; Conversions between primitive numerical types

(coalton-library/utils:defstdlib-package #:coalton-library/math/conversions
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/functions
   #:coalton-library/math/bounded)
  (:export
   #:integer->double-float
   #:integer->single-float))

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(in-package #:coalton-library/math/conversions)

(coalton-toplevel
  ;;
  ;; Conversions from U8
  ;;

  (define-instance (TryInto U8 I8)
    (define (tryInto x)
      (when (> (into x) (the Integer (into (the I8 maxBound))))
        (return (Err "value out of range")))

      (Ok (lisp I8 (x) x))))

  (define-instance (Into U8 U16)
    (define (into x)
      (lisp U16 (x) x)))

  (define-instance (Into U8 I16)
    (define (into x)
      (lisp I16 (x) x)))

  (define-instance (Into U8 U32)
    (define (into x)
      (lisp U32 (x) x)))

  (define-instance (Into U8 I32)
    (define (into x)
      (lisp I32 (x) x)))

  (define-instance (Into U8 U64)
    (define (into x)
      (lisp U64 (x) x)))

  (define-instance (Into U8 I64)
    (define (into x)
      (lisp I64 (x) x)))

  (define-instance (Into U8 Integer)
    (define (into x)
      (lisp Integer (x) x)))

  ;;
  ;; Conversions from I8
  ;;

  (define-instance (TryInto I8 U8)
    (define (tryInto x)
      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U8 (x) x))))

  (define-instance (TryInto I8 U16)
    (define (tryInto x)
      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U16 (x) x))))

  (define-instance (Into I8 I16)
    (define (into x)
      (lisp I16 (x) x)))

  (define-instance (TryInto I8 U32)
    (define (tryInto x)
      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U32 (x) x))))

  (define-instance (Into I8 I32)
    (define (into x)
      (lisp I32 (x) x)))

  (define-instance (TryInto I8 U64)
    (define (tryInto x)
      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U64 (x) x))))

  (define-instance (Into I8 I64)
    (define (into x)
      (lisp I64 (x) x)))

  (define-instance (Into I8 Integer)
    (define (into x)
      (lisp Integer (x) x)))

  ;;
  ;; Conversions from U16
  ;;

  (define-instance (TryInto U16 U8)
    (define (tryInto x)
      (when (> x (into (the U8 maxBound)))
        (return (Err "value out of range")))

      (Ok (lisp U8 (x) x))))

  (define-instance (TryInto U16 I8)
    (define (tryInto x)
      (when (> (into x) (the Integer (into (the I8 maxBound))))
        (return (Err "value out of range")))

      (Ok (lisp I8 (x) x))))

  (define-instance (TryInto U16 I16)
    (define (tryInto x)
      (when (> (into x) (the Integer (into (the I16 maxBound))))
        (return (Err "value out of range")))

      (Ok (lisp I16 (x) x))))

  (define-instance (Into U16 U32)
    (define (into x)
      (lisp U32 (x) x)))

  (define-instance (Into U16 I32)
    (define (into x)
      (lisp I32 (x) x)))

  (define-instance (Into U16 U64)
    (define (into x)
      (lisp U64 (x) x)))

  (define-instance (Into U16 I64)
    (define (into x)
      (lisp I64 (x) x)))

  (define-instance (Into U16 Integer)
    (define (into x)
      (lisp Integer (x) x)))

  ;;
  ;; Conversions from I16
  ;;

  (define-instance (TryInto I16 U8)
    (define (tryInto x)
      (when (> x (into (the U8 maxBound)))
        (return (Err "value out of range")))

      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U8 (x) x))))

  (define-instance (TryInto I16 I8)
    (define (tryInto x)
      (when (> x (into (the I8 maxBound)))
        (return (Err "value out of range")))

      (when (< x (into (the I8 minBound)))
        (return (Err "valut out of range")))

      (Ok (lisp I8 (x) x))))

  (define-instance (TryInto I16 U16)
    (define (tryInto x)
      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U16 (x) x))))

  (define-instance (TryInto I16 U32)
    (define (tryInto x)
      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U32 (x) x))))

  (define-instance (Into I16 I32)
    (define (into x)
      (lisp I32 (x) x)))

  (define-instance (TryInto I16 U64)
    (define (tryInto x)
      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U64 (x) x))))

  (define-instance (Into I16 I64)
    (define (into x)
      (lisp I64 (x) x)))

  (define-instance (Into I16 Integer)
    (define (into x)
      (lisp Integer (x) x)))

  ;;
  ;; Conversions from U32
  ;;

  (define-instance (TryInto U32 U8)
    (define (tryInto x)
      (when (> x (into (the U8 maxBound)))
        (return (Err "value out of range")))

      (Ok (lisp U8 (x) x))))

  (define-instance (TryInto U32 I8)
    (define (tryInto x)
      (when (> (into x) (the Integer (into (the I8 maxBound))))
        (return (Err "value out of range")))

      (Ok (lisp I8 (x) x))))

  (define-instance (TryInto U32 U16)
    (define (tryInto x)
      (when (> x (into (the U16 maxBound)))
        (return (Err "value out of range")))

      (Ok (lisp U16 (x) x))))

  (define-instance (TryInto U32 I16)
    (define (tryInto x)
      (when (> (into x) (the Integer (into (the I16 maxBound))))
        (return (Err "value out of range")))

      (Ok (lisp I16 (x) x))))

  (define-instance (TryInto U32 I32)
    (define (tryInto x)
      (when (> (into x) (the Integer (into (the I32 maxBound))))
        (return (Err "value out of range")))

      (Ok (lisp I32 (x) x))))

  (define-instance (Into U32 U64)
    (define (into x)
      (lisp U64 (x) x)))

  (define-instance (Into U32 I64)
    (define (into x)
      (lisp I64 (x) x)))

  (define-instance (Into U32 Integer)
    (define (into x)
      (lisp Integer (x) x)))

  ;;
  ;; Conversions from I32
  ;;

  (define-instance (TryInto I32 U8)
    (define (tryInto x)
      (when (> x (into (the U8 maxBound)))
        (return (Err "value out of range")))

      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U8 (x) x))))

  (define-instance (TryInto I32 I8)
    (define (tryInto x)
      (when (> x (into (the I8 maxBound)))
        (return (Err "value out of range")))

      (when (< x (into (the I8 minBound)))
        (return (Err "value out of range")))

      (Ok (lisp I8 (x) x))))

  (define-instance (TryInto I32 U16)
    (define (tryInto x)
      (when (> x (into (the U16 maxBound)))
        (return (Err "value out of range")))

      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U16 (x) x))))

  (define-instance (TryInto I32 I16)
    (define (tryInto x)
      (when (> x (into (the I16 maxBound)))
        (return (Err "value out of range")))

      (when (< x (into (the I16 minBound)))
        (return (Err "value out of range")))

      (Ok (lisp I16 (x) x))))

  (define-instance (TryInto I32 U32)
    (define (tryInto x)
      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U32 (x) x))))

  (define-instance (TryInto I32 U64)
    (define (tryInto x)
      (when (< x 0)
        (return (Err "value out of range")))

        (Ok (lisp U64 (x) x))))

  (define-instance (Into I32 I64)
    (define (into x)
      (lisp I64 (x) x)))

  (define-instance (Into I32 Integer)
    (define (into x)
      (lisp Integer (x) x)))

  ;;
  ;; Conversions from U64
  ;;

  (define-instance (TryInto U64 U8)
    (define (tryInto x)
      (when (> x (into (the U8 maxBound)))
        (return (Err "value out of range")))

      (Ok (lisp U8 (x) x))))

  (define-instance (TryInto U64 I8)
    (define (tryInto x)
      (when (> (into x) (the Integer (into (the I8 maxBound))))
        (return (Err "value out of range")))

      (Ok (lisp I8 (x) x))))

  (define-instance (TryInto U64 U16)
    (define (tryInto x)
      (when (> x (into (the U16 maxBound)))
        (return (Err "value out of range")))

      (Ok (lisp U16 (x) x))))

  (define-instance (TryInto U64 I16)
    (define (tryInto x)
      (when (> (into x) (the Integer (into (the I16 maxBound))))
        (return (Err "value out of range")))

      (Ok (lisp I16 (x) x))))

  (define-instance (TryInto U64 U32)
    (define (tryInto x)
      (when (> x (into (the U32 maxBound)))
        (return (Err "value out of range")))

      (Ok (lisp U32 (x) x))))

  (define-instance (TryInto U64 I32)
    (define (tryInto x)
      (when (> (into x) (the Integer (into (the I32 maxBound))))
        (return (Err "value out of range")))

      (Ok (lisp I32 (x) x))))

  (define-instance (TryInto U64 I64)
    (define (tryInto x)
      (when (> (into x) (the Integer (into (the I64 maxBound))))
        (return (Err "value out of range")))

      (Ok (lisp I64 (x) x))))

  (define-instance (Into U64 Integer)
    (define (into x)
      (lisp Integer (x) x)))

  ;;
  ;; Conversions from I64
  ;;

  (define-instance (TryInto I64 U8)
    (define (tryInto x)
      (when (> x (into (the U8 maxBound)))
        (return (Err "value out of range")))

      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U8 (x) x))))

  (define-instance (TryInto I64 I8)
    (define (tryInto x)
      (when (> x (into (the I8 maxBound)))
        (return (Err "value out of range")))

      (when (< x (into (the I8 minBound)))
        (return (Err "value out of range")))

      (Ok (lisp I8 (x) x))))

  (define-instance (TryInto I64 U16)
    (define (tryInto x)
      (when (> x (into (the U16 maxBound)))
        (return (Err "value out of range")))

      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U16 (x) x))))

  (define-instance (TryInto I64 I16)
    (define (tryInto x)
      (when (> x (into (the I16 maxBound)))
        (return (Err "value out of range")))

      (when (< x (into (the I16 minBound)))
        (return (Err "value out of range")))

      (Ok (lisp I16 (x) x))))

  (define-instance (TryInto I64 U32)
    (define (tryInto x)
      (when (> x (into (the U32 maxBound)))
        (return (Err "value out of range")))

      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U32 (x) x))))

  (define-instance (TryInto I64 I32)
    (define (tryInto x)
      (when (> x (into (the I32 maxBound)))
        (return (Err "value out of range")))

      (when (< x (into (the I32 minBound)))
        (return (Err "value out of range")))

      (Ok (lisp I32 (x) x))))

  (define-instance (TryInto I64 U64)
    (define (tryInto x)
      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U64 (x) x))))

  (define-instance (Into I64 Integer)
    (define (into x)
      (lisp Integer (x) x)))

  ;;
  ;; Conversions from Integer
  ;;

  (define-instance (TryInto Integer U8)
    (define (tryInto x)
      (when (> x (into (the U8 maxBound)))
        (return (Err "value out of range")))

      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U8 (x) x))))

  (define-instance (TryInto Integer I8)
    (define (tryInto x)
      (when (> x (into (the I8 maxBound)))
        (return (Err "value out of range")))

      (when (< x (into (the I8 minBound)))
        (return (Err "value out of range")))

      (Ok (lisp I8 (x) x))))

  (define-instance (TryInto Integer U16)
    (define (tryInto x)
      (when (> x (into (the U16 maxBound)))
        (return (Err "value out of range")))

      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U16 (x) x))))

  (define-instance (TryInto Integer I16)
    (define (tryInto x)
      (when (> x (into (the I16 maxBound)))
        (return (Err "value out of range")))

      (when (< x (into (the I16 minBound)))
        (return (Err "value out of range")))

      (Ok (lisp I16 (x) x))))

  (define-instance (TryInto Integer U32)
    (define (tryInto x)
      (when (> x (into (the U32 maxBound)))
        (return (Err "value out of range")))

      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U32 (x) x))))

  (define-instance (TryInto Integer I32)
    (define (tryInto x)
      (when (> x (into (the I32 maxBound)))
        (return (Err "value out of range")))

      (when (< x (into (the I32 minBound)))
        (return (Err "value out of range")))

      (Ok (lisp I32 (x) x))))

  (define-instance (TryInto Integer U64)
    (define (tryInto x)
      (when (> x (into (the U64 maxBound)))
        (return (Err "value out of range")))

      (when (< x 0)
        (return (Err "value out of range")))

      (Ok (lisp U64 (x) x))))

  (define-instance (TryInto Integer I64)
    (define (tryInto x)
      (when (> x (into (the I64 maxBound)))
        (return (Err "value out of range")))

      (when (< x (into (the I64 minBound)))
        (return (Err "value out of range")))

      (Ok (lisp I64 (x) x)))))

(coalton-toplevel
  (define-instance (Into IFix Integer)
    (define (into x)
      (lisp Integer (x) x)))

  (define-instance (Into UFix Integer)
    (define (into x)
      (lisp Integer (x) x))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/CONVERSIONS")
