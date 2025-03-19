(cl:in-package #:coalton-native-tests)

;; LispArray accessor is specialized for primitive types.   Make sure
;; they all work.

(coalton-toplevel
  (declare array/generic (array:LispArray (Optional Integer)))
  (define array/generic (array:make 10 None))

  (declare array/i8 (array:LispArray I8))
  (define array/i8 (array:make 10 0))
  (declare array/u8 (array:LispArray U8))
  (define array/u8 (array:make 10 0))

  (declare array/i16 (array:LispArray I16))
  (define array/i16 (array:make 10 0))
  (declare array/u16 (array:LispArray U16))
  (define array/u16 (array:make 10 0))

  (declare array/i32 (array:LispArray I32))
  (define array/i32 (array:make 10 0))
  (declare array/u32 (array:LispArray U32))
  (define array/u32 (array:make 10 0))

  (declare array/i64 (array:LispArray I64))
  (define array/i64 (array:make 10 0))
  (declare array/u64 (array:LispArray U64))
  (define array/u64 (array:make 10 0))

  (declare array/ifix (array:LispArray IFix))
  (define array/ifix (array:make 10 0))
  (declare array/ufix (array:LispArray UFix))
  (define array/ufix (array:make 10 0))

  (declare array/single-float (array:LispArray Single-Float))
  (define array/single-float (array:make 10 0.0))
  (declare array/double-float (array:LispArray Double-Float))
  (define array/double-float (array:make 10 0.0d0))

  (declare array/array/complex-single-float (array:LispArray (array:LispArray (math:Complex Single-Float))))
  (define array/array/complex-single-float (array:make 10 (array:make 10 0)))
  )

(define-test array-length ()
  (is (== (array:length array/generic) 10))
  )

(define-test array-access ()
  (is (== (array:set! array/generic 0 (Some 3)) Unit))
  (is (== (array:aref array/generic 0) (Some 3)))

  (is (== (array:set! array/i8 0 -4) Unit))
  (is (== (array:aref array/i8 0) -4))
  (is (== (array:set! array/u8 0 57) Unit))
  (is (== (array:aref array/u8 0) 57))

  (is (== (array:set! array/i16 0 -444) Unit))
  (is (== (array:aref array/i16 0) -444))
  (is (== (array:set! array/u16 0 575) Unit))
  (is (== (array:aref array/u16 0) 575))

  (is (== (array:set! array/i32 0 -104444) Unit))
  (is (== (array:aref array/i32 0) -104444))
  (is (== (array:set! array/u32 0 575939) Unit))
  (is (== (array:aref array/u32 0) 575939))

  (is (== (array:set! array/i64 0 -9223372036854775807) Unit))
  (is (== (array:aref array/i64 0) -9223372036854775807))
  (is (== (array:set! array/u64 0 18446744073709551615) Unit))
  (is (== (array:aref array/u64 0) 18446744073709551615))

  (let ((ifixnum (lisp IFix () cl:most-negative-fixnum))
        (ufixnum (lisp UFix () cl:most-positive-fixnum)))
    (is (== (array:set! array/ifix 0 ifixnum) Unit))
    (is (== (array:aref array/ifix 0) ifixnum))
    (is (== (array:set! array/ufix 0 ufixnum) Unit))
    (is (== (array:aref array/ufix 0) ufixnum))
    )

  (is (== (array:set! array/single-float 0 3.1415) Unit))
  (is (== (array:aref array/single-float 0) 3.1415))
  (is (== (array:set! array/double-float 0 2.71828d0) Unit))
  (is (== (array:aref array/double-float 0) 2.71828d0))
  )

(define-test nested-complex-array-test ()
  (let ((ty (types:runtime-repr-of array/array/complex-single-float)))
    (is (lisp Boolean (ty)
          (cl:equal ty '(cl:simple-array
                            (cl:simple-array
                             (cl:complex cl:single-float)
                             (cl:*))
                            (cl:*)))))))

(define-test array-copy ()
  (let ((v (array:copy array/generic)))
    (is (== (array:length v) (array:length array/generic)))
    (is (experimental:everytimes (i (array:length array/generic))
          (== (array:aref v i) (array:Aref array/generic i))))
    (array:set! v 2 (Some 5))
    (is (== (array:aref v 2) (Some 5)))
    (is (== (array:aref array/generic 2) None)))

  (let ((v (array:copy array/single-float)))
    (is (== (array:length v) (array:length array/single-float)))
    (is (experimental:everytimes (i (array:length array/single-float))
          (== (array:aref v i) (array:aref array/single-float i))))
    (array:set! v 2 0.2)
    (is (== (array:aref v 2) 0.2))
    (is (== (array:aref array/single-float 2) 0.0))))

(define-test array-iso-list ()
  (let ((xs (as (List Single-Float) array/single-float))
        (v (as (array:LispArray Single-Float) xs)))
    (experimental:dolist-enumerated (i x xs)
      (is (== x (array:aref array/single-float i)))
      (is (== x (array:aref v i))))
    (is (experimental:everytimes (i (array:length array/single-float))
          (== (array:aref v i) (array:aref array/single-float i))))))

(define-test array-foldable ()
  (let ((v (as (array:LispArray Integer) (make-list 0 1 2 3 4)))
        (fl (fn (str x) (msum (make-list "(" str (into x) ")"))))
        (fr (fn (x str) (msum (make-list "(" (into x) str ")"))))
        (sl (fold fl "" v))
        (sr (foldr fr "" v)))
    (is (== "(((((0)1)2)3)4)" sl))
    (is (== "(0(1(2(3(4)))))" sr))))

