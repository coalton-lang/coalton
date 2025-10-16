;;;; lisparray.lisp
;;;;
;;;; An interface to Common Lisp rank-1 SIMPLE-ARRAYs.

(coalton-library/utils:defstdlib-package #:coalton-library/lisparray
  (:use
   #:coalton
   #:coalton-library/classes)
  (:local-nicknames
   (#:types #:coalton-library/types)
   (#:complex #:coalton-library/math/complex)
   (#:ram #:coalton-library/randomaccess))
  (:export
   #:LispArray
   #:make
   #:make-uninitialized
   #:length
   #:aref
   #:set!
   #:copy))

(in-package #:coalton-library/lisparray)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  ;; The representation of (LispArray :t) is specially dealt with by
  ;; the compiler in lisp-type.lisp.
  (define-type (LispArray :t)
    "A one-dimensional, non-resizable array of elements.

These arrays are represented as possibly specialized `(cl:simple-array <type> (cl:*))` and are meant to be used as a tool either to interface with Lisp code or to implement efficient data structures. One should consult `Vector` or `Seq` for more general sequential data structure needs.

Whether or not the arrays are specialized depends on the underlying Lisp implementation. Consult `cl:upgraded-array-element-type` to determine whether `LispArray` may get specialized.")

  (define-instance (types:RuntimeRepr :t => types:RuntimeRepr (LispArray :t))
    (define (types:runtime-repr v)
      (let ((element-type (types:runtime-repr (types:proxy-inner v))))
        (lisp types:LispType (element-type)
          `(cl:simple-array ,element-type (cl:*)))))
    (define (types:coalton-type-string _)
      ;; FIXME!!
      "LispArray"))

  (declare make (types:RuntimeRepr :t => UFix -> :t -> LispArray :t))
  (define (make n x)
    "Make a new `LispArray` of length `n` initialized to `x`.

If the type of `x` represents a specialized array "
    ;; FIXME: how can we get this statically?
    (let ((type (types:runtime-repr (types:proxy-of x))))
      (lisp (LispArray :t) (n x type)
        (cl:make-array n :element-type type :initial-element x))))

  (declare make-uninitialized (types:RuntimeRepr :t => UFix -> LispArray :t))
  (define (make-uninitialized n)
    "Make a new LispArray of length `n` that can store elements of type `:t`.

WARNING: The consequences are undefined if an uninitialized element is read before being set.
"
    (let p = types:Proxy)
    (let p_ = (types:proxy-inner p))
    (let type = (types:runtime-repr p_))
    (types:as-proxy-of
     (lisp (LispArray :t) (n type)
       (cl:make-array n :element-type type))
     p))

  (inline)
  (declare length (LispArray :t -> UFix))
  (define (length v)
    "Return the length of the `LispArray` `v`."
    (lisp UFix (v)
      (cl:length v)))

  (inline)
  (declare aref (LispArray :t -> UFix -> :t))
  (define (aref v i)
    "Read the `i`th value of the `LispArray` `v`."
    (lisp :t (v i)
      (cl:aref v i)))

  (inline)
  (declare set! (LispArray :t -> UFix -> :t -> Unit))
  (define (set! v i x)
    "Set the `i`th value of the `LispArray` `v` to `x`."
    (lisp Unit (v i x)
      (cl:setf (cl:aref v i) x)
      Unit))

  (inline)
  (declare copy (LispArray :t -> LispArray :t))
  (define (copy v)
    "Make a deep copy of the `LispArray` `v`."
    (lisp (LispArray :t) (v)
      (cl:copy-seq v)))

  (define-instance (types:RuntimeRepr :t => Into (List :t) (LispArray :t))
    (inline)
    (define (into xs)
      (let ((type (types:runtime-repr (types:proxy-inner (types:proxy-of xs)))))
        (lisp (LispArray :t) (xs type)
          (cl:make-array (cl:length xs) :element-type type :initial-contents xs)))))

  (define-instance (Into (LispArray :t) (List :t))
    (inline)
    (define (into v)
      (let ((len (length v)))
        (if (== 0 len)
            Nil
            (let ((%into (fn (xs i)
                           (if (== 0 i)
                               (Cons (aref v 0) xs)
                               (%into (Cons (aref v i) xs) (- i 1))))))
              (%into Nil (- len 1)))))))

  (define-instance (types:RuntimeRepr :t => Iso (LispArray :t) (List :t)))

  (define-instance (Foldable LispArray)
    (define (fold f init v)
      (let len = (length v))
      (rec % ((i 0) (acc init))
        (if (== i len)
            acc
            (% (+ 1 i) (f acc (aref v i))))))

    (define (foldr f init v)
      (let len = (length v))
      (cond
        ((== 0 len)
         init)
        (True
         (rec % ((i (- len 1)) (acc init))
           (if (== i 0)
               (f (aref v 0) acc)
               (% (- i 1) (f (aref v i) acc))))))))

  (define-instance (types:RuntimeRepr :t => ram:RandomAccess (LispArray :t) :t)
    (inline)
    (define (ram:make n x)
      (make n x))

    (inline)
    (define (ram:make-uninitialized n)
      (make-uninitialized n))

    (inline)
    (define (ram:length v)
      (length v))

    (inline)
    (define (ram:readable? _)
      True)

    (inline)
    (define (ram:writable? _)
      True)

    (inline)
    (define (ram:unsafe-aref v i)
      (aref v i))

    (inline)
    (define (ram:unsafe-set! v i x)
      (set! v i x)))

  (lisp-toplevel ()
    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defmacro define-lisparray-specialization (coalton-type lisp-type)
        "Specialize lisparray access to known primitive types.  This allows the lisp compiler to inline array access."
        (cl:let ((mak (cl:intern (cl:format cl:nil "make/~a" coalton-type)))
                 (mun (cl:intern (cl:format cl:nil "make-uninitialized/~a" coalton-type)))
                 (ref (cl:intern (cl:format cl:nil "aref/~a" coalton-type)))
                 (set (cl:intern (cl:format cl:nil "set!/~a" coalton-type))))
          `(progn
             (specialize make ,mak (UFix -> ,coalton-type -> LispArray ,coalton-type))
             (inline)
             (declare ,mak (UFix -> ,coalton-type -> LispArray ,coalton-type))
             (define (,mak n x)
               (lisp (LispArray ,coalton-type) (n x)
                 (cl:make-array n :element-type ',lisp-type :initial-element x)))
             (specialize make-uninitialized ,mun (UFix -> LispArray ,coalton-type))
             (inline)
             (declare ,mun (UFix -> LispArray ,coalton-type))
             (define (,mun n)
               (lisp (LispArray ,coalton-type) (n)
                 (cl:make-array n :element-type ',lisp-type)))
             (specialize aref ,ref (LispArray ,coalton-type -> UFix -> ,coalton-type))
             (inline)
             (declare ,ref (LispArray ,coalton-type -> UFix -> ,coalton-type))
             (define (,ref v i)
               (lisp ,coalton-type (v i)
                 (cl:aref (cl:the (cl:simple-array ,lisp-type (cl:*)) v) i)))
             (specialize set! ,set (LispArray ,coalton-type -> UFix -> ,coalton-type -> Unit))
             (inline)
             (declare ,set (LispArray ,coalton-type -> UFix -> ,coalton-type -> Unit))
             (define (,set v i x)
               (lisp Unit (v i x)
                 (cl:setf (cl:aref (cl:the (cl:simple-array ,lisp-type (cl:*)) v) i) x)
                 Unit)))))))

  (define-lisparray-specialization F32 cl:single-float)
  (define-lisparray-specialization F64 cl:double-float)
  (define-lisparray-specialization (complex:Complex F32) (cl:complex cl:single-float))
  (define-lisparray-specialization (complex:Complex F64) (cl:complex cl:double-float))
  (define-lisparray-specialization IFix cl:fixnum)
  (define-lisparray-specialization UFix (cl:and cl:fixnum cl:unsigned-byte))
  (define-lisparray-specialization I8 (cl:signed-byte 8))
  (define-lisparray-specialization U8 (cl:unsigned-byte 8))
  (define-lisparray-specialization I16 (cl:signed-byte 16))
  (define-lisparray-specialization U16 (cl:unsigned-byte 16))
  (define-lisparray-specialization I32 (cl:signed-byte 32))
  (define-lisparray-specialization U32 (cl:unsigned-byte 32))
  (define-lisparray-specialization I64 (cl:signed-byte 64))
  (define-lisparray-specialization U64 (cl:unsigned-byte 64)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/LISPARRAY")

