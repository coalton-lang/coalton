;;;; lisparray.lisp
;;;;
;;;; An interface to Common Lisp rank-1 SIMPLE-ARRAYs.

(coalton-library/utils:defstdlib-package #:coalton-library/lisparray
  (:use #:coalton)
  (:local-nicknames
   (#:types #:coalton-library/types))
  (:export
   #:LispArray
   #:make
   #:make-uninitialized
   #:length
   #:aref
   #:set!))

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
          `(cl:simple-array ,element-type (cl:*))))))

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

  (lisp-toplevel ()
    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defmacro define-lisparray-specialization (coalton-type lisp-type)
        "Specialize lisparray access to known primitive types.  This allows the lisp compiler to inline array access."
        (cl:let ((ref (cl:intern (cl:format cl:nil "aref/~a" coalton-type)))
                 (set (cl:intern (cl:format cl:nil "set!/~a" coalton-type))))
          `(progn
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

  (define-lisparray-specialization Single-Float cl:single-float)
  (define-lisparray-specialization Double-Float cl:double-float)
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
