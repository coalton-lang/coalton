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

  (declare length (LispArray :t -> UFix))
  (define (length v)
    "Return the length of the `LispArray` `v`."
    (lisp UFix (v)
      (cl:length v)))

  (declare aref (LispArray :t -> UFix -> :t))
  (define (aref v i)
    "Read the `i`th value of the `LispArray` `v`."
    (lisp :t (v i)
      (cl:aref v i)))

  (declare set! (LispArray :t -> UFix -> :t -> Unit))
  (define (set! v i x)
    "Set the `i`th value of the `LispArray` `v` to `x`."
    (lisp Unit (v i x)
      (cl:setf (cl:aref v i) x)
      Unit)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/LISPARRAY")
