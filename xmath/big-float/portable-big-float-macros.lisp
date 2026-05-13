;;;; portable-big-float-macros.lisp
;;;;
;;;; Helper macros used by the portable Big-Float Coalton implementation.

(cl:in-package #:coalton/xmath/big-float)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro define-big-float-primitive-conversions ()
    `(progn
       ,@(cl:loop :for integer-type :in '(Bit U8 I8 U16 I16 U32 I32 U64 I64 UFix IFix)
                  :collect
                  `(define-instance (Into ,integer-type Big-Float)
                     (inline)
                     (define (into a)
                       (integer->big-float-exact (toInteger a)))))))

  (cl:defmacro define-big-float-tryinto-float (coalton-float lisp-float neg-zero least-positive most-positive)
    (cl:let* ((digits (cl:float-digits (cl:coerce 1 lisp-float)))
              (min-exp (cl:nth-value 1 (cl:integer-decode-float
                                        (cl:symbol-value least-positive))))
              (max-exp (cl:nth-value 1 (cl:integer-decode-float
                                        (cl:symbol-value most-positive)))))
      `(define-instance (TryInto Big-Float ,coalton-float)
         (define (tryInto a)
           (match a
             ((BFConst f) (tryInto (f)))
             ((BFInf) (Some infinity))
             ((BFNegInf) (Some negative-infinity))
             ((BFNaN) (Some nan))
             ((BFNegZero) (Some (lisp (-> ,coalton-float) () ,neg-zero)))
             ((BFValue (Dyadic m k))
              (match (dyadic->native-float-exact m k ,digits ,min-exp ,max-exp)
                ((Some (Tuple q e))
                 (lisp (-> (Optional ,coalton-float)) (q e)
                   (cl:let ((b (cl:ignore-errors
                                (cl:scale-float (cl:coerce q ',lisp-float) e))))
                     (cl:if b
                            (Some b)
                            None))))
                ((None) None)))))))))

  (cl:defmacro define-big-float-round-to-float
      (name coalton-float lisp-float neg-zero least-positive most-positive)
    (cl:let* ((digits (cl:float-digits (cl:coerce 1 lisp-float)))
              (min-exp (cl:nth-value 1 (cl:integer-decode-float
                                        (cl:symbol-value least-positive))))
              (max-exp (cl:nth-value 1 (cl:integer-decode-float
                                        (cl:symbol-value most-positive)))))
      `(progn
         (declare ,name (Big-Float -> ,coalton-float))
         (define (,name a)
           "Round a Big-Float to the nearest representable native float, with ties to even."
           (match a
             ((BFConst f) (,name (f)))
             ((BFInf) infinity)
             ((BFNegInf) negative-infinity)
             ((BFNaN) nan)
             ((BFNegZero) (lisp (-> ,coalton-float) () ,neg-zero))
             ((BFValue (Dyadic m k))
              (match (dyadic->native-float-rounded m k ,digits ,min-exp ,max-exp)
                ((NativeFloatZero negative?)
                 (if negative?
                     (lisp (-> ,coalton-float) () ,neg-zero)
                     0))
                ((NativeFloatOverflow negative?)
                 (if negative?
                     negative-infinity
                     infinity))
                ((NativeFloatFinite q e)
                 (lisp (-> ,coalton-float) (q e)
                   (cl:scale-float (cl:coerce q ',lisp-float) e))))))))))
