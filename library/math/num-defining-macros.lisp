;;;; num-defining-macros.lisp
;;;;
;;;; Ordinarily we would like to interleave these macros definitions
;;;; with LISP-TOPLEVEL in Coalton code. However, due to issue #1408,
;;;; nuances of when things get evaluated and whether they persist in
;;;; FASLs linger. In the mean time, we define the Lisp macros
;;;; separately before being used by Coalton.
;;;;
;;;; See also hash-defining-macros.lisp
;;;;
;;;; NOTE: This package is not intended to be used by users.

(defpackage #:coalton-library/math/num-defining-macros
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/functions
   #:coalton-library/utils
   #:coalton-library/math/arith
   #:coalton-compatibility-layer)
  (:local-nicknames
   (#:ff #:float-features)
   (#:bits #:coalton-library/bits)
   (#:compat #:coalton-compatibility-layer))
  (:export
   #:+fixnum-bits+
   #:+unsigned-fixnum-bits+
   #:define-eq
   #:define-ord
   #:define-num-checked
   #:%handle-8bit-overflow
   #:%handle-16bit-overflow
   #:%handle-32bit-overflow
   #:%handle-64bit-overflow
   #:%handle-fixnum-overflow
   #:define-num-wrapping
   #:define-num-float
   #:define-float-fraction-conversion
   #:define-reciprocable-float
   #:define-dividable-float
   #:define-bits-checked
   #:define-bits-wrapping
   #:define-default-num))

(in-package #:coalton-library/math/num-defining-macros)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defconstant +fixnum-bits+
    (compat:get-fixnum-bits))
  (cl:defconstant +unsigned-fixnum-bits+
    (cl:1- +fixnum-bits+)))


;;; Utilities to define Eq types.
(cl:defmacro define-eq (type)
  `(define-instance (Eq ,type)
     (inline)
     (define (== a b)
       (lisp Boolean (a b)
         ;; Use cl:= so that (== 0.0 -0.0) => True
         (cl:= a b)))))


;;; Utilities to define Ord types.
(cl:defmacro define-ord (type)
  (cl:let ((>-spec (alexandria:format-symbol cl:*package* "~A->" type))
           (>=-spec (alexandria:format-symbol cl:*package* "~A->=" type))
           (<-spec (alexandria:format-symbol cl:*package* "~A-<" type))
           (<=-spec (alexandria:format-symbol cl:*package* "~A-<=" type))
           (min-spec (alexandria:format-symbol cl:*package* "~A-MIN" type))
           (max-spec (alexandria:format-symbol cl:*package* "~A-MAX" type)))

    ;; Generates the instance and specializations to use more direct
    ;; comparison functions when possible.

    `(progn
       (define-instance (Ord ,type)
         (inline)
         (define (<=> a b)
           (lisp Ord (a b)
             (cl:cond
               ((cl:< a b)
                LT)
               ((cl:> a b)
                GT)
               (cl:t
                EQ)))))

       (specialize > ,>-spec (,type -> ,type -> Boolean))
       (inline)
       (declare ,>-spec (,type -> ,type -> Boolean))
       (define (,>-spec a b)
         (lisp Boolean (a b)
           (to-boolean (cl:> a b))))

       (specialize >= ,>=-spec (,type -> ,type -> Boolean))
       (inline)
       (declare ,>=-spec (,type -> ,type -> Boolean))
       (define (,>=-spec a b)
         (lisp Boolean (a b)
           (to-boolean (cl:>= a b))))

       (specialize < ,<-spec (,type -> ,type -> Boolean))
       (inline)
       (declare ,<-spec (,type -> ,type -> Boolean))
       (define (,<-spec a b)
         (lisp Boolean (a b)
           (to-boolean (cl:< a b))))

       (specialize <= ,<=-spec (,type -> ,type -> Boolean))
       (inline)
       (declare ,<=-spec (,type -> ,type -> Boolean))
       (define (,<=-spec a b)
         (lisp Boolean (a b)
           (to-boolean (cl:<= a b))))

       (specialize min ,min-spec (,type -> ,type -> ,type))
       (inline)
       (declare ,min-spec (,type -> ,type -> ,type))
       (define (,min-spec a b)
         (lisp ,type (a b)
           (cl:min a b)))

       (specialize max ,max-spec (,type -> ,type -> ,type))
       (inline)
       (declare ,max-spec (,type -> ,type -> ,type))
       (define (,max-spec a b)
         (lisp ,type (a b)
           (cl:max a b))))))


;;; Utilities to define integer overflow and wrapping Num instances.
(cl:declaim (cl:inline %unsigned->signed))
(cl:defun %unsigned->signed (bits x)
  ;; This is the two's complement conversion of X (interpreted as BITS
  ;; bits) to a signed integer (as a Lisp object).
  (cl:-
   (cl:ldb (cl:byte (cl:1- bits) 0) x)
   (cl:dpb 0 (cl:byte (cl:1- bits) 0) x)))

(cl:defmacro %define-overflow-handler (name bits)
  `(cl:progn
     (cl:declaim (cl:inline ,name))
     (cl:defun ,name (value)
       (cl:typecase value
         ((cl:signed-byte ,bits) value)
         (cl:otherwise
          (cl:cerror "Continue, wrapping around."
                     ,(cl:format cl:nil "Signed value overflowed ~D bits." bits))
          (%unsigned->signed ,bits (cl:mod value ,(cl:expt 2 bits))))))))


(%define-overflow-handler %handle-8bit-overflow 8)
(%define-overflow-handler %handle-16bit-overflow 16)
(%define-overflow-handler %handle-32bit-overflow 32)
(%define-overflow-handler %handle-64bit-overflow 64)
(%define-overflow-handler %handle-fixnum-overflow #.+fixnum-bits+)

(cl:defmacro define-num-checked (type overflow-handler)
  "Define a `Num' instance for TYPE which signals on overflow."
  `(define-instance (Num ,type)
     (inline)
     (define (+ a b)
       (lisp ,type (a b)
         (,overflow-handler (cl:+ a b))))

     (inline)
     (define (- a b)
       (lisp ,type (a b)
         (,overflow-handler (cl:- a b))))

     (inline)
     (define (* a b)
       (lisp ,type (a b)
         (,overflow-handler (cl:* a b))))

     (inline)
     (define (fromInt x)
       (lisp ,type (x)
         (,overflow-handler x)))))

(cl:defmacro define-num-wrapping (type bits)
  "Define a `Num' instance for TYPE which wraps on overflow."
  `(define-instance (Num ,type)
     (inline)
     (define (+ a b)
       (lisp ,type (a b)
         (cl:values (cl:mod (cl:+ a b) ,(cl:expt 2 bits)))))

     (inline)
     (define (- a b)
       (lisp ,type (a b)
         (cl:values (cl:mod (cl:- a b) ,(cl:expt 2 bits)))))

     (inline)
     (define (* a b)
       (lisp ,type (a b)
         (cl:values (cl:mod (cl:* a b) ,(cl:expt 2 bits)))))

     (inline)
     (define (fromInt x)
       (lisp ,type (x)
         (cl:values (cl:mod x ,(cl:expt 2 bits)))))))


;;; Float Num instances.
(cl:defmacro define-num-float (type lisp-type plus-inf minus-inf)
  "Define `Num' for the float type TYPE."

  ;;
  ;; CCL has a tendency to re-enable float traps. The explicit float
  ;; trap masking keeps the test suite working during interactive
  ;; development.
  ;;
  ;; Allegro appears to have some checks that make some arithmetic
  ;; functions error on some inputs. The explicit checks in division
  ;; keep the behavior consistent with IEEE 754.
  ;;

  `(define-instance (Num ,type)
     (inline)
     (define (+ a b)
       (lisp ,type (a b)
         (#+(not ccl) cl:progn
            #+ccl ff:with-float-traps-masked #+ccl cl:t
            (cl:+ a b))))

     (inline)
     (define (- a b)
       (lisp ,type (a b)
         (#+(not ccl) cl:progn
            #+ccl ff:with-float-traps-masked #+ccl cl:t
            (cl:- a b))))

     (inline)
     (define (* a b)
       (lisp ,type (a b)
         (#+(not ccl) cl:progn
            #+ccl ff:with-float-traps-masked #+ccl cl:t
            (cl:* a b))))

     (inline)
     (define (fromInt x)
       (lisp ,type (x)
         (cl:or (cl:ignore-errors (cl:coerce x ',lisp-type))
                (cl:if (cl:< x 0)
                       ,minus-inf
                       ,plus-inf))))))


;;; Utility to define type -> Fraction conversions.
(cl:defmacro define-float-fraction-conversion (type)
  `(define-instance (TryInto ,type Fraction String)
     (define (tryInto x)
       (if (finite? x)
           (Ok (lisp Fraction (x) (cl:rational x)))
           (Err "Could not convert NaN or infinity into a Fraction")))))


;;; Utility to define Reciprocable instances on floats.
(cl:defmacro define-reciprocable-float (type)
  `(define-instance (Reciprocable ,type)
     (inline)
     (define (/ x y)
       (cond
         #+allegro
         ((or (nan? x)
              (nan? y))
          nan)

         #+allegro
         ((and (== x 0) (== y 0))
          nan)

         #+allegro
         ((and (positive? x) (== y 0))
          infinity)

         #+allegro
         ((and (negative? x) (== y 0))
          negative-infinity)

         (True
          (lisp ,type (x y)
            (#+(not ccl) cl:progn
               #+ccl ff:with-float-traps-masked #+ccl cl:t
               (cl:/ x y))))))

     (inline)
     (define (reciprocal x)
       (cond
         #+allegro
         ((== x 0)
          infinity)

         (True
          (lisp ,type (x)
            (#+(not ccl) cl:progn
               #+ccl ff:with-float-traps-masked #+ccl cl:t
               (cl:/ x))))))))

(cl:defmacro define-dividable-float (type lisp-type)
  `(define-instance (Dividable Integer ,type)
     (inline)
     (define (general/ x y)
       (if (== y 0)
           (/ (fromInt x) (fromInt y))
           (lisp ,type (x y)
             (cl:or (cl:ignore-errors (cl:coerce (cl:/ x y) ',lisp-type))
                    (cl:if (cl:eq (cl:< x 0) (cl:< y 0))
                           (coalton (the ,type infinity))
                           (coalton (the ,type negative-infinity)))))))))


;;; Utilities to define Bits instances.
(cl:defmacro define-bits-checked (type handle-overflow)
  `(define-instance (bits:Bits ,type)
     (inline)
     (define (bits:and a b)
       (lisp ,type (a b)
         (cl:logand a b)))

     (inline)
     (define (bits:or a b)
       (lisp ,type (a b)
         (cl:logior a b)))

     (inline)
     (define (bits:xor a b)
       (lisp ,type (a b)
         (cl:logxor a b)))

     (inline)
     (define (bits:not x)
       (lisp ,type (x)
         (cl:lognot x)))

     (inline)
     (define (bits:shift amount bits)
       (lisp ,type (amount bits)
         (,handle-overflow (cl:ash bits amount))))))

(cl:declaim (cl:inline unsigned-lognot))
(cl:defun unsigned-lognot (int n-bits)
  (cl:declare (cl:type cl:unsigned-byte int)
              (cl:type cl:unsigned-byte n-bits)
              (cl:values cl:unsigned-byte))

  (cl:- (cl:ash 1 n-bits) int 1))

(cl:defmacro define-bits-wrapping (type width)
  `(define-instance (bits:Bits ,type)
     (inline)
     (define (bits:and a b)
       (lisp ,type (a b)
         (cl:logand a b)))

     (inline)
     (define (bits:or a b)
       (lisp ,type (a b)
         (cl:logior a b)))

     (inline)
     (define (bits:xor a b)
       (lisp ,type (a b)
         (cl:logxor a b)))

     (inline)
     (define (bits:not x)
       (lisp ,type (x)
         (unsigned-lognot x ,width)))

     (inline)
     (define (bits:shift amount bits)
       (lisp ,type (amount bits)
         (cl:logand (cl:ash bits amount)
                    ,(cl:1- (cl:ash 1 width)))))))

;;; Utility to define Default instances.
(cl:defmacro define-default-num (type)
  `(define-instance (Default ,type)
     (inline)
     (define (default) 0)))
