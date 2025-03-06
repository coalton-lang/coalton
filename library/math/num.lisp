;;;; num.lisp
;;;;
;;;; Instances for primitive numerical types

(coalton-library/utils:defstdlib-package #:coalton-library/math/num
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/functions
   #:coalton-library/utils
   #:coalton-library/math/arith)
  (:import-from
   #:coalton-library/hash
   #:define-sxhash-hasher)
  (:local-nicknames
   (#:ff #:float-features)
   (#:bits #:coalton-library/bits)))

(in-package #:coalton-library/math/num)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

;;;
;;; Constants
;;;

  (lisp-toplevel ()

    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defconstant +fixnum-bits+
        #+sbcl sb-vm:n-fixnum-bits
        #-sbcl (cl:1+ (cl:floor (cl:log cl:most-positive-fixnum 2))))
      (cl:defconstant +unsigned-fixnum-bits+
        (cl:1- +fixnum-bits+)))

;;;
;;; Eq Instances
;;;

    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defmacro define-eq (type)
        `(progn
           (lisp-toplevel ()
             (cl:pushnew ',type coalton-impl/typechecker/types:*number-types*))
           (define-instance (Eq ,type)
             (inline)
             (define (== a b)
               (lisp Boolean (a b)
                 ;; Use cl:= so that (== 0.0 -0.0) => True
                 (cl:= a b))))))))

  (define-eq Integer)
  (define-eq IFix)
  (define-eq UFix)
  (define-eq I8)
  (define-eq U8)
  (define-eq I16)
  (define-eq U16)
  (define-eq I32)
  (define-eq U32)
  (define-eq I64)
  (define-eq U64)
  (define-eq Single-Float)
  (define-eq Double-Float)

;;;
;;; Ord Instances
;;;

  (lisp-toplevel ()

    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defmacro define-ord (type)
        (cl:let ((>-spec (alexandria:format-symbol cl:*package* "~A->" type))
                 (>=-spec (alexandria:format-symbol cl:*package* "~A->=" type))
                 (<-spec (alexandria:format-symbol cl:*package* "~A-< type" type))
                 (<=-spec (alexandria:format-symbol cl:*package* "~A-<=" type)))

          ;; Generates the instance and specializations to use more direct
          ;; comparison functions when possible.

          `(progn
             (define-instance (Ord ,type)
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
             (declare ,>-spec (,type -> ,type -> Boolean))
             (define (,>-spec a b)
               (lisp Boolean (a b)
                 (to-boolean (cl:> a b))))

             (specialize >= ,>=-spec (,type -> ,type -> Boolean))
             (declare ,>=-spec (,type -> ,type -> Boolean))
             (define (,>=-spec a b)
               (lisp Boolean (a b)
                 (to-boolean (cl:>= a b))))

             (specialize < ,<-spec (,type -> ,type -> Boolean))
             (declare ,<-spec (,type -> ,type -> Boolean))
             (define (,<-spec a b)
               (lisp Boolean (a b)
                 (to-boolean (cl:< a b))))

             (specialize <= ,<=-spec (,type -> ,type -> Boolean))
             (declare ,<=-spec (,type -> ,type -> Boolean))
             (define (,<=-spec a b)
               (lisp Boolean (a b)
                 (to-boolean (cl:<= a b)))))))))

  (define-ord Integer)
  (define-ord IFix)
  (define-ord UFix)
  (define-ord I8)
  (define-ord U8)
  (define-ord I16)
  (define-ord U16)
  (define-ord I32)
  (define-ord U32)
  (define-ord I64)
  (define-ord U64)
  (define-ord Single-Float)
  (define-ord Double-Float)

;;;
;;; Overflow checks for signed values
;;;

  (lisp-toplevel ()

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

;;;
;;; Num instances for integers
;;;

    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defmacro define-num-checked (type overflow-handler)
        "Define a `Num' instance for TYPE which signals on overflow."
        `(define-instance (Num ,type)
           (define (+ a b)
             (lisp ,type (a b)
               (,overflow-handler (cl:+ a b))))

           (define (- a b)
             (lisp ,type (a b)
               (,overflow-handler (cl:- a b))))

           (define (* a b)
             (lisp ,type (a b)
               (,overflow-handler (cl:* a b))))

           (define (fromInt x)
             (lisp ,type (x)
               (,overflow-handler x))))))

    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defmacro define-num-wrapping (type bits)
        "Define a `Num' instance for TYPE which wraps on overflow."
        `(define-instance (Num ,type)
           (define (+ a b)
             (lisp ,type (a b)
               (cl:values (cl:mod (cl:+ a b) ,(cl:expt 2 bits)))))

           (define (- a b)
             (lisp ,type (a b)
               (cl:values (cl:mod (cl:- a b) ,(cl:expt 2 bits)))))

           (define (* a b)
             (lisp ,type (a b)
               (cl:values (cl:mod (cl:* a b) ,(cl:expt 2 bits)))))

           (define (fromInt x)
             (lisp ,type (x)
               (cl:values (cl:mod x ,(cl:expt 2 bits)))))))))


  (define-num-checked Integer cl:identity)

  (define-num-checked I8 %handle-8bit-overflow)
  (define-num-checked I16 %handle-16bit-overflow)
  (define-num-checked I32 %handle-32bit-overflow)
  (define-num-checked I64 %handle-64bit-overflow)
  (define-num-checked IFix %handle-fixnum-overflow)

  (define-num-wrapping U8 8)
  (define-num-wrapping U16 16)
  (define-num-wrapping U32 32)
  (define-num-wrapping U64 64)
  (define-num-wrapping UFix #.+unsigned-fixnum-bits+)

;;;
;;; Num instances for floats
;;;

  (lisp-toplevel ()

    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defun %optional-coerce (z cl-type)
        "Attempts to coerce Z to an Optional CL-TYPE, returns NONE if failed."
        (cl:let ((x (cl:ignore-errors
                     (cl:coerce z cl-type))))
          (cl:if (cl:null x)
                 None
                 (Some x))))

      (cl:defmacro define-num-float (type lisp-type)
        "Define `Num' for TYPE"

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
           (define (+ a b)
             (lisp ,type (a b)
               (#+(not ccl) cl:progn
                  #+ccl ff:with-float-traps-masked #+ccl cl:t
                  (cl:+ a b))))

           (define (- a b)
             (lisp ,type (a b)
               (#+(not ccl) cl:progn
                  #+ccl ff:with-float-traps-masked #+ccl cl:t
                  (cl:- a b))))

           (define (* a b)
             (lisp ,type (a b)
               (#+(not ccl) cl:progn
                  #+ccl ff:with-float-traps-masked #+ccl cl:t
                  (cl:* a b))))

           (define (fromInt x)
             (match (lisp (Optional ,type) (x)
                      (%optional-coerce x ',lisp-type))
               ((Some x) x)
               ((None) (if (< 0 x)
                           negative-infinity
                           infinity))))))))

  (define-num-float Single-Float cl:single-float)
  (define-num-float Double-Float cl:double-float)

;;;
;;; Float to `Fraction' conversions
;;;

  (lisp-toplevel ()

    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defmacro define-float-fraction-conversion (type)
        `(define-instance (TryInto ,type Fraction String)
           (define (tryInto x)
             (if (finite? x)
                 (Ok (lisp Fraction (x) (cl:rational x)))
                 (Err "Could not convert NaN or infinity into a Fraction")))))))

  (define-float-fraction-conversion Single-Float)
  (define-float-fraction-conversion Double-Float)

;;;
;;; `Dividable' and `Reciprocable' instances for floata
;;;

  (lisp-toplevel ()

    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defmacro define-reciprocable-float (type)
        `(define-instance (Reciprocable ,type)
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

           (define (reciprocal x)
             (cond
               #+allegro
               ((== x 0)
                infinity)

               (True
                (lisp ,type (x)
                  (#+(not ccl) cl:progn
                     #+ccl ff:with-float-traps-masked #+ccl cl:t
                     (cl:/ x)))))))))

    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defmacro define-dividable-float (type lisp-type)
        `(define-instance (Dividable Integer ,type)
           (define (general/ x y)
             (if (== y 0)
                 (/ (fromInt x) (fromInt y))
                 (match (lisp (Optional ,type) (x y)
                          (%optional-coerce (cl:/ x y) ',lisp-type))
                   ((Some x) x)
                   ((None) (if (and (> x 0) (> y 0))
                               infinity
                               negative-infinity)))))))))

  (define-reciprocable-float Single-Float)
  (define-reciprocable-float Double-Float)

  (define-dividable-float Single-Float cl:single-float)
  (define-dividable-float Double-Float cl:double-float)

;;;
;;; `Bits' instances
;;;

  (lisp-toplevel ()

    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defmacro define-bits-checked (type handle-overflow)
        `(define-instance (bits:Bits ,type)
           (define (bits:and a b)
             (lisp ,type (a b)
               (cl:logand a b)))

           (define (bits:or a b)
             (lisp ,type (a b)
               (cl:logior a b)))

           (define (bits:xor a b)
             (lisp ,type (a b)
               (cl:logxor a b)))

           (define (bits:not x)
             (lisp ,type (x)
               (cl:lognot x)))

           (define (bits:shift amount bits)
             (lisp ,type (amount bits)
               (,handle-overflow (cl:ash bits amount)))))))

    (cl:declaim (cl:inline unsigned-lognot))
    (cl:defun unsigned-lognot (int n-bits)
      (cl:declare (cl:type cl:unsigned-byte int)
                  (cl:type cl:unsigned-byte n-bits)
                  (cl:values cl:unsigned-byte))

      (cl:- (cl:ash 1 n-bits) int 1))

    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defmacro define-bits-wrapping (type width)
        `(define-instance (bits:Bits ,type)
           (define (bits:and a b)
             (lisp ,type (a b)
               (cl:logand a b)))

           (define (bits:or a b)
             (lisp ,type (a b)
               (cl:logior a b)))

           (define (bits:xor a b)
             (lisp ,type (a b)
               (cl:logxor a b)))

           (define (bits:not x)
             (lisp ,type (x)
               (unsigned-lognot x ,width)))

           (define (bits:shift amount bits)
             (lisp ,type (amount bits)
               (cl:logand (cl:ash bits amount)
                          ,(cl:1- (cl:ash 1 width)))))))))

  (define-bits-checked Integer cl:identity)

  (define-bits-checked I8 %handle-8bit-overflow)
  (define-bits-checked I16 %handle-16bit-overflow)
  (define-bits-checked I32 %handle-32bit-overflow)
  (define-bits-checked I64 %handle-64bit-overflow)
  (define-bits-checked IFix %handle-fixnum-overflow)

  (define-bits-wrapping U8 8)
  (define-bits-wrapping U16 16)
  (define-bits-wrapping U32 32)
  (define-bits-wrapping U64 64)
  (define-bits-wrapping UFix #.+unsigned-fixnum-bits+)


  (lisp-toplevel ()

;;; `Hash' instances

    (define-sxhash-hasher Integer)
    (define-sxhash-hasher I8)
    (define-sxhash-hasher I16)
    (define-sxhash-hasher I32)
    (define-sxhash-hasher I64)
    (define-sxhash-hasher U8)
    (define-sxhash-hasher U16)
    (define-sxhash-hasher U32)
    (define-sxhash-hasher U64)
    (define-sxhash-hasher IFix)
    (define-sxhash-hasher UFix)
    (define-sxhash-hasher Single-Float)
    (define-sxhash-hasher Double-Float)

;;;
;;; Default instances
;;;

    (cl:eval-when (:compile-toplevel :load-toplevel)
      (cl:defmacro define-default-num (type)
        `(define-instance (Default ,type)
           (define (default) 0)))))

  (define-default-num I8)
  (define-default-num U8)
  (define-default-num I16)
  (define-default-num I32)
  (define-default-num I64)
  (define-default-num U16)
  (define-default-num U32)
  (define-default-num U64)
  (define-default-num IFix)
  (define-default-num UFix)
  (define-default-num Integer)
  (define-default-num Double-Float)
  (define-default-num Single-Float))
