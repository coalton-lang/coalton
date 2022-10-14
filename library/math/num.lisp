;;;; num.lisp
;;;;
;;;; Num instances for primitive numerical types

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
   (#:bits #:coalton-library/bits)))

(in-package #:coalton-library/math/num)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(cl:declaim (cl:inline %unsigned->signed))
(cl:defun %unsigned->signed (bits x)
  ;; This is the two's complement conversion of X (interpreted as BITS
  ;; bits) to a signed integer (as a Lisp object).
  (cl:-
   (cl:ldb (cl:byte (cl:1- bits) 0) x)
   (cl:dpb 0 (cl:byte (cl:1- bits) 0) x)))

(cl:defmacro %define-overflow-handler (name bits)
  `(cl:defun ,name (value)
     (cl:typecase value
       ((cl:signed-byte ,bits) value)
       (cl:otherwise
        (cl:cerror "Continue, wrapping around."
                   ,(cl:format cl:nil "Signed value overflowed ~D bits." bits))
        (%unsigned->signed ,bits (cl:mod value ,(cl:expt 2 bits)))))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:defparameter +fixnum-bits+
    #+sbcl sb-vm:n-fixnum-bits
    #-sbcl (cl:1+ (cl:floor (cl:log cl:most-positive-fixnum 2))))
  (cl:defparameter +unsigned-fixnum-bits+
    (cl:1- +fixnum-bits+)))

(%define-overflow-handler %handle-8bit-overflow 8)
(%define-overflow-handler %handle-16bit-overflow 16)
(%define-overflow-handler %handle-32bit-overflow 32)
(%define-overflow-handler %handle-64bit-overflow 64)
(%define-overflow-handler %handle-fixnum-overflow #.+fixnum-bits+)

(cl:defmacro %define-number-stuff (coalton-type)
  (cl:let* ((ops   '(cl:< cl:<= cl:>= cl:>))
            (specs (cl:loop
                      :for op :in ops
                      :collect (alexandria:format-symbol cl:*package* "~A-~A" coalton-type op))))
    `(cl:progn
       ;; Inline these functions unconditionally.
       (cl:declaim (cl:inline ,@specs))
       (coalton-toplevel
         (define-instance (Eq ,coalton-type)
           (define (== a b)
             (lisp Boolean (a b)
               (to-boolean (cl:= a b)))))

         (define-instance (Ord ,coalton-type)
           (define (<=> a b)
             (lisp Ord (a b)
               (cl:cond
                 ((cl:< a b)
                  LT)
                 ((cl:> a b)
                  GT)
                 (cl:t
                  EQ)))))

         ;; These are originally defined in classes.lisp.
         ;;
         ;; These are specializations so we don't need to produce
         ;; intermediate MATCH calls.
         ,@(cl:loop
              :for op :in ops
              :for coalton-op := (cl:find-symbol (cl:symbol-name op) "COALTON-LIBRARY/CLASSES")
              :for spec :in specs
              :for type := `(,coalton-type -> ,coalton-type -> Boolean)
              :collect `(declare ,spec ,type)
              :collect `(define (,spec a b)
                          (lisp Boolean (a b)
                            (to-boolean (,op a b))))
              :collect `(specialize ,coalton-op ,spec ,type))))))

(%define-number-stuff U8)
(%define-number-stuff U16)
(%define-number-stuff U32)
(%define-number-stuff U64)
(%define-number-stuff I8)
(%define-number-stuff I16)
(%define-number-stuff I32)
(%define-number-stuff I64)
(%define-number-stuff Integer)
(%define-number-stuff Fraction)
(%define-number-stuff IFix)
(%define-number-stuff UFix)
(%define-number-stuff Single-Float)
(%define-number-stuff Double-Float)

(coalton-toplevel
  (define-instance (Num Integer)
    (define (+ a b)
      (lisp Integer (a b) (cl:+ a b)))
    (define (- a b)
      (lisp Integer (a b) (cl:- a b)))
    (define (* a b)
      (lisp Integer (a b) (cl:* a b)))
    (define (fromInt x)
      x))

  (define-instance (Num I8)
    (define (+ a b)
      (lisp I8 (a b)
        (%handle-8bit-overflow (cl:+ a b))))
    (define (- a b)
      (lisp I8 (a b)
        (%handle-8bit-overflow (cl:- a b))))
    (define (* a b)
      (lisp I8 (a b)
        (%handle-8bit-overflow (cl:* a b))))
    (define (fromInt x)
      (lisp I8 (x)
        (%handle-8bit-overflow x))))

  (define-instance (Num I16)
    (define (+ a b)
      (lisp I16 (a b)
        (%handle-16bit-overflow (cl:+ a b))))
    (define (- a b)
      (lisp I16 (a b)
        (%handle-16bit-overflow (cl:- a b))))
    (define (* a b)
      (lisp I16 (a b)
        (%handle-16bit-overflow (cl:* a b))))
    (define (fromInt x)
      (lisp I16 (x)
        (%handle-16bit-overflow x))))

  (define-instance (Num I32)
    (define (+ a b)
      (lisp I32 (a b)
        (%handle-32bit-overflow (cl:+ a b))))
    (define (- a b)
      (lisp I32 (a b)
        (%handle-32bit-overflow (cl:- a b))))
    (define (* a b)
      (lisp I32 (a b)
        (%handle-32bit-overflow (cl:* a b))))
    (define (fromInt x)
      (lisp I32 (x)
        (%handle-32bit-overflow x))))

  (define-instance (Num I64)
    (define (+ a b)
      (lisp I64 (a b)
        (%handle-64bit-overflow (cl:+ a b))))
    (define (- a b)
      (lisp I64 (a b)
        (%handle-64bit-overflow (cl:- a b))))
    (define (* a b)
      (lisp I64 (a b)
        (%handle-64bit-overflow (cl:* a b))))
    (define (fromInt x)
      (lisp I64 (x)
        (%handle-64bit-overflow x))))

  (define-instance (Num IFix)
    (define (+ a b)
      (lisp IFix (a b)
        (cl:+ a b)))
    (define (- a b)
      (lisp IFix (a b)
        (cl:- a b)))
    (define (* a b)
      (lisp IFix (a b)
        (cl:* a b)))
    (define (fromInt x)
      (lisp IFix (x)
        (%handle-fixnum-overflow x)))))


(cl:defmacro %define-unsigned-num-instance (coalton-type bits)
  `(coalton-toplevel
     (define-instance (Num ,coalton-type)
       (define (+ a b)
         (lisp ,coalton-type (a b)
           (cl:values (cl:mod (cl:+ a b) ,(cl:expt 2 bits)))))
       (define (- a b)
         (lisp ,coalton-type (a b)
           (cl:values (cl:mod (cl:- a b) ,(cl:expt 2 bits)))))
       (define (* a b)
         (lisp ,coalton-type (a b)
           (cl:values (cl:mod (cl:* a b) ,(cl:expt 2 bits)))))
       (define (fromInt x)
         (lisp ,coalton-type (x)
           (cl:values (cl:mod x ,(cl:expt 2 bits))))))))

(%define-unsigned-num-instance U8   8)
(%define-unsigned-num-instance U16  16)
(%define-unsigned-num-instance U32  32)
(%define-unsigned-num-instance U64  64)
(%define-unsigned-num-instance UFix #.+unsigned-fixnum-bits+)

(cl:defun %optional-coerce (z cl-type)
  "Attempts to coerce Z to an Optional CL-TYPE, returns NONE if failed."
  (cl:let ((x (cl:ignore-errors
               (cl:coerce z cl-type))))
    (cl:if (cl:null x)
           None
           (Some x))))

(cl:defmacro %define-real-float-arith (coalton-type underlying-type)
  "Defines the arithmetic instances for a lisp floating-point type"
  `(coalton-toplevel
     (define-instance (Num ,coalton-type)
       (define (+ a b)
         (lisp ,coalton-type (a b)
           (float-features:with-float-traps-masked cl:t
             (cl:+ a b))))
       (define (- a b)
         (lisp ,coalton-type (a b)
           (float-features:with-float-traps-masked cl:t
             (cl:- a b))))
       (define (* a b)
         (lisp ,coalton-type (a b)
           (float-features:with-float-traps-masked cl:t
             (cl:* a b))))
       (define (fromInt x)
         (match (lisp (Optional ,coalton-type) (x)
                  (%optional-coerce x ,underlying-type))
           ((Some x) x)
           ((None) (if (< 0 x)
                       negative-infinity
                       infinity)))))

     (define-instance (Reciprocable ,coalton-type)
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
            (lisp ,coalton-type (x y)
              (float-features:with-float-traps-masked cl:t
                (cl:/ x y))))))

       (define (reciprocal x)
         (cond
           #+allegro
           ((== x 0)
            infinity)

           (True
            (lisp ,coalton-type (x)
              (float-features:with-float-traps-masked cl:t
                (cl:/ x)))))))

     (define-instance (Dividable Integer ,coalton-type)
       (define (general/ x y)
         (if (== y 0)
             (/ (fromInt x) (fromInt y))
             (match (lisp (Optional ,coalton-type) (x y)
                      (%optional-coerce (cl:/ x y) ,underlying-type))
               ((Some x) x)
               ((None) (if (and (> x 0) (> y 0))
                           infinity
                           negative-infinity))))))

     (define-instance (TryInto ,coalton-type Fraction)
       (define (tryInto x)
         (if (finite? x)
             (Ok (lisp Fraction (x) (cl:rational x)))
             (Err "Could not convert NaN or infinity into a Fraction"))))))

(%define-real-float-arith Single-Float 'cl:single-float)
(%define-real-float-arith Double-Float 'cl:double-float)

;;;; `Bits' instances
;;; signed

(cl:defmacro define-signed-bit-instance (type handle-overflow)
  (cl:flet ((lisp-binop (op)
              `(lisp ,type (left right)
                 (,op left right))))
    `(coalton-toplevel
       (define-instance (bits:Bits ,type)
         (define (bits:and left right)
           ,(lisp-binop 'cl:logand))
         (define (bits:or left right)
           ,(lisp-binop 'cl:logior))
         (define (bits:xor left right)
           ,(lisp-binop 'cl:logxor))
         (define (bits:not bits)
           (lisp ,type (bits) (cl:lognot bits)))
         (define (bits:shift amount bits)
           (lisp ,type (amount bits)
             (,handle-overflow (cl:ash bits amount))))))))

(define-signed-bit-instance I8 %handle-8bit-overflow)
(define-signed-bit-instance I16 %handle-16bit-overflow)
(define-signed-bit-instance I32 %handle-32bit-overflow)
(define-signed-bit-instance I64 %handle-64bit-overflow)
(define-signed-bit-instance IFix cl:identity)
(define-signed-bit-instance Integer cl:identity)

;;; unsigned

(cl:declaim (cl:inline unsigned-lognot)
            (cl:ftype (cl:function (cl:unsigned-byte cl:unsigned-byte)
                                   (cl:values cl:unsigned-byte cl:&optional))
                      unsigned-lognot))
(cl:defun unsigned-lognot (int n-bits)
  (cl:- (cl:ash 1 n-bits) int 1))

(cl:declaim (cl:inline handle-unsigned-overflow)
            (cl:ftype (cl:function (cl:unsigned-byte cl:unsigned-byte)
                                   (cl:values cl:unsigned-byte cl:&optional))
                      handle-unsigned-overflow))
(cl:defun handle-unsigned-overflow (int n-bits)
  (cl:logand (cl:1- (cl:ash 1 n-bits))
             int))

(cl:defmacro define-unsigned-bit-instance (type width)
  (cl:flet ((define-binop (coalton-name lisp-name)
              `(define (,coalton-name left right)
                 (lisp ,type (left right)
                   (,lisp-name left right)))))
    `(coalton-toplevel
       (define-instance (bits:Bits ,type)
         ,(define-binop 'bits:and 'cl:logand)
         ,(define-binop 'bits:or 'cl:logior)
         ,(define-binop 'bits:xor 'cl:logxor)
         (define (bits:not bits)
           (lisp ,type (bits) (unsigned-lognot bits ,width)))
         (define (bits:shift amount bits)
           (lisp ,type (amount bits)
             (cl:logand (cl:ash bits amount)
                        (cl:1- (cl:ash 1 ,width)))))))))

(define-unsigned-bit-instance U8 8)
(define-unsigned-bit-instance U16 16)
(define-unsigned-bit-instance U32 32)
(define-unsigned-bit-instance U64 64)
(define-unsigned-bit-instance UFix #.coalton-library/math/num::+unsigned-fixnum-bits+)

;;;; `Hash' instances

(define-sxhash-hasher I8)
(define-sxhash-hasher I16)
(define-sxhash-hasher I32)
(define-sxhash-hasher I64)
(define-sxhash-hasher U8)
(define-sxhash-hasher U16)
(define-sxhash-hasher U32)
(define-sxhash-hasher U64)
(define-sxhash-hasher Integer)
(define-sxhash-hasher IFix)
(define-sxhash-hasher UFix)
(define-sxhash-hasher Single-Float)
(define-sxhash-hasher Double-Float)
