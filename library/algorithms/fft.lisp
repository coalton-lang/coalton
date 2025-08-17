;;;; fft.lisp

(defpackage #:coalton-library/algorithms/fft
  (:documentation "A coalton package for performing FFTs.")
  (:use
   #:coalton)
  (:import-from
   #:coalton-library/system
   #:error)
  (:import-from
   #:coalton-library/math
   #:1+
   #:1-
   #:zero?
   #:mod
   #:Complex
   #:/
   #:positive?
   )
  (:import-from
   #:coalton-library/classes
   #:>
   #:<
   #:<=
   #:Num
   #:+
   #:-
   #:*)
  (:local-nicknames
   (#:array #:coalton-library/lisparray)
   (#:bits #:coalton-library/bits)
   (#:loops #:coalton-library/experimental/loops)
   (#:math #:coalton-library/math)
   (#:ram #:coalton-library/randomaccess))
  (:export
   #:Group
   #:add-identity
   #:add
   #:add-inverse
   #:subtract
   #:Ring
   #:multiply-identity
   #:multiply
   #:Field
   #:multiply-inverse
   #:divide
   #:CyclicGroup
   #:cyclic-add-identity
   #:cyclic-add
   #:cyclic-add-inverse
   #:cyclic-nth-generator
   #:dif-fft-raw
   #:dif-ifft-raw
   #:dit-fft-raw
   #:dit-ifft-raw
   #:fft!
   #:ifft!
   #:fft-into!
   #:ifft-into!
   #:fft
   #:ifft))

(named-readtables:in-readtable coalton:coalton)

(in-package #:coalton-library/algorithms/fft)

(cl:declaim (cl:optimize (cl:speed 3) (cl:safety 0)))

;;; Define the relevant utilities.

(coalton-toplevel

  (declare power-of-2? (UFix -> Boolean))
  (define (power-of-2? x)
    "Is `x` a power of 2? Assumes `(< 0 x)`."
    (zero? (bits:and x (1- x)))))

(coalton-toplevel

  (declare ilog2 (UFix -> UFix))
  (define (ilog2 x)
    "The base-2 logarithm of `x`. Assumes `(power-of-2? x)`."
    (1- (lisp UFix (x) (cl:integer-length x)))))

(coalton-toplevel

  (declare bit-reversed-permutation! ((ram:RandomAccess :c :t)
                                      => UFix -> :c -> Unit))
  (define (bit-reversed-permutation! n storage)
    "Sort the first `n` elements of `storage` into bit-reversed order. Assumes `(power-of-2? n)` and `(<= n (ram:length storage))`."
    (let ((nbits (ilog2 n)))
      (loops:dotimes (i n)
        (let ((j (inline (bits:reverse-n-bits nbits i))))
          (when (< i j)
            (inline (ram:unsafe-rotate! storage i j))))))))

(coalton-toplevel

  (declare copy-bit-reversed-permutation
           ((ram:RandomAccess :c :t) (ram:RandomAccess :d :t)
            => :c -> :d))
  (define (copy-bit-reversed-permutation storage)
    "Create a copy of `storage` in bit-reversed order. Assumes `(power-of-2? (ram:length storage))`."
    (let ((n (ram:length storage))
          (nbits (ilog2 n))
          (dst (ram:make-uninitialized n)))
      (loops:dotimes (i n)
        (let ((j (inline (bits:reverse-n-bits nbits i))))
          (if (< i j)
              (ram:unsafe-set! dst i (ram:unsafe-aref storage j))
              (ram:unsafe-set! dst j (ram:unsafe-aref storage i)))))
      dst)))

;;; Define the type classes that support a discrete Fourier transform.

(coalton-toplevel

  (define-class (Group :t)
    (add-identity :t)
    (add (:t -> :t -> :t))
    (add-inverse (:t -> :t)))

  (declare subtract (Group :t => :t -> :t -> :t))
  (define (subtract x y)
    (add x (add-inverse y)))

  (define-class (Group :t => Ring :t)
    (multiply-identity :t)
    (multiply (:t -> :t -> :t)))

  (define-class (Ring :t => Field :t)
    (multiply-inverse (:t -> :t)))

  (declare divide (Field :t => :t -> :t -> :t))
  (define (divide x y)
    (multiply x (multiply-inverse y)))

  (define-class (CyclicGroup :t)
    (cyclic-add-identity :t)
    (cyclic-add (:t -> :t -> :t))
    (cyclic-add-inverse (:t -> :t))
    (cyclic-nth-generator (UFix -> :t))))

;;; Define the decimation-in-frequency raw FFT and inverse FFT.

(coalton-toplevel

  (declare dif-butterfly ((ram:RandomAccess :c :t) (ram:RandomAccess :d :t)
                          (Ring :t) (CyclicGroup :t)
                          => UFix -> UFix -> :t -> :c -> :d -> Unit))
  (define (dif-butterfly j k w dst src)
    "Perform a single decimation-in-frequency butterfly operation for indices `j` and `k` with the twiddle factor `w`, reading from `src` and writing to `dst`."
    (let ((a (ram:unsafe-aref src j))
          (b (ram:unsafe-aref src k))
          (a+b (add a b))
          (wa-wb (multiply w (add a (multiply (cyclic-nth-generator 2) b)))))
      (ram:unsafe-set! dst j a+b)
      (ram:unsafe-set! dst k wa-wb))))

(coalton-toplevel

  (declare dif-kernel ((ram:RandomAccess :c :t) (ram:RandomAccess :d :t)
                       (Ring :t) (CyclicGroup :t)
                       => UFix -> UFix -> UFix -> :t -> :t
                       -> UFix -> UFix -> :c -> :d -> Unit))
  (define (dif-kernel n m m/2 wm w j k dst src)
    "A tail-recursive kernel for performing a decimation-in-frequency fourier transform for storage of length `n`, for butterfly clusters of size `m`, reading from `src` and writing to `dst`."
    (cond
      ((zero? (bits:and j m/2)) ; Equivalent to (< (mod j m) m/2)
       ;; Perform a butterfly operation.
       (dif-butterfly j k w dst src)
       ;; Rotate the twiddle factor.
       (let w = (cyclic-add w wm))
       ;; Increment the indices.
       (let j = (1+ j))
       (let k = (1+ k))
       ;; Iterate.
       (dif-kernel n m m/2 wm w j k dst src))
      ((< (+ j m/2) n)
       ;; Reset the twiddle factor.
       (let w = cyclic-add-identity)
       ;; Shift the indices to the next block.
       (let j = (+ j m/2))
       (let k = (+ k m/2))
       ;; Iterate.
       (dif-kernel n m m/2 wm w j k dst src))
      ((> m 2)
       ;; Update the parameters for the next layer of butterfly operations.
       (let m   = m/2)
       (let m/2 = (bits:shift -1 m/2))
       (let wm  = (cyclic-add wm wm))
       ;; Reset the twiddle factor.
       (let w   = cyclic-add-identity)
       ;; Reset the indices.
       (let j   = 0)
       (let k   = m/2)
       ;; Ensure data is read from and written to the same storage after
       ;; the first iteration.
       (let src = dst)
       ;; Iterate.
       (dif-kernel n m m/2 wm w j k dst src))
      (True
       Unit))))

(coalton-toplevel

  (declare dif-fft-raw ((ram:RandomAccess :c :t) (ram:RandomAccess :d :t)
                        (Ring :t) (CyclicGroup :t)
                        => :c -> :d -> Unit))
  (define (dif-fft-raw dst src)
    "A decimation-in-frequency fast fourier transform, reading from `src` and writing to `dst`.

Input: natural order
Output: bit-reversed order
Normalization: none"
    (let n    = (ram:length src))
    (let m    = n)
    (let m/2  = (bits:shift -1 m))
    (let wm   = (cyclic-add-inverse (cyclic-nth-generator m)))
    (let w    = cyclic-add-identity)
    (let j    = 0)
    (let k    = m/2)
    (dif-kernel n m m/2 wm w j k dst src)))

(coalton-toplevel

  (declare dif-ifft-raw ((ram:RandomAccess :c :t) (ram:RandomAccess :d :t)
                         (Ring :t) (CyclicGroup :t)
                         => :c -> :d -> Unit))
  (define (dif-ifft-raw dst src)
    "A decimation-in-frequency inverse fast fourier transform, reading from `src` and writing to `dst`.

Input: natural order
Output: bit-reversed order
Normalization: none"
    (let n    = (ram:length src))
    (let m    = n)
    (let m/2  = (bits:shift -1 m))
    (let wm   = (cyclic-nth-generator m))
    (let w    = cyclic-add-identity)
    (let j    = 0)
    (let k    = m/2)
    (dif-kernel n m m/2 wm w j k dst src)))

;;; Define the decimation-in-time raw FFT and inverse FFT.

(coalton-toplevel

  (declare dit-butterfly ((ram:RandomAccess :c :t) (ram:RandomAccess :d :t)
                          (Ring :t) (CyclicGroup :t)
                          => UFix -> UFix -> :t -> :c -> :d -> Unit))
  (define (dit-butterfly j k w dst src)
    "Perform a single decimation-in-time butterfly operation for indices `j` and `k` with the twiddle factor `w`, reading from `src` and writing to `dst`."
    (let ((a (ram:unsafe-aref src j))
          (b (ram:unsafe-aref src k))
          (wb (multiply w b))
          (a+wb (add a wb))
          (a-wb (add a (cyclic-add (cyclic-nth-generator 2) wb))))
      (ram:unsafe-set! dst j a+wb)
      (ram:unsafe-set! dst k a-wb))))

(coalton-toplevel

  (declare dit-kernel
           ((ram:RandomAccess :c :t) (ram:RandomAccess :d :t)
            (Ring :t) (CyclicGroup :t)
            => Boolean -> UFix -> UFix -> UFix -> :t -> :t
            -> UFix -> UFix -> :c -> :d -> Unit))
  (define (dit-kernel inv? n m m/2 wm w j k dst src)
    "A tail-recursive kernel for performing a decimation-in-time fourier transform for storage of length `n`, for butterfly clusters of size `m`, reading from `src` and writing to `dst`."
    (cond
      ((zero? (bits:and j m/2)) ; Equivalent to (< (mod j m) m/2)
       ;; Perform a butterfly operation.
       (dit-butterfly j k w dst src)
       ;; Rotate the twiddle factor.
       (let w = (cyclic-add w wm))
       ;; Increment the indices.
       (let j = (1+ j))
       (let k = (1+ k))
       ;; Iterate.
       (dit-kernel inv? n m m/2 wm w j k dst src))
      ((< (+ j m/2) n)
       ;; Reset the twiddle factor.
       (let w = cyclic-add-identity)
       ;; Shift the indices to the next block.
       (let j = (+ j m/2))
       (let k = (+ k m/2))
       ;; Iterate.
       (dit-kernel inv? n m m/2 wm w j k dst src))
      ((< m n)
       ;; Update the parameters for the next layer of butterfly operations.
       (let m   = (bits:shift 1 m))
       (let m/2 = (bits:shift 1 m/2))
       (let wm  = (cyclic-nth-generator m))
       (let wm  = (if inv? wm (cyclic-add-inverse wm)))
       ;; Reset the twiddle factor.
       (let w   = cyclic-add-identity)
       ;; Reset the indices.
       (let j   = 0)
       (let k   = m/2)
       ;; Ensure data is read from and written to the same storage
       ;; after the first iteration.
       (let src = dst)
       ;; Iterate.
       (dit-kernel inv? n m m/2 wm w j k dst src))
      (True
       Unit))))

(coalton-toplevel

  (declare dit-fft-raw ((ram:RandomAccess :c :t) (ram:RandomAccess :d :t)
                        (Ring :t) (CyclicGroup :t)
                        => :c -> :d -> Unit))
  (define (dit-fft-raw dst src)
    "A decimation-in-time fast fourier transform, reading from `src` and writing to `dst`.

Input: bit-reversed order
Output: natural order
Normalization: none"
    (let inv? = False)
    (let n    = (ram:length src))
    (let m    = 2)
    (let m/2  = (bits:shift -1 m))
    (let wm   = (cyclic-add-inverse (cyclic-nth-generator m)))
    (let w    = cyclic-add-identity)
    (let j    = 0)
    (let k    = m/2)
    (dit-kernel inv? n m m/2 wm w j k dst src)))

(coalton-toplevel

  (declare dit-ifft-raw ((ram:RandomAccess :c :t) (ram:RandomAccess :d :t)
                         (Ring :t) (CyclicGroup :t)
                         => :c -> :d -> Unit))
  (define (dit-ifft-raw dst src)
    "A decimation-in-time inverse fast fourier transform, reading from `src` and writing to `dst`.

Input: bit-reversed order
Output: natural order
Normalization: none"
    (let inv? = True)
    (let n    = (ram:length src))
    (let m    = 2)
    (let m/2  = (bits:shift -1 m))
    (let wm   = (cyclic-nth-generator m))
    (let w    = cyclic-add-identity)
    (let j    = 0)
    (let k    = m/2)
    (dit-kernel inv? n m m/2 wm w j k dst src)))

;;; Define the primary interface.

(coalton-toplevel

  (declare fft! ((ram:RandomAccess :c :t)
                 (Ring :t) (CyclicGroup :t)
                 => :c -> :c))
  (define (fft! storage)
    "Perform an in-place fast Fourier transform on `storage`."
    (let ((n (ram:length storage)))
      (cond
        ((zero? n)
         storage)
        ((power-of-2? n)
         (dif-fft-raw storage storage)
         (bit-reversed-permutation! n storage)
         storage)
        (True
         (error "Currently, only radix-2 ffts are supported.")))))

  (declare ifft! ((ram:RandomAccess :c :t)
                  (Field :t) (CyclicGroup :t) (Num :t)
                  => :c -> :c))
  (define (ifft! storage)
    "Perform an in-place inverse fast Fourier transform on `storage`."
    (let ((n (ram:length storage)))
      (cond
        ((zero? n)
         storage)
        ((power-of-2? n)
         (dif-ifft-raw storage storage)
         (bit-reversed-permutation! n storage)
         (let ((norm (multiply-inverse (math:integral->num n))))
           (loops:dotimes (i n)
             (let ((x (multiply norm (ram:unsafe-aref storage i))))
               (ram:unsafe-set! storage i x))))
         storage)
        (True
         (error "Currently, only radix-2 ffts are supported.")))))

  (declare fft-into! ((ram:RandomAccess :c :t) (ram:RandomAccess :d :t)
                      (Ring :t) (CyclicGroup :t)
                      => :d -> :c -> :d))
  (define (fft-into! dst src)
    "Perform a fast Fourier transform of `src`, writing the result to `dst`. If `dst` is longer than `src`, then remaining elements of `dst` are left unmutated."
    (let ((n (ram:length src)))
      (assert (<= n (ram:length dst)) "`src` cannot be longer than `dst`.")
      (cond
        ((zero? n)
         dst)
        ((power-of-2? n)
         (dif-fft-raw dst src)
         (bit-reversed-permutation! n dst)
         dst)
        (True
         (error "Currently, only radix-2 ffts are supported.")))))

  (declare ifft-into! ((ram:RandomAccess :c :t) (ram:RandomAccess :d :t)
                      (Field :t) (CyclicGroup :t) (Num :t)
                      => :d -> :c -> :d))
  (define (ifft-into! dst src)
    "Perform an inverse fast Fourier transform of `src`, writing the result to `dst`. If `dst` is longer than `src`, then remaining elements of `dst` are left unmutated."
    (let ((n (ram:length src)))
      (assert (<= n (ram:length dst)) "`src` cannot be longer than `dst`.")
      (cond
        ((zero? n)
         dst)
        ((power-of-2? n)
         (dif-ifft-raw dst src)
         (bit-reversed-permutation! n dst)
         (let ((norm (multiply-inverse (math:integral->num n))))
           (loops:dotimes (i n)
             (let ((x (multiply norm (ram:unsafe-aref dst i))))
               (ram:unsafe-set! dst i x))))
         dst)
        (True
         (error "Currently, only radix-2 ffts are supported.")))))

  (declare fft ((ram:RandomAccess :c :t) (ram:RandomAccess :d :t)
                (Ring :t) (CyclicGroup :t)
                => :c -> :d))
  (define (fft storage)
    "Perform a fast Fourier transform on the data in `storage`."
    (let ((n (ram:length storage)))
      (cond
        ((zero? n)
         (ram:make-uninitialized 0))
        ((power-of-2? n)
         (let ((dst (ram:make-uninitialized n)))
           (dif-fft-raw dst storage)
           (bit-reversed-permutation! n dst)
           dst))
        (True
         (error "Currently, only radix-2 ffts are supported.")))))

  (declare ifft ((ram:RandomAccess :c :t) (ram:RandomAccess :d :t)
                (Field :t) (CyclicGroup :t) (Num :t)
                => :c -> :d))
  (define (ifft storage)
    "Perform an inverse fast Fourier transform on the data in `storage`."
    (let ((n (ram:length storage)))
      (cond
        ((zero? n)
         (ram:make-uninitialized 0))
        ((power-of-2? n)
         (let ((dst (ram:make-uninitialized n)))
           (dif-ifft-raw dst storage)
           (bit-reversed-permutation! n dst)
           (let ((norm (multiply-inverse (math:integral->num n))))
             (loops:dotimes (i n)
               (let ((x (multiply norm (ram:unsafe-aref dst i))))
                 (ram:unsafe-set! dst i x))))
           dst))
        (True
         (error "Currently, only radix-2 ffts are supported."))))))

;; Define instances and specializations for efficiently computing FFTs
;; with F32s and F64s.

(coalton-toplevel

  (define-type-alias Array array:LispArray)
  (define-type-alias C64 (Complex F32))
  (define-type-alias C128 (Complex F64))

  (define-instance (Group C64)
    (define add-identity (lisp C64 () #C(0f0 0f0)))
    (inline)
    (define (add x y) (+ x y))
    (inline)
    (define (add-inverse x) (- add-identity x)))

  (define-instance (Ring C64)
    (define multiply-identity (lisp C64 () #C(1f0 0f0)))
    (inline)
    (define (multiply x y) (* x y)))

  (define-instance (Field C64)
    (inline)
    (define (multiply-inverse x) (/ multiply-identity x)))

  (define-instance (CyclicGroup C64)
    (define cyclic-add-identity (lisp C64 () #C(1f0 0f0)))
    (inline)
    (define (cyclic-add x y) (* x y))
    (inline)
    (define (cyclic-add-inverse x) (/ cyclic-add-identity x))
    (inline)
    (define (cyclic-nth-generator n)
      (inline (math:cis (/ (* 2f0 math:pi) (math:integral->num n))))))

  (inline)
  (declare dif-butterfly/c64 (UFix -> UFix -> C64
                                      -> Array C64 -> Array C64 -> Unit))
  (define (dif-butterfly/c64 j k w dst src)
    (let ((a (ram:unsafe-aref src j))
          (b (ram:unsafe-aref src k)))
      (ram:unsafe-set! dst j (+ a b))
      (ram:unsafe-set! dst k (* w (- a b)))))

  (specialize dif-butterfly dif-butterfly/c64
              (UFix -> UFix -> C64 -> Array C64 -> Array C64 -> Unit))

  (inline)
  (declare dit-butterfly/c64 (UFix -> UFix -> C64
                                   -> Array C64 -> Array C64 -> Unit))
  (define (dit-butterfly/c64 j k w dst src)
    (let ((a (ram:unsafe-aref src j))
          (wb (* w (ram:unsafe-aref src k))))
      (ram:unsafe-set! dst j (+ a wb))
      (ram:unsafe-set! dst k (- a wb))))

  (specialize dit-butterfly dit-butterfly/c64
              (UFix -> UFix -> C64 -> Array C64 -> Array C64 -> Unit))

  (define-instance (Group C128)
    (define add-identity (lisp C128 () #C(0d0 0d0)))
    (inline)
    (define (add x y) (+ x y))
    (inline)
    (define (add-inverse x) (- add-identity x)))

  (define-instance (Ring C128)
    (define multiply-identity (lisp C128 () #C(1d0 0d0)))
    (inline)
    (define (multiply x y) (* x y)))

  (define-instance (Field C128)
    (inline)
    (define (multiply-inverse x) (/ multiply-identity x)))

  (define-instance (CyclicGroup C128)
    (define cyclic-add-identity (lisp C128 () #C(1d0 0d0)))
    (inline)
    (define (cyclic-add x y) (* x y))
    (inline)
    (define (cyclic-add-inverse x) (/ cyclic-add-identity x))
    (inline)
    (define (cyclic-nth-generator n)
      (inline (math:cis (/ (* 2d0 math:pi) (math:integral->num n))))))

  (inline)
  (declare dif-butterfly/c128 (UFix -> UFix -> C128
                                    -> Array C128 -> Array C128 -> Unit))
  (define (dif-butterfly/c128 j k w dst src)
    (let ((a (ram:unsafe-aref src j))
          (b (ram:unsafe-aref src k)))
      (ram:unsafe-set! dst j (+ a b))
      (ram:unsafe-set! dst k (* w (- a b)))))

  (specialize dif-butterfly dif-butterfly/c128
              (UFix -> UFix -> C128 -> Array C128 -> Array C128 -> Unit))

  (inline)
  (declare dit-butterfly/c128 (UFix -> UFix -> C128
                                    -> Array C128 -> Array C128 -> Unit))
  (define (dit-butterfly/c128 j k w dst src)
    (let ((a (ram:unsafe-aref src j))
          (wb (* w (ram:unsafe-aref src k))))
      (ram:unsafe-set! dst j (+ a wb))
      (ram:unsafe-set! dst k (- a wb))))

  (specialize dit-butterfly dit-butterfly/c128
              (UFix -> UFix -> C128 -> Array C128 -> Array C128 -> Unit)))

