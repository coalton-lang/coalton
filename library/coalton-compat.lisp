#|
   Goal of this file is to collect all non-portable code, so as to
   clean up the rest of the code, and hopefully make porting easier.
|#
(cl:defpackage #:coalton-library/coalton-compat
  (:use #:cl)
  (:export
;;; try-* are macros/functions that most probably will never be
;;; implemented by all Lisps.
;;;
;;; For the rest (not starting with try-), while an implementation may
;;; not exist at the moment for some Lisp, it might exist in the
;;; future.
   #:try-lock-package
   #:try-freeze-type
   #:hash-combine))

(in-package #:coalton-library/coalton-compat)

(defmacro try-lock-package (the-package)
  #+sb-package-locks
  `(sb-ext:lock-package ,the-package))

(defmacro try-unlock-package (the-package)
  #+sb-package-locks
  `(sb-ext:unlock-package ,the-package))

(defmacro try-freeze-type (the-type)
  #+sbcl
  `(declaim (sb-ext:freeze-type ,the-type)))

(pushnew
 (cond ((= 16 (integer-length cl:most-positive-fixnum))
        ':|COALTON:16-BIT-FIXNUM|)
       ((>= 32 (integer-length cl:most-positive-fixnum))
        ':|COALTON:32-BIT-FIXNUM|)
       ((>= 64 (integer-length cl:most-positive-fixnum))
        ':|COALTON:64-BIT-FIXNUM|)
       (t (error "Unknown fixnum size")))
 cl:*features*)

;;; (defun lisp-combine-hashes (lhs rhs)
;;;   ;; SBCL has a hash combination function
;;;   #+sbcl(sb-int:mix lhs rhs))
;;;
;;; #+(and |COALTON:32-BIT-FIXNUM| (not sbcl))
;;;   ;;
;;;   ;; Generic hash combination functions copied from:
;;;   ;; https://stackoverflow.com/questions/5889238/why-is-xor-the-default-way-to-combine-hashes/27952689#27952689
;;;   ;;
;;;   ;; 32bit hash combination
;;;   ;; #+(or allegro abcl)
;;;   (cl:logxor lhs (cl:+ rhs #x9E3779B9 (cl:ash lhs 6) (cl:ash lhs -2))))
;;;
;;; #+(and |COALTON:64-BIT-FIXNUM| (not sbcl))
;;;   ;; #+(or ccl ecl)
;;;   ;; 64bit hash combination
;;;   ;; logand required on ccl to force the output to be a fixnum
;;;   (cl:logand (cl:logxor lhs (cl:+ rhs #x517CC1B727220A95 (cl:ash lhs 6) (cl:ash lhs -2))) cl:most-positive-fixnum))

;; Notes:
;;
;; 1) Despite a Lisp having 32/64 bit fixnums, they don't necessarily
;; use all the bits - sbcl seems to use 63 bits, and ecl 62 bits.
;;
;; 2) The above stackoverflow discussion seems to have been critisized
;; in another one here, discussing an earlier implementation of
;; boost::hash_combine that used that approach:
;; https://stackoverflow.com/questions/35985960/c-why-is-boosthash-combine-the-best-way-to-combine-hash-values
;;
;; Boost itself uses this approach now:
;; https://www.boost.org/doc/libs/latest/libs/container_hash/doc/html/hash.html#notes_hash_combine
;;
;; And for the definition of combine:
;; https://www.boost.org/doc/libs/latest/libs/container_hash/doc/html/hash.html#ref_hash_combine
;;               combine(s, v) = mix(s + 0x9e3779b9 + v)
;;
;; So, we'll implement the two (32/64bit) boost::hash_combine
;; versions, and then choose the one that's closer to the fixnum size
;; of the current Lisp.
(declaim (inline mod-pos))
(defun mod-pos (x max)
  (declare ((integer) x max))
  (mod (if (> 0 x) (- x) x) ( + 1 max)))

(declaim (inline mix-formula))
(defun mix-formula (x k1 k2 k3 m1 m2 max)
  (declare (fixnum k1 k2 k3 m1 m2) ((integer) max))
  "The original C++ code assumes that it operates on an N-bit unsigned integer - thus mod-pos"
  ;; See https://www.boost.org/doc/libs/latest/libs/container_hash/doc/html/hash.html#notes_hash_combine
  ;; x ^= x >> k1;
  ;; x *= m1;
  ;; x ^= x >> k2;
  ;; x *= m2;
  ;; x ^= x >> k3;
  (let* ((round1 (mod-pos (* (cl:logxor x      (cl:ash x      k1)) m1)
                          max))
         (round2 (mod-pos (* (cl:logxor round1 (cl:ash round1 k2)) m2)
                          max))
         (round3 (mod-pos    (cl:logxor round2 (cl:ash round2 k3))
                             max)))
            round3))

(declaim (inline combine-32bit))
(defun combine-32bit (lhs rhs)
  (declare ((unsigned-byte 32) lhs rhs))
  (mix-formula (mod-pos (+ lhs #x9E3779B9 rhs) #xFFFFFFFF)
               16 15 15 #x21F0AAAD #x735A2D97
               #xFFFFFFFF))

(declaim (inline combine-64bit))
(defun combine-64bit (lhs rhs)
  (declare ((unsigned-byte 64) lhs rhs))
  ;; boost::hash_combine uses #x9E3779B9 for this version too.
  ;;
  ;; see definition of has_combine in lines 469-473 of
  ;; https://github.com/boostorg/container_hash/blob/060d4aea6b5b59d2c9146b7d8e994735b2c0a582/include/boost/container_hash/hash.hpp
  (mix-formula (mod-pos (+ lhs #x517CC1B727220A95 rhs) #xFFFFFFFFFFFFFFFF)
               32 32 28 #xE9846AF9B1A615D #xE9846AF9B1A615D
               #xFFFFFFFFFFFFFFFF))

(defun hash-combine (lhs rhs)
  #+(or |COALTON:32-BIT-FIXNUM| |COALTON:16-BIT-FIXNUM|)
  (declare ((unsigned-byte 32) lhs rhs)) ; may not be fixnums!
  #-(or |COALTON:32-BIT-FIXNUM| |COALTON:16-BIT-FIXNUM|)
  (declare ((unsigned-byte 64) lhs rhs)) ; may not be fixnums!
  "Uses either the 32bit or the 64bit implementation of boost::hash_combine, as these were described on 2025, Oct 11, with constant #x9E3779B9 changed to #xE9846AF9B1A615D in the 64bit case"
  ;; https://web.archive.org/web/20251011141945/https://www.boost.org/doc/libs/latest/libs/container_hash/doc/html/hash.html#notes_hash_combine
  ;;
  ;; https://web.archive.org/web/20251011141945/https://www.boost.org/doc/libs/latest/libs/container_hash/doc/html/hash.html#ref_hash_combine#ref_hash_combine
  ;;
  ;; https://github.com/boostorg/container_hash/blob/060d4aea6b5b59d2c9146b7d8e994735b2c0a582/include/boost/container_hash/detail/hash_mix.hpp
  (mod-pos                              ; ensure it's <= most-positive-fixnum
   #+(or |COALTON:32-BIT-FIXNUM| |COALTON:16-BIT-FIXNUM|)
   (combine-32bit lhs rhs)
   #-(or |COALTON:32-BIT-FIXNUM| |COALTON:16-BIT-FIXNUM|)
   (combine-64bit lhs rhs)
   cl:most-positive-fixnum))
