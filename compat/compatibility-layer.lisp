;;;; Goal of this file is to collect all non-portable code, so as to
;;;; clean up the rest of the code, and hopefully make porting easier.
(cl:defpackage
    #:coalton-compatibility
    ;; #:coalton/compatibility-layer
  (:use #:cl)
  (:export
   #:get-fixnum-bits
;;; try-* are macros/functions that most probably will never be
;;; implemented by all Lisps. So are macros with-*-if-possible.
;;;
;;; For the rest (not starting with try-), while an implementation may
;;; not exist at the moment for some Lisp, it might exist in the
;;; future (and probably should if we want to port there).
   #:with-muffled-code-deletion-note-condition-if-possible
   #:with-start-end-block-if-possible
   #:try-muffle-redefinition-warning-condition
   #:try-unmuffle-redefinition-warning-condition
   #:try-muffle-compiler-note-condition
   #:try-lock-package
   #:try-unlock-package
   #:try-freeze-type
   #:try-optimize-type-check
   #:try-always-bound
   #:get-hash-type
   #:unset-all-float-traps
   #:get-bytes-consed
   #:hash-combine))

(cl:in-package #:coalton-compatibility)

;; define first, as it's used by others and we want it inlined
(declaim (inline get-fixnum-bits))
(defun get-fixnum-bits ()
  #+sbcl sb-vm:n-fixnum-bits
  #-sbcl (+ 0 (cl:ceiling (cl:log cl:most-positive-fixnum 2))))

;;; using (undefined) features debug-try-<macro-name> to enable
;;; canaries for testing the following macros.
;;;
;;; Note: the (wrong on purpose) ignore declaration enabled by the
;;; canary needs to be preceded by whatever sequence of quotes as the
;;; actual macro, otherwise your canary is in a different mine...
;;;
;;; Note: These canaries may increase confidence that the code behaves
;;; as expected but they're no proof of that - one would need to look
;;; at the actual macroexpanded code and verify manually each use of
;;; the macros. But if the canaries don't work, then the code is most
;;; probably *NOT* behaving as intended! Looking at you
;;; "try-muffle-code-deletion-note-condition"... (canary still active
;;; and well on it -- is it because it was optimised away?)
(defmacro with-muffled-code-deletion-note-condition-if-possible (&rest alambda)
  `(locally
       (declare #+sbcl
                (sb-ext:muffle-conditions sb-ext:code-deletion-note)
                ;; does it work? warning if it's enabled
                #+debug-try-muffle-code-deletion-note-condition
                (ignore foo-muffle-code-deletion-note-condition))
     ,@alambda))

(defmacro with-start-end-block-if-possible (funs &rest body)
  (declare #-sbcl(ignore funs))
  `(progn
     #+sbcl
     (declaim (sb-ext:start-block ,@funs))
     ,@body
     #+sbcl
     (declaim (sb-ext:end-block))))

(defmacro try-muffle-redefinition-warning-condition ()
  #+sbcl
  ''(sb-ext:muffle-conditions sb-kernel:redefinition-warning)
  ;; but does it work? warning, if this is enabled
  #+debug-try-muffle-redefinition-warning-condition
  ''(ignore foo-muffle-redefinition-warning-condition))

(defmacro try-unmuffle-redefinition-warning-condition ()
  #+sbcl
  ''(sb-ext:unmuffle-conditions sb-kernel:redefinition-warning)
  ;; but does it work? warning, if this is enabled
  #+debug-try-unmuffle-redefinition-warning-condition
  ''(ignore foo-unmuffle-redefinition-warning-condition))

(defmacro try-muffle-compiler-note-condition ()
  #+sbcl
  ''(sb-ext:muffle-conditions sb-ext:compiler-note)
  ;; but does it work? style-warning, if this is enabled
  #+debug-try-muffle-compiler-note-condition
  ''(ignore foo-muffle-compiler-note-condition))

#+ecl (require '#:package-locks)
(defmacro try-lock-package (the-package)
  (declare #-sbcl(ignore the-package))
  #+sb-package-locks
  `(sb-ext:lock-package ,the-package)
  #+ecl
  `(ext:lock-package ,the-package)
  ;; but does it work? error, if this is enabled
  #+debug-try-lock-package
  `(ignore foo-lock-package))

(defmacro try-unlock-package (the-package)
  (declare #-sbcl(ignore the-package))
  #+sb-package-locks
  `(sb-ext:unlock-package ,the-package)
  #+ecl
  `(ext:unlock-package ,the-package)
  ;; but does it work? error, if this is enabled
  #+debug-try-unlock-package
  `(ignore foo-unlock-package))

(defmacro try-freeze-type (the-type)
  (declare #-sbcl(ignore the-type))
  #+sbcl
  `(declaim (sb-ext:freeze-type ,the-type)
            ;; but does it work? warning, if this is enabled
            #+debug-try-freeze-type
            (ignore foo-freeze-type)))

(defmacro try-optimize-type-check (the-level)
  (declare #-sbcl(ignore the-level))
  #+sbcl
  `'(optimize (sb-c::type-check ,the-level)
              ;; but does it work? warning, if this is enabled
              #+debug-try-optimize-type-check
              (ignore foo-optimize-type-check)))

(defmacro try-always-bound (the-var)
  (declare #-sbcl(ignore the-var))
  #+sbcl
  `(declaim (sb-ext:always-bound ,the-var)
            ;; but does it work? error, if this is enabled
            #+debug-try-always-bound
            (ignore foo-always-bound)))

;;; get-hash-type looks like (cl:unsigned-byte (get-fixnum-bits)), no?
(defmacro get-hash-type-original ()
  #+sbcl
  ''(cl:unsigned-byte 62)
  #+allegro
  ''(cl:unsigned-byte 32)
  ;; https://github.com/Clozure/ccl/blob/ff51228259d9dbc8a9cc7bbb08858ef4aa9fe8d0/level-0/l0-hash.lisp#L1885
  #+ccl
  ''(cl:and cl:fixnum cl:unsigned-byte)
  ;; https://github.com/search?q=repo%3Aarmedbear%2Fabcl++sxhash&type=code
  #+abcl
  ''(cl:and cl:fixnum cl:unsigned-byte)
  ;; https://gitlab.com/embeddable-common-lisp/ecl/-/blob/develop/src/cmp/proclamations.lsp
  #+ecl
  `'(cl:unsigned-byte ,@(coalton-compatibility:get-fixnum-bits))
  #-(or sbcl allegro ccl abcl ecl)
  #.(cl:error "hashing is not supported on ~A" (cl:lisp-implementation-type)))
(defmacro get-hash-type ()
  `'(cl:unsigned-byte ,(coalton-compatibility:get-fixnum-bits)))

(defmacro unset-all-float-traps ()
  '(cl:eval-when (:compile-toplevel :load-toplevel :execute)
    #+ccl (ccl:set-fpu-mode :overflow nil :underflow nil :division-by-zero nil :invalid nil :inexact nil)
    #+sbcl (sb-int:set-floating-point-modes :traps nil)
    #+abcl (extensions:set-floating-point-modes :traps nil)
    #+ecl  (ext:trap-fpe 'cl:t nil)
    ))

#+abcl
(defconstant jvm-runtime
  (java:jstatic
   (java:jmethod "java.lang.Runtime" "getRuntime") nil))
#+abcl
(defconstant max-memory-method (java:jmethod "java.lang.Runtime" "maxMemory"))
#+abcl
(defconstant free-memory-method (java:jmethod "java.lang.Runtime" "freeMemory"))
#+abcl
(defun get-max-memory ()
  (java:jcall max-memory-method jvm-runtime))
#+abcl
(defun get-free-memory()
  (java:jcall free-memory-method jvm-runtime))
#+abcl
(defun get-memory-used()
  "This is not monotonic - the memory used may decrease when the gc runs"
  (- (get-max-memory) (get-free-memory)))
#+(or sbcl )
(pushnew ':|COALTON:HAS-GET-BYTES-CONSED| cl:*features*)
(defun get-bytes-consed ()
  #+sbcl
  (sb-ext:get-bytes-consed)
  #+(and abcl nil)
  (get-memory-used)
  #-(or sbcl )
  0)

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
