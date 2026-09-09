;;;; Goal of this file is to collect all non-portable code, so as to
;;;; clean up the rest of the code, and hopefully make porting easier.
(cl:defpackage
    #:coalton-compatibility
  (:use #:cl #:fiasco)
  (:export
   ;; try-* are macros/functions that most probably will never be
   ;; implemented by all Lisps.
   ;;
   ;; For the rest (not starting with try-), while an implementation
   ;; may not exist at the moment for some Lisp, it might exist in the
   ;; future (and probably should if we want to port there).
   #:try-lock-package
   #:try-unlock-package

   #:get-bytes-consed

   #:unset-all-float-traps

   #:get-fixnum-bits
   #:get-hash-type
   #:hash-combine))

(cl:in-package #:coalton-compatibility)

;;; Hashing (or sbcl allegro ccl abcl ecl) - hopefully it's generic.

;; define first, as it's used by others and we want it inlined
(declaim (inline get-fixnum-bits))
(defun get-fixnum-bits ()
  #+sbcl sb-vm:n-fixnum-bits
  #-sbcl (1+ (cl:floor (cl:log cl:most-positive-fixnum 2))))

;; ;;; get-hash-type looks like (cl:unsigned-byte (get-fixnum-bits)), no?
;; (defmacro get-hash-type-original ()
;;   #+sbcl
;;   ''(cl:unsigned-byte 62)
;;   #+allegro
;;   ''(cl:unsigned-byte 32)
;;   ;; https://github.com/Clozure/ccl/blob/ff51228259d9dbc8a9cc7bbb08858ef4aa9fe8d0/level-0/l0-hash.lisp#L1885
;;   #+ccl
;;   ''(cl:and cl:fixnum cl:unsigned-byte)
;;   ;; https://github.com/search?q=repo%3Aarmedbear%2Fabcl++sxhash&type=code
;;   #+abcl
;;   ''(cl:and cl:fixnum cl:unsigned-byte)
;;   ;; https://gitlab.com/embeddable-common-lisp/ecl/-/blob/develop/src/cmp/proclamations.lsp
;;   #+ecl
;;   `'(cl:unsigned-byte ,@(coalton-compatibility:get-fixnum-bits))
;;   #-(or sbcl allegro ccl abcl ecl)
;;   #.(cl:error "hashing is not supported on ~A" (cl:lisp-implementation-type)))
;; add 1 bit since we're using the sign bit as well (the hash is unsigned)
(defmacro get-hash-type ()
  `'(cl:unsigned-byte ,(coalton-compatibility:get-fixnum-bits)))

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

;;; Unset float traps (or sbcl allegro ccl abcl ecl clasp)
(defmacro unset-all-float-traps ()
  '(cl:eval-when (:compile-toplevel :load-toplevel :execute)
    #+ccl (ccl:set-fpu-mode :overflow nil :underflow nil :division-by-zero nil :invalid nil :inexact nil)
    #+sbcl (sb-int:set-floating-point-modes :traps nil)
    #+abcl (extensions:set-floating-point-modes :traps nil)
    #+ecl  (ext:trap-fpe 'cl:t nil)
    #+clasp (core:fe-disable-except (logior core:+fe-underflow+
                                            core:+fe-overflow+
                                            core:+fe-invalid+
                                            core:+fe-inexact+
                                            core:+fe-divbyzero+))
    #-(or sbcl allegro ccl abcl ecl clasp)
    #.(cl:error "don't know how to unset all float traps on ~A" (cl:lisp-implementation-type))
    ))

;;; Get bytes consed. (or sbcl abcl)
#+abcl
(defconstant the-com-sun-management-ThreadMXBean-interface
  (java:jstatic
   (java:jmethod "java.lang.Class" "forName" "java.lang.String")
   nil
   "com.sun.management.ThreadMXBean"))
#+abcl
(defconstant thePlatformMXBean
  (java:jstatic
   (java:jmethod "java.lang.management.ManagementFactory" "getPlatformMXBean"
                 "java.lang.Class")
   nil
   the-com-sun-management-ThreadMXBean-interface))
#+abcl
(defun getTotalThreadAllocatedBytes ()
  (java:jcall
   (java:jmethod "com.sun.management.ThreadMXBean" "getTotalThreadAllocatedBytes")
   thePlatformMXBean))
#+(or sbcl abcl)
(pushnew ':|COALTON:HAS-GET-BYTES-CONSED| cl:*features*)
(defun get-bytes-consed ()
  #+sbcl
  (sb-ext:get-bytes-consed)
  #+(and abcl)
  (getTotalThreadAllocatedBytes)
  #-(or sbcl abcl)
  0)

;;; Package locks (and (or sb-package-locks ecl clasp) (not coalton-without-package-locks))

#+ecl(require '#:package-locks)
(defmacro try-lock-package (the-package)
  #-(and (or sb-package-locks ecl clasp) (not coalton-without-package-locks))
  (declare (ignore the-package))
  #+(and sb-package-locks (not coalton-without-package-locks))
  `(sb-ext:lock-package ,the-package)
  #+(and (or ecl clasp) (not coalton-without-package-locks))
  `(ext:lock-package ,the-package))

(defmacro try-unlock-package (the-package)
  #-(and (or sb-package-locks ecl clasp) (not coalton-without-package-locks))
  (declare (ignore the-package))
  #+(and sb-package-locks (not coalton-without-package-locks))
  `(sb-ext:unlock-package ,the-package)
  #+(and (or ecl clasp) (not coalton-without-package-locks))
  `(ext:unlock-package ,the-package))

;;; Run tests in order of definition
;; Registry to maintain chronological order of definitions
(defvar *defined-tests* (make-hash-table :test #'equalp)
  "Maps a name to an ordered list of test symbols.")

;; Extract the original macro function and store it in a new symbol's
;; macro-function
(setf (macro-function 'original-fiasco-deftest)
      (macro-function 'fiasco:deftest))

(defun as-a-string (x)
  (cond
    ((stringp x) x)
    ((packagep x) (package-name x))
    ((symbolp x) (symbol-name x))
    (t (format nil "~A" x))))

(defmacro deftest-prelude (&whole whole name args &body body)
  (destructuring-bind (name &rest test-args &key (in nil in-provided?)
                                                 timeout &allow-other-keys)
      (if (listp name) name (list name))
    (let* ((pkg *package*)
           (key (as-a-string (if in-provided? in pkg)))
           (key-info (if in-provided?
                         (format nil ", targeting ~A (~A)" in key)
                         "")))
      #+nil
      (cl:format cl:*error-output*
                 "fiasco-deftest-wrapper called in package ~A for name ~A~A~%"
                 pkg name key-info)
      ;; in-what is ignored here - is this an issue?
      (pushnew name
               (gethash key coalton-compatibility::*defined-tests*)))))

;; Redefine the original symbol's macro-function to a wrapper expander
(setf (macro-function 'fiasco:deftest)
      ;; (macro-function 'my-deftest)
      (lambda (form env)
        (let* ((prelude-code (funcall
                              (macro-function 'deftest-prelude)
                              form env))
               (expanded-code (funcall
                               (macro-function 'original-fiasco-deftest)
                               form env)))
          `(progn
             ,@expanded-code))))

(defun get-ordered-tests (package)
  "Retains the chronological push order by reversing the accumulated list."
  (let ((key (as-a-string package)))
    (reverse (gethash key *defined-tests*))))

(defun run-ordered-package-tests
    (&key
       (package *package* package-supplied-p)
       (packages (list *package*) packages-supplied-p)
       (describe-failures t)
       verbose
       (stream *standard-output*)
       interactive)
  "Equivalent to fiasco:run-package-tests, but executes tests chronologically.
Fiasco doc:
Execute test suite(s) associated with PACKAGE or PACKAGES.

PACKAGE defaults to the current package. Don't supply both both
PACKAGE and PACKAGES.

See RUN-TESTS for the meaning of the remaining keyword arguments."
  (assert (not (and packages-supplied-p package-supplied-p))
          nil
          "Supply either :PACKAGE or :PACKAGES, not both")
  #+nil
  (progn
    (format *error-output* "coalton-compatibility::*defined-tests* contains:~%")
    (maphash (lambda (key value)
               (format *error-output* "~A => ~A~%" key value))
             *defined-tests*))
  (let* ((all-packages (if packages-supplied-p packages (list package)))
         (all-results
           (mapcar
            #'(lambda (pkg)
                (let ((target-package (as-a-string pkg))
                      ;; Extract the chronologically defined tests
                      ;; from our tracking registry
                      (ordered-tests (get-ordered-tests pkg)))
                  (if (null ordered-tests)
                      (progn
                        (format *error-output*
                                "No ordered tests registered for package ~A.~%"
                                target-package)
                        (list t nil))
                      (progn
                        (format *error-output*
                                "Running ordered tests for package ~A:~%~A"
                                target-package ordered-tests)
                        ;; Pass the ordered list of test symbols
                        ;; directly to Fiasco's core runner
                        (multiple-value-call #'list
                          (fiasco:run-tests ordered-tests
                                            :describe-failures describe-failures
                                            :verbose verbose
                                            :stream stream
                                            :interactive interactive))))))
            all-packages)))
    (values (and (mapcar #'car all-results))
            all-results)))

;; Extract the original function object and store it in a new symbol
(setf (symbol-function 'original-fiasco-run-package-tests)
      (symbol-function 'fiasco:run-package-tests))

;; Redefine the original symbol to refer to the replacement function
(setf (symbol-function 'fiasco:run-package-tests)
      (symbol-function 'run-ordered-package-tests))
