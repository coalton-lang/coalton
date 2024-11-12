(coalton-library/utils:defstdlib-package #:coalton-library/math/define-byte-types
  (:use
   #:coalton
   #:coalton-library/classes)
  (:import-from
   #:coalton-library/hash
   #:define-sxhash-hasher)
  (:local-nicknames
   (#:num #:coalton-library/math/num)
   (#:integral #:coalton-library/math/integral))
  (:export
   #:define-signed-byte-type
   #:define-unsigned-byte-type))

(in-package #:coalton-library/math/define-byte-types)

(named-readtables:in-readtable coalton:coalton)

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:defmacro define-unsigned-byte-type (byte-size)
    "This macro defines an unsigned byte type of a given size.

The class instances included are:
(Into <type> Integer), Eq, Ord, Num, Bits, Default, Hash, Integral.

Any conversion instances besides to or from Integer must be handled manually."

    (cl:let ((type (cl:intern (cl:format cl:nil "U~D" byte-size))))
      `(coalton-toplevel
         (repr :native (cl:unsigned-byte ,byte-size))
         (define-type ,type)
         (define-instance (Into ,type Integer)
           (define (into x)
             (lisp Integer (x)
               x)))
         (num::define-eq ,type)
         (num::define-ord ,type)
         (num::define-num-wrapping ,type ,byte-size)
         (num::define-bits-wrapping ,type ,byte-size)
         (num::define-default-num ,type)
         (num::define-sxhash-hasher ,type)
         (integral::%define-integral-native ,type cl:nil)))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:defmacro define-signed-byte-type (byte-size)
    "This macro defines a signed byte type of a given size.

The class instances included are:
(Into <type> Integer), Eq, Ord, Num, Bits, Default, Hash, Integral.

Any conversion instances besides to or from Integer must be handled manually."
    (cl:let ((type (cl:intern (cl:format cl:nil "I~D" byte-size)))
             (handler-name (cl:intern (cl:format cl:nil "%handle-~abit-overflow" byte-size))))
      `(cl:progn
         (num::%define-overflow-handler ,handler-name ,byte-size)
         (coalton-toplevel
           (repr :native (cl:signed-byte ,byte-size))
           (define-type ,type)
           (define-instance (Into ,type Integer)
             (define (into x)
               (lisp Integer (x)
                 x)))
           (num::define-eq ,type)
           (num::define-ord ,type)

           (num::define-num-checked ,type ,handler-name)
           (num::define-bits-checked ,type ,handler-name)
           (num::define-default-num ,type)
           (num::define-sxhash-hasher ,type)
           (integral::%define-integral-native ,type cl:nil))))))
