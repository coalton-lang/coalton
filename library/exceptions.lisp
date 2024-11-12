(coalton-library/utils:defstdlib-package #:coalton-library/exceptions
  (:use
   #:coalton
   #:coalton-library/classes
   #:coalton-impl/runtime/exceptions)
  (:export
   #:raise))

(in-package #:coalton-library/exceptions)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)


(coalton-toplevel
  (declare raise ((Exception :e) => :e -> :a))
  (define (raise exc)
    (let ((erase (fn () (ErasedException (fn () (into exc))))))
      (lisp :a (exc erase)
	(cl:signal 'exception-condition
		   :datum exc
		   :erase erase)))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/EXCEPTIONS")
