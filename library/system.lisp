(coalton-library/utils:defstdlib-package #:coalton-library/system
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:export
   #:gc
   #:time
   #:sleep))

(in-package #:coalton-library/system)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel
  (declare gc (Unit -> Unit))
  (define (gc _)
    "Perform a full garbage collection."
    (lisp Unit ()
      (trivial-garbage:gc :full cl:t)
      Unit))

  (declare time ((Unit -> :a) -> (Tuple :a Integer)))
  (define (time f)
    "Run the thunk `f` and return a tuple containing its value along with the run time in microseconds.

While the result will always contain microseconds, some implementations may return a value rounded to less precision (e.g., rounded to the nearest second or millisecond)."
    (let start = (lisp Integer () (cl:get-internal-run-time)))
    (let value = (f))
    (let end   = (lisp Integer () (cl:get-internal-run-time)))
    (Tuple value
           (lisp Integer (start end)
             (cl:values
              (cl:round
               (cl:* 1000000 (cl:- end start))
               cl:internal-time-units-per-second)))))

  (declare sleep (Integer -> Unit))
  (define (sleep n)
    "Sleep for `n` seconds."
    (lisp Unit (n)
      (cl:sleep n)
      Unit)))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/SYSTEM")
