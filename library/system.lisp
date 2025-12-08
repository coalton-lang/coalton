(coalton-library/utils:defstdlib-package #:coalton-library/system
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-compatibility-layer)
  (:local-nicknames
   (#:math #:coalton-library/math)
   (#:compat #:coalton-compatibility-layer))
  (:export
   #:gc
   #:sleep)
  (:export
   #:get-real-time
   #:internal-time-units-per-second
   #:time-units->seconds
   #:time-units->rounded-microseconds
   #:monotonic-bytes-consed

   #:time
   #:space

   #:MeteredResult
   #:spacetime)
  (:export

   #:LispCondition

   #:getenv
   #:setenv!
   
   #:architecture
   #:os
   #:hostname
   #:implementation
   #:lisp-version
   #:features
   #:add-feature
   
   #:cmd-args
   #:argv0))

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

  (declare sleep ((math:Rational :a) => :a -> Unit))
  (define (sleep n)
    "Sleep for `n` seconds, where `n` can be of any type with an instance of `Rational`.

Sleep uses type class `Rational`'s `best-approx` instead of `Real`'s `real-approx` because it handles the approximation without arbitrary precision. The only `Real` type excluded by this decision is `CReal`."
    (if (math:negative? n)
        (error "sleep must be a nonnegative number.")
        (let ((frac (math:best-approx n)))
          (lisp Unit (frac)
            (cl:sleep frac)
            Unit)))))

;;;
;;; Pofiling
;;;

(coalton-toplevel

  (declare get-run-time (Unit -> Integer))
  (define (get-run-time)
    "Gets the run-time in internal time units. This is implementation specific: it may measure real time, run time, CPU cycles, or some other quantity.

The difference between two successive calls to this function represents quantity accumulated during that period of time.

This function is not exported as its output is too implementation specific."
    (lisp Integer ()
      (cl:get-internal-run-time)))

  (declare get-real-time (Unit -> Integer))
  (define (get-real-time)
    "Gets the real-time in internal time units. The difference between two successive calls to this function represents the time that has elapsed."
    (lisp Integer ()
      (cl:get-internal-real-time)))

  (declare internal-time-units-per-second Integer)
  (define internal-time-units-per-second
    "The number of internal time units per second. This is implementation specific."
    (lisp Integer ()
      cl:internal-time-units-per-second))

  (declare time-units->seconds (Integer -> Fraction))
  (define (time-units->seconds t)
    "Converts internal time units into `Fraction` seconds."
    (math:exact/ t internal-time-units-per-second))

  (declare time-units->rounded-microseconds (Integer -> Integer))
  (define (time-units->rounded-microseconds t)
    "Converts internal time units into an integer number of rounded microseconds."
    (math:round/ (* 1000000 t)
                 internal-time-units-per-second))

  (declare monotonic-bytes-consed (Unit -> (Optional Integer)))
  (define (monotonic-bytes-consed)
    "Returns the number of bytes consed since some unspecified point in time.

The difference between two successive calls to this function represents the number of bytes consed in that period of time."
    #+sbcl
    (Some (lisp Integer ()
            (sb-ext:get-bytes-consed)))
    #-sbcl
    None)

  ;;;
  ;;; Function instrumentation
  ;;;

  (declare time ((Unit -> :a) -> (Tuple :a Integer)))
  (define (time f)
    "Run the thunk `f` and return a tuple containing its value along with the run time in microseconds.

While the result will always contain microseconds, some implementations may return a value rounded to less precision (e.g., rounded to the nearest second or millisecond)."
    (let start = (get-real-time))
    (let value = (f))
    (let end   = (get-real-time))
    (Tuple value (time-units->rounded-microseconds (- end start))))

  (declare space ((Unit -> :a) -> (Tuple :a (Optional Integer))))
  (define (space f)
    "Run the thunk `f` and return a tuple containing its value along with the approximate number of bytes consed during the course of executing f.

The amount of space used may be peculiar to the implementation, such as rounding to certain page boundaries.

A garbage collection will be forced prior to invoking `f`."
    (gc)
    (let start = (monotonic-bytes-consed))
    (let value = (f))
    (let end   = (monotonic-bytes-consed))
    (Tuple value (liftA2 - end start)))

  (define-struct (MeteredResult :a)
    "Function output with space and timing metedata."
    (result
     "The result of the function." :a)
    (time-elapsed
     "The real time elapsed running the function (in internal time units)." Integer)
    (bytes-consed
     "The number of bytes consed during the run." (Optional Integer)))

  (declare spacetime ((Unit -> :a) -> (MeteredResult :a)))
  (define (spacetime f)
    "Runs a function, gathering space and timing information and returning a `MeteredResults` object.

Garbage collection will be performed before profiling is performed."
    (gc)
    ;; The order of these bindings ensures that slight inaccuracy of
    ;; the measurements is shared across both bytes consed and
    ;; elapsed time.
    (let start-bytes-consed = (monotonic-bytes-consed))
    (let start-real-time    = (get-real-time))
    (let value              = (f))
    (let end-bytes-consed   = (monotonic-bytes-consed))
    (let end-real-time      = (get-real-time))
    (MeteredResult
     value
     (- end-real-time start-real-time)
     (liftA2 - end-bytes-consed start-bytes-consed))))


;;;
;;; Gathering System information
;;;

(coalton-toplevel

  (repr :native cl:condition)
  (define-type LispCondition
    "Condition for lisp error handling. Uses `cl:condition`.")

  (define-instance (Signalable LispCondition)
    (define (error condition)
      (lisp :a (condition)
        (cl:error condition))))

  ;;
  ;; Accessing Environment Variables
  ;;
  
  (declare getenv (String -> (Optional String)))
  (define (getenv var)
    "Gets the value of the environmental variable `var`, errors if `var` doesn't exist."
    (lisp (Optional String) (var)
      (cl:let ((env (uiop:getenvp var)))
        (cl:if env
               (Some env)
               None))))

  
  (declare setenv! (String -> String -> Unit))
  (define (setenv! var val)
    "Sets an environment variable `var` to string `val`, only if `var` already exists."
    (lisp Unit (var val)
      (cl:setf (uiop:getenv var) val)
      Unit))

  ;;
  ;; Typical Environment/System variables
  ;;
  
  (declare architecture (Unit -> String))
  (define (architecture)
    "The system's architecture (stored at compile time)."
    (lisp String ()
      (cl:string (uiop:architecture))))

  (declare os (Unit -> String))
  (define (os)
    "The system's operating system (stored at compile time)."
    (lisp String ()
      (cl:string (uiop:detect-os))))

  (declare hostname (Unit -> String))
  (define (hostname)
    "Returns the system's hostname. This is a function because the hostname can be redefined."
    (lisp String ()
      (uiop:hostname)))

  (declare implementation (Unit -> String))
  (define (implementation)
    "The lisp implementation (stored at compile time)."
    (lisp String ()
      (cl:string (uiop:implementation-type))))

  (declare lisp-version (Unit -> String))
  (define (lisp-version)
    "The lisp implementation version (stored at compile time)."
    (lisp String ()
      (uiop:lisp-version-string)))

  (declare features (Unit -> (List String)))
  (define (features)
    "Returns a list of active features, from `cl:*features*`."
    (lisp (list String) ()
      (cl:mapcar #'cl:symbol-name cl:*features*)))

  (declare add-feature (String -> Unit))
  (define (add-feature feat)
    "Adds a feature `feat` to `cl:*features*`."
    (lisp Boolean (feat)
      (cl:push (cl:intern feat "KEYWORD")
               cl:*features*)
      cl:t)
    Unit)

  ;;
  ;; Command line arguments
  ;;
  
  (declare cmd-args (Unit -> (List String)))
  (define (cmd-args)
    "The current command line arguments (stored at compile time)."
    (lisp (List String) ()
      (uiop:command-line-arguments)))

  (declare argv0 (Unit -> (Optional String)))
  (define (argv0)
    "The first command line argument (stored at compile time)."
    (lisp (Optional String) ()
      (cl:let ((arg (uiop:argv0)))
        (cl:if arg
               (Some (uiop:argv0))
               None)))))

(compat:try-lock-package "COALTON-LIBRARY/SYSTEM")
