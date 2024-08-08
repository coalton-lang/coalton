(coalton-library/utils:defstdlib-package #:coalton-library/system
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:export
   #:gc
   #:time
   #:sleep)
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

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/SYSTEM")
