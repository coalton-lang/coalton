(defpackage #:coalton-benchmarking/benchmarking
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-benchmarking/printing)
  (:local-nicknames
   (#:vec   #:coalton-library/vector)
   (#:cell  #:coalton-library/cell)
   (#:hash  #:coalton-library/hashtable)
   (#:iter  #:coalton-library/iterator)
   (#:sys   #:coalton-library/system)
   (#:list  #:coalton-library/list)
   (#:state #:coalton-library/monad/state)
   (#:math  #:coalton-library/math)
   (#:seq   #:coalton-library/seq)
   (#:str   #:coalton-library/string))
  (:export

   ;; settings/options
   #:*verbose-benchmarking*
   #:verbose?
   #:*benchmark-width*
   #:benchmark-width
   #:*benchmark-sci-notation*
   #:sci-notation?

   #:BenchmarkName

   #:Benchmark
   #:BenchmarkSuite
   #:add-benchmark-suite
   #:find-benchmark-suite
   #:current-package
   #:ensure-benchmark-suite
   #:add-benchmark
   #:package-benchmarks
   #:local-benchmarks
   #:find-benchmark
   #:define-benchmark

   #:ScalingBenchmark
   #:define-scaling-benchmark

   #:BenchmarkResults
   #:BenchmarkSystem
   #:benchmark-system-info
   #:PackageBenchmarkResults

   #:run-benchmark
   #:run-package-benchmarks
   #:run-benchmarks))

(in-package #:coalton-benchmarking/benchmarking)

;;;
;;; Settings/options
;;;

(coalton-toplevel

  (declare *verbose-benchmarking* (Cell Boolean))
  (define *verbose-benchmarking*
    "When true, benchmarks will print to the repl in addition to returning a BenchmarkResults object."
    (cell:new True))

  (declare verbose? (Unit -> Boolean))
  (define (verbose?)
    "Should benchmarks print to the repl?

Is `*verbose-benchmarking*` set to `True`?"
    (cell:read *verbose-benchmarking*))

  (declare *benchmark-width* (Cell UFix))
  (define *benchmark-width*
    "This is the printed width of the benchmark table output."
    (cell:new 90))

  (declare benchmark-width (Unit -> UFix))
  (define (benchmark-width)
    "Returns the width in characters for printing benchmark table output."
    (cell:read *benchmark-width*))

  (declare *benchmark-sci-notation* (Cell Boolean))
  (define *benchmark-sci-notation*
    "When `True`, benchmarks will print times with scientific notation.

When `False`, they will print in microseconds."
    (cell:new False))

  (declare sci-notation? (Unit -> Boolean))
  (define (sci-notation?)
    "Should benchmark times be printed in scientific notation?

Is `*benchmark-sci-notation*` set to `True`?"
    (cell:read *benchmark-sci-notation*)))


;;;
;;; BenchmarkName type for handling package symbols
;;;

(coalton-toplevel

  (repr :native cl:symbol)
  (define-type BenchmarkName
    "Benchmark names are interned as symbols in their associated package.")

  (define-instance (EQ BenchmarkName)
    (define (== a b)
      (lisp Boolean (a b)
        (cl:eq a b))))

  (define-instance (Into BenchmarkName String)
    (define (into s)
      (lisp String (s)
        (cl:string s))))

  (define-instance (Into String BenchmarkName)
    (define (into s)
      (lisp BenchmarkName (s)
        (cl:intern s))))

  (declare BenchmarkName (String -> BenchmarkName))
  (define (BenchmarkName str)
    "A constructor that takes a string and returns a BenchmarkName symbol."
    (into str)))

(coalton-library/hash:define-sxhash-hasher BenchmarkName)

;;;
;;; Benchmark, BenchmarkSuite, and benchmark environment
;;;

(coalton-toplevel

  (define-struct Benchmark
    "A Coalton `Benchmark` object."
    (name
     "The name of the `Benchmark`, interned as a symbol in the local package."
     BenchmarkName)
    (samples
     "The number of times the code will be run."
     UFix)
    (code
     "A function to be benchmarked."
     (Unit -> Unit))
    (Comprehensive?
     "Should this benchmark be run comprehensively?

Comprehensive benchmarks provide additional information and dynamic sample scaling, though sacrifice time efficiency."
     Boolean))

  (define-struct BenchmarkSuite
    "A suite of benchmarks, associated with a Coalton package."
    (package-name
     "The name of the package associated with the `BenchmarkSuite`." String)
    (benchmarks
     "The benchmarks contained in the `BenchmarkSuite`."
     (Hashtable BenchmarkName Benchmark)))

  (declare *benchmark-environment* (hash:Hashtable String BenchmarkSuite))
  (define *benchmark-environment*
    "A global environment holding Coalton benchmark suites.

Key is package name."
    (hash:new))

  (declare add-benchmark-suite (BenchmarkSuite -> Unit))
  (define (add-benchmark-suite suite)
    "Adds a `BenchmarkSuite` to `*benchmark-environment*`."
    (hash:set! *benchmark-environment*
               (.package-name suite)
               suite))

  (declare find-benchmark-suite (String -> (Optional BenchmarkSuite)))
  (define (find-benchmark-suite name)
    "Finds a `BenchmarkSuite` given the package's name."
    (let package = (lisp String (name)
                     (cl:string-upcase name)))
    (hash:get *benchmark-environment* package))

  (declare current-package (Unit -> String))
  (define (current-package)
    "Returns the current local package, `cl:*package*`"
    (lisp String ()
      (cl:package-name cl:*package*)))

  (declare ensure-benchmark-suite (Unit -> BenchmarkSuite))
  (define (ensure-benchmark-suite)
    "Ensures that a local `BenchmarkSuite` exists for the current package, returns the suite."
    (unwrap-or-else (fn (suite)
                      suite)
                    (fn ()
                      (let ((suite (BenchmarkSuite
                                    (current-package)
                                    (hash:new))))
                        (add-benchmark-suite suite)
                        suite))
                    (find-benchmark-suite (current-package))))

  (declare add-benchmark (Benchmark -> Unit))
  (define (add-benchmark bmark)
    "Adds a `Benchmark` to the current package's `BenchmarkSuite`."
    (let suite = (ensure-benchmark-suite))
    (hash:set! (.benchmarks suite)
               (.name bmark)
               bmark))

  (declare package-benchmarks (String -> (Iterator Benchmark)))
  (define (package-benchmarks package-name)
    "Returns an `Iterator` of all benchmarks contained within a specified package."
    (unwrap-or-else
       (fn (suite)
         (hash:values (.benchmarks suite)))
       (fn ()
         (error "Benchmark suite not found."))
       (find-benchmark-suite package-name)))

  (declare local-benchmarks (Unit -> (Iterator Benchmark)))
  (define (local-benchmarks)
    "Returns an `Iterator` of all benchmarks contained within the current package."
    (package-benchmarks (current-package)))

  (declare find-benchmark (BenchmarkName -> (Optional Benchmark)))
  (define (find-benchmark name)
    "Finds a `Benchmark` in the current package."
    (iter:find! (fn (b)
                  (== (.name b) name))
                (local-benchmarks)))

  (declare %define-benchmark (String -> UFix -> (Unit -> Unit) -> Boolean -> Unit))
  (define (%define-benchmark name iterations fn comprehensive?)
    "Define a Coalton `Benchmark` in the local package."
    (add-benchmark
     (Benchmark
      (into name)
      iterations
      fn
      comprehensive?))))

(cl:defmacro define-benchmark (name iterations func cl:&key (comprehensive? cl:nil))
  "Define a Coalton `Benchmark` in the local package- called outside of Coalton."
  (cl:let* ((name (cl:string name)))
    `(coalton (%define-benchmark ,name ,iterations ,func (lisp Boolean () ,comprehensive?)))))

;;;
;;;
;;;

(coalton-toplevel

  (define-struct (ScalingBenchmark :a)
    "Scaling benchmarks allow for multiple inputs."
    (name
     "The name of the benchmark. This will be combined with an index for each element in the series."
     String)
    (samples
     "The number of samples to take of each benchmark.

If comprehensive, this is the minimum number of samples."
     UFix)
    (code
     "The function to benchmark."
     (:a -> Unit))
    (inputs
     "A sequence of inputs to run this benchmark on."
     (seq:Seq :a))
    (Comprehensive?
     "Should this be run comprehensively?"
     Boolean))

  (define (%add-scaling-benchmark (ScalingBenchmark name samples func inputs comprehensive?))
    "Adds a benchmark for each ScalingBenchmark input."
    (for i in (iter:zip! (iter:into-iter inputs)
                         (iter:range-increasing 1 0 (seq:size inputs)))
      (add-benchmark
       (Benchmark
        (BenchmarkName (str:concat name
                                   (into (snd i))))
        samples
        (fn () (func (fst i)))
        comprehensive?)))))

(cl:defmacro define-scaling-benchmark (name samples func inputs cl:&key (comprehensive? cl:nil))
  "Defines a series of benchmarks on a function taking a series of inputs."
  (cl:let ((name (cl:string name)))
    `(coalton (%add-scaling-benchmark
               (ScalingBenchmark
                ,name
                ,samples
                ,func
                ,inputs
                (lisp Boolean () ,comprehensive?))))))
;;;
;;; Benchmark Results
;;;

(coalton-toplevel

  (define-struct BenchmarkResults
    "Results from a `Benchmark` run."
    (name
     "The name of the `Benchmark`, interned as a symbol in the local package."
     BenchmarkName)
    (iterations
     "The number of times the benchmarked function was run."
     UFix)
    (total-time-elapsed
     "The amount of time in internal time units that all iterations took to run."
     Integer)
    (average-time-elapsed
     "The amount of time in internal time units that the average iteration took to run."
     Integer)
    (time-std-dev
     "The standard deviation of times."
     (Optional Double-Float))
    (bytes-consed
     "The amount of space used during the benchmark run."
     (Optional Integer)))

  (define-struct BenchmarkSystem
    "Information about the system the benchmark is run on."
    (architecture
     "The architecture of the system."
     String)
    (OS
     "The operating system."
     String)
    (lisp-impl
     "The Lisp implementation used."
     String)
    (lisp-version
     "The version of the Lisp Implementation"
     String)
    (release?
     "Is this in release mode or development mode?"
     Boolean)
    (inlining?
     "Is inlining enabled?"
     Boolean))

  (declare benchmark-system-info (Unit -> BenchmarkSystem))
  (define (benchmark-system-info)
    "This gathers information about the system the benchmark is run on."
    (BenchmarkSystem
     (sys:architecture)
     (sys:os)
     (sys:implementation)
     (sys:lisp-version)
     (lisp Boolean ()
       (cl:if (cl:member 'coalton-release cl:*features*)
              cl:t
              cl:nil))
     (lisp Boolean ()
       coalton-impl/settings:*coalton-heuristic-inlining*)))

  (define-struct PackageBenchmarkResults
    "This is information about a run of package benchmarks."
    (package-name
     "The name of the package containing the benchmark suite." String)
    (system
     "Information about the system the benchmark was run on."
     BenchmarkSystem)
    (Results
     "The results of each benchmark."
     (vector BenchmarkResults))))

;;;
;;; Print formatting utilities
;;;

(coalton-toplevel

  (declare print-item ((Into :a String) => :a -> Unit))
  (define (print-item item)
    "Equivalent to Coalton's `print` function except without a trailing newline."
    (let str = (as String item))
    (lisp Unit (str)
      (cl:format cl:*standard-output* "~A" str)
      Unit))

  (define (%format-time-microseconds rtime)
    "Formats time units into microseconds."
    (let t = (math:round/ (sys:time-units->rounded-microseconds rtime) 1000))
    (lisp String (t)
      (cl:format cl:nil "~d" t)))

  (define (%format-time-scientific rtime)
    "Formats time units into seconds in scientific notation."
    (let t = (sys:time-units->seconds rtime))
    (lisp String (t)
      (cl:format cl:nil "~,4e" t)))

  (declare format-time (Integer -> String))
  (define (format-time rtime)
    "Converts time from microseconds to seconds then prunes down to a 10 characters."
    (if (sci-notation?)
        (%format-time-scientific rtime)
        (%format-time-microseconds rtime)))

  (declare %format-time-std-dev (Double-Float -> String))
  (define (%format-time-std-dev sdev)
    (let ms = (/ (/ (* 1000000 sdev)
                    (fromint sys:internal-time-units-per-second))
                 1000))
    (into ms))

  (declare system-header-text (BenchmarkSystem -> (Tuple TableCell TableCell)))
  (define (system-header-text (BenchmarkSystem architecture os lisp-impl lisp-version release inlining))
    "Returns formatted system information for printing purposes."
    (Tuple (TableCell (lisp String (architecture os lisp-impl lisp-version)
                        (cl:format cl:nil "System: ~a ~a ~a~a"
                                   architecture
                                   os
                                   lisp-impl
                                   lisp-version))
                      Center)
           (TableCell (lisp String (release inlining)
                        (cl:format cl:nil "Coalton ~a mode ~a heuristic inlining"
                                   (cl:if release
                                          "release"
                                          "development")
                                   (cl:if inlining
                                          "with"
                                          "without")))
                      Center)))

  (declare benchmark-column-names (seq:Seq TableCell))
  (define benchmark-column-names
    "The column headers for benchmark table printing."
    (seq:make (TableCell "Benchmark" Center)
              (TableCell "Time (ms)" Center)
              (TableCell "Avg Time (ms)" Center)
              (TableCell "Time std dev" Center)
              (TableCell "Space (B)" Center)
              (TableCell "# Samples" Center)))

  (declare column-values (BenchmarkResults -> (seq:Seq TableCell)))
  (define (column-values (BenchmarkResults name iterations time-elapsed avg-time time-std-dev bytes-consed))
    "Returns the column values for a row of the benchmark table."
    (seq:make (TableCell (the String (into name)) Left)
              (TableCell (format-time time-elapsed) Right)
              (TableCell (format-time avg-time) Right)
              (TableCell (unwrap-or-else %format-time-std-dev
                                         (fn () "n/a")
                                         time-std-dev)
                         Right)
              (TableCell (unwrap-or-else (fn (x)
                                           (into x))
                                         (fn () "n/a")
                                         bytes-consed)
                         Right)
              (TableCell (the String (into iterations))
                         Right)))

  (declare package-header (String -> BenchmarkSystem -> String))
  (define (package-header name system)
    "Returns a formatted package header, including package and system information."
    (let sys = (system-header-text system))
    (coalton-table
     (benchmark-width)
     (Header (lisp String (name)
               (cl:format cl:nil "Package '~a'" name)))
     (SecondaryHeader (fst sys))
     (SecondaryHeader (snd sys))
     (TopRow benchmark-column-names))))

;;;
;;; Comprehensive Benchmarking
;;;

(coalton-toplevel

  (define *benchmarking-convergence-threshold* 0.01d0)

  (define-struct BenchState
    "State for a Benchmark Run."
    (samples           (Cell Integer))
    (times             (Vector Integer))
    (total-time        (Cell Integer))
    (bytes-consed      (Vector (Optional Integer)))
    (std-dev           (Cell Double-Float)))

  (define-instance (Default BenchState)
    (define (default)
      (BenchState (cell:new 0)
                  (vec:new)
                  (cell:new 0)
                  (vec:new)
                  (cell:new 0.0d0))))

  (declare increment-samples (Unit -> (state:ST BenchState Unit)))
  (define (increment-samples)
    "Increment the number of samples."
    (do
     (bench <- state:get)
     (pure (cell:increment! (.samples bench)))
      (state:put bench)))

  (declare add-time (Integer -> (state:ST BenchState Unit)))
  (define (add-time t)
    "Add a profiled time to both the vector of times and the total-time."
    (do
     (bench <- state:get)
     (pure (vec:push! t (.times bench)))
      (pure (cell:update! (fn (x) (+ t x)) (.total-time bench)))
      (state:put bench)))

  (declare add-bytes ((Optional Integer) -> (state:ST BenchState Unit)))
  (define (add-bytes b)
    (do
     (bench <- state:get)
     (pure (vec:push! b (.bytes-consed bench)))
      (state:put bench)))

  (declare mean-time (Unit -> (state:ST BenchState Double-Float)))
  (define (mean-time)
    (do
     (bench <- state:get)
     (pure (math:inexact/
            (cell:read (.total-time bench))
            (cell:read (.samples bench))))))

  (declare total-bytes-consed (BenchState -> (Optional Integer)))
  (define (total-bytes-consed (BenchState _ _ _ bytes-consed _))
    (fold + (Some 0) bytes-consed))

  (declare mean-bytes-consed (BenchState -> (Optional Double-Float)))
  (define (mean-bytes-consed (BenchState samples _ _ bytes-consed _))
    (match (vec:index-unsafe 0 bytes-consed)
      ((Some _x)
       (Some (math:inexact/ (fold + 0 (map unwrap bytes-consed))
                            (cell:read samples))))
      ((None)
       None)))

  (declare std-deviation-time (Unit -> (state:ST BenchState Double-Float)))
  (define (std-deviation-time)
    (do
     (bench <- state:get)
     (mean <- (mean-time))
      (pure (math:sqrt (math:general/ (fold + 0 (map (fn (x)
                                                       (^ (- (fromint x) mean) 2))
                                                     (.times bench)))
                                      (1- (fromint (cell:read (.samples bench)))))))))

  (declare std-deviation-convergence? (Double-Float -> (state:ST BenchState Boolean)))
  (define (std-deviation-convergence? new-std-dev)
    (do
     (bench <- state:get)
     (pure (< (abs (- new-std-dev (cell:read (.std-dev bench)))) *benchmarking-convergence-threshold*))))

  (declare update-std-deviation (Double-Float -> (state:ST BenchState Unit)))
  (define (update-std-deviation new-std-dev)
    (do
     (bench <- state:get)
     (pure (cell:write! (.std-dev bench) new-std-dev))
      (state:put bench)))

  (declare profile-benchmark ((Unit -> Unit) -> Integer -> (state:ST BenchState Unit)))
  (define (profile-benchmark func min-samples)
    (do
     (let profile = (sys:spacetime func))
     (bench <- state:get)
      (increment-samples)
      (add-time (.time-elapsed profile))
      (add-bytes (.bytes-consed profile))
      (cond ((> (cell:read (.samples bench)) min-samples)
             (do
              (std-dev <- (std-deviation-time))
              (converges? <- (std-deviation-convergence? std-dev))
               (if converges?
                   (pure Unit)
                   (do
                    (update-std-deviation std-dev)
                    (profile-benchmark func min-samples)))))
            (True
             (profile-benchmark func min-samples)))))

  (declare %run-comprehensive-benchmark (Benchmark -> BenchmarkResults))
  (define (%run-comprehensive-benchmark (Benchmark name samples code _))
    (let results = (fst (state:run (profile-benchmark code (into samples)) (default))))
    results

    (BenchmarkResults
     name
     (unwrap (tryinto (cell:read (.samples results))))
     (cell:read (.total-time results))
     (math:round/ (cell:read (.total-time results))
                  (cell:read (.samples results)))
     (Some (cell:read (.std-dev results)))
     (total-bytes-consed results))))


;;;
;;; Running benchmarks
;;;

(Coalton-toplevel

  (declare %run-benchmark (Benchmark -> BenchmarkResults))
  (define (%run-benchmark (Benchmark name iterations func _))
    "Runs a `Benchmark`."
    (let profile = (sys:spacetime (fn ()
                                    (for i in (iter:up-to iterations)
                                      (func)
                                      Unit))))
    (BenchmarkResults
     name
     (into iterations)
     (.time-elapsed profile)
     (math:round/ (.time-elapsed profile)
                  (into iterations))
     None
     (.bytes-consed profile)))

  (declare run-benchmark (BenchmarkName -> BenchmarkResults))
  (define (run-benchmark name)
    "Runs a `Benchmark` in the current package."
    (let ((results (unwrap-or-else (fn (b)
                                     (if (.comprehensive? b)
                                         (%run-comprehensive-benchmark b)
                                         (%run-benchmark b)))
                                   (fn () (error (lisp String (name)
                                                   (cl:format cl:nil "No benchmark defined by this name: ~a" name))))
                                   (find-benchmark name)))
          (sys (system-header-text (benchmark-system-info))))
      (when (verbose?)
        (print
         (coalton-table
          (benchmark-width)
          (Header (lisp String (name) (cl:format cl:nil "Benchmark ~a" name)))
          (SecondaryHeader (fst sys))
          (SecondaryHeader (snd sys))
          (TopRow benchmark-column-names)
          (Row (column-values results))
          (Bottom (seq:size benchmark-column-names)))))
      results))

  (declare run-package-benchmarks (String -> PackageBenchmarkResults))
  (define (run-package-benchmarks name)
    "Runs all benchmarks for a package"
    (let system = (benchmark-system-info))
    (let results = (vec:new))
    (when (verbose?)
      (print-item (package-header name system)))

    (for b in (package-benchmarks name)
      (let res = (if (.comprehensive? b)
                     (%run-comprehensive-benchmark b)
                     (%run-benchmark b)))
      (when (verbose?)
        (print-item (coalton-table
                     (benchmark-width)
                     (Row (column-values res)))))
      (vec:push! res results))

    (when (verbose?)
      (print-item (coalton-table
                   (benchmark-width)
                   (Bottom 4))))

    (PackageBenchmarkResults
     name
     system
     results))

  (define (run-benchmarks)
    "Runs the benchmarks for the current package."
    (run-package-benchmarks (current-package)))

  (declare print-results ((List BenchmarkResults) -> (state:ST TableState Unit)))
  (define (print-results results)
    "Adds results to the table printout."
    (match results
      ((Cons x xs)
       (do
        (Row (column-values x))
        (print-results xs)))
      ((Nil) (pure Unit))))

  (define-instance (Into PackageBenchmarkResults String)
    (define (into (PackageBenchmarkResults name system results))
      (let sys = (system-header-text system))
      (coalton-table (benchmark-width)
                     (Header (lisp String (name)
                               (cl:format cl:nil "Package '~a'" name)))
                     (SecondaryHeader (fst sys))
                     (SecondaryHeader (snd sys))
                     (TopRow benchmark-column-names)
                     (print-results (into results))
                     (Bottom 6)))))
