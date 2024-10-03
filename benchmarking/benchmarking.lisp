(defpackage #:coalton-benchmarking/benchmarking
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-benchmarking/printing)
  (:local-nicknames
   (#:vec #:coalton-library/vector)
   (#:cell #:coalton-library/cell)
   (#:hash #:coalton-library/hashtable)
   (#:iter #:coalton-library/iterator)
   (#:sys #:coalton-library/system)
   (#:list #:coalton-library/list)
   (#:state #:coalton-library/monad/state))
  (:export
   #:Benchmark
   #:BenchmarkResults
   #:PackageBenchmarkResults

   #:define-benchmark
   #:find-benchmark
   #:find-package-benchmarks
   #:run-benchmark
   #:run-package-benchmarks

   #:import-benchmarks
   #:reexport-benchmarks))

(in-package #:coalton-benchmarking/benchmarking)

(cl:defvar *coalton-verbose-benchmarking* cl:t)

(cl:defvar *coalton-benchmark-width* 90)

(cl:defvar *coalton-benchmark-sci-notation* cl:t
  "Coalton benchmarks should use scientific notation for times (or not).")

(coalton-toplevel

  (define-struct Benchmark
    "A benchmark object"
    (name       String)
    (iterations UFix)
    (code       (Unit -> Unit))
    (packages   (Vector String)))

  (declare verbose-benchmarking (Unit -> Boolean))
  (define (verbose-benchmarking)
    "This variable indicates whether benchmarks should print to the repl or just return a BenchmarkResult object."
    (lisp Boolean () *coalton-verbose-benchmarking*))

  (define (benchmark-width)
    "The width of the benchmark table output. Ideally should be divisible by 5."
    (lisp UFix () *coalton-benchmark-width*))

  (declare *benchmark-environment* (hash:Hashtable String Benchmark))
  (define *benchmark-environment*
    "A global environment holding Coalton benchmarks. Key is benchmark name."
    (hash:new)))

(coalton-toplevel

  (define-struct BenchmarkResults
    "Results from a Benchmark run."
    (name         String)
    (iterations   UFix)
    (run-time     UFix)
    (real-time    UFix)
    #+sbcl
    (bytes-consed "Bytes-consed is only currently enabled for SBCL" UFix))

  (define-struct BenchmarkSystem
    "Information about the system the benchmark is run on."
    (architecture String)
    (OS           String)
    (lisp-impl    String)
    (lisp-version String)
    (release?     "Is this in release mode or development mode?" Boolean)
    (inlining?    "Is inlining enabled?" Boolean))

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
    (package-name String)
    (system       BenchmarkSystem)
    (Results      (vector BenchmarkResults))))

(coalton-toplevel

  (declare current-package (Unit -> String))
  (define (current-package)
    "Returns the current local package."
    (lisp String ()
      (cl:package-name cl:*package*)))

  (declare %define-benchmark (String -> UFix -> (Unit -> Unit) -> Unit))
  (define (%define-benchmark name iterations fn)
    "Defines a Coalton benchmark, stored in *benchmark-environment*."
    (hash:set!
     *benchmark-environment*
     name
     (Benchmark
      name
      iterations
      fn
      (vec:make (current-package)))))

  (declare find-benchmark (String -> (Optional Benchmark)))
  (define (find-benchmark name)
    "Finds a benchmark given its name."
    (hash:get *benchmark-environment* name))

  (declare find-package-benchmarks (String -> (Iterator Benchmark)))
  (define (find-package-benchmarks package)
    "Finds all benchmarks defined in a `package`"
    (let pkg = (lisp String (package) (cl:string-upcase package)))
    (iter:filter! (fn (b) (unwrap-or-else (fn (_x) True)
                                          (fn () False)
                                          (vec:find-elem pkg (.packages b))))
                  (hash:values *benchmark-environment*))))

(cl:defmacro define-benchmark (name iterations func)
  "Defines a Coalton benchmark"
  (cl:let ((name (cl:string name)))
    `(coalton (%define-benchmark ,name ,iterations ,func))))

;;;
;;; Allow importing of benchmarks into other packages,
;;; for the sake of building package-per-file benchmark hierarchies.
;;;

(coalton-toplevel

  (declare %add-package (String -> Benchmark -> Unit))
  (define (%add-package package-name benchmark)
    "Adds a package to the benchmark's packages."
    (vec:push! package-name (.packages benchmark))
    Unit)

  (declare %reexport-package-benchmarks (String -> Unit))
  (define (%reexport-package-benchmarks package)
    (for bmark in (find-package-benchmarks package)
      (%add-package (current-package) bmark)
      Unit)))

(cl:defun reexport-benchmarks (cl:&rest packages)
  "This imports and reexports benchmarks from another package, for package-per-file hierarchy."
  (cl:loop :for pkg :in packages
     :do (%reexport-package-benchmarks pkg)))

;;;
;;; Running and Printing
;;;

(coalton-toplevel


  (declare format-time (UFix -> String))
  (define (format-time rtime)
    "Converts time from microseconds to seconds then prunes down to a 10 characters."
    (lisp String (rtime)
      (cl:let ((control-string (cl:if *coalton-benchmark-sci-notation*
                                      "~,4e s"
                                      "~,7f s")))
        (cl:format cl:nil control-string (cl:/ rtime 1e6)))))

  (declare *benchmark-column-names* (Vector String))
  (define *benchmark-column-names* (vec:make "Benchmark"
                                             "Run time"
                                             "Real time"
                                             #+sbcl
                                             "Bytes consed"
                                             "# Iterations"))

  (declare column-values (BenchmarkResults -> (Vector String)))
  (define (column-values (BenchmarkResults name iterations run-time real-time #+sbcl bytes-consed))
    "Returns the column values for a row."
    (vec:make name
              (format-time run-time)
              (format-time real-time)
              #+sbcl
              (into bytes-consed)
              (into iterations)))

  (declare system-header-text (BenchmarkSystem -> (Tuple String String)))
  (define (system-header-text (BenchmarkSystem architecture os lisp-impl lisp-version release inlining))
    "Returns formatted system information for printing purposes."
    (Tuple (lisp String (architecture os lisp-impl lisp-version)
             (cl:format cl:nil "System: ~a ~a ~a~a"
                        architecture
                        os
                        lisp-impl
                        lisp-version))
           (lisp String (release inlining)
             (cl:format cl:nil "Coalton ~a mode ~a heuristic inlining"
                        (cl:if release
                               "release"
                               "development")
                        (cl:if inlining
                               "with"
                               "without")))))

  (declare %run-benchmark (Benchmark -> BenchmarkResults))
  (define (%run-benchmark (Benchmark name iterations func _package))
    "Runs a benchmark a given number of iterations."
    (let profile = (sys:capture-profile (fn ()
                                          (for i in (iter:up-to iterations)
                                            (func)
                                            Unit))))
    (BenchmarkResults
     name
     iterations
     (.run-time profile)
     (.real-time profile)
     #+sbcl
     (.bytes-consed profile)))

  (declare run-benchmark (String -> BenchmarkResults))
  (define (run-benchmark name)
    "Looks up a benchmark by name and runs it if it exists."
    (let ((results (unwrap-or-else %run-benchmark
                                   (fn () (error (lisp String (name)
                                                   (cl:format cl:nil "No benchmark defined by this name: ~a" name))))
                                   (find-benchmark name)))
          (sys (system-header-text (benchmark-system-info))))
      (when (verbose-benchmarking)
        (print
         (coalton-table
          (benchmark-width)
          (Header (lisp String (name) (cl:format cl:nil "Benchmark ~a" name)))
          (Header (fst sys))
          (Header (snd sys))
          (TopRow *benchmark-column-names*)
          (Row (column-values results))
          (Bottom (vec:length *benchmark-column-names*)))))
      results))

  (declare package-header (String -> BenchmarkSystem -> String))
  (define (package-header name system)
    "Returns a formatted package header, including package and system information."
    (let sys = (system-header-text system))
    (coalton-table
     (benchmark-width)
     (Header (lisp String (name)
               (cl:format cl:nil "Package '~a'" name)))
     (Header (fst sys))
     (Header (snd sys))
     (TopRow *benchmark-column-names*)))

  (declare run-package-benchmarks (String -> PackageBenchmarkResults))
  (define (run-package-benchmarks name)
    "Runs all benchmarks for a package"
    (let system = (benchmark-system-info))
    (let results = (vec:new))
    (when (verbose-benchmarking)
      (print (package-header name system)))

    (for b in (find-package-benchmarks name)
      (let res = (%run-benchmark b))
      (when (verbose-benchmarking)
        (print (coalton-table
                (benchmark-width)
                (Row (column-values res)))))
      (vec:push! res results))

    (when (verbose-benchmarking)
      (print (coalton-table
              (benchmark-width)
              (Bottom #+sbcl 5 #-sbcl 4))))

    (PackageBenchmarkResults
     name
     system
     results))

  (declare print-results ((List BenchmarkResults) -> (state:ST Table Unit)))
  (define (print-results results)
    "Adds results to the table object."
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
                     (Header (fst sys))
                     (Header (snd sys))
                     (TopRow *benchmark-column-names*)
                     (print-results (into results))
                     (Bottom #+sbcl 5 #-sbcl 4)))))

(cl:defmacro define-benchmark (name iterations func)
  "Defines a Coalton benchmark"
  (cl:let ((name (cl:string name)))
    `(coalton (%define-benchmark ,name ,iterations ,func))))
