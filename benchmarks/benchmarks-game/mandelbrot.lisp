(defpackage #:coalton/benchmarks/benchmarks-game/mandelbrot
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames (#:math #:coalton-library/math)
                    (#:vec #:coalton-library/vector)
                    (#:cell #:coalton-library/cell)
                    (#:file #:coalton-library/file)
                    (#:iter #:coalton-library/iterator)
                    (#:char #:coalton-library/char)
                    (#:str #:coalton-library/string)
                    (#:bits #:coalton-library/bits))
  (:export
   #:mandelbrot-main
   #:mandelbrot-small))

(in-package #:coalton/benchmarks/benchmarks-game/mandelbrot)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:setf coalton-impl/settings:*coalton-heuristic-inlining* cl:t))

(cl:declaim (cl:optimize (cl:speed 3)
                         (cl:space 0)
                         (cl:compilation-speed 0)
                         (cl:safety 0)
                         (cl:debug 0)))

(coalton-toplevel

  (declare mandelbrot-function ((Num (Complex :a))
                                => (Complex :a)
                                -> (Complex :a)
                                -> (Complex :a)))
  (define (mandelbrot-function c z)
    (+ (^ z 2) c))

  (declare mandelbrot-div? ((Ord :a) (Complex :a)
                            => (Complex :a)
                            -> Boolean))
  (define (mandelbrot-div? z)
    "Is Z guaranteed to diverge in the implementation of the mandelbrot function. True means it is guaranteed, False means it's unsure."
    (> (math:square-magnitude z) 4))

  (declare escapes? (Integer
                     -> ((Complex :a) -> (Complex :a))
                     -> ((Complex :a) -> Boolean)
                     -> (Complex :a)
                     -> Integer))
  (define (escapes? limit fc div? z0)
    "Returns the number of iterations of the function F "
    (let count = (cell:new 0))
    (let result = (cell:new z0))

    (while :escape (< (cell:read count) limit)
      (cell:update! fc result)
      (if (div? (cell:read result))
          (break :escape)
          (progn (cell:increment! count) Unit)))
    (if (== limit (cell:read count))
        -1
        (cell:read count)))

  (declare sample ((Reciprocable :a) (Ord :a) (Complex :a)
                   => (Complex :a)
                   -> (Complex :a)
                   -> Integer
                   -> Integer
                   -> ((Complex :a) -> (Complex :a) -> (Complex :a))
                   -> ((Complex :a) -> Boolean)
                   -> (Complex :a)
                   -> (Vector Integer)))
  (define (sample negbound posbound steps limit f div? z0)
    (let v = (vec:new))
    (let steps-num = (fromint steps))
    (let width = (- (real-part posbound) (real-part negbound)))
    (let height = (- (imag-part posbound) (imag-part negbound)))
    (let real-offset = (real-part negbound))
    (let imag-offset = (imag-part negbound))
    (for bi in (iter:up-to steps-num)
      (lisp Unit (bi)
            (cl:when (cl:zerop (cl:mod bi 100))
              (cl:format cl:t ";; ~D~%" bi))
            Unit)
      (for ai in (iter:up-to steps-num)
        (let a = (+ real-offset (* width (/ ai steps-num)))
          )
        (let b = (+ imag-offset (* height (/ bi steps-num)))
          )
        (let c = (Complex a b))
        (vec:push! (escapes? limit (f c) div? z0) v)
        Unit))
    v)

  (define (mandelbrot negbound posbound steps limit)
    (sample negbound posbound steps limit mandelbrot-function mandelbrot-div? 0)))

;;;
;;; Writing the header
;;;

(coalton-toplevel

  (define magic-number
    (map (fn (c)
           (the U8 (unwrap (tryinto
                            (char:char-code c)))))
         (vec:make #\P #\4 #\Newline)))

  (declare width-height-data (String -> (Vector U8)))
  (define (width-height-data n)
    (map (fn (c)
           (unwrap (tryinto (char:char-code c))))
         (as (Vector Char)
             (as (List Char)
                 (str:concat n
                             (str:concat " "
                                         (str:concat n
                                                     (into (make-list #\Newline))))))))))

(coalton-toplevel

  (declare bit-vector ((Vector Integer) -> (Vector U8)))
  (define (bit-vector iterated)
    "Converts a list of Integers to a bit vector of U8's."
    (let out = (vec:new))
    (let current-byte = (cell:new (the U8 0)))
    (let bitnum = (cell:new 0))
    (for el in iterated

      (when (== (cell:read bitnum) 8)
        (vec:push! (cell:read current-byte) out)
        (cell:write! bitnum 0)
        (cell:write! current-byte 0)
        Unit)

      (cell:update! (bits:shift 1) current-byte)
      (cell:increment! bitnum)
      (when (== -1 el)
        (cell:increment! current-byte)
        Unit)
      Unit)

    (when (positive? (cell:read bitnum))
      (vec:push! (cell:read current-byte) out)
      Unit)

    out))

(coalton-toplevel

  (monomorphize)
  (declare benchmark-mandelbrot (Integer -> (Vector Integer)))
  (define (benchmark-mandelbrot n)
    (mandelbrot
     (Complex -1.5d0 -1.0d0)
     (Complex 0.5d0 1.0d0)
     n
     50))

  (declare mandelbrot-benchmark ((Into :a file:Pathname) => :a -> String -> (Result file:FileError Unit)))
  (define (mandelbrot-benchmark filename arg-n)
    (file:with-open-file
        (file:Output (into filename)
                     file:Supersede)
      (fn (stream)
        ;; write the header
        (file:write-vector stream magic-number)
        (file:write-vector stream (width-height-data arg-n))
        ;; write the raster
        (file:write-vector stream (bit-vector
                                   (benchmark-mandelbrot (unwrap (str:parse-int arg-n)))))
        (Ok Unit)))))

(coalton-toplevel

  (define (mandelbrot-small)
    (time (fn () (mandelbrot-benchmark "/dev/stdout" "1000"))))

  (define (mandelbrot-main)
    (time (fn () (mandelbrot-benchmark "/dev/stdout" "16000")))))
