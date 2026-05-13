(in-package #:coalton-tests)

(defvar *unboxed-product-runtime-test-counter* 0)

(defun next-unboxed-product-runtime-package-name ()
  (format nil "coalton-tests/stack-products-runtime-generated-~D"
          (incf *unboxed-product-runtime-test-counter*)))

(defun unboxed-product-runtime-source (package-name)
  (format nil
          "(package ~A
             (import coalton-prelude)
             (export sum-return-match
                     sum-return-accessor
                     sum-arg-direct
                     sum-arg-return
                     sum-arg-two-products
                     sum-product-map-direct
                     sum-product-map-return
                     sum-product-map-consumed
                     sum-product-merge
                     sum-forwarded-arg
                     sum-forwarded-return
                     sum-state-pipeline))

           (define-struct BenchPair
             (x IFix)
             (y IFix))

           (define-struct BenchState
             (current IFix)
             (total IFix))

           (declare build-pair (IFix * IFix -> BenchPair))
           (define (build-pair x y)
             (BenchPair x y))

           (declare pair-sum (BenchPair -> IFix))
           (define (pair-sum p)
             (match p
               ((BenchPair a b) (+ a b))))

           (declare combine-pairs (BenchPair * BenchPair -> IFix))
           (define (combine-pairs p q)
             (+ (.x p)
                (match q
                  ((BenchPair a b) (+ a b)))))

           (declare shift-pair (BenchPair -> BenchPair))
           (define (shift-pair p)
             (match p
               ((BenchPair a b) (BenchPair (+ a 1) (+ b 1)))))

           (declare forwarded-pair-sum (BenchPair -> IFix))
           (define (forwarded-pair-sum p)
             (pair-sum p))

           (declare forwarded-shift-pair (BenchPair -> BenchPair))
           (define (forwarded-shift-pair p)
             (shift-pair p))

           (declare add-pairs (BenchPair * BenchPair -> BenchPair))
           (define (add-pairs p q)
             (BenchPair (+ (.x p) (.x q))
                        (+ (.y p) (.y q))))

           (declare step-state (IFix * IFix -> BenchState))
           (define (step-state current total)
             (BenchState (+ current 1)
                         (+ total (+ current 1))))

           (declare state-score (BenchState -> IFix))
           (define (state-score state)
             (match state
               ((BenchState current total) (+ current total))))

           (declare sum-return-match (IFix -> IFix))
           (define (sum-return-match n)
             (rec loop ((declare i IFix)
                        (i 0)
                        (declare acc IFix)
                        (acc 0))
               (if (< i n)
                   (loop (+ i 1)
                         (+ acc
                            (match (build-pair i (+ i 1))
                              ((BenchPair a b) (+ a b)))))
                   acc)))

           (declare sum-return-accessor (IFix -> IFix))
           (define (sum-return-accessor n)
             (rec loop ((declare i IFix)
                        (i 0)
                        (declare acc IFix)
                        (acc 0))
               (if (< i n)
                   (loop (+ i 1)
                         (+ acc (.x (build-pair i (+ i 1)))))
                   acc)))

           (declare sum-arg-direct (IFix -> IFix))
           (define (sum-arg-direct n)
             (rec loop ((declare i IFix)
                        (i 0)
                        (declare acc IFix)
                        (acc 0))
               (if (< i n)
                   (loop (+ i 1)
                         (+ acc (pair-sum (BenchPair i (+ i 1)))))
                   acc)))

           (declare sum-arg-return (IFix -> IFix))
           (define (sum-arg-return n)
             (rec loop ((declare i IFix)
                        (i 0)
                        (declare acc IFix)
                        (acc 0))
               (if (< i n)
                   (loop (+ i 1)
                         (+ acc (pair-sum (build-pair i (+ i 1)))))
                   acc)))

           (declare sum-arg-two-products (IFix -> IFix))
           (define (sum-arg-two-products n)
             (rec loop ((declare i IFix)
                        (i 0)
                        (declare acc IFix)
                        (acc 0))
               (if (< i n)
                   (loop (+ i 1)
                         (+ acc
                            (combine-pairs (BenchPair i (+ i 1))
                                           (build-pair (+ i 2) (+ i 3)))))
                   acc)))

           (declare sum-product-map-direct (IFix -> IFix))
           (define (sum-product-map-direct n)
             (rec loop ((declare i IFix)
                        (i 0)
                        (declare acc IFix)
                        (acc 0))
               (if (< i n)
                   (match (shift-pair (BenchPair i (+ i 1)))
                     ((BenchPair a b)
                      (loop (+ i 1) (+ acc (+ a b)))))
                   acc)))

           (declare sum-product-map-return (IFix -> IFix))
           (define (sum-product-map-return n)
             (rec loop ((declare i IFix)
                        (i 0)
                        (declare acc IFix)
                        (acc 0))
               (if (< i n)
                   (match (shift-pair (build-pair i (+ i 1)))
                     ((BenchPair a b)
                      (loop (+ i 1) (+ acc (+ a b)))))
                   acc)))

           (declare sum-product-map-consumed (IFix -> IFix))
           (define (sum-product-map-consumed n)
             (rec loop ((declare i IFix)
                        (i 0)
                        (declare acc IFix)
                        (acc 0))
               (if (< i n)
                   (loop (+ i 1)
                         (+ acc
                            (pair-sum
                             (shift-pair (BenchPair i (+ i 1))))))
                   acc)))

           (declare sum-product-merge (IFix -> IFix))
           (define (sum-product-merge n)
             (rec loop ((declare i IFix)
                        (i 0)
                        (declare acc IFix)
                        (acc 0))
               (if (< i n)
                   (match (add-pairs (BenchPair i (+ i 1))
                                     (build-pair (+ i 2) (+ i 3)))
                      ((BenchPair a b)
                       (loop (+ i 1) (+ acc (+ a b)))))
                   acc)))

           (declare sum-forwarded-arg (IFix -> IFix))
           (define (sum-forwarded-arg n)
             (rec loop ((declare i IFix)
                        (i 0)
                        (declare acc IFix)
                        (acc 0))
               (if (< i n)
                   (loop (+ i 1)
                         (+ acc
                            (forwarded-pair-sum
                             (BenchPair i (+ i 1)))))
                   acc)))

           (declare sum-forwarded-return (IFix -> IFix))
           (define (sum-forwarded-return n)
             (rec loop ((declare i IFix)
                        (i 0)
                        (declare acc IFix)
                        (acc 0))
               (if (< i n)
                   (match (forwarded-shift-pair
                           (BenchPair i (+ i 1)))
                     ((BenchPair a b)
                      (loop (+ i 1) (+ acc (+ a b)))))
                   acc)))

           (declare sum-state-pipeline (IFix -> IFix))
           (define (sum-state-pipeline n)
             (rec loop ((declare i IFix)
                        (i 0)
                        (declare acc IFix)
                        (acc 0))
               (if (< i n)
                   (match (step-state i (+ i 3))
                     ((BenchState current total)
                      (loop (+ i 1)
                            (+ acc (state-score
                                     (BenchState current total))))))
                   acc)))"
          package-name))

(defun compile-unboxed-product-runtime-source (package-name)
  (let ((*features* (adjoin :coalton-release *features*))
        (*readtable* (copy-readtable nil)))
    (handler-bind ((style-warning #'muffle-warning))
      (entry:compile
       (source:make-source-string (unboxed-product-runtime-source package-name)
                                  :name "unboxed-product-runtime-test.coal")
       :load t)))
  package-name)

(defun generated-runtime-function (package-name function-name)
  (let* ((package (or (find-package package-name)
                      (find-package (string-upcase package-name))))
         (symbol (and package
                      (or (find-symbol function-name package)
                          (find-symbol (string-upcase function-name) package)))))
    (unless (and symbol (fboundp symbol))
      (error "Could not find generated function ~A::~A."
             package-name
             function-name))
    (symbol-function symbol)))

(defun triangular-number (n)
  (/ (* n (1- n)) 2))

(defun expected-linear-sum (n coefficient constant)
  (+ (* coefficient (triangular-number n))
     (* constant n)))

#+sbcl
(defun bytes-consed-by (thunk)
  (sb-ext:gc :full t)
  (let ((before (sb-ext:get-bytes-consed))
        (result (funcall thunk)))
    (values result (- (sb-ext:get-bytes-consed) before))))

#+sbcl
(defun bytes-consed-by/retry (thunk)
  (loop :repeat 3
        :for (result bytes) := (multiple-value-list (bytes-consed-by thunk))
        :when (zerop bytes)
          :return (values result bytes)
        :finally (return (values result bytes))))

#+sbcl
(defun check-no-consing (name function n expected)
  (funcall function 32)
  (multiple-value-bind (result bytes)
      (bytes-consed-by/retry (lambda () (funcall function n)))
    (is (= expected result)
        "~A returned ~D, expected ~D"
        name
        result
        expected)
    (is (zerop bytes)
        "~A consed ~D bytes"
        name
        bytes)))

(deftest release-unboxed-product-optimization-runtime ()
  #+sbcl
  (let* ((package-name (compile-unboxed-product-runtime-source
                        (next-unboxed-product-runtime-package-name)))
         (n 100000))
    (check-no-consing "sum-return-match"
                      (generated-runtime-function package-name "sum-return-match")
                      n
                      (* n n))
    (check-no-consing "sum-return-accessor"
                      (generated-runtime-function package-name "sum-return-accessor")
                      n
                      (triangular-number n))
    (check-no-consing "sum-arg-direct"
                      (generated-runtime-function package-name "sum-arg-direct")
                      n
                      (* n n))
    (check-no-consing "sum-arg-return"
                      (generated-runtime-function package-name "sum-arg-return")
                      n
                      (* n n))
    (check-no-consing "sum-arg-two-products"
                      (generated-runtime-function package-name "sum-arg-two-products")
                      n
                      (expected-linear-sum n 3 5))
    (check-no-consing "sum-product-map-direct"
                      (generated-runtime-function package-name "sum-product-map-direct")
                      n
                      (expected-linear-sum n 2 3))
    (check-no-consing "sum-product-map-return"
                      (generated-runtime-function package-name "sum-product-map-return")
                      n
                      (expected-linear-sum n 2 3))
    (check-no-consing "sum-product-map-consumed"
                      (generated-runtime-function package-name "sum-product-map-consumed")
                      n
                      (expected-linear-sum n 2 3))
    (check-no-consing "sum-product-merge"
                      (generated-runtime-function package-name "sum-product-merge")
                      n
                      (expected-linear-sum n 4 6))
    (check-no-consing "sum-forwarded-arg"
                      (generated-runtime-function package-name "sum-forwarded-arg")
                      n
                      (* n n))
    (check-no-consing "sum-forwarded-return"
                      (generated-runtime-function package-name "sum-forwarded-return")
                      n
                      (expected-linear-sum n 2 3))
    (check-no-consing "sum-state-pipeline"
                      (generated-runtime-function package-name "sum-state-pipeline")
                      n
                      (expected-linear-sum n 3 5)))
  #-sbcl
  (is t))
