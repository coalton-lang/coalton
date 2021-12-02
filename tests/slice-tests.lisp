(in-package #:coalton-tests)

(deftest test-slice-basic ()
  (let* ((v (coalton:coalton
            (coalton:the
             (coalton-library:Vector coalton:Integer)
             (coalton-library:into (coalton-library:make-list 1 2 3 4 5 6 7 8)))))

         (s (coalton-library:make-slice 0 3 v)))

    ;; Length should be correct
    (is (equal 3 (coalton-library:slice-length s)))

    ;; Indexing should work
    (is (equal 1 (coalton-library:slice-index-unsafe 0 s)))
    (is (equal 2 (coalton-library:slice-index-unsafe 1 s)))
    (is (equal 3 (coalton-library:slice-index-unsafe 2 s)))

    ;; Writes to the backing array should be visible in the slice
    (coalton-library:vector-set 0 25 v)
    (is (equal 25 (coalton-library:slice-index-unsafe 0 s)))

    ;; Writes to the slice should be visible in the backing array
    (coalton-library:slice-set 0 26 s)
    (is (equal 26 (coalton-library:vector-index-unsafe 0 v)))))

(deftest test-slice-offset ()
  (let* ((v (coalton:coalton
            (coalton:the
             (coalton-library:Vector coalton:Integer)
             (coalton-library:into (coalton-library:make-list 1 2 3 4 5 6 7 8)))))

         (s (coalton-library:make-slice 2 3 v)))

    (is (equal 3 (coalton-library:slice-index-unsafe 0 s)))
    (is (equal 4 (coalton-library:slice-index-unsafe 1 s)))
    (is (equal 5 (coalton-library:slice-index-unsafe 2 s)))))

(deftest test-slice-sliding-iteration ()
  (let* ((v (coalton:coalton
             (coalton:the
              (coalton-library:Vector coalton:Integer)
              (coalton-library:into (coalton-library:make-list 1 2 3 4 5 6 7 8)))))

         (out nil))

    (coalton-library:vector-sliding
     (coalton-impl/codegen::F1
      (lambda (s)
        (push
         (list (coalton-library:slice-index-unsafe 0 s)
               (coalton-library:slice-index-unsafe 1 s)
               (coalton-library:slice-index-unsafe 2 s))
         out)))
     3 v)

    (is (equal (reverse out) '((1 2 3)
                               (2 3 4)
                               (3 4 5)
                               (4 5 6)
                               (5 6 7)
                               (6 7 8))))))

(deftest test-slice-chunked-iteration ()
  (let* ((v (coalton:coalton
             (coalton:the
              (coalton-library:Vector coalton:Integer)
              (coalton-library:into (coalton-library:make-list 1 2 3 4 5 6 7 8)))))

         (out nil))

    (coalton-library:vector-chunked
     (coalton-impl/codegen::F1
      (lambda (s)
        (push
         (list (coalton-library:slice-index-unsafe 0 s)
               (coalton-library:slice-index-unsafe 1 s)
               (coalton-library:slice-index-unsafe 2 s))
         out)))
     3 v)

    (is (equal (reverse out) '((1 2 3)
                               (4 5 6))))))
