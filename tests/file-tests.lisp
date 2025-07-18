(in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

;;;
;;; Checking whether a function errors
;;;

(coalton-toplevel

  (declare res-succeeds ((Result :a :b) -> Boolean))
  (define (res-succeeds res)
    (match res
      ((Ok _)
       True)
      ((Err _)
       False)))

  (declare res-fails ((Result :a :b) -> Boolean))
  (define (res-fails res)
    (not (res-succeeds res))))

;;;
;;; Tests
;;;

(define-test test-pathnames ()
  (is (file:directory-pathname? "wow/"))
  (is (not (file:directory-pathname? "wow")))
  (is (== (the file:Pathname (into "wow/ok.txt"))
          (file:merge "wow/" "ok.txt")))
  (is (== (the file:Pathname (into "wow/ok/"))
          (file:merge "wow/" "ok/"))))

(define-test test-existence ()
  (let coalton-dir = (unwrap (file:system-relative-pathname "coalton" "")))
  (is (unwrap (file:exists? coalton-dir)))
  (is (unwrap (file:directory-exists? coalton-dir)))
  (is (not (unwrap (file:empty? coalton-dir))))
  (is (unwrap (file:exists? (file:merge coalton-dir "coalton.asd"))))
  (is (unwrap (file:file-exists? (file:merge coalton-dir "coalton.asd"))))
  (res-succeeds (file:directory-files coalton-dir))
  (is (math:positive? (cln:length (unwrap (file:directory-files coalton-dir)))))
  (res-succeeds (file:subdirectories coalton-dir))
  (is (math:positive? (cln:length (unwrap (file:subdirectories coalton-dir))))))

(define-test test-temporary-file-char ()
  (is (== #\h
          (unwrap (file:with-temp-file ".txt"
                    (fn (stream)
                      (file:write stream #\h)
                      (file:set-file-position stream 0)
                      (file:read stream)))))))

(define-test test-temporary-file-string ()
  (is (== "Hello World!"
          (into (the (List Char)
                     (into (unwrap (file:with-temp-file ".txt"
                                     (fn (stream)
                                       (file:write-string stream "Hello World!")
                                       (file:set-file-position stream 0)
                                       (file:read-file-to-vector stream))))))))))

(define-test test-temporary-file-byte ()
  (is (== 3
          (unwrap (file:with-temp-file ".txt"
                    (fn (stream)
                      (file:write stream (the U64 3))
                      (file:set-file-position stream 0)
                      (file:read stream)))))))

(define-test test-temporary-directory ()
  (is (== 0 (cln:length (unwrap (file:with-temp-directory (fn (dir)
                                                             (file:directory-files dir)))))))
  (is (== "Hello World!" (unwrap (file:with-temp-directory (fn (dir)
                                                             (let file = (file:merge dir "test.txt"))
                                                             (file:with-open-file (file:Bidirectional file file:EError)
                                                               (fn (stream)
                                                                 (file:write-string stream "Hello World!")))
                                                             (file:read-file-to-string file)))))))

(define-test test-more-file-functions ()
  (file:with-temp-directory
      (fn (dir)
        (let testpath = (file:merge dir "file-tests/"))
        (is (file:directory-pathname? testpath))
        (is (not (unwrap (file:directory-exists? testpath))))
        (is (res-succeeds (file:create-directory! testpath)))
        (is (unwrap (file:directory-exists? testpath)))

        (let filepath = (file:merge testpath "test-file.txt"))
        (is (not (unwrap (file:file-exists? filepath))))
        (is (res-succeeds
             (file:with-open-file (file:Output filepath file:EError)
               (fn (stream)
                 (file:write-string stream "Hello World!")))))
        (is (unwrap (file:file-exists? filepath)))
        (is (== (unwrap (file:read-file-to-string filepath))
                "Hello World!"))

        (let filepath2 = (file:merge testpath "test-file2.txt"))
        (is (res-succeeds (file:copy! filepath filepath2)))
        (is (unwrap (file:file-exists? filepath2)))
        (is (== (unwrap (file:read-file-to-string filepath2))
                "Hello World!"))
        (is (res-succeeds (file:write-to-file! filepath2 (iter:collect! (iter:into-iter "wow")))))
        (is (== (unwrap (file:read-file-to-string filepath2))
                "wow"))
        (is (res-succeeds (file:append-to-file! filepath2 (iter:collect! (iter:into-iter " and more")))))
        (is (== (unwrap (file:read-file-to-string filepath2))
                "wow and more"))

        ;; sorted because Allegro CL is not picky about file order
        (is (== (cln:sort (unwrap (file:directory-files testpath)))
                (make-list filepath filepath2)))

        ;; clearing the file-test directory
        (is (res-fails (file:remove-directory! testpath)))
        (is (res-fails (file:remove-directory! filepath)))
        (is (res-fails (file:remove-directory-recursive! filepath)))

        (is (res-succeeds (file:delete-file! filepath)))
        (is (res-succeeds (file:delete-file! filepath2)))
        (is (unwrap (file:empty? testpath)))
        (is (res-succeeds (file:remove-directory! testpath)))

        ;; clearing the file-test directory for real this time
        (is (not (unwrap (file:directory-exists? testpath))))

        (pure Unit))))
