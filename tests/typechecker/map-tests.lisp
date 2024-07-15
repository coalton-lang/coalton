(in-package #:coalton-tests)

(defstruct map-container
  map)

(defmethod make-load-form ((self map-container) &optional environment)
  (make-load-form-saving-slots self :environment environment))

(deftest test-print-environment-map ()
  "Maps with hash table storage survive print, read."
  (let ((map (tc:make-map :initial-contents '(("key" . "value")))))
    (with-standard-io-syntax
      (let* ((obj (make-map-container :map map))
             (obj-string (with-output-to-string (stream)
                              (prin1 obj stream)))
             (obj-roundtrip (with-input-from-string (stream obj-string)
                                 (read stream))))
        (is (equalp obj
                    obj-roundtrip))))))

;; *test-map* is bound when the test below loads a compiled file
;; containing a map.

(defvar *test-map*)

(deftest test-compile-environment-map ()
  "Maps with hash table storage survive print, compile-file, load."
  (let* ((sym (gensym))
         (map (tc:make-map :initial-contents `(("key" . ,sym))))
         (obj (make-map-container :map map)))
    (uiop:with-temporary-file (:stream stream
                               :pathname input-file
                               :suffix "lisp"
                               :direction :output)
      (with-standard-io-syntax
        (prin1 `(setf *test-map* ,obj) stream))

      :close-stream

      (uiop:with-temporary-file (:pathname output-file
                                 :type #+ccl (pathname-type ccl:*.fasl-pathname*)
                                       #+(not ccl) "fasl")
        (load (compile-file input-file :output-file output-file :verbose nil :print nil))
        (is (string-equal sym
                          (tc:get-value (map-container-map *test-map*) "key")))))))
