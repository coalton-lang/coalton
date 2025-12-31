(in-package #:quil-coalton-tests)

#|
;; abcl breaks on this (and other tests) - complains about "Undefined
;; variable TEST-PARSER assumed special"
;;
;; But... if after the error I do:
  (in-package #:fiasco)
  (maphash (lambda (k v) (format t "Key: ~A Value: ~A~%" k v)) *tests*)
;; the tests appear. And if I do:
  (in-package #:quil-coalton-tests)
  (run-quil-coalton-tests)
;; then the test runs fine...
|#
(deftest test-parser ()
  ;; Test basic gates
  (parser-succeeds "H 0")
  (parser-succeeds "CNOT 0 1")
  (parser-succeeds "H 0; CNOT 0 1")

  ;; More complex instructions
  (parser-succeeds "HALT")
  (parser-succeeds "H 0; CNOT 0 1; HALT")
  (parser-succeeds "H 0; RESET 0")
  (parser-succeeds "H 0; RESET")
  (parser-succeeds "WAIT")

  ;; Measurement
  (parser-succeeds "H 0; MEASURE 0 b")
  (parser-succeeds "H 0; MEASURE 0 b[0]")

  ;; Control flow
  (parser-succeeds "LABEL @a; JUMP @a")

  ;; Comments
  (parser-succeeds "# This is a comment")
  (parser-succeeds "H 0 # This is a comment")

  ;; Doing nothing is a valid quil program
  (parser-succeeds ""))

(defun parser-succeeds (program)
  (is (typep (run-quil-parser program)
             'coalton-library/classes::Result/Ok)))
