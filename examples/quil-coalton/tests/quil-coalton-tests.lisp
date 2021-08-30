(in-package #:quil-coalton-tests)

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
             'coalton-user::Result/Ok)))
