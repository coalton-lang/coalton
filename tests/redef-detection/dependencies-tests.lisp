(defpackage #:coalton-tests/redef-detection/dependencies
  (:use #:cl #:fiasco)
  (:local-nicknames
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source))
  (:export
   #:test-collect-variables-problem))
(in-package #:coalton-tests/redef-detection/dependencies)

(deftest test-collect-variables-problem ()
  "Demonstrate that collect-variables returns local bindings (the problem we need to fix)."
  ;; Parse just the expression part: (let ((y 5)) (+ x y))
  (let ((program "(let ((y 5)) (+ x y))"))
    (let ((source (source:make-source-string program :name "<test>")))
      (with-open-stream (stream (source:source-stream source))
        (let* ((cst (eclector.concrete-syntax-tree:read stream))
               (node (parser:parse-expression cst source))
               (vars (parser:collect-variables node)))
          ;; collect-variables returns ALL variables, including the let-bound 'y'
          (let ((var-names (mapcar #'parser:node-variable-name vars)))
            (format t "~&Variables found: ~S~%" var-names)
            ;; The problem: 'y' is included even though it's a local binding
            ;; Note: Symbols will be in whatever package they were read in
            (is (find 'y var-names :test #'string= :key #'symbol-name))
            (is (find 'x var-names :test #'string= :key #'symbol-name))  ; Not locally bound
            (is (find '+ var-names :test #'string= :key #'symbol-name))))))))
