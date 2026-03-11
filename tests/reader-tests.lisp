(in-package #:coalton-tests)

(defun read-form-with-source (string)
  (let ((source (source:make-source-string string :name "test")))
    (with-open-stream (stream (source:source-stream source))
      (parser:with-reader-context stream
        (values (parser:maybe-read-form stream source parser::*coalton-eclector-client*)
                source)))))

(defun cst-symbol-spans (form name)
  (labels ((walk (node spans)
             (cond
               ((concrete-syntax-tree:consp node)
                (walk (concrete-syntax-tree:rest node)
                      (walk (concrete-syntax-tree:first node) spans)))
               ((concrete-syntax-tree:atom node)
                (let ((raw (concrete-syntax-tree:raw node)))
                  (if (and (symbolp raw)
                           (string= (symbol-name raw) name))
                      (cons (concrete-syntax-tree:source node) spans)
                      spans)))
               (t
                spans))))
    (nreverse (walk form nil))))

(defmacro coalton-example-with-gensym ()
  (let ((x (gensym)))
    `(coalton:coalton
      (coalton:let ((,x 5))
        ,x))))

(deftest test-uninterned-symbols ()
  (let ((y (coalton-example-with-gensym)))
    (is (= y 5))))

(deftest read-eval-error ()
  "Check that errors signalled during direct evaluation of Coalton have correct messages and are printable."
  (let ((*compile-file-truename* nil)
        (*load-truename* nil)
        (*package* (find-package "COALTON-USER")))
    (handler-case
        (progn
          (eval (read-from-string
                 "(coalton-toplevel (define 1 x))"))
          (is nil "error was not signalled"))
      (coalton-impl/parser/base:parse-error (c)
        (is (string= "Invalid variable"
                     (source:message c))
            "condition message is correct")
        (is (princ-to-string c)
            "condition prints without error")))

    (eval (read-from-string "(coalton-toplevel
  (declare add-3 (UFix -> UFix))
  (define (add-3 x)
    (+ 3 x)))"))
    (handler-case
        (eval (read-from-string "(coalton (add-3 \"two\"))"))
      (coalton-impl/typechecker/base:tc-error (c)
        (is (string= "Type mismatch"
                     (source:message c))
            "condition message is correct")))))

(deftest repeated-symbols-retain-distinct-source-spans ()
  (let ((form-string "(:a -> Tuple Integer Integer -> List Tuple Integer Integer)"))
    (multiple-value-bind (form source)
        (read-form-with-source form-string)
      (declare (ignore source))
      (let* ((first (search "Tuple" form-string))
             (second (search "Tuple" form-string :start2 (1+ first)))
             (expected (list (cons first (+ first (length "Tuple")))
                             (cons second (+ second (length "Tuple"))))))
        (is (equal expected
                   (cst-symbol-spans form "TUPLE")))))))

(deftest reader-defers-coalton-compilation ()
  (uiop:with-temporary-file (:stream stream
                             :pathname input-file
                             :suffix ".lisp"
                             :direction :output
                             :keep t)
    (write-string "(named-readtables:in-readtable coalton:coalton)
(in-package #:coalton-user)
;; λ
(coalton-toplevel
  (define-struct (ReaderTestStruct :a)
    (reader-test-slot :a))
  (declare reader-test-f (UFix -> UFix))
  (define (reader-test-f x) x))
" stream)
    :close-stream
    (let* ((compile-sym (find-symbol "COMPILE-COALTON-TOPLEVEL" "COALTON-IMPL/ENTRY"))
           (orig-compile (symbol-function compile-sym))
           (saved-environment entry:*global-environment*)
           (compile-count 0)
           (source-names nil))
      (unwind-protect
           (progn
             (setf (symbol-function compile-sym)
                   (lambda (program)
                     (incf compile-count)
                     (let* ((form (first (coalton-impl/parser/toplevel:program-defines program)))
                            (location (source:location form)))
                       (push (source:source-name
                              (source:location-source location))
                             source-names))
                     (funcall orig-compile program)))
             (with-open-file (stream input-file)
               (let ((*package* (find-package "CL-USER"))
                     (*readtable* (copy-readtable nil))
                     (*load-truename* input-file))
                 (eval (read stream nil nil))
                 (eval (read stream nil nil))
                 (let* ((form (read stream nil nil))
                        (expansion-1 (macroexpand-1 form))
                        (expansion-2 (macroexpand-1 form))
                        (struct-sym (find-symbol "READERTESTSTRUCT" "COALTON-USER"))
                        (type-entry (coalton-impl/typechecker:lookup-type entry:*global-environment* struct-sym :no-error t))
                        (struct-entry (coalton-impl/typechecker:lookup-struct entry:*global-environment* struct-sym :no-error t)))
                   (is (= 1 compile-count)
                       "reader path should compile at most once per source form")
                   (is (eq expansion-1 expansion-2)
                       "cached macroexpansion should be reused")
                   (is (string= (namestring input-file)
                                (first source-names))
                       "source name should refer to the original file")
                   (is (string= "ReaderTestStruct"
                                (coalton-impl/typechecker:type-entry-source-name type-entry))
                       "type source-name should preserve the original spelling")
                   (is (string= "ReaderTestStruct"
                                (coalton-impl/typechecker:struct-entry-source-name struct-entry))
                       "struct source-name should preserve the original spelling")))))
        (setf (symbol-function compile-sym) orig-compile
              entry:*global-environment* saved-environment)))))
