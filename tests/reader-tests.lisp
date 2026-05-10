(in-package #:coalton-tests)

(defun read-form-with-source (string)
  (let ((source (source:make-source-string string :name "test")))
    (with-open-stream (stream (source:source-stream source))
      (parser:with-coalton-reader-context stream
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

(defun cst-form-spans-starting-with (form marker)
  (labels ((walk (node spans)
             (cond
               ((concrete-syntax-tree:consp node)
                (let ((spans (if (and (concrete-syntax-tree:atom
                                       (concrete-syntax-tree:first node))
                                      (eq marker
                                          (concrete-syntax-tree:raw
                                           (concrete-syntax-tree:first node))))
                                 (cons (concrete-syntax-tree:source node) spans)
                                 spans)))
                  (walk (concrete-syntax-tree:rest node)
                        (walk (concrete-syntax-tree:first node) spans))))
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

(deftest short-lambda-reader-desugars ()
  (let ((*package* (make-package "COALTON-SHORT-LAMBDA-READER-PACKAGE"
                                 :use '("COALTON"))))
    (unwind-protect
         (multiple-value-bind (form source)
             (read-form-with-source "ƒxy.x")
           (declare (ignore source))
           (let ((x (intern "X" *package*))
                 (y (intern "Y" *package*)))
             (is (equal (list 'coalton:fn (list x y) x)
                        (concrete-syntax-tree:raw form)))
             (is (equal (list (cons 1 2) (cons 4 5))
                        (cst-symbol-spans form "X")))
             (is (equal (list (cons 2 3))
                        (cst-symbol-spans form "Y")))))
      (delete-package *package*))))

(deftest bracket-reader-preserves-generated-source-spans ()
  (let ((*package* (make-package "COALTON-BRACKET-READER-PACKAGE"
                                 :use '("COALTON"))))
    (unwind-protect
         (progn
           (multiple-value-bind (form source)
               (read-form-with-source "[x => y z => w]")
             (declare (ignore source))
             (is (equal (list (cons 1 7) (cons 8 14))
                        (cst-form-spans-starting-with
                         form
                         (parser:association-entry-marker))))
             (is (equal (list (cons 3 5) (cons 10 12))
                        (cst-symbol-spans form "%ASSOCIATION-ENTRY"))))
           (multiple-value-bind (form source)
               (read-form-with-source "[x :for y :in z :when w]")
             (declare (ignore source))
             (is (equal (list (cons 3 15))
                        (cst-form-spans-starting-with
                         form
                         (parser:builder-for-marker))))
             (is (equal (list (cons 16 23))
                        (cst-form-spans-starting-with
                         form
                         (parser:builder-when-marker))))
             (is (equal (list (cons 3 7))
                        (cst-symbol-spans form "%BUILDER-FOR")))
             (is (equal (list (cons 16 21))
                        (cst-symbol-spans form "%BUILDER-WHEN"))))
           (multiple-value-bind (form source)
               (read-form-with-source "(the (List Integer) [x :for x :in xs])")
             (declare (ignore source))
             (is (equal (list (cons 20 37))
                        (cst-form-spans-starting-with
                         form
                         (parser:collection-comprehension-marker))))
             (is (equal (list (cons 23 36))
                        (cst-form-spans-starting-with
                         form
                         (parser:builder-for-marker))))))
      (delete-package *package*))))

(deftest coalton-readtable-syntax-in-lisp-fallback ()
  (let ((*package* (make-package "COALTON-SYNTAX-FALLBACK-PACKAGE" :use nil)))
    (unwind-protect
         (let ((*readtable* (named-readtables:ensure-readtable 'coalton:coalton))
               (list-symbol (intern "LIST" *package*))
               (f-symbol (intern "F" *package*))
               (x-symbol (intern "X" *package*)))
           (is (equal (list list-symbol
                            (list (parser:collection-builder-marker) x-symbol))
                      (read-from-string "(list [x])"))
               "bracket syntax should survive Lisp fallback after the first symbol")
           (is (equal (list (list (parser:collection-builder-marker) f-symbol)
                            x-symbol)
                      (read-from-string "([f] x)"))
               "bracket syntax should survive Lisp fallback in operator position")
           (is (equal (list list-symbol
                            (list 'coalton:fn (list x-symbol) x-symbol))
                      (read-from-string "(list ƒx.x)"))
               "short lambda syntax should survive Lisp fallback")
           (is (equal (list 'coalton:fn (list x-symbol) x-symbol)
                      (read-from-string "ƒx.x"))
               "short lambda syntax should be active on the Coalton readtable"))
      (delete-package *package*))))

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
