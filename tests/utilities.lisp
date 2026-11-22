(in-package #:coalton-tests)

(defun run-coalton-tests ()
  (run-package-tests
   :packages '(:coalton-tests
               :quil-coalton-tests
               :thih-coalton/fiasco-test-package)
   :interactive t))

(deftest test-list-type-predicates ()
  (let ((circular (list 'a)))
    (setf (cdr circular) circular)
    (dolist (value (list 1 'a #(a) '(a . b) circular))
      (is (not (util::symbol-list-p value)))
      (is (not (typep value 'util:symbol-list)))))
  (dolist (value '(nil (a b) (nil :keyword)))
    (is (util::symbol-list-p value))
    (is (typep value 'util:symbol-list)))
  (is (not (util::symbol-list-p '(a 1))))
  (is (typep '("a" "b") 'util:string-list))
  (is (not (typep '("a" b) 'util:string-list)))
  (is (typep '(a b) 'parser:identifier-list))
  (is (not (typep '(a :keyword) 'parser:identifier-list))))

(deftest test-list-type-compile-time-availability ()
  (let* ((package (make-package (symbol-name (gensym "LIST-TYPE-TEST-")) :use '(#:cl)))
         (element (intern "ELEMENT" package))
         (element-p (intern "ELEMENT-P" package))
         (elements (intern "ELEMENTS" package))
         (elements-p (intern "ELEMENTS-P" package)))
    (unwind-protect
         (uiop:with-temporary-file (:stream stream :pathname source :suffix "lisp")
           (let ((*package* package))
             (dolist (form `((in-package ,(package-name package))
                            (util:define-list-type ,elements ,element-p :compile-toplevel t)
                            (eval-when (:compile-toplevel)
                              (assert (,elements-p nil)))
                            (defstruct ,element)
                            (defstruct ,(intern "CONTAINER" package)
                              (items nil :type ,elements :read-only t))))
               (prin1 form stream)
               (terpri stream)))
           :close-stream
           (uiop:with-temporary-file (:pathname output
                                      :type #+ccl (pathname-type ccl:*.fasl-pathname*)
                                            #+(not ccl) "fasl")
             (multiple-value-bind (fasl warnings-p failure-p)
                 (compile-file source :output-file output :verbose nil :print nil)
               (declare (ignore warnings-p))
               (is (not failure-p))
               (is (fboundp elements-p))
               (is (funcall elements-p nil))
               (fmakunbound elements-p)
               (load fasl :verbose nil)
               (is (fboundp elements-p))
               (is (typep (list (funcall (intern "MAKE-ELEMENT" package))) elements))
               (is (not (typep '(1) elements))))))
      (delete-package package))))

(defun set-equalp (set1 set2)
  (null (set-exclusive-or set1 set2 :test #'equalp)))

(defun dag-equalp (dag1 dag2)
  ;; XXX: This will not check ordering of edges within vertices
  (set-equalp dag1 dag2))

(defun check-string= (context a b)
  "Signal a test failure assertion if strings A and B differ, reporting the first position at which this is true."
  (let ((compare-len (min (length a)
                          (length b))))
    (loop :for i :from 0 :below compare-len
          :unless (char= (aref a i)
                         (aref b i))
            :do (is (string= a b)
                    (format nil "Strings differ at offset ~A of ~A:~%A: ~A~%B: ~A"
                            i context a b))
                (return-from check-string=))
    (is (= (length a)
           (length b))
        (format nil "Strings differ at offset ~A of ~A:~%~A~%~A"
                compare-len context a b))))

(defun check-coalton-types (toplevel-string &rest expected-types)
  (let ((*package* (make-package (or (and fiasco::*current-test*
                                          (fiasco::name-of fiasco::*current-test*))
                                     "COALTON-TEST-COMPILE-PACKAGE")
                                 :use '("COALTON" "COALTON-PRELUDE"))))
    (unwind-protect
         (let ((source (source:make-source-string toplevel-string)))
           (with-open-stream (stream (source:source-stream source))
             (let ((program (parser:with-reader-context stream
                              (parser:read-program stream source))))

               (multiple-value-bind (program env)
                   (entry:entry-point program)
                 (declare (ignore program))

                 (when expected-types
                   (loop :for (unparsed-symbol . unparsed-type) :in expected-types
                         :do (let ((symbol (intern (string-upcase unparsed-symbol) *package*))
                                   (source (source:make-source-string unparsed-type)))
                               (with-open-stream (stream (source:source-stream source))
                                 (let* ((ast-type (parser:parse-qualified-type
                                                   (parser:with-reader-context stream
                                                     (eclector.concrete-syntax-tree:read stream))
                                                   source))
                                        (parsed-type (tc:parse-ty-scheme ast-type env)))
                                   (is (tc:ty-scheme=
                                        (tc:lookup-value-type env symbol)
                                        parsed-type))))))))))
           (values))
      (delete-package *package*))))

(defun compile-and-load-forms (coalton-forms)
  "Write the COALTON-FORMS to a temporary file, compile it to a fasl, then load the compiled file.

Returns (values SOURCE-PATHNAME COMPILED-PATHNAME)."
  (uiop:with-temporary-file (:stream out-stream
                             :pathname input-file
                             :suffix "lisp"
                             :direction :output
                             :keep t)
    (dolist (expr coalton-forms)
      (prin1 expr out-stream)
      (terpri out-stream))
    :close-stream
    (uiop:with-temporary-file (:pathname output-file
                               :type #+ccl (pathname-type ccl:*.fasl-pathname*)
                                     #+(not ccl) "fasl"
                               :keep t)
      (compile-file input-file :output-file output-file)
      (load output-file)
      (values input-file output-file))))

(defmacro with-coalton-compilation ((&key package (muffle 'cl:style-warning)) &body coalton-code)
  `(handler-bind
       ((,muffle #'muffle-warning))
     (compile-and-load-forms '(,@(when package `((cl:in-package ,package)))
                               ,@coalton-code))))

(defun test-file (pathname)
  "Create a pathname relative to the coalton/test system."
  (merge-pathnames pathname (asdf:system-source-directory "coalton/tests")))

(defun collect-compiler-error (program)
  (let ((source (source:make-source-string program :name "test"))
        (saved-environment entry:*global-environment*))
    (unwind-protect
         (handler-case
             (handler-bind ((simple-error
                              (lambda (c)
                                (if (search "incompatibly with the current definition"
                                            (princ-to-string c))
                                    (progn
                                      (format t "~%;; Allowing redefinition and continuing.~%")
                                      (invoke-restart 'continue))
                                    (signal c))))
                            ;; Test-file suites intentionally reuse package and
                            ;; type names across cases, which can trigger
                            ;; incidental redefinition style warnings on SBCL.
                            (style-warning #'muffle-warning))
               (entry:compile source)
               nil)
           (source:source-warning (c)
             (string-trim '(#\Space #\Newline)
                          (princ-to-string c)))
           (error (c)
             (string-trim '(#\Space #\Newline)
                          (princ-to-string c))))
      (setf entry:*global-environment* saved-environment))))

;;; The structure of test definition files is described in the header
;;; of ./loader.lisp

(defun run-test-file (pathname)
  "Run the test file at PATHNAME."
  (format t "~&;; --- Running test file: ~A~%" pathname)
  (let ((file (test-file pathname))
        (coalton-impl/settings:*coalton-print-unicode* nil)
        (*readtable* (copy-readtable nil)))
    (loop :for (line number flags description program expected-error)
            :in (coalton-tests/loader:load-test-file file)
          :unless (position :disable flags)
            :do (let ((generated-error (collect-compiler-error program)))
                  (cond ((null generated-error)
                         (is (zerop (length expected-error))
                             "program should have failed to compile: ~A" description))
                        (t
                         (check-string= (format nil "error text.~%~
input file: ~A~%~
line number: ~A~%~
test number: ~A~%~
test header: ~A~%~
~%~
Rerun single test case with:~%~
  (coalton-tests:run-test ~S ~A)~%~
~A~%~
~A"
                                                file line
                                                (or number "(unassigned)")
                                                description
                                                pathname
                                                (or number "N")
                                                (if number "" "after assigning a number to this test case")
                                                (if (zerop (length expected-error))
                                                    "unexpected error"
                                                    "expected error (A) and generated error (B)"))
                                        expected-error
                                        generated-error)))))))

(defun run-test (pathname test-number)
  "Run the test case TEST-NUMBER in the test file at PATHNAME without binding any condition handlers.
This exists to simplify access to point-of-error debugging."
  (let ((file (test-file pathname))
        (run? nil))
    (loop :for (line number flags description program expected-error)
            :in (coalton-tests/loader:load-test-file file)
          :when (eq test-number number)
            :do (entry:compile (source:make-source-string program :name "test"))
                (setf run? t))
    (unless run?
      (error "Test not found: ~A" test-number))))
