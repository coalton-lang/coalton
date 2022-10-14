(in-package #:coalton-tests)

(defun run-coalton-tests ()
  (run-package-tests
   :packages '(:coalton-tests
               :quil-coalton-tests
               :thih-coalton-tests)
   :interactive t))

(defun set-equalp (set1 set2)
  (null (set-exclusive-or set1 set2 :test #'equalp)))

(defun dag-equalp (dag1 dag2)
  ;; XXX: This will not check ordering of edges within vertices
  (set-equalp dag1 dag2))

(defun check-coalton-types (toplevel-string &rest expected-types)
  (let ((*package* (make-package (or (and fiasco::*current-test*
                                          (fiasco::name-of fiasco::*current-test*))
                                     "COALTON-TEST-COMPILE-PACKAGE")
                                 :use '("COALTON" "COALTON-PRELUDE"))))
    (unwind-protect
         (let* ((stream (make-string-input-stream toplevel-string))

                (file (error:make-coalton-file :stream stream :name "<test>"))

                (program (parser:read-program stream file :mode :test)))

           (multiple-value-bind (program env)
               (entry:entry-point program)
             (declare (ignore program))
             
             (when expected-types
               (loop :for (unparsed-symbol . unparsed-type) :in expected-types
                     :for symbol := (intern (string-upcase unparsed-symbol) *package*)

                     :for stream := (make-string-input-stream unparsed-type)
                     :for file := (error:make-coalton-file :stream stream :name "<unknown>")

                     :for ast-type := (parser:parse-qualified-type
                                       (eclector.concrete-syntax-tree:read stream)
                                       file)
                     :for parsed-type := (tc:parse-ty-scheme ast-type env file)
                     :do (is (equalp
                              (tc:lookup-value-type env symbol)
                              parsed-type)))))

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
