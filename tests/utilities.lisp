(in-package #:coalton-tests)

(defun run-coalton-tests ()
  (run-package-tests
   :packages '(:coalton-tests
               :quil-coalton-tests
               :thih-coalton-tests
               :coalton-json-tests)
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

                ;; Setup eclector readtable
                (eclector.readtable:*readtable*
                  (eclector.readtable:copy-readtable eclector.readtable:*readtable*))

                ;; Read unspecified floats as double floats
                (*read-default-float-format* 'double-float)

                (env coalton-impl::*global-environment*)

                (file (parser:make-coalton-file :stream stream :name "<unknown>"))

                (program (parser:make-program :package *package* :file file))

                (attributes (make-array 0 :adjustable t :fill-pointer t)))
           (progn
             (loop :named parse-loop
                   :with elem := nil

                   :do (setf elem (eclector.concrete-syntax-tree:read stream nil 'eof))

                   :when (eq elem 'eof)
                     :do (return-from parse-loop)

                   :do (when (and (parser:parse-toplevel-form elem program attributes file)
                                  (plusp (length attributes)))
                         (util:coalton-bug "parse-toplevel-form indicated that a form was parsed but did not consume all attributes")))

             (unless (zerop (length attributes))
               (error 'parse-error
                      :err (parser:coalton-error
                            :span (cst:source (cdr (aref attributes 0)))
                            :file file
                            :message "Orphan attribute"
                            :primary-note "attribute must be attached to another form"))))

           (setf (parser:program-types program) (nreverse (parser:program-types program)))
           (setf (parser:program-declares program) (nreverse (parser:program-declares program)))
           (setf (parser:program-defines program) (nreverse (parser:program-defines program)))
           (setf (parser:program-classes program) (nreverse (parser:program-classes program)))

           (multiple-value-bind (type-definitions env)
               (coalton-impl/typechecker2/define-type::toplevel-define-type (parser:program-types program) file env)
             (declare (ignore type-definitions))

             (let ((env
                     (coalton-impl/typechecker2/define::toplevel-define
                      (parser:program-defines program)
                      (parser:program-declares program)
                      file
                      env)))

               (when expected-types
                 (loop :for (unparsed-symbol . unparsed-type) :in expected-types
                       :for symbol := (intern (string-upcase unparsed-symbol) *package*)

                       :for stream := (make-string-input-stream unparsed-type)
                       :for file := (parser:make-coalton-file :stream stream :name "<unknown>")

                       :for ast-type := (parser:parse-qualified-type
                                         (eclector.concrete-syntax-tree:read stream)
                                         file)
                       :for parsed-type := (coalton-impl/typechecker2/parse-type::parse-ty-scheme ast-type env file)
                       :do (is (equalp
                                (tc:lookup-value-type env symbol)
                                parsed-type)))))

             (values)))
      (delete-package *package*))))

(defun run-coalton-toplevel-walker (toplevel)
  (coalton-impl::collect-toplevel-forms toplevel))

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
                               :suffix "fasl"
                               :keep t)
      (compile-file input-file :output-file output-file)
      (load output-file)
      (values input-file output-file))))

(defmacro with-coalton-compilation ((&key package (muffle 'cl:style-warning)) &body coalton-code)
  `(handler-bind
       ((,muffle #'muffle-warning))
     (compile-and-load-forms '(,@(when package `((cl:in-package ,package)))
                               ,@coalton-code))))
