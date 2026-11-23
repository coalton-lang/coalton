(in-package #:coalton-tests)

(deftest test-environment-copy-with ()
  (let* ((env (tc:make-default-environment))
         (replacement (tc:make-default-environment))
         (package (find-package '#:coalton-impl/typechecker/environment))
         (slots '(value-environment type-environment constructor-environment
                  type-alias-environment struct-environment class-environment
                  fundep-environment instance-environment function-environment
                  name-environment method-inline-environment code-environment
                  specialization-environment source-name-environment))
         (accessors (mapcar (lambda (slot)
                              (alexandria:format-symbol package "ENVIRONMENT-~A" slot))
                            slots))
         (copy (env::update-environment env)))
    (is (not (eq copy env)))
    (dolist (accessor accessors)
      (is (eq (funcall accessor copy) (funcall accessor env))))
    (loop :for slot :in slots
          :for accessor :in accessors
          :for value := (funcall accessor replacement)
          :for updated := (apply #'env::update-environment env
                                 (list (alexandria:make-keyword slot) value))
          :do (dolist (other accessors)
                (is (eq (funcall other updated)
                        (if (eq accessor other)
                            value
                            (funcall other env))))))
    (is (tc:lookup-type env 'coalton:Boolean))
    (is (tc:lookup-constructor env 'coalton:True))
    (signals util::coalton-bug
      ;; Exercise the missing-slot error at run time, not during compilation.
      (locally (declare (notinline env::make-environment))
        (env::make-environment)))))

(deftest test-environment-update-log ()
  (let ((env (tc:make-default-environment))
        (updates nil))
    (let* ((tc:*update-hook* (lambda (name args)
                              (push (cons name args) updates)))
           (updated (tc:set-code env 'test-code '(test-code))))
      (is (equal '((tc:set-code test-code (test-code))) updates))
      (is (null (tc:lookup-code env 'test-code :no-error t)))
      (is (equal '(test-code) (tc:lookup-code updated 'test-code)))
      (env::update-environment updated)
      (is (= 1 (length updates))))))

(deftest gadt-evidence-redefinition ()
  ;; Inspect compiler environments only. In particular, never call code
  ;; compiled against one version after accepting an incompatible version.
  (with-codegen-test-environment
    (let* ((base entry:*global-environment*)
           (original
             "(repr :enum)
              (define-type (Tag :a :b)
                (declare Same (Tag :x :x)) (declare MixedTag (Tag Integer String)))")
           (changed
             "(repr :enum)
              (define-type (Tag :a :b)
                (declare Same (Tag :x :y)) (declare MixedTag (Tag Integer String)))"))
      (codegen-test-inspect original)
      (codegen-test-inspect
       "(declare cast-tagged (Tag :a :b * :a -> Optional :b))
        (define (cast-tagged tag x) (match tag ((Same) (Some x)) ((MixedTag) None)))")
      (codegen-test-inspect
       "(define-type-alias TextIndex String)
        (repr :enum)
        (define-type (Tag :left :right)
          (declare MixedTag (Tag Integer TextIndex)) (declare Same (Tag :same :same)))")
      (unless (coalton-impl/settings:coalton-release-p)
        (let ((old entry:*global-environment*))
          (dolist (invalid
                    (list changed
                          "(define-type Other (Same String))"
                          "(repr :enum) (define-type (Tag :a :b) (declare Same (Tag :x :x)))"
                          "(repr :enum) (define-type (Tag :a :b) (declare Same (Tag :x :x))
                             (declare MixedTag (Tag Integer String)) (declare BothTag (Tag Integer Integer)))"))
            (signals coalton-impl/redef-detection:incompatible-gadt-redefinition
              (codegen-test-inspect invalid)))
          (is (eq :aborted
                  (restart-case
                      (handler-bind ((coalton-impl/redef-detection:incompatible-redefinition
                                       (lambda (c)
                                         (invoke-restart (find-restart
                                                          'coalton-impl/redef-detection:abort-redefinition c)))))
                        (codegen-test-inspect changed))
                    (abort () :aborted))))
          (is (eq old entry:*global-environment*))
          (finishes (codegen-test-inspect
                     "(declare still-same (Tag String String)) (define still-same Same)"))
          ;; Replay updates compiled in a different environment. Do not eval
          ;; any program forms: the updater alone must enforce the contract.
          (let ((updater
                  (entry::with-environment-updates log
                    (let ((entry:*global-environment* base)) (codegen-test-inspect changed))
                    (entry::make-environment-updater log))))
            (signals coalton-impl/redef-detection:incompatible-gadt-redefinition (eval updater))
            (let ((warned nil)
                  (coalton-impl/settings:*auto-continue-redefinition* t))
              (handler-bind ((warning (lambda (c) (setf warned t) (muffle-warning c))))
                (eval updater))
              (is warned)
              (finishes (codegen-test-inspect
                         "(declare now-unrelated (Tag String Integer)) (define now-unrelated Same)"))))))
      (codegen-test-inspect "(define-type (Boxed :a) (declare Pack (:x -> Boxed :x)))")
      (unless (coalton-impl/settings:coalton-release-p)
        (signals coalton-impl/redef-detection:incompatible-gadt-redefinition
          (codegen-test-inspect
           "(repr :transparent) (define-type (Boxed :a) (declare Pack (:x -> Boxed :x)))"))
        (handler-bind ((coalton-impl/redef-detection:incompatible-gadt-redefinition
                         (lambda (c)
                           (invoke-restart (find-restart 'coalton-impl/redef-detection:continue-anyway c))))
                       (warning #'muffle-warning))
          (codegen-test-inspect
           "(define-type (Boxed :a) (declare Pack (:hidden -> Boxed Integer)))"))
        (finishes (codegen-test-inspect
                   "(declare pack-string (String -> Boxed Integer)) (define (pack-string s) (Pack s))")))
      ;; Ordinary ADTs have no GADT evidence contract to invalidate.
      (codegen-test-inspect "(define-type Plain (Item Integer))")
      (finishes (codegen-test-inspect "(define-type Plain (Item String))")))))

(deftest test-persist-value ()
  "Test that the value environment persists across loads and fasl loads"
  (let ((test-expr
          '((in-package #:coalton-native-tests)
            (coalton-toplevel
             (define (test-id a) a)))))

    ;; Write out our test program to a temporary file
    (uiop:with-temporary-file (:stream f
                               :pathname input-file
                               :suffix "lisp"
                               :direction :output)

      (dolist (expr test-expr)
        (write-string (princ-to-string expr) f))

      ;; Close the file so we can start testing
      :close-stream

      ;; Create a temp output fasl file
      (uiop:with-temporary-file (:pathname output-file
                                 :type #+ccl (pathname-type ccl:*.fasl-pathname*)
                                       #+(not ccl) "fasl")
        (handler-bind
            ;; Tell the compiler to be quiet
            ((style-warning (lambda (c)
                              (declare (ignore c))
                              (invoke-restart 'muffle-warning))))
          (let ((entry:*global-environment* (tc:make-default-environment)))
            ;; Be sure that the environment is clean
            (is (null (tc:lookup-value-type entry:*global-environment* 'coalton-native-tests::test-id :no-error t)))

            ;; Load our coalton code and check for function availability
            (load input-file :verbose nil)

            ;; Be sure that we have the correct function type
            (is (tc:lookup-value-type entry:*global-environment* 'coalton-native-tests::test-id :no-error t))

            ;; Ok, now clean the environment
            (setf entry:*global-environment* (tc:make-default-environment))
            (is (null (tc:lookup-value-type entry:*global-environment* 'coalton-native-tests::test-id :no-error t)))

            ;; And compile the file
            (compile-file input-file :output-file output-file :verbose nil :print nil)

            ;; After compiling we should have the function available
            (is (not (null (tc:lookup-value-type entry:*global-environment* 'coalton-native-tests::test-id :no-error t))))
            ;; Clean
            (setf entry:*global-environment* (tc:make-default-environment))
            (is (null (tc:lookup-value-type entry:*global-environment* 'coalton-native-tests::test-id :no-error t)))

            ;; Now load the fasl
            (load output-file :verbose nil)

            ;; Now we should have our function
            (is (not (null (tc:lookup-value-type entry:*global-environment* 'coalton-native-tests::test-id :no-error t))))

            t))))))

#+sbcl
(deftest test-compiled-instance-interface ()
  (let* ((provider (asdf:system-relative-pathname "coalton" "tests/instance-interface/provider.lisp"))
         (client (asdf:system-relative-pathname "coalton" "tests/instance-interface/client.lisp"))
         (fasl-type (pathname-type (compile-file-pathname provider)))
         ;; Use the same checkouts and dependencies in the fresh Lisp, without
         ;; relying on the user's init file or a particular Quicklisp install.
         (directories (remove-duplicates
                       (loop :for name :in (asdf:already-loaded-systems)
                             :for path := (asdf:system-source-file name)
                             :when path :collect (uiop:pathname-directory-pathname path))
                       :test #'equal)))
    (uiop:with-temporary-file (:pathname provider-fasl :type fasl-type)
      (let ((entry:*global-environment* entry:*global-environment*))
        (is (not (nth-value 2 (compile-file provider :output-file provider-fasl)))))
      (uiop:with-temporary-file (:pathname client-fasl :type fasl-type)
        (uiop:with-temporary-file (:stream out :pathname script :type "lisp")
          (with-standard-io-syntax
            (dolist (form `((require :asdf)
                            (asdf:initialize-source-registry
                             '(:source-registry ,@(loop :for dir :in directories :collect `(:directory ,dir))
                               :ignore-inherited-configuration))
                            ,@(when (coalton-impl/settings:coalton-release-p)
                                '((pushnew :coalton-release *features*)))
                            (asdf:load-system "coalton")
                            (load ,provider-fasl)
                            (assert (not (nth-value 2 (compile-file ,client :output-file ,client-fasl))))
                            (load ,client-fasl)))
              (print form out)))
          :close-stream
          (uiop:run-program (list (namestring sb-ext:*runtime-pathname*) "--noinform" "--no-userinit"
                                  "--no-sysinit" "--script" (namestring script))
                            :output *standard-output* :error-output *error-output*))))))
