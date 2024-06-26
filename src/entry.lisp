(defpackage #:coalton-impl/entry
  (:use
   #:cl)
  (:local-nicknames
   (#:se #:source-error)
   (#:settings #:coalton-impl/settings)
   (#:stream #:coalton-impl/stream)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:analysis #:coalton-impl/analysis)
   (#:codegen #:coalton-impl/codegen))
  (:export
   #:*global-environment*
   #:entry-point                        ; FUNCTION
   #:expression-entry-point             ; FUNCTION
   #:compile-coalton                    ; FUNCTION
   #:compile-coalton-toplevel           ; FUNCTION
   ))

(in-package #:coalton-impl/entry)

(defvar *global-environment* (tc:make-default-environment))

(defun entry-point (program)
  (declare (type parser:program program))

  (let* ((*package* (parser:program-package program))

         (program (parser:rename-variables program))

         (file (parser:program-file program))

         (env *global-environment*))

    (multiple-value-bind (type-definitions instances env)
        (tc:toplevel-define-type (parser:program-types program)
                                 (parser:program-structs program)
                                 file
                                 env)

      (multiple-value-bind (class-definitions env)
          (tc:toplevel-define-class (parser:program-classes program)
                                    file
                                    env)

        (multiple-value-bind (ty-instances env)
            (tc:toplevel-define-instance (append instances (parser:program-instances program)) env file)

          (multiple-value-bind (toplevel-definitions env)
              (tc:toplevel-define (parser:program-defines program)
                                  (parser:program-declares program)
                                  file
                                  env)

            (multiple-value-bind (toplevel-instances)
                (tc:toplevel-typecheck-instance ty-instances
                                                (append instances (parser:program-instances program))
                                                env
                                                file)

              (setf env (tc:toplevel-specialize (parser:program-specializations program) env file))

              (let ((monomorphize-table (make-hash-table :test #'eq))

                    (translation-unit
                      (tc:make-translation-unit
                       :types type-definitions
                       :definitions toplevel-definitions
                       :classes class-definitions
                       :instances toplevel-instances
                       :package *package*)))

                (loop :for define :in (parser:program-defines program)
                      :when (parser:toplevel-define-monomorphize define)
                        :do (setf (gethash (parser:node-variable-name (parser:toplevel-define-name define))
                                           monomorphize-table)
                                  t))

                (loop :for declare :in (parser:program-declares program)
                      :when (parser:toplevel-declare-monomorphize declare)
                        :do (setf (gethash (parser:identifier-src-name (parser:toplevel-declare-name declare))
                                           monomorphize-table)
                                  t))

                (analysis:analyze-translation-unit translation-unit env file)

                (codegen:compile-translation-unit translation-unit monomorphize-table env)))))))))


(defun expression-entry-point (node file)
  (declare (type parser:node node)
           (type se:file file))

  (let ((env *global-environment*))

    (multiple-value-bind (ty preds accessors node subs)
        (tc:infer-expression-type (parser:rename-variables node)
                                  (tc:make-variable)
                                  nil
                                  (tc:make-tc-env :env env)
                                  file)

      (multiple-value-bind (preds subs)
          (tc:solve-fundeps env preds subs)

        (setf accessors (tc:apply-substitution subs accessors))

        (multiple-value-bind (accessors subs_)
            (tc:solve-accessors accessors file env)
          (setf subs (tc:compose-substitution-lists subs subs_))

          (when accessors
            (error 'tc:tc-error
                   :err (se:source-error
                         :span (tc:accessor-source (first accessors))
                         :file file
                         :message "Ambiguous accessor"
                         :primary-note "accessor is ambiguous")))

          (let* ((preds (tc:reduce-context env preds subs))
                 (subs (tc:compose-substitution-lists
                        (tc:default-subs env nil preds)
                        subs))
                 (preds (tc:reduce-context env preds subs))

                 (node (tc:apply-substitution subs node))
                 (ty (tc:apply-substitution subs ty))

                 (qual-ty (tc:qualify preds ty))
                 (scheme (tc:quantify (tc:type-variables qual-ty) qual-ty)))

            (when (null preds)
              (return-from expression-entry-point
                (let ((node (codegen:optimize-node
                             (codegen:translate-expression node nil env)
                             env)))
                  (codegen:codegen-expression 
                   (codegen:direct-application
                    node
                    (codegen:make-function-table env))
                   nil
                   env))))

            (let* ((tvars
                     (loop :for i :to (1- (length (remove-duplicates (tc:type-variables qual-ty)
                                                                     :test #'equalp)))
                           :collect (tc:make-variable)))
                   (qual-type (tc:instantiate
                               tvars
                               (tc:ty-scheme-type scheme))))

              (error 'tc:tc-error
                     :err (se:source-error
                           :span (tc:node-source node)
                           :file file
                           :message "Unable to codegen"
                           :primary-note (format nil
                                                 "expression has type ~A~{ ~S~}.~{ (~S)~} => ~S with unresolved constraint~A ~S"
                                                 (if settings:*coalton-print-unicode*
                                                     "∀"
                                                     "FORALL")
                                                 tvars
                                                 (tc:qualified-ty-predicates qual-type)
                                                 (tc:qualified-ty-type qual-type)
                                                 (if (= (length (tc:qualified-ty-predicates qual-type)) 1)
                                                     ""
                                                     "s")
                                                 (tc:qualified-ty-predicates qual-type))
                           :notes
                           (list
                            (se:make-source-error-note
                             :type :secondary
                             :span (tc:node-source node)
                             :message "Add a type assertion with THE to resolve ambiguity")))))))))))

(defmacro with-environment-updates (updates &body body)
  "Collect environment updates into a vector bound to UPDATES."
  `(let* ((,updates (make-array 0 :adjustable t :fill-pointer 0))
          (tc:*update-hook* (lambda (name arg-list)
                              (vector-push-extend (cons name arg-list) ,updates))))
     ,@body))

(defun make-environment-updater (update-log)
  "Produce source form of the contents of an environment UPDATE-LOG (i.e., calls to functions in typechecker/environment)."
  (let ((updates (remove-duplicates (coerce update-log 'list) :test #'code-update-eql)))
    `(let ((env *global-environment*))
       ,@(loop :for (fn . args) :in updates
               :collect `(setf env (,fn env ,@(mapcar #'util:runtime-quote args))))
       (setf *global-environment* env))))

(defun compile-coalton-toplevel (program)
  "Compile PROGRAM and return a single form suitable for direct inclusion by Lisp compiler. For implementing coalton-toplevel macro."
  (with-environment-updates updates
    (multiple-value-bind (program env)
        (entry-point program)
      (setf *global-environment* env)
      `(progn
         ,(make-prologue)
         ,(make-environment-updater updates)
         ,program))))

(defun code-update-eql (a b)
  "Compare environment updates, returning t for set-code updates of the same symbol."
  (and (eql (first a) 'coalton-impl/typechecker/environment:set-code)
       (eql (first b) 'coalton-impl/typechecker/environment:set-code)
       (eql (second a)
            (second b))))

(defun make-prologue ()
  "Return source form of an assertion that prevents mixing development and release modes."
  `(eval-when (:load-toplevel)
     (unless (eq (settings:coalton-release-p) ,(settings:coalton-release-p))
       ,(if (settings:coalton-release-p)
            `(error "~A was compiled in release mode but loaded in development."
                    ,(or *compile-file-pathname* *load-truename*))
            `(error "~A was compiled in development mode but loaded in release."
                    ,(or *compile-file-pathname* *load-truename*))))))

(defun print-form (stream form)
  "Print a FORM to a STREAM, separated by 2 lines."
  (with-standard-io-syntax
    (let ((*package* (find-package "CL")))
      (prin1 form stream)
      (terpri stream)
      (terpri stream))))

(defun compile-to-lisp (name input output)
  "Read Coalton source from INPUT and write Lisp source to OUTPUT."
  (parser:with-reader-context input
    (with-environment-updates updates
      (let* ((file (se:make-file
                    :stream input
                    :name name))
             (program (entry-point (parser:read-program input file :mode :file))))
        (print-form output (make-prologue))
        (print-form output (make-environment-updater updates))
        (print-form output program)))))

(defun compile-coalton (coal-file &key (output-file nil) (format :default))
  "Compile a Coalton file. The FORMAT keyword argument controls how the output is generated:

  :DEFAULT Generate Lisp source, and compile immediately to .fasl. If no output file is specified, compiled Lisp code will be written to a temporary file, and that path will be returned.
  :SOURCE  Write compiled Lisp code to console or specified output file."
  (with-open-file (stream coal-file
                               :direction ':input
                               :element-type 'character)
    (let* ((coal-stream (stream:make-char-position-stream stream))
           (coal-file-name (etypecase coal-file
                             (pathname (pathname-name coal-file))
                             (string coal-file))))
      (ecase format
        (:default
         (uiop:with-temporary-file (:stream lisp-stream
                                    :pathname lisp-file
                                    :suffix "lisp"
                                    :direction ':output
                                    :keep t)
           (compile-to-lisp coal-file-name coal-stream lisp-stream)
           :close-stream
           (compile-file lisp-file :output-file output-file)))
        (:source
         (cond
           (output-file
            (with-open-file (lisp-stream output-file
                                         :direction ':output
                                         :element-type 'character
                                         :if-exists ':supersede)
              (compile-to-lisp coal-file-name coal-stream lisp-stream)))
           (t
            (with-output-to-string (lisp-stream)
              (compile-to-lisp coal-file-name coal-stream lisp-stream)))))))))
