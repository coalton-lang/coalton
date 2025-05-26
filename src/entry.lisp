(defpackage #:coalton-impl/entry
  (:use
   #:cl)
  (:shadow
   #:compile)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)
   (#:source #:coalton-impl/source)
   (#:tc #:coalton-impl/typechecker)
   (#:analysis #:coalton-impl/analysis)
   (#:codegen #:coalton-impl/codegen))
  (:export
   #:*global-environment*
   #:entry-point                        ; FUNCTION
   #:expression-entry-point             ; FUNCTION
   #:codegen                            ; FUNCTION
   #:compile                            ; FUNCTION
   #:compile-coalton-toplevel           ; FUNCTION
   #:compile-to-lisp                    ; FUNCTION
   ))

(in-package #:coalton-impl/entry)

(defvar *global-environment* (tc:make-default-environment))

(defun entry-point (program)
  (declare (type parser:program program))

  (let ((*package* (parser:program-lisp-package program))

        (program (parser:rename-variables program))

        (env *global-environment*))

    (multiple-value-bind (type-definitions instances env)
        (tc:toplevel-define-type (parser:program-types program)
                                 (parser:program-structs program)
                                 (parser:program-type-aliases program)
                                 env)

      (let ((all-instances (append instances (parser:program-instances program))))

        (multiple-value-bind (class-definitions env)
            (tc:toplevel-define-class (parser:program-classes program)
                                      env)

          (multiple-value-bind (ty-instances env)
              (tc:toplevel-define-instance all-instances env)

            (multiple-value-bind (toplevel-definitions env)
                (tc:toplevel-define (parser:program-defines program)
                                    (parser:program-declares program)
                                    env)

              (multiple-value-bind (toplevel-instances)
                  (tc:toplevel-typecheck-instance ty-instances
                                                  all-instances
                                                  env)

                (setf env (tc:toplevel-specialize (parser:program-specializations program) env))

                (let ((monomorphize-table (make-hash-table :test #'eq))

                      (inline-p-table (make-hash-table :test #'eq))

                      (translation-unit
                        (tc:make-translation-unit
                         :types type-definitions
                         :definitions toplevel-definitions
                         :classes class-definitions
                         :instances toplevel-instances
                         :lisp-forms (parser:program-lisp-forms program)
                         :package *package*)))

                  (loop :for define :in (parser:program-defines program)
                        :when (parser:toplevel-define-monomorphize define)
                          :do (setf (gethash (parser:node-variable-name (parser:toplevel-define-name define))
                                             monomorphize-table)
                                    t)
                        :when (parser:toplevel-define-inline define)
                          :do (setf (gethash (parser:node-variable-name (parser:toplevel-define-name define))
                                             inline-p-table)
                                    t))

                  (loop :for declare :in (parser:program-declares program)
                        :when (parser:toplevel-declare-monomorphize declare)
                          :do (setf (gethash (parser:identifier-src-name (parser:toplevel-declare-name declare))
                                             monomorphize-table)
                                    t)
                        :when (parser:toplevel-declare-inline declare)
                          :do (setf (gethash (parser:identifier-src-name (parser:toplevel-declare-name declare))
                                             inline-p-table)
                                    t))

                  (loop :for ty-instance :in ty-instances
                        :for method-codegen-inline-p := (tc:ty-class-instance-method-codegen-inline-p ty-instance)
                        :do (loop :for (method-codegen-sym . inline-p) :in method-codegen-inline-p
                                  :do (when inline-p (setf (gethash method-codegen-sym inline-p-table) t))))

                  (analysis:analyze-translation-unit translation-unit env)

                  (codegen:compile-translation-unit translation-unit monomorphize-table inline-p-table env))))))))))


(defun expression-entry-point (node)
  (declare (type parser:node node))

  (let ((env *global-environment*))

    (multiple-value-bind (ty preds accessors node subs)
        (tc:infer-expression-type (parser:rename-variables node)
                                  (tc:make-variable)
                                  nil
                                  (tc:make-tc-env :env env))

      (multiple-value-bind (preds subs)
          (tc:solve-fundeps env preds subs)

        (setf accessors (tc:apply-substitution subs accessors))

        (multiple-value-bind (accessors subs_)
            (tc:solve-accessors accessors env)
          (setf subs (tc:compose-substitution-lists subs subs_))

          (when accessors
            (tc:tc-error "Ambiguous accessor"
                         (source:note (first accessors)
                                      "accessor is ambiguous")))

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
                   env))))

            (let* ((tvars
                     (loop :for i :to (1- (length (remove-duplicates (tc:type-variables qual-ty)
                                                                     :test #'equalp)))
                           :collect (tc:make-variable)))
                   (qual-type (tc:instantiate
                               tvars
                               (tc:ty-scheme-type scheme))))

              (tc:tc-error "Unable to codegen"
                           (tc:tc-note node
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
                           (tc:tc-note node
                                       "Add a type assertion with THE to resolve ambiguity")))))))))

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

(defun print-form (stream form &optional (package "CL"))
  "Print a FORM to a STREAM, separated by 2 lines."
  (with-standard-io-syntax
    (let ((*package* (find-package package))
          (*print-case* ':downcase)
          ;; *print-circle* t allows gensym-generated, uninterned
          ;; *symbols to serve as variables in readable source.
          (*print-circle* t)
          (*print-pretty* t)
          (*print-right-margin* 80))
      (prin1 form stream)
      (terpri stream)
      (terpri stream))))

(defun compile-to-lisp (source output)
  "Read Coalton source from SOURCE and write Lisp source to OUTPUT. NAME may be the filename related to the input stream."
  (declare (optimize (debug 3)))
  (with-open-stream (stream (source:source-stream source))
    (parser:with-reader-context stream
      (with-environment-updates updates
        (let* ((program (parser:read-program stream source ':file))
               (program-text (entry-point program))
               (program-package (parser:program-package program))
               (package-name (parser:toplevel-package-name program-package)))
          (print-form output (make-prologue))
          (print-form output (make-environment-updater updates))
          (print-form output (parser:make-defpackage program-package))
          (print-form output `(in-package ,package-name))
          ;; coalton-impl/codegen/program:compile-translation-unit wraps
          ;; definitions in progn to provide a single expression as the
          ;; macroexpansion of coalton-toplevel: unwrap for better
          ;; readability
          (dolist (form (cdr program-text))
            (print-form output form package-name)))))))

(defun codegen (source)
  "Compile Coalton source from SOURCE and return Lisp program text. NAME may be the filename related to the input stream."
  (with-output-to-string (output)
    (compile-to-lisp source output)))

(defun compile (source &key (load t) (output-file nil))
   "Compile Coalton code in SOURCE, returning the pathname of the generated .fasl file. If OUTPUT-FILE is nil, the built-in compiler default output location will be used."
   (uiop:with-temporary-file (:stream lisp-stream
                              :pathname lisp-file
                              :type "lisp"
                              :direction ':output)
     (compile-to-lisp source lisp-stream)
     :close-stream
     (cond ((null output-file)
            (setf output-file (compile-file lisp-file)))
           (t
            (compile-file lisp-file :output-file output-file)))
     (when load
       (load output-file))
     output-file))
