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

(defun warn-ineffective-inline-attributes (program env)
  (declare (type parser:program program)
           (type tc:environment env))
  (dolist (form (append (parser:program-defines program)
                       (parser:program-declares program)))
    (multiple-value-bind (attribute name)
        (etypecase form
          (parser:toplevel-define
           (values (parser:toplevel-define-inline form)
                   (parser:node-variable-name (parser:toplevel-define-name form))))
          (parser:toplevel-declare
           (values (parser:toplevel-declare-inline form)
                   (parser:identifier-src-name (parser:toplevel-declare-name form)))))
      (when (and attribute
                 (not (tc:lookup-function env name :no-error t)))
        (source:style-warn "Ineffective inline attribute"
                           (source:note attribute
                                        "inline has no effect on the value ~A" name))))))

(defun entry-point (program)
  (let* ((selections nil)
         (tc:*defer-instance-validation* t)
         (tc:*changed-instance-classes* nil)
         (tc:*instance-selection-hook*
           (lambda (pred instance)
             (push (list pred instance (or *compile-file-truename* *load-truename*
                                          (package-name *package*))) selections))))
    (multiple-value-bind (code env) (%entry-point program)
      (setf env (tc:register-instance-selections env selections))
      (dolist (class tc:*changed-instance-classes*) (tc:validate-instance-selections env class))
      (values code env))))

(defun define-program-types-and-classes (program env)
  "Typecheck and register types, structs, aliases, and classes in dependency order."
  (let ((types (parser:program-types program))
        (structs (parser:program-structs program))
        (aliases (parser:program-type-aliases program))
        (classes (parser:program-classes program)))
    (flet ((def-name (def)
             (parser:identifier-src-name (parser:type-definition-name def)))
           (cls-name (cls)
             (parser:identifier-src-name (parser:toplevel-define-class-name cls)))
           (ref-classes (def)
             (mapcar #'parser:identifier-src-name (parser:collect-referenced-classes def)))
           (ref-types (item)
             (mapcar #'parser:tycon-name (parser:collect-referenced-types item))))
      (let* ((all-type-defs (append types structs aliases))
             (local-class-names (mapcar #'cls-name classes)))
        (unless (and classes
                     (some (lambda (def)
                             (intersection (ref-classes def) local-class-names :test #'eq))
                           all-type-defs))
          (multiple-value-bind (type-definitions instances env)
              (tc:toplevel-define-type types structs aliases env)
            (unless (settings:coalton-release-p)
              (tc:validate-gadt-redefinitions *global-environment* env
                                              (mapcar #'tc:type-definition-name type-definitions)))
            (multiple-value-bind (class-definitions env)
                (tc:toplevel-define-class classes env)
              (return-from define-program-types-and-classes
                (values type-definitions instances class-definitions env)))))
        (tc:check-duplicates
         all-type-defs
         #'def-name
         (lambda (first second)
           (tc:tc-error "Duplicate type definitions"
                        (tc:tc-note first "first definition here")
                        (tc:tc-note second "second definition here"))))
        (tc:check-duplicates
         (mapcan (alexandria:compose #'copy-list #'parser:type-definition-ctors)
                 (append types structs))
         (alexandria:compose #'parser:identifier-src-name #'parser:type-definition-ctor-name)
         (lambda (first second)
           (tc:tc-error "Duplicate constructor definitions"
                        (tc:tc-note first "first definition here")
                        (tc:tc-note second "second definition here"))))
        (tc:check-duplicates
         classes
         #'cls-name
         (lambda (first second)
           (tc:tc-error "Duplicate class definition"
                        (tc:tc-location (parser:toplevel-define-class-head-location first)
                                        "first definition here")
                        (tc:tc-location (parser:toplevel-define-class-head-location second)
                                        "second definition here"))))
        (let ((rem-types types)
              (rem-structs structs)
              (rem-aliases aliases)
              (rem-classes classes)
              (type-definitions nil)
              (instances nil)
              (class-definitions nil))
          (loop :while (or rem-types rem-structs rem-aliases rem-classes)
                :do (let* ((rem-class-names (mapcar #'cls-name rem-classes))
                           (rem-all-types (append rem-types rem-structs rem-aliases))
                           (blocked-type-names
                             (loop :with blocked := (loop :for def :in rem-all-types
                                                          :when (intersection (ref-classes def) rem-class-names :test #'eq)
                                                            :collect (def-name def))
                                   :with changed := t
                                   :while changed
                                   :do (setf changed nil)
                                       (dolist (def rem-all-types)
                                         (let ((name (def-name def)))
                                           (when (and (not (member name blocked :test #'eq))
                                                      (intersection (ref-types def) blocked :test #'eq))
                                             (push name blocked)
                                             (setf changed t))))
                                   :finally (return blocked)))
                           (ready-types (remove-if (lambda (d) (member (def-name d) blocked-type-names :test #'eq)) rem-types))
                           (ready-structs (remove-if (lambda (d) (member (def-name d) blocked-type-names :test #'eq)) rem-structs))
                           (ready-aliases (remove-if (lambda (d) (member (def-name d) blocked-type-names :test #'eq)) rem-aliases)))
                      (when (or ready-types ready-structs ready-aliases)
                        (multiple-value-bind (wave-types wave-instances next-env)
                            (tc:toplevel-define-type ready-types ready-structs ready-aliases env)
                          (unless (settings:coalton-release-p)
                            (tc:validate-gadt-redefinitions *global-environment* next-env
                                                            (mapcar #'tc:type-definition-name wave-types)))
                          (setf env next-env
                                type-definitions (append type-definitions wave-types)
                                instances (append instances wave-instances)
                                rem-types (set-difference rem-types ready-types :test #'eq)
                                rem-structs (set-difference rem-structs ready-structs :test #'eq)
                                rem-aliases (set-difference rem-aliases ready-aliases :test #'eq))))
                      (let* ((rem-type-names (mapcar #'def-name (append rem-types rem-structs rem-aliases)))
                             (blocked-class-names
                               (loop :with blocked := (loop :for cls :in rem-classes
                                                            :when (intersection (ref-types cls) rem-type-names :test #'eq)
                                                              :collect (cls-name cls))
                                     :with changed := t
                                     :while changed
                                     :do (setf changed nil)
                                         (dolist (cls rem-classes)
                                           (let ((name (cls-name cls)))
                                             (when (and (not (member name blocked :test #'eq))
                                                        (intersection (ref-classes cls) blocked :test #'eq))
                                               (push name blocked)
                                               (setf changed t))))
                                     :finally (return blocked)))
                             (ready-classes (remove-if (lambda (c) (member (cls-name c) blocked-class-names :test #'eq))
                                                       rem-classes)))
                        (cond
                          (ready-classes
                           (multiple-value-bind (wave-classes next-env)
                               (tc:toplevel-define-class ready-classes env)
                             (setf env next-env
                                   class-definitions (append class-definitions wave-classes)
                                   rem-classes (set-difference rem-classes ready-classes :test #'eq))))
                          ((not (or ready-types ready-structs ready-aliases))
                           ;; Cyclic or unresolved dependency: run remaining to signal diagnostic.
                           (multiple-value-bind (wave-types wave-instances next-env)
                               (tc:toplevel-define-type rem-types rem-structs rem-aliases env)
                             (setf env next-env
                                   type-definitions (append type-definitions wave-types)
                                   instances (append instances wave-instances)
                                   rem-types nil rem-structs nil rem-aliases nil))
                           (when rem-classes
                             (multiple-value-bind (wave-classes next-env)
                                 (tc:toplevel-define-class rem-classes env)
                               (setf env next-env
                                     class-definitions (append class-definitions wave-classes)
                                     rem-classes nil))))))))
          (values type-definitions instances class-definitions env))))))

(defun %entry-point (program)
  (declare (type parser:program program))

  (let ((*package* (parser:program-lisp-package program))

        (program (parser:rename-variables program))

        (env *global-environment*))

    (setf (parser:program-defines program)
          (tc:resolve-control-flow (parser:program-defines program)))
    (setf (parser:program-instances program)
          (tc:resolve-control-flow (parser:program-instances program)))

    (multiple-value-bind (type-definitions instances class-definitions env)
        (define-program-types-and-classes program env)

        (let ((all-instances
                (append instances
                        (parser:program-instances program)
                        (tc:derive-class-instances (parser:program-types program)
                                                   (parser:program-structs program)
                                                   env)))) 

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

                      (calling-convention-table (make-hash-table :test #'eq))

                      (translation-unit
                        (tc:make-translation-unit
                         :types type-definitions
                         :definitions toplevel-definitions
                         :classes class-definitions
                         :instances toplevel-instances
                         :lisp-forms (parser:program-lisp-forms program)
                         :package *package*)))

                  (loop :for define :in (parser:program-defines program)
                        :for name := (parser:node-variable-name (parser:toplevel-define-name define))
                        :when (parser:toplevel-define-monomorphize define)
                          :do (setf (gethash name monomorphize-table) t)
                        :when (parser:toplevel-define-inline define)
                          :do (setf (gethash name inline-p-table) t)
                        :when (parser:toplevel-define-calling-convention define)
                          :do (setf (gethash name calling-convention-table)
                                    (parser:keyword-src-name
                                     (parser:attribute-calling-convention-type
                                      (parser:toplevel-define-calling-convention define)))))

                  (loop :for declare :in (parser:program-declares program)
                        :for name := (parser:identifier-src-name (parser:toplevel-declare-name declare))
                        :when (parser:toplevel-declare-monomorphize declare)
                          :do (setf (gethash name monomorphize-table) t)
                        :when (parser:toplevel-declare-inline declare)
                          :do (setf (gethash name inline-p-table) t)
                        :when (parser:toplevel-declare-calling-convention declare)
                          :do (setf (gethash name calling-convention-table)
                                    (parser:keyword-src-name
                                     (parser:attribute-calling-convention-type
                                      (parser:toplevel-declare-calling-convention declare)))))

                  (loop :for ty-instance :in ty-instances
                        :for parsed-instance :in all-instances
                        :for method-codegen-inline-p := (tc:ty-class-instance-method-codegen-inline-p ty-instance)
                        :for method-codegen-syms := (tc:ty-class-instance-method-codegen-syms ty-instance)
                        :for class := (tc:lookup-class env (tc:ty-predicate-class (tc:ty-class-instance-predicate ty-instance)))
                        :for class-methods := (tc:ty-class-unqualified-methods class)
                        :do (loop :for (method-codegen-sym . inline-p) :in method-codegen-inline-p
                                  :do (when inline-p (setf (gethash method-codegen-sym inline-p-table) t)))
                            (loop :for class-method :in class-methods
                                  :for method-name := (tc:ty-class-method-name class-method)
                                  :for method-codegen-sym :in method-codegen-syms
                                  :for parsed-method := (find method-name
                                                              (parser:toplevel-define-instance-methods parsed-instance)
                                                              :key (lambda (m)
                                                                     (parser:node-variable-name
                                                                      (parser:instance-method-definition-name m))))
                                  :when (and parsed-method
                                             (parser:instance-method-definition-calling-convention parsed-method))
                                    :do (setf (gethash method-codegen-sym calling-convention-table)
                                              (parser:keyword-src-name
                                               (parser:attribute-calling-convention-type
                                                (parser:instance-method-definition-calling-convention parsed-method))))))

                  (analysis:analyze-translation-unit translation-unit env)

                  (multiple-value-bind (code env)
                      (codegen:compile-translation-unit translation-unit
                                                       monomorphize-table
                                                       inline-p-table
                                                       env
                                                       calling-convention-table)
                    (warn-ineffective-inline-attributes program env)
                    (values code env))))))))))


(defun expression-entry-point (node)
  (let* ((selections nil)
         (tc:*instance-selection-hook*
           (lambda (pred instance)
             (push (list pred instance (or *compile-file-truename* *load-truename*
                                          (package-name *package*))) selections)))
         (code (tc:with-fundep-errors ((source:location node) *global-environment*)
                 (%expression-entry-point node))))
    (setf *global-environment* (tc:register-instance-selections *global-environment* selections))
    `(progn
       (load-time-value
        (progn
          (setf *global-environment*
                (tc:register-instance-selections *global-environment* ,(util:runtime-quote selections)))
          nil)
        t)
       ,code)))

(defun discharge-fundep-preds (env preds subs)
  "Remove PREDS that instance resolution with functional dependency improvement
fully discharges. REDUCE-CONTEXT cannot discharge predicates whose instance
contexts mention variables determined only by functional dependencies."
  (remove-if (lambda (pred)
               (null (tc:solve-fundeps env (list pred) subs)))
             preds))

(defun %expression-entry-point (node)
  (declare (type parser:node node))

  (let ((env *global-environment*))

    (multiple-value-bind (ty preds accessors node subs)
        (tc:infer-expression-type (tc:resolve-control-flow
                                   (parser:rename-variables node))
                                  (tc:make-variable :kind tc:+kstar+ :allow-result-p t)
                                  nil
                                  (tc:make-tc-env :env env))

      (setf subs (nth-value 1 (tc:solve-fundeps env preds subs)))

      (setf accessors (tc:apply-substitution subs accessors))

      (multiple-value-bind (accessors subs_)
          (tc:solve-accessors accessors env)
        (setf subs (tc:compose-substitution-lists subs_ subs))

        (when accessors
          (tc:tc-error "Ambiguous accessor"
                       (source:note (first accessors)
                                    "accessor is ambiguous")))

        (setf preds (tc:apply-substitution subs preds))
        (setf subs (nth-value 1 (tc:solve-fundeps env preds subs)))
        (setf preds (tc:apply-substitution subs preds))

        (multiple-value-setq (preds subs)
          (tc:default-builder-context env nil preds subs))

        (let* ((preds (discharge-fundep-preds env (tc:reduce-context env preds subs) subs))
               (subs (tc:compose-substitution-lists
                      (tc:default-subs env nil preds)
                      subs))
               (preds (discharge-fundep-preds env (tc:reduce-context env preds subs) subs))

               (node (tc:apply-substitution subs node))
               (ty (tc:apply-substitution subs ty))

               (qual-ty (tc:qualify preds ty))
               (scheme (tc:quantify (tc:type-variables qual-ty) qual-ty)))

          (when (null preds)
            (return-from %expression-entry-point
              (codegen:codegen-expression
               (codegen:optimize-expression-node
                (codegen:translate-expression node nil env)
                env)
               env)))

          (tc:tc-error "Unable to codegen"
                       (tc:tc-note node
                                   "expression has ambiguous type ~A"
                                   (tc:type-to-string scheme env))
                       (tc:tc-note node
                                   "Add a type assertion with THE to resolve ambiguity")))))))

(defmacro with-environment-updates (updates &body body)
  "Collect environment updates into a vector bound to UPDATES."
  `(let* ((,updates (make-array 0 :adjustable t :fill-pointer 0))
          (tc:*update-hook* (lambda (name arg-list)
                              (vector-push-extend (cons name arg-list) ,updates))))
     ,@body))

(defun make-environment-updater (update-log)
  "Produce source form of the contents of an environment UPDATE-LOG (i.e., calls to functions in typechecker/environment)."
  (let* ((updates (remove-duplicates (coerce update-log 'list) :test #'code-update-eql))
         (type-names (unless (settings:coalton-release-p)
                       (loop :for (fn name) :in updates
                             :when (eq fn 'tc:set-type) :collect name))))
    `(let ((env *global-environment*)
           (tc:*defer-instance-validation* t)
           (tc:*changed-instance-classes* nil))
       ,@(loop :for (fn . args) :in updates
               :collect `(setf env (,fn env ,@(mapcar #'util:runtime-quote args))))
       (dolist (class tc:*changed-instance-classes*) (tc:validate-instance-selections env class))
       ,@(when type-names
           `((tc:validate-gadt-redefinitions *global-environment* env ',type-names)))
       (setf *global-environment* env)
       (tc:synchronize-type-variable-counter env))))

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
