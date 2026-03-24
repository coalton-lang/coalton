(defpackage #:coalton-impl/typechecker/type-string
  (:use
   #:cl
   #:coalton-impl/typechecker/base
   #:coalton-impl/typechecker/kinds
   #:coalton-impl/typechecker/types
   #:coalton-impl/typechecker/predicate
   #:coalton-impl/typechecker/scheme
   #:coalton-impl/typechecker/environment)
  (:local-nicknames
   (#:settings #:coalton-impl/settings))
  (:export
   #:render-type-object
   #:type-to-string))

(in-package #:coalton-impl/typechecker/type-string)

(defun %type-name (env symbol)
  (let ((entry (and env (lookup-type env symbol :no-error t))))
    (or (and entry
             (type-entry-source-name entry))
        (symbol-name symbol))))

(defun %class-name (env symbol)
  (let ((entry (and env (lookup-class env symbol :no-error t))))
    (or (and entry
             (ty-class-source-name entry))
        (symbol-name symbol))))

(defun %arrow-string ()
  (if settings:*coalton-print-unicode*
      " → "
      " -> "))

(defun %implication-string ()
  (if settings:*coalton-print-unicode*
      " ⇒ "
      " => "))

(defun %forall-string ()
  (if settings:*coalton-print-unicode*
      "∀"
      "forall"))

(defun %write-default-type-name (stream symbol env)
  (cond
    ((eq (symbol-package symbol) (find-package '#:keyword))
     (format stream "~S" symbol))
    (t
     (write-string (%type-name env symbol) stream))))

(defun %write-default-class-name (stream symbol env)
  (write-string (%class-name env symbol) stream))

(defun %write-default-tyvar (stream ty)
  (cond
    (*coalton-pretty-print-tyvars*
     (let ((name (tycon-name (pprint-tvar ty))))
       (cond
         ((eq (symbol-package name) (find-package '#:keyword))
          (format stream "~S" name))
         (t
          (write-string (symbol-name name) stream)))))
    (t
     (write-string "#T" stream)
     (write (tyvar-id ty) :stream stream))))

(defun %write-default-keyword-name (stream keyword)
  (format stream ":~(~A~)" (symbol-name keyword)))

(defun %call-with-existing-or-fresh-tyvar-context (thunk)
  (cond
    ((boundp '*pprint-tyvar-dict*)
     (with-pprint-variable-scope ()
       (funcall thunk)))
    (t
     (with-pprint-variable-context ()
       (funcall thunk)))))

(defun %write-symbolic-type-name (stream symbol env)
  (declare (ignore env))
  (write symbol :stream stream))

(defun %write-symbolic-class-name (stream symbol env)
  (declare (ignore env))
  (write symbol :stream stream))

(defun %write-symbolic-tyvar (stream ty)
  (cond
    (*coalton-pretty-print-tyvars*
     (pprint-ty stream (pprint-tvar ty)))
    (t
     (write-string "#T" stream)
     (write (tyvar-id ty) :stream stream))))

(defun render-type-object (stream object &key
                                          env
                                          (type-name-writer #'%write-default-type-name)
                                          (class-name-writer #'%write-default-class-name)
                                          (tyvar-writer #'%write-default-tyvar)
                                          (keyword-name-writer #'%write-default-keyword-name)
                                          (arrow-string (%arrow-string))
                                          (implication-string (%implication-string))
                                          (forall-string (%forall-string)))
  (labels ((write-parenthesized (thunk)
             (write-char #\( stream)
             (funcall thunk)
             (write-char #\) stream))
           (function-context-parenthesized-p (context)
             (member context '(:input :app-head :app-arg) :test #'eq))
           (result-context-parenthesized-p (context)
             (member context '(:input :app-head :app-arg) :test #'eq))
           (application-context-parenthesized-p (context)
             (eq context ':app-arg))
           (write-output-pack (output-types single-output-context)
             (cond
               ((null output-types)
                (write-string "Void" stream))
               ((null (cdr output-types))
                (write-ty (car output-types) single-output-context))
               (t
               (loop :for output :in output-types
                      :for firstp := t :then nil
                      :do
                         (unless firstp
                           (write-string " * " stream))
                         (write-ty output ':input)))))
           (write-function-ty (ty context)
             (flet ((body ()
                      (let ((inputs (function-ty-positional-input-types ty))
                            (keywords (function-ty-keyword-input-types ty))
                            (keyword-open-p (function-ty-keyword-open-p ty)))
                        (cond
                          ((and (null inputs) (null keywords) (not keyword-open-p))
                           (write-string "Void" stream))
                          (t
                           (loop :for input :in inputs
                                 :for firstp := t :then nil
                                 :do
                                    (unless firstp
                                      (write-string " * " stream))
                                    (write-ty input ':input))
                           (when (or keywords keyword-open-p)
                             (when inputs
                               (write-char #\Space stream))
                             (write-string "&key " stream)
                             (loop :for entry :in keywords
                                   :for firstp := t :then nil
                                   :do
                                      (unless firstp
                                        (write-char #\Space stream))
                                      (write-char #\( stream)
                                      (funcall keyword-name-writer
                                               stream
                                               (keyword-ty-entry-keyword entry))
                                      (write-char #\Space stream)
                                      (write-ty (keyword-ty-entry-type entry) ':input)
                                      (write-char #\) stream))
                             (when keyword-open-p
                               (when keywords
                                 (write-char #\Space stream))
                               (write-string "..." stream)))))
                        (write-string arrow-string stream)
                        (write-output-pack (function-ty-output-types ty) ':output))))
               (cond
                 ((function-context-parenthesized-p context)
                  (write-parenthesized #'body))
                 (t
                  (body)))))
           (write-arrow-ty (ty context)
             (flet ((body ()
                      (write-ty (function-type-from ty) ':input)
                      (write-string arrow-string stream)
                      (write-ty (function-type-to ty) ':output)))
               (cond
                 ((function-context-parenthesized-p context)
                  (write-parenthesized #'body))
                 (t
                  (body)))))
           (write-result-ty (ty context)
             (flet ((body ()
                      (write-output-pack (result-ty-output-types ty) ':output)))
               (cond
                 ((result-context-parenthesized-p context)
                  (write-parenthesized #'body))
                 (t
                  (body)))))
           (write-application-ty (ty context)
             (let* ((tcon ty)
                    (tcon-args (loop :while (tapp-p tcon)
                                     :collect (tapp-to tcon)
                                     :do (setf tcon (tapp-from tcon)))))
               (flet ((body ()
                        (cond
                          ((and (typep tcon 'tycon)
                                (simple-kind-p (tycon-kind tcon))
                                (<= (length tcon-args)
                                    (kind-arity (tycon-kind tcon))))
                           (write-ty tcon ':app-head)
                           (dolist (arg (reverse tcon-args))
                             (write-char #\Space stream)
                             (write-ty arg ':app-arg)))
                          (t
                           (write-ty (tapp-from ty) ':app-head)
                           (write-char #\Space stream)
                           (write-ty (tapp-to ty) ':app-arg)))))
                 (cond
                   ((application-context-parenthesized-p context)
                    (write-parenthesized #'body))
                   (t
                    (body))))))
           (write-ty (ty context)
             (declare (type stream stream)
                      (type ty ty))

             (when (and (eq *coalton-type-printing-mode* ':aliases)
                        (ty-alias ty))
               (write-type-object (first (ty-alias ty)))
               (return-from write-ty ty))

             (when (and (eq *coalton-type-printing-mode* ':types-and-aliases)
                        (ty-alias ty))
               (write-char #\[ stream)
               (dolist (alias (ty-alias ty))
                 (write-type-object alias)
                 (write-string " := " stream)))

             (etypecase ty
               (tyvar
                (funcall tyvar-writer stream ty))
               (tycon
                (funcall type-name-writer stream (tycon-name ty) env))
               (result-ty
                (write-result-ty ty context))
               (function-ty
                (write-function-ty ty context))
               (tapp
                (cond
                  ((function-type-p ty)
                   (write-arrow-ty ty context))
                  (t
                   (write-application-ty ty context))))
               (tgen
                (write-string "#GEN" stream)
                (write (tgen-id ty) :stream stream)))

             (when (and (eq *coalton-type-printing-mode* ':types-and-aliases)
                        (ty-alias ty))
               (write-char #\] stream))

             ty)
           (write-predicate (predicate)
             (funcall class-name-writer stream (ty-predicate-class predicate) env)
             (dolist (type (ty-predicate-types predicate))
               (write-char #\Space stream)
               (write-ty type ':app-arg)))
           (write-qualified-ty (qualified-ty)
             (let ((predicates (qualified-ty-predicates qualified-ty)))
               (cond
                 ((null predicates)
                  (write-ty (qualified-ty-type qualified-ty) ':top))
                 ((null (cdr predicates))
                  (write-predicate (car predicates))
                  (write-string implication-string stream)
                  (write-ty (qualified-ty-type qualified-ty) ':top))
                 (t
                  (loop :for pred :in predicates
                        :for firstp := t :then nil
                        :do
                           (unless firstp
                             (write-char #\Space stream))
                           (write-char #\( stream)
                           (write-predicate pred)
                           (write-char #\) stream))
                  (write-string implication-string stream)
                  (write-ty (qualified-ty-type qualified-ty) ':top)))))
           (write-scheme (scheme)
             (cond
               ((null (ty-scheme-kinds scheme))
                (write-type-object (ty-scheme-type scheme)))
               (t
                (let* ((types (ty-scheme-instantiation-types scheme))
                       (new-type (instantiate types (ty-scheme-type scheme))))
                  (write-string forall-string stream)
                  (dolist (type types)
                    (write-char #\Space stream)
                    (write-ty type ':top))
                  (write-string ". " stream)
                  (write-type-object new-type)))))
           (write-class-instance (instance)
             (let ((ctx (ty-class-instance-constraints instance))
                   (pred (ty-class-instance-predicate instance)))
               (cond
                 ((null ctx)
                  (write-predicate pred))
                 ((null (cdr ctx))
                  (write-predicate (car ctx))
                  (write-string implication-string stream)
                  (write-predicate pred))
                 (t
                  (loop :for constraint :in ctx
                        :for firstp := t :then nil
                        :do
                           (unless firstp
                             (write-char #\Space stream))
                           (write-char #\( stream)
                           (write-predicate constraint)
                           (write-char #\) stream))
                  (write-string implication-string stream)
                  (write-predicate pred)))))
           (write-type-object (object)
             (typecase object
               (ty-scheme
                (write-scheme object))
               (qualified-ty
                (write-qualified-ty object))
               (ty-predicate
                (write-predicate object))
               (ty-class-instance
                (write-class-instance object))
               (ty
                (write-ty object ':top))
               (t
                (write object :stream stream)))))
    (write-type-object object)))

(defun type-to-string (object &optional env)
  (with-pprint-variable-context ()
    (with-output-to-string (stream)
      (render-type-object stream object :env env))))

(defun pprint-type-object (stream object)
  (%call-with-existing-or-fresh-tyvar-context
   (lambda ()
     (render-type-object stream
                         object
                         :type-name-writer #'%write-symbolic-type-name
                         :class-name-writer #'%write-symbolic-class-name
                         :tyvar-writer #'%write-symbolic-tyvar))))

(defun pprint-ty (stream ty)
  (declare (type stream stream)
           (type ty ty)
           (values ty))
  (pprint-type-object stream ty)
  ty)

(defun pprint-predicate (stream predicate)
  (declare (type stream stream)
           (type ty-predicate predicate))
  (pprint-type-object stream predicate)
  nil)

(defun coalton-impl/typechecker/predicate::pprint-qualified-ty (stream qualified-ty)
  (declare (type stream stream)
           (type qualified-ty qualified-ty))
  (pprint-type-object stream qualified-ty)
  nil)

(defun coalton-impl/typechecker/scheme::pprint-scheme (stream scheme)
  (declare (type stream stream)
           (type ty-scheme scheme))
  (pprint-type-object stream scheme)
  nil)

(defmethod print-object ((ty ty) stream)
  (cond
    (*print-readably*
     (call-next-method))
    (t
     (print-unreadable-object (ty stream :type t)
       (%call-with-existing-or-fresh-tyvar-context
        (lambda ()
          (pprint-ty stream ty)))))))

(defmethod print-object ((predicate ty-predicate) stream)
  (cond
    (*print-readably*
     (call-next-method))
    (t
     (pprint-predicate stream predicate))))

(defmethod print-object ((qualified-ty qualified-ty) stream)
  (cond
    (*print-readably*
     (call-next-method))
    (t
     (coalton-impl/typechecker/predicate::pprint-qualified-ty stream qualified-ty))))

(defmethod print-object ((scheme ty-scheme) stream)
  (cond
    (*print-readably*
     (call-next-method))
    (t
     (coalton-impl/typechecker/scheme::pprint-scheme stream scheme))))
