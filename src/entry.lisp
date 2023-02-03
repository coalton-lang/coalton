(defpackage #:coalton-impl/entry
  (:use
   #:cl)
  (:local-nicknames
   (#:settings #:coalton-impl/settings)
   (#:parser #:coalton-impl/parser)
   (#:tc #:coalton-impl/typechecker)
   (#:codegen #:coalton-impl/codegen)))

(in-package #:coalton-impl/entry)

(defvar *global-environment* (tc:make-default-environment))

(defun entry-point (program)
  (declare (type parser:program program))

  (let* ((*package* (parser:program-package program))

         (program (parser:rename-variables program))

         (file (parser:program-file program))

         (env *global-environment*))

    (multiple-value-bind (type-definitions env)
        (tc:toplevel-define-type (parser:program-types program) file env)

      (multiple-value-bind (class-definitions env)
          (tc:toplevel-define-class (parser:program-classes program)
                                    file
                                    env)

        (multiple-value-bind (ty-instances env)
            (tc:toplevel-define-instance (parser:program-instances program) env file)

          (multiple-value-bind (toplevel-definitions env)
              (tc:toplevel-define (parser:program-defines program)
                                  (parser:program-declares program)
                                  file
                                  env)

            (multiple-value-bind (toplevel-instances)
                (tc:toplevel-typecheck-instance ty-instances (parser:program-instances program) env file)

              (let ((translation-unit
                      (tc:make-translation-unit
                       :types type-definitions
                       :definitions toplevel-definitions
                       :classes class-definitions
                       :instances toplevel-instances
                       :package *package*
                       :specializations nil ;; TODO
                       )))

                (multiple-value-bind (program env)
                    (codegen:compile-translation-unit translation-unit env)

                  (values
                   (if settings:*coalton-skip-update*
                       program
                       `(progn
                          (eval-when (:load-toplevel)
                            (unless (eq (settings:coalton-release-p) ,(settings:coalton-release-p))
                              ,(if (settings:coalton-release-p)
                                   `(error "~A was compiled in release mode but loaded in development."
                                           ,(or *compile-file-pathname* *load-truename*))
                                   `(error "~A was compiled in development mode but loaded in release."
                                           ,(or *compile-file-pathname* *load-truename*)))))
                          #+ignore
                          ,(coalton-impl/typechecker::generate-diff
                            translation-unit
                            env
                            '*global-environment*)
                          ,program))
                   env))))))))))

(defun file-entry-point (filename)
  (declare (type string filename))

  (with-open-file (file-stream filename :if-does-not-exist :error)
    (let ((coalton-file (parser:make-coalton-file
                         :stream file-stream
                         :name filename)))
      (multiple-value-bind (code env)
          (entry-point (parser:read-program file-stream coalton-file :mode :file))

        (setf *global-environment* env)

        code))))

(defun debug-file-entry-point (filename)
  (declare (type string filename))

  (let ((settings:*coalton-skip-update* t)
        (settings:*emit-type-annotations* nil))
    (file-entry-point filename)))

;; TODO: remove this
;; Temporary hack to define Num so that integer literals can be
;; typechecked.
(defpackage #:coalton-library/classes
  (:use #:coalton)
  (:export
   #:Tuple
   #:Num #:fromInt #:+
   #:id
   #:undefined
   #:singleton
   #:append
   #:Eq #:==))

(eval (file-entry-point "./pre-bootstrap.coalton"))
(eval (file-entry-point "./bootstrap.coalton"))
