(defpackage #:coalton-impl/main
  (:use
   #:cl)
  (:shadow
   #:compile)
  (:local-nicknames
   (#:codegen #:coalton-impl/codegen)
   (#:entry #:coalton-impl/entry)
   (#:error #:coalton-impl/error)
   (#:parser #:coalton-impl/parser)
   (#:util #:coalton-impl/util)))

(in-package #:coalton-impl/main)

(defclass file-compiler ()
  ((stream :initarg :stream)))

(defmethod codegen:emit ((collector file-compiler) form)
  (with-slots (stream) collector
    (prin1 form stream)
    (terpri stream)
    (terpri stream)))

(defmethod codegen:emit-env ((collector file-compiler) name args)
  (codegen:emit collector `(setf entry:*global-environment*
                                 (,name entry:*global-environment*
                                        ,@(mapcar #'util:runtime-quote args)))))

(defun compile (input-file output-file)
  (with-open-file (stream input-file
                          :direction :input
                          :element-type 'character)
    (with-open-file (output-stream output-file
                                   :direction :output
                                   :element-type 'character
                                   :if-exists :supersede)
      (let ((output (make-instance 'file-compiler
                                   :stream output-stream))
            (file (error:make-coalton-file :stream stream
                                           :name input-file)))
        (parser:with-reader-context stream
          (entry:entry-point (parser:read-file stream file) output))))))

(defun compile-example ()
  (compile "/Users/jbouwman/git/coalton/diff.coalton"
           "/Users/jbouwman/git/coalton/diff.lisp"))
