(defpackage #:coalton-impl/reader
  (:use
   #:cl)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:util #:coalton-impl/util)
   (#:parser #:coalton-impl/parser)))

(in-package #:coalton-impl/reader)

(defun read-coalton-toplevel-open-paren (stream char)
  (labels ((try-read-string (expected-string)
             (%try-read-string (coerce expected-string 'list) nil))
           (%try-read-string (expected taken)
             (cond
               ((null expected)
                ;; Check if the next character is whitespace, hackily!
                (not (eql (peek-char nil stream) (peek-char t stream))))
               ((not (char-equal (car expected) (peek-char nil stream)))
                nil)
               (t
                (read-char stream)
                (cond
                  ((%try-read-string (cdr expected) (cons (car expected) taken))
                   t)
                  (t
                   (unread-char (car expected) stream)
                   nil))))))
    (cond
      ((try-read-string "coalton-toplevel")
       (let* ((pathname (or *compile-file-truename* *load-truename*))
              (filename (if pathname (namestring pathname) "<unknown>"))
              
              (file-input-stream
                (cond
                  ((or #+sbcl (sb-int:form-tracking-stream-p stream)
                       nil)
                   (open (pathname stream)))
                  (t
                   stream)))
              (file (parser:make-coalton-file :stream file-input-stream :name filename))

              (program
                (handler-case
                    (parser:read-program stream file :mode :toplevel-macro)

                  ;; We need some way of setting FILE-POSITION so that
                  ;; when Slime grabs the location of the error it
                  ;; highlights the correct form.
                  ;;
                  ;; In SBCL, we can't unread more than ~512
                  ;; characters due to limitations with ANSI streams,
                  ;; which breaks our old method of unreading
                  ;; characters until the file position is
                  ;; correct. Instead, we now patch in our own
                  ;; version of FILE-POSITION.
                  ;;
                  ;; This is a massive hack and might start breaking
                  ;; with future changes in SBCL.
                  #+sbcl
                  (parser:parse-error (c)
                    (let* ((err (parser:parse-error-err c))
                           (file-offset
                             (- (sb-impl::fd-stream-get-file-position stream)
                                (file-position stream)))
                           (loc (parser:coalton-error-location err)))
                      (setf (sb-impl::fd-stream-misc stream)
                            (lambda (stream operation arg1)
                              (if (= (sb-impl::%stream-opcode :get-file-position) operation)
                                  (+ file-offset loc 1)
                                  (sb-impl::tracking-stream-misc stream operation arg1))))
                      (error c))))))

         `(format t "~A" ,(format nil "~A" program))))
      ((try-read-string "coalton")
       (let* ((pathname (or *compile-file-truename* *load-truename*))
              (filename (if pathname (namestring pathname) "<unknown>"))

              (file-input-stream
                (cond
                  ((or #+sbcl (sb-int:form-tracking-stream-p stream)
                       nil)
                   (open (pathname stream)))
                  (t
                   stream)))
              (file (parser:make-coalton-file :stream file-input-stream :name filename))

              (expression
                (handler-case
                    (parser:read-expression stream file)

                  ;; We need some way of setting FILE-POSITION so that
                  ;; when Slime grabs the location of the error it
                  ;; highlights the correct form.
                  ;;
                  ;; In SBCL, we can't unread more than ~512
                  ;; characters due to limitations with ANSI streams,
                  ;; which breaks our old method of unreading
                  ;; characters until the file position is
                  ;; correct. Instead, we now patch in our own
                  ;; version of FILE-POSITION.
                  ;;
                  ;; This is a massive hack and might start breaking
                  ;; with future changes in SBCL.
                  (parser:parse-error (c)
                    (let* ((err (parser:parse-error-err c))
                           (file-offset
                             (- (sb-impl::fd-stream-get-file-position stream)
                                (file-position stream)))
                           (loc (parser:coalton-error-location err)))
                      (setf (sb-impl::fd-stream-misc stream)
                            (lambda (stream operation arg1)
                              (if (= (sb-impl::%stream-opcode :get-file-position) operation)
                                  (+ file-offset loc 1)
                                  (sb-impl::tracking-stream-misc stream operation arg1))))
                      (error c))))))
         `(format t "~A" ,(format nil "~A" expression))))
      ;; Fall back to the default open paren reader
      (t
       (funcall #'#.(get-macro-character #\() stream char)))))

(named-readtables:defreadtable coalton:coalton
  (:merge :standard)
  (:macro-char #\( #'read-coalton-toplevel-open-paren))
