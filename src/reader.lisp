(defpackage #:coalton-impl/reader
  (:use
   #:cl
   #:coalton-impl/parser/base
   #:coalton-impl/parser/types
   #:coalton-impl/parser/expression
   #:coalton-impl/parser/parser)
  (:shadowing-import-from
   #:coalton-impl/parser/base
   #:parse-error)
  (:local-nicknames
   (#:cst #:concrete-syntax-tree)
   (#:util #:coalton-impl/util)))

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

              ;; Setup eclector readtable
              (eclector.readtable:*readtable*
                (eclector.readtable:copy-readtable eclector.readtable:*readtable*))

              ;; Read unspecified floats as double floats
              (*read-default-float-format* 'double-float))
         (let* ((file-input-stream
                  (cond
                    ((or #+sbcl (sb-int:form-tracking-stream-p stream)
                         nil)
                     (open (pathname stream)))
                    (t
                     stream)))
                (file (make-coalton-file :stream file-input-stream :name filename))

                (program (make-program :package *package* :file file))

                (attributes (make-array 0 :adjustable t :fill-pointer t)))

           (handler-case
               (progn
                 (loop :named parse-loop
                       :with elem := nil

                       :when (eql #\) (peek-char t stream))
                         :do (read-char stream)
                             (return-from parse-loop)
                  
                       :do (setf elem (eclector.concrete-syntax-tree:read stream nil 'eof))

                       :when (eq elem 'eof)
                         :do (error "unexpected EOF")

                       :do (when (and (parse-toplevel-form elem program attributes file)
                                      (plusp (length attributes)))
                             (util:coalton-bug "parse-toplevel-form indicated that a form was parsed but did not consume all attributes")))

                 (unless (zerop (length attributes))
                   (error 'parse-error
                          :err (coalton-error
                                :span (cst:source (cdr (aref attributes 0)))
                                :file file
                                :message "Orphan attribute"
                                :primary-note "attribute must be attached to another form"))))
             (parse-error (c)
               (let* ((err (parse-error-err c))
                      (loc (coalton-error-location err)))
                 ;; In SBCL, we can unread characters to step back the
                 ;; FILE-POSITION so that when Slime grabs the location of
                 ;; the error it highlights the correct form.
                 #+sbcl
                 (loop :for i :below (- (file-position stream) loc 1) :do
                   (unread-char #\Null stream))
                 (error c))))

           (setf (program-types program) (nreverse (program-types program)))
           (setf (program-declares program) (nreverse (program-declares program)))
           (setf (program-defines program) (nreverse (program-defines program)))
           (setf (program-classes program) (nreverse (program-classes program)))

           `(format t "~A" ,(format nil "~A" program)))))
      ((try-read-string "coalton")
       (let* ((pathname (or *compile-file-truename* *load-truename*))
              (filename (if pathname (namestring pathname) "<unknown>"))

              ;; Setup eclector readtable
              (eclector.readtable:*readtable*
                (eclector.readtable:copy-readtable eclector.readtable:*readtable*))

              ;; Read unspecified floats as double floats
              (*read-default-float-format* 'double-float))
         (let* ((file-input-stream
                  (cond
                    ((or #+sbcl (sb-int:form-tracking-stream-p stream)
                         nil)
                     (open (pathname stream)))
                    (t
                     stream)))
                (file (make-coalton-file :stream file-input-stream :name filename))

                expression)

           (handler-case
               (let* ((forms
                        (loop :named parse-loop
                              :with forms := nil
                              :with elem := nil

                              :when (eql #\) (peek-char t stream))
                                :do (read-char stream)
                                    (return-from parse-loop (nreverse forms))
                                   
                              :do (setf elem (eclector.concrete-syntax-tree:read stream nil 'eof))
                                  
                              :when (eq elem 'eof)
                                :do (error "unexpected EOF")

                              :do (push elem forms)))
                      (form (cst:cstify forms :source (cons (car (cst:source (first forms)))
                                                            (cdr (cst:source (car (last forms))))))))
                 
                 (setf expression (parse-expression form file)))
             (parse-error (c)
               (let* ((err (parse-error-err c))
                      (loc (coalton-error-location err)))
                 ;; In SBCL, we can unread characters to step back the
                 ;; FILE-POSITION so that when Slime grabs the location of
                 ;; the error it highlights the correct form.
                 #+sbcl
                 (loop :for i :below (- (file-position stream) loc 1) :do
                   (unread-char #\Null stream))
                 (error c))))

           `(format t "~A" ,(format nil "~A" expression)))))
      ;; Fall back to the default open paren reader
      (t
       (funcall #'#.(get-macro-character #\() stream char)))))

(named-readtables:defreadtable coalton:coalton
  (:merge :standard)
  (:macro-char #\( #'read-coalton-toplevel-open-paren))
