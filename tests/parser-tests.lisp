(in-package #:coalton-tests)

(deftest test-parser ()
  (labels ((test-files (pattern)
             (let ((files (directory (merge-pathnames pattern (asdf:system-source-directory "coalton/tests")))))
               (when (endp files)
                 (error "No test files match pattern '~A'" pattern))
               files))

           (parse-file (file)
             (with-open-file (stream file
                                     :direction :input
                                     :element-type 'character)
               (parser:with-reader-context stream
                 (parser:read-program stream (error:make-coalton-file :stream stream :name (namestring file)) :mode :file))))

           (parse-error-text (file)
             (with-open-file (stream file
                                     :direction :input
                                     :element-type 'character)
               (handler-case
                   (parser:with-reader-context stream
                     (entry:entry-point
                      (parser:read-program stream (error:make-coalton-file :stream stream :name "test") :mode :file))
                     "no errors")
                 (error:coalton-base-error (c)
                   (princ-to-string c))))))
    (dolist (file (test-files "tests/parser-test-files/bad-files/*.coal"))
      (let ((error-file (make-pathname :type "error"
                                       :defaults file)))
        (cond ((uiop:file-exists-p error-file)
               (check-string= (format nil "expected error ~A (A) and generated error (B)" error-file)
                              (alexandria:read-file-into-string error-file)
                              (parse-error-text file)))
              (t
               (signals parser:parse-error
                 (parse-file file))))))

    (dolist (file (test-files "tests/parser-test-files/good-files/*.coal"))
      (parse-file file))))


(defun load-test-cases (pathname)
  (labels ((combine-lines (lines)
             (string-trim '(#\Space #\Newline)
                          (format nil "~{~A~%~}" (reverse lines))))
           (collect-line (state line)
             (destructuring-bind (key header program error cases) state
               (ecase key
                 (:header (list key (cons line header) program error cases))
                 (:program (list key header (cons line program) error cases))
                 (:error (list key header program (cons line error) cases)))))
           (collect (state)
             (destructuring-bind (key header program error cases) state
               (list key nil nil nil
                     (cons (list (combine-lines header)
                                 (combine-lines program)
                                 (combine-lines error))
                           cases))))
           (arc (state key)
             (cons key (cdr state)))
           (process-line (state line)
             (cond ((null line)
                    (arc state :done))
                   ((and (< 0 (length line)) (char= (aref line 0) #\=))
                    (ecase (car state)
                      (:init (arc state :header))
                      (:header (arc state :program))
                      (:error (arc (collect state) :header))))
                   ((and (< 0 (length line)) (char= (aref line 0) #\-))
                    (ecase (car state)
                      (:program (arc state :error))))
                   (t
                    (collect-line state line)))))
    (with-open-file (stream pathname :direction ':input
                                     :element-type 'character)
      (loop :for state := (list :init nil nil nil nil)
              :then (process-line state (read-line stream nil nil))
            :when (eq :done (car state))
              :return (reverse (nth 4 state))))))

(defun collect-error (program)
  (with-input-from-string (stream program)
    (handler-case
        (parser:with-reader-context stream
          (entry:entry-point
           (parser:read-program stream
                                (error:make-coalton-file :stream stream
                                                         :name "test")
                                :mode :file))
          "no error")
      (error:coalton-base-error (c)
        (string-trim '(#\Space #\Newline) (princ-to-string c))))))

(defun check-parser-suite (filename)
  (let ((counter 0))
    (loop :for (header program error)
            :in (load-test-cases (merge-pathnames filename
                                                  (asdf:system-source-directory "coalton/tests")))
          :do (check-string= (format nil "~A: expected error (A), generated error (B)" header)
                             error
                             (collect-error program))
              (incf counter))
    (is (< 0 counter)
        "Test suite was not empty")))

(deftest test-parse-type ()
  (check-parser-suite "tests/parser-test-files/bad-files/parse-type.txt"))
