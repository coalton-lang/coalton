(in-package #:mine-tests)

(defun %runtime-check (value control &rest arguments)
  (unless value
    (error (apply #'format nil control arguments))))

(defun %call-with-runtime-wire-file (function)
  (let ((path (merge-pathnames (format nil "mine-runtime-wire-~A.bin" (gensym))
                              (uiop:temporary-directory))))
    (unwind-protect
         (with-open-file (stream path :direction :io :element-type '(unsigned-byte 8)
                                     :if-exists :supersede :if-does-not-exist :create)
           (funcall function stream))
      (ignore-errors (delete-file path)))))

(defun check-runtime-protocol-io-isolation ()
  (let ((*print-base* 16) (*print-radix* t) (*print-length* 1) (*print-level* 1)
        (*read-base* 16) (*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    (%call-with-runtime-wire-file
     (lambda (stream)
       (%runtime-check
        (mine/protocol/server::write-message stream '(:return 12 (:ok "result")))
        "Expected protocol write to succeed")
       (file-position stream 0)
       (%runtime-check
        (equal '(:return 12 (:ok "result"))
               (mine/protocol/server::read-message stream))
        "Protocol round trip inherited user reader/printer controls")))
    (%runtime-check
     (string= "12" (mine/protocol/wire:encode-sexpr (mine/protocol/wire:SInteger 12)))
     "Coalton wire integer encoding inherited printer controls")
    (let* ((*print-base* 10) (*print-radix* nil)
           (payload (mine/runtime/eval::%encode-result-values
                    (list (list 1 2)) (find-package "CL-USER"))))
      (%runtime-check
       (equal (list :values (list "(1 ...)"))
              (mine/protocol/server::decode-protocol-sexp payload))
       "Nested value transport must preserve payload structure and user value printing"))))

(defun check-runtime-protocol-rejects-reader-evaluation ()
  (%runtime-check
   (handler-case
       (progn (mine/protocol/server::decode-protocol-sexp "#.(+ 1 2)") nil)
     (error () t))
   "Protocol reader accepted read-time evaluation")
  (%runtime-check
   (handler-case
       (progn (mine/protocol/server::decode-protocol-sexp "(:ping 1) (:quit 2)") nil)
     (error () t))
   "Protocol reader accepted trailing data"))

(defun %call-with-replaced-runtime-function (name replacement function)
  (let ((original (symbol-function name)))
    (unwind-protect
         (progn (setf (symbol-function name) replacement) (funcall function))
      (setf (symbol-function name) original))))

(defun %call-with-tui-responses (responses function)
  (let ((requests 0))
    (%call-with-replaced-runtime-function
     'mine/protocol/server::%request-input-from-tui
     (lambda (stream prompt)
       (declare (ignore stream prompt))
       (incf requests)
       (pop responses))
     (lambda ()
       (funcall function (make-instance 'mine/protocol/server:tui-input-stream))))
    requests))

(defun check-runtime-input-preserves-lines-and-unread ()
  (%runtime-check
   (= 1 (%call-with-tui-responses
         '("42")
         (lambda (stream) (%runtime-check (= 42 (read stream)) "READ joined submissions"))))
   "READ should consume one submitted numeric line")
  (%runtime-check
   (= 1 (%call-with-tui-responses
         '("abc")
         (lambda (stream)
           (%runtime-check (char= #\a (read-char stream)) "Wrong first character")
           (%runtime-check (string= "bc" (read-line stream)) "READ-LINE discarded buffered text"))))
   "READ-LINE must use remaining buffered characters")
  (%call-with-tui-responses
   '("")
   (lambda (stream)
     (%runtime-check (char= #\Newline (read-char stream)) "Empty line lost its newline")
     (unread-char #\Newline stream)
     (%runtime-check (listen stream) "Unread newline should be available")
     (multiple-value-bind (text eof-p) (read-line stream)
       (%runtime-check (and (string= text "") (not eof-p)) "Unread newline was not restored"))))
  (%runtime-check
   (= 1 (%call-with-tui-responses
         '(nil)
         (lambda (stream)
           (%runtime-check (eq :eof (read-char stream nil :eof)) "Expected EOF")
           (%runtime-check (eq :eof (read-char stream nil :eof)) "Expected persistent EOF"))))
   "EOF should not repeatedly request input"))

(defun %call-with-runtime-messages (function &optional replies)
  (let ((messages nil))
    (%call-with-replaced-runtime-function
     'mine/protocol/server::write-message
     (lambda (stream message)
       (declare (ignore stream))
       (push message messages)
       t)
     (lambda ()
       (%call-with-replaced-runtime-function
        'mine/protocol/server::read-message
        (lambda (stream) (declare (ignore stream)) (pop replies))
        function)))
    (nreverse messages)))

(defun check-runtime-output-flushes-before-debugger-and-abort ()
  (let* ((messages
           (%call-with-runtime-messages
            (lambda ()
              (mine/protocol/server::handle-eval
               41 "(progn (write-string \"before-error\") (error \"boom\"))"
               "CL-USER" :test-wire nil))
            '((:debug-abort 41))))
         (first (first messages)))
    (%runtime-check (equal '(:notify (:output-chunk 41 "before-error")) first)
                    "Expected stdout before debugger, got ~S" first)
    (%runtime-check (eq :debug (first (second messages))) "Expected interactive debugger")
    (%runtime-check (equal '(:return 41 (:error "Aborted.")) (car (last messages)))
                    "Expected an abort reply"))
  (let ((messages
          (%call-with-runtime-messages
           (lambda ()
             (mine/protocol/server::call-with-tui-io
              :test-wire 42
              (lambda ()
                (write-string "a")
                (force-output)
                (write-string "b" *error-output*)
                (write-string "c" *trace-output*)))))))
    (%runtime-check
     (equal '((:notify (:output-chunk 42 "a")) (:notify (:output-chunk 42 "bc"))) messages)
     "Output chunks lost explicit flush boundaries or stream ordering: ~S" messages)))

(defun %runtime-eval-values (text &optional (package "CL-USER") coalton-p)
  (second (mine/protocol/server::decode-protocol-sexp
           (mine/runtime/eval:debug-eval text package nil nil coalton-p))))

(defun check-runtime-multiform-eval-and-history ()
  (let ((+ nil) (++ nil) (+++ nil) (* nil) (** nil) (*** nil)
        (/ nil) (// nil) (/// nil))
    (%runtime-check (equal '("7") (%runtime-eval-values "(+ 1 2) (+ 3 4)"))
                    "REPL ignored trailing forms")
    (%runtime-check (equal '("(+ 3 4)") (%runtime-eval-values "+"))
                    "+ should refer to the previously evaluated form")
    (%runtime-check (equal '("-") (%runtime-eval-values "-"))
                    "- should refer to the form currently being evaluated")
    (%runtime-eval-values "(values 1 2)")
    (%runtime-eval-values "(values)")
    (%runtime-check (equal '("NIL") (%runtime-eval-values "/"))
                    "/ must record zero values")
    (%runtime-check (null (%runtime-eval-values "; only a comment"))
                    "Comment-only input should return no values")))

(defun check-runtime-package-identity-and-success-reporting ()
  (let* ((name (string-downcase (symbol-name (gensym "mine-exact-package-"))))
         (package (make-package name :use '("CL")))
         (missing (symbol-name (gensym "MINE-MISSING-PACKAGE-"))))
    (unwind-protect
         (progn
           (%runtime-check
            (eq package (mine/runtime/eval::find-evaluation-package name))
            "Lowercase package resolved to a different package")
           (let ((messages
                   (%call-with-runtime-messages
                    (lambda ()
                      (mine/protocol/server::handle-eval
                       51 (format nil "(cl:in-package ~S) (+ 1 2)" name)
                       "CL-USER" :test-wire nil)))))
             (%runtime-check (member (list :notify (list :package 51 name)) messages :test #'equal)
                             "Successful qualified IN-PACKAGE was not reported"))
           (let ((messages
                   (%call-with-runtime-messages
                    (lambda ()
                      (mine/protocol/server::handle-eval
                       52 (format nil "(in-package ~S)" missing)
                       "CL-USER" :test-wire nil)))))
             (%runtime-check
              (notany (lambda (message)
                        (and (eq :notify (first message))
                             (eq :package (first (second message))))) messages)
              "Failed package change was reported as successful")
             (%runtime-check (null (find-package missing)) "Missing package was silently created")))
      (delete-package package))))

(defun check-runtime-coalton-multiform-eval ()
  (let* ((name (symbol-name (gensym "MINE-RUNTIME-VALUE-")))
         (input (format nil "(define ~A 10) (+ ~A 2)" name name)))
    (%runtime-check (equal '("12") (%runtime-eval-values input "COALTON-USER" t))
                    "Coalton wrapping must be selected separately for each form")))

(defun run-runtime-regression-tests ()
  (dolist (test '(check-runtime-protocol-io-isolation
                  check-runtime-protocol-rejects-reader-evaluation
                  check-runtime-input-preserves-lines-and-unread
                  check-runtime-output-flushes-before-debugger-and-abort
                  check-runtime-multiform-eval-and-history
                  check-runtime-package-identity-and-success-reporting
                  check-runtime-coalton-multiform-eval))
    (handler-case (funcall test)
      (error (condition)
        (format *error-output* "~&Runtime regression ~A failed: ~A~%" test condition)
        (error condition))))
  t)
