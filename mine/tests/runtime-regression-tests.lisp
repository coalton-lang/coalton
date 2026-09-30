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

(defun run-runtime-regression-tests ()
  (dolist (test '(check-runtime-protocol-io-isolation
                  check-runtime-protocol-rejects-reader-evaluation
                  check-runtime-input-preserves-lines-and-unread))
    (funcall test))
  t)
