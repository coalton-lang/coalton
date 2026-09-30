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

(defun run-runtime-regression-tests ()
  (dolist (test '(check-runtime-protocol-io-isolation
                  check-runtime-protocol-rejects-reader-evaluation))
    (funcall test))
  t)
