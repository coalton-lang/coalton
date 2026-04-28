;;;; socket.lisp -- SBCL-specific TCP socket bindings.
;;;;
;;;; Thin wrappers around sb-bsd-sockets for use from Coalton
;;;; via the `lisp` escape hatch.

(in-package #:mine/bindings/socket)

;;; Error sentinel

(define-condition socket-error (error)
  ((message :initarg :message :reader socket-error-message))
  (:report (lambda (c s)
             (format s "Socket error: ~A" (socket-error-message c)))))

(defconstant +socket-error+ 'socket-error
  "Condition type for socket errors, exported for handler-case matching.")

;;; Helpers

(defun %make-inet-socket ()
  "Create a new TCP/IPv4 socket."
  (make-instance 'sb-bsd-sockets:inet-socket
                 :type :stream
                 :protocol :tcp))

(defun %loopback-address ()
  "Return the 127.0.0.1 address as a vector."
  #(127 0 0 1))

;;; Public API

(defun socket-listen (port)
  "Create an IPv4 TCP socket, bind to 127.0.0.1:PORT, and listen.
PORT = 0 means the OS assigns an ephemeral port (retrieve it via
SOCKET-LOCAL-PORT afterwards).  Returns the listening socket object."
  (declare (type (integer 0 65535) port))
  (let ((sock (%make-inet-socket)))
    (handler-case
        (progn
          ;; Allow quick rebinding after a crash
          (setf (sb-bsd-sockets:sockopt-reuse-address sock) t)
          (sb-bsd-sockets:socket-bind sock (%loopback-address) port)
          (sb-bsd-sockets:socket-listen sock 4)
          sock)
      (error (c)
        (ignore-errors (sb-bsd-sockets:socket-close sock))
        (error 'socket-error
               :message (format nil "Listen on port ~D failed: ~A" port c))))))

(defun socket-accept (listening-socket)
  "Accept a connection on LISTENING-SOCKET.
Blocks until a client connects.
Returns a bivalent (input + output) stream for the accepted connection."
  (multiple-value-bind (client-socket address port)
      (sb-bsd-sockets:socket-accept listening-socket)
    (declare (ignore address port))
    (sb-bsd-sockets:socket-make-stream
     client-socket
     :element-type '(unsigned-byte 8)
     :input t
     :output t
     :buffering :full)))

(defun socket-connect (port)
  "Connect to 127.0.0.1:PORT.
Returns a bivalent stream for the connection."
  (declare (type (integer 1 65535) port))
  (let ((sock (%make-inet-socket)))
    (handler-case
        (progn
          (sb-bsd-sockets:socket-connect sock (%loopback-address) port)
          (sb-bsd-sockets:socket-make-stream
           sock
           :element-type '(unsigned-byte 8)
           :input t
           :output t
           :buffering :full))
      (error (c)
        (ignore-errors (sb-bsd-sockets:socket-close sock))
        (error 'socket-error
               :message (format nil "Connect to port ~D failed: ~A" port c))))))

(defun socket-write-string (stream string)
  "Write STRING to STREAM as UTF-8 encoded bytes."
  (declare (type string string))
  (let ((octets (sb-ext:string-to-octets string :external-format :utf-8)))
    (write-sequence octets stream)
    (force-output stream))
  (values))

(defun socket-close (socket-or-stream)
  "Close SOCKET-OR-STREAM.
Works for both raw sb-bsd-sockets:socket objects and CL streams."
  (ignore-errors
    (etypecase socket-or-stream
      (sb-bsd-sockets:socket
       (sb-bsd-sockets:socket-close socket-or-stream))
      (stream
       (close socket-or-stream))))
  (values))

(defun socket-local-port (listening-socket)
  "Return the port number that LISTENING-SOCKET is bound to.
Useful when the socket was bound with port 0 (OS-assigned)."
  (multiple-value-bind (address port)
      (sb-bsd-sockets:socket-name listening-socket)
    (declare (ignore address))
    port))
