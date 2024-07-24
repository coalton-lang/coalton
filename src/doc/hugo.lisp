;;;; Hugo docs generator for coalton-lang.github.io

(defpackage #:coalton/doc/hugo
  (:documentation "Hugo backend for coalton doc generator.")
  (:use
   #:cl
   #:coalton/doc/base
   #:coalton/doc/model))

(in-package #:coalton/doc/hugo)

(defclass hugo-backend ()
  ((stream :initarg :stream
           :reader output-stream)))

(register-backend :hugo 'hugo-backend)

(defmethod write-packages ((backend hugo-backend) packages)
  (let ((stream (output-stream backend)))
    (write-string "---
identifier: Reference
---

<style>
@media only screen and (max-width: 1250px) {
  .sidebar {
    display: none;
  }
}
</style>

<div class=\"sidebar\" style=\"height: 0; position: sticky; top: 10px\">
<div style=\"position: relative; right: 50%; width: 50%;\">

### Reference
" stream)

    ;; package menu
    (dolist (package packages)
      (format stream "- ~A~%" (object-link package)))
    (format stream "</div></div><div>~%~%")

    ;; markdown content
    (let ((backend (make-backend ':markdown stream)))
      (dolist (package packages)
        (write-object backend package)))
    (format stream "</div></div></div>")))
