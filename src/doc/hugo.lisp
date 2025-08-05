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
summary: 'The Coalton standard library reference.'
math: true
layout: two-pane
---

<aside class=\"sidebar\">

### Reference

<div class=\"symbol-search\">
  <input type=\"text\" id=\"symbol-search-input\" placeholder=\"Search symbols...\" autocomplete=\"off\">
  <div class=\"search-results\" id=\"search-results\"></div>
</div>

" stream)

    ;; package menu
    (dolist (package packages)
      (format stream "- ~A~%" (object-link package)))
    (format stream "</aside>~%<div class=\"main-content\">~%~%")

    ;; markdown content
    (let ((backend (make-backend ':markdown stream)))
      (dolist (package packages)
        (write-object backend package)))
    (format stream "</div>")))
