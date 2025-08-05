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
    ;; Header
    (write-line "---
identifier: Reference
summary: 'The Coalton standard library reference.'
math: true
layout: two-pane
---"
                stream)

    ;; Title and side bar
    (write-line "<aside class=\"sidebar\">

### Reference

<div class=\"symbol-search\">
  <input type=\"text\" id=\"symbol-search-input\" placeholder=\"Search symbols...\" autocomplete=\"off\">
  <div class=\"search-results\" id=\"search-results\"></div>
</div>
"
                stream)

    ;; package menu
    (dolist (package packages)
      (format stream "- ~A~%" (object-link package)))
    (write-line "</aside>" stream)

    ;; Main content
    (write-line "<div class=\"main-content\">" stream)
    (let ((backend (make-backend ':markdown stream)))
      (dolist (package packages)
        (write-object backend package)))
    (write-line "</div>" stream)))
