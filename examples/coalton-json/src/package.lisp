(cl:in-package #:cl-user)

(defpackage #:coalton-json
  (:documentation "Public interface to COALTON-JSON.")
  (:use #:coalton
        #:coalton-library)
  (:export
   #:String-Map
   #:SM-Alist
   #:empty-sm
   #:sm-lookup
   #:sm-insert)
  (:export
   #:Json
   #:Json-Null
   #:Json-Boolean
   #:Json-String
   #:Json-Number
   #:Json-Object
   #:Json-Array)
  (:export
   #:parse-json))

(defpackage #:json-streams-binding
  (:documentation "Private package for binding to JSON-STREAMS.")
  (:use #:cl)
  (:export
   #:%parse-json))
