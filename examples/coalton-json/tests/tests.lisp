(fiasco:define-test-package #:coalton-json-tests
  (:use #:cl)
  (:export #:run-coalton-json-tests))

(cl:in-package #:coalton-json-tests)

(defun run-coalton-json-tests ()
  (fiasco:run-package-tests
   :package ':coalton-json-tests
   :interactive t))

(defun is-type (ty x)
  (fiasco:is (typep x ty)))

(fiasco:deftest test-json ()
  (is-type 'coalton-json::json/json-null
           (coalton-json:parse-json "null"))
  (is-type 'coalton-json::json/json-boolean
           (coalton-json:parse-json "false"))
  (is-type 'coalton-json::json/json-boolean
           (coalton-json:parse-json "true"))
  (is-type 'coalton-json::json/json-string
           (coalton-json:parse-json "\"x\""))
  (is-type 'coalton-json::json/json-number
           (coalton-json:parse-json "1"))
  (is-type 'coalton-json::json/json-number
           (coalton-json:parse-json "2.0"))
  (is-type 'coalton-json::json/json-number
           (coalton-json:parse-json "2.0"))
  (is-type 'coalton-json::json/json-array
           (coalton-json:parse-json "[]"))
  (is-type 'coalton-json::json/json-array
           (coalton-json:parse-json "[[]]"))
  (is-type 'coalton-json::json/json-array
           (coalton-json:parse-json "[1, 1.0, true, false, [], \"x\"]"))
  (is-type 'coalton-json::json/json-array
           (coalton-json::parse-json 
            (coalton-json::json->string 
             (coalton-json:parse-json "[1, 1.0, true, false, [], \"x\"]"))))
  (is-type 'coalton-json::json/json-object
           (coalton-json:parse-json "{}"))
  (is-type 'coalton-json::json/json-object
           (coalton-json:parse-json "{\"x\":0, \"y\":{\"z\":\"z\"}}"))
  (is-type 'coalton-json::json/json-object
           (coalton-json::parse-json 
            (coalton-json::json->string
             (coalton-json:parse-json "{\"x\":0, \"y\":{\"z\":\"z\"}}")))))
  
