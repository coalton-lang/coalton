(fiasco:define-test-package #:coalton-json-tests
  (:use #:cl #:coalton-json)
  (:export #:run-coalton-json-tests))

(cl:in-package #:coalton-json-tests)

(defun run-coalton-json-tests ()
  (fiasco:run-package-tests
   :package ':coalton-json-tests
   :interactive t))

(defun is-type (ty x)
  (fiasco:is (typep x ty)))

(fiasco:deftest test-json->string ()
  (fiasco:is (equal "1.0" (coalton:coalton 
                          (json->string (Json-Number 1d0)))))
  (fiasco:is (equal "-2.0" (coalton:coalton 
                          (json->string (Json-Number -2d0)))))
  (fiasco:is (equal "0.00023" (coalton:coalton 
                          (json->string (Json-Number 0.00023d0)))))
  (fiasco:is (equal "909112334.454323" (coalton:coalton 
                          (json->string (Json-Number 909112334.454323d0)))))
  (fiasco:is (equal "\"helloworld\"" (coalton:coalton 
                          (json->string (Json-String "helloworld")))))
  (fiasco:is (equal "[1.0, 1.0, true, false, [], \"x\"]" 
                    (coalton:coalton 
                     (json->string 
                      (parse-json "[1, 1.0, true, false, [], \"x\"]")))))
  (fiasco:is (equal "{\"email\":\"john@email.com\", \"name\":\"John Smith\", \"id\":1.0}" 
                    (coalton:coalton 
                     (json->string     
                      (Json-Object     
                       (sm-insert      
                        (sm-insert 
                         (sm-insert 
                          empty-sm
                          "name" (Json-String "John Smith"))
                         "email" (Json-String "john@email.com"))
                                "id" (Json-Number 1d0)))))))
  (fiasco:is (equal "false" (coalton:coalton 
                          (json->string (Json-Boolean coalton:false)))))
  (fiasco:is (equal "true" (coalton:coalton 
                         (json->string (Json-Boolean coalton:true))))))

(fiasco:deftest test-json ()
  (is-type 'coalton-json::json/json-null
           (parse-json "null"))
  (is-type 'coalton-json::json/json-boolean
           (parse-json "false"))
  (is-type 'coalton-json::json/json-boolean
           (parse-json "true"))
  (is-type 'coalton-json::json/json-string
           (parse-json "\"x\""))
  (is-type 'coalton-json::json/json-number
           (parse-json "1"))
  (is-type 'coalton-json::json/json-number
           (parse-json "2.0"))
  (is-type 'coalton-json::json/json-number
           (parse-json "2.0"))
  (is-type 'coalton-json::json/json-array
           (parse-json "[]"))
  (is-type 'coalton-json::json/json-array
           (parse-json "[[]]"))
  (is-type 'coalton-json::json/json-array
           (parse-json "[1, 1.0, true, false, [], \"x\"]"))
  (is-type 'coalton-json::json/json-array
           (parse-json 
            (json->string 
             (parse-json "[1, 1.0, true, false, [], \"x\"]"))))
  (is-type 'coalton-json::json/json-object
           (parse-json "{}"))
  (is-type 'coalton-json::json/json-object
           (parse-json "{\"x\":0, \"y\":{\"z\":\"z\"}}"))
  (is-type 'coalton-json::json/json-object
           (parse-json 
            (json->string
             (parse-json "{\"x\":0, \"y\":{\"z\":\"z\"}}")))))
  
