(cl:in-package #:json-streams-binding)

(defun %parse-json-stream (stream)
  (labels ((_next ()
             (json-streams:json-read stream))
           (_parse-token (tok)
             (etypecase tok
                 ((member :NULL)
                  coalton-json:Json-Null)
                 ((member :TRUE)
                  (coalton-json:Json-Boolean coalton-library:True))
                 ((member :FALSE)
                  (coalton-json:Json-Boolean coalton-library:False))
                 (number
                  (coalton-json:Json-Number (coerce tok 'double-float)))
                 (string
                  (coalton-json:Json-String tok))
                 ((member :BEGIN-OBJECT)
                  (let ((sm coalton-json:empty-sm))
                    (loop :for n := (_next) :do
                      (etypecase n
                        ((member :END-OBJECT)
                         (return-from _parse-token (coalton-json:json-object sm)))
                        (string
                         (setf sm (coalton-json:sm-insert sm n (_parse-token (_next)))))))))
                 ((member :BEGIN-ARRAY)
                  (let ((l coalton-library:Nil))
                    (loop :for n := (_next) :do
                      (etypecase n
                        ((member :END-ARRAY)
                         (return-from _parse-token (coalton-json:json-array
                                                    (coalton-library:reverse l))))
                        (t
                         (setf l (coalton-library:Cons (_parse-token n) l)))))))
                 ((member :EOF :END-OBJECT :END-ARRAY)
                  (error "Shouldn't be reached")))))
    (_parse-token (_next))))

(defun %parse-json (string)
  (let ((stream (json-streams:make-json-input-stream string)))
    (unwind-protect (%parse-json-stream stream)
      (json-streams:json-close stream))))


(cl:in-package #:coalton-json)

(coalton-toplevel
  (declare parse-json (String -> Json))
  (define (parse-json s)
    "Parse JSON string S into a `Json` object."
    (lisp Json (s)
      (json-streams-binding:%parse-json s))))
