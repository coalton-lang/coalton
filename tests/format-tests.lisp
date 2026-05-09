(cl:in-package #:coalton-native-tests)

(named-readtables:in-readtable coalton:coalton)

;;; Test Successes

(define-test format-no-directives ()
  (is (== "hello"
          (format:format format:Str "hello"))))

(define-test format-simple-directive ()
  (is (== (lisp (-> String) ()
            (cl:format cl:nil "~C" #\Space))
          (format:format format:Str "~C" #\Space))))

(define-test format-colon-directive ()
  (is (== (lisp (-> String) ()
            (cl:format cl:nil "~:C" #\Space))
          (format:format format:Str "~:C" #\Space))))

(define-test format-at-directive ()
  (is (== (lisp (-> String) ()
            (cl:format cl:nil "~@C" #\Space))
          (format:format format:Str "~@C" #\Space))))

(define-test format-both-modifiers-directive ()
  (is (== (lisp (-> String) ()
            (cl:format cl:nil "~@:C" #\Space))
          (format:format format:Str "~@:C" #\Space))))

(define-test format-multiple-directives ()
  (is (== "123321"
          (format:format format:Str "~a~a" 123 321))))

(define-test format-iteration ()
  (is (== "1 2 3 4 5"
          (format:format format:Str "~{~a~^ ~}" (make-list 1 2 3 4 5)))))

(define-test format-jump-iteration ()
  (is (== "1 one 2 two 3 three"
          (format:format format:Str "~{~D ~1:*~R~^ ~}" [1 2 3]))))

;;; Test Failures

(define-test format-not-enough-args ()
  (is (result:err?
       (format::transform-format-args% "test ~a" (make-list))))
  (is (result:err?
       (format::transform-format-args% "test ~a~a"
                                       (make-list
                                        (lisp (-> format::Code) ()
                                          '1))))))

(define-test format-too-many-args ()
  (is (result:err?
       (format::transform-format-args% "test"
                                       (make-list
                                        (lisp (-> format::Code) ()
                                          '1))))))

(define-test format-improper-modifiers ()
  (is (result:err?
       (format::transform-format-args% "~:%" (make-list)))))

(define-test format-unclosed-iteration ()
  (is (result:err?
       (format::transform-format-args% "~{" (make-list))))
  (is (result:err?
       (format::transform-format-args% "~}~{" (make-list)))))


(define-test format-unclosed-justify ()
  (is (result:err?
       (format::transform-format-args% "~<" (make-list))))
  (is (result:err?
       (format::transform-format-args% "~>~<" (make-list)))))

(define-test format-unclosed-case ()
  (is (result:err?
       (format::transform-format-args% "~(" (make-list))))
  (is (result:err?
       (format::transform-format-args% "~)~(" (make-list)))))

(define-test format-jump-outside-iteration ()
  (is (result:err?
       (format::transform-format-args% "~1*" (make-list)))))
