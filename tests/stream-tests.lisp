(in-package #:coalton-native-tests)

(coalton-toplevel
  (declare make-test-stream (types:RuntimeRepr :elt => String -> (stream:InputStream :elt)))
  (define (make-test-stream str)
    (let prox = types:Proxy)
    (let type = (types:runtime-repr (types:proxy-inner prox)))
    (types:as-proxy-of 
     (lisp (stream:InputStream :elt) (str type)
       (flex:make-flexi-stream
        (flex:make-in-memory-input-stream
         (flex:string-to-octets str))
        :element-type type))
     prox)))

(define-test test-read-u8-stream ()
  (let ((declare s (stream:InputStream U8))
        (s (make-test-stream "Hello")))
    (let s = (stream:make-peekable s))
    (is (== 72 (unwrap (stream:read s))))
    (let ((arr (array:make 2 0))) 
      (stream:read-array s arr)
      (is (== 101 (array:aref arr 0)))
      (is (== 108 (unwrap (stream:peek s))))
      (stream:read-array s arr)
      (is (== 108 (array:aref arr 0))))
    (is (none? (stream:read s)))))

(define-test test-read-char-stream ()
  (let ((declare s (stream:InputStream Char))
        (s (make-test-stream "Hello")))
    (is (== #\H (unwrap (stream:read s))))
    (let ((arr (array:make 2 #\a))) 
      (stream:read-array s arr)
      (is (== #\e (array:aref arr 0)))
      (is (== #\l (unwrap (stream:peek s))))
      (stream:read-array s arr)
      (is (== #\l (array:aref arr 0))))
    (is (none? (stream:read s)))))

;; (define-test test-read-token-stream ()
;;   (let ((declare s (stream:InputStream Char))
;;         (s (make-test-stream "Hello world
;; I love Coalton!")))
;;     (is (== "Hello" (into (unwrap (stream:read-word s)))))
;;     (is (== " world" (into (unwrap (stream:read-line s)))))
;;     (let partial = (stream:read-line s))
;;     (matches (Err (stream:EOF _)) partial)
;;     (match partial
;;       ((Err (stream:EOF vec)) (is (== "I love Coalton!" (into vec)))))))
