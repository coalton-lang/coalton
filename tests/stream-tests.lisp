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
    (is (== 72 (unwrap (stream:read s))))
    (is (== (vector:make 101 108) (unwrap (stream:read-vector s 2))))
    (is (== 108 (unwrap (stream:peek s))))
    (is (== (vector:make 108 111) (unwrap (stream:read-vector s 2))))
    (is (result:err? (stream:read s)))))

(define-test test-read-char-stream ()
  (let ((declare s (stream:InputStream Char))
        (s (make-test-stream "Hello")))
    (is (== #\H (unwrap (stream:read s))))
    (is (== (vector:make #\e #\l) (unwrap (stream:read-vector s 2))))
    (is (== #\l (unwrap (stream:peek s))))
    (is (== (vector:make #\l #\o) (unwrap (stream:read-vector s 2))))
    (is (result:err? (stream:read s)))))

(define-test test-read-token-stream ()
  (let ((declare s (stream:InputStream Char))
        (s (make-test-stream "Hello world
I love Coalton!")))
    (is (== "Hello" (into (unwrap (stream:read-word s)))))
    (is (== " world" (into (unwrap (stream:read-line s)))))
    (let partial = (stream:read-line s))
    (matches (Err (stream:EOF _)) partial)
    (match partial
      ((Err (stream:EOF vec)) (is (== "I love Coalton!" (into vec)))))))
