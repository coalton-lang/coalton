(cl:in-package #:coalton-native-tests)

(coalton-toplevel
  (define *split-path* "wow/ok/dir/file.txt")

  (define *split-path-undotted* (make-list
                             (make-list #\w #\o #\w #\/ #\o #\k #\/ #\d #\i #\r #\/ #\f #\i #\l #\e)
                             (make-list #\t #\x #\t)))
  
  (define *split-path-unslashed* (make-list
                              (make-list #\w #\o #\w)
                              (make-list #\o #\k)
                              (make-list #\d #\i #\r)
                              (make-list #\f #\i #\l #\e #\. #\t #\x #\t))))

;;;
;;; splitting lists
;;;

(define-test split-list ()
  (is (== (iter:collect! (list:split #\. (iter:collect! (string:chars *split-path*)))) *split-path-undotted*))
  (is (== (iter:collect! (list:split #\/ (iter:collect! (string:chars *split-path*)))) *split-path-unslashed*))
  (is (== (iter:collect! (list:split 2 (make-list 1 2 3 4 2 5 2))) (make-list (make-list 1)
                                                                               (make-list 3 4)
                                                                               (make-list 5))))
  (is (== (iter:collect! (list:split #\A (iter:collect! (string:chars "BANANA"))))
          (make-list (make-list #\B)
                     (make-list #\N)
                     (make-list #\N)))))

;;;
;;; splitting vectors
;;;


(define-test split-vector ()
  (is (== (iter:collect! (vector:split #\. (list->vec (iter:collect! (string:chars *split-path*)))))
          (map list->vec *split-path-undotted*)))
  (is (== (iter:collect! (vector:split #\/ (list->vec (iter:collect! (string:chars *split-path*)))))
          (map list->vec *split-path-unslashed*))))

;;;
;;; splitting vector
;;;


(coalton-toplevel

  (define (seq-num-list)
    (the (seq:Seq Integer) (iter:collect! (iter:up-to 10)))))

(define-test split-seq ()
  (is (== (the (List (seq:Seq Integer))
               (iter:collect! (seq:split 5 (seq-num-list))))
          (make-list (the (seq:Seq Integer) (iter:collect! (iter:up-to 5)))
                     (the (seq:Seq Integer) (iter:collect! (map (fn (x) (+ x 6)) (iter:up-to 4))))))))
