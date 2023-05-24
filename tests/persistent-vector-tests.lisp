(in-package #:coalton-tests)

(deftest test-copy-vector ()
  (is (equalp #() (pvec::copy-vector #())))
  (is (equalp #(1) (pvec::copy-vector #(1))))
  (is (equalp #(1 2) (pvec::copy-vector #(1 2))))
  (is (equalp #(1 2 3) (pvec::copy-vector #(1 2) :adjoin 3))))

(deftest test-shrink-vector ()
  (is (equalp #(1 2 3) (pvec::shrink-vector #(1 2 3) 0)))
  (is (equalp #(1 2) (pvec::shrink-vector #(1 2 3) 1)))
  (is (equalp #(1) (pvec::shrink-vector #(1 2 3) 2)))
  (is (equalp #() (pvec::shrink-vector #(1 2 3) 3))))

(defun pvec-equal (a b)
  (and (= (pvec:length a) (pvec:length b))
       (loop :for i :below (pvec:length a)
             :always (equalp (pvec:get a i) (pvec:get b i)))))

(defun list-range (n)
  (loop :for i :below n :collect i))

(defun vec-range (n)
  (coerce (list-range n) 'vector))

;;; It's important to test sizes that are > pvec::+branching-factor+
(deftest test-persistent-vector ()
  ;; test construction
  (is (pvec-equal (pvec:persistent-vector)
                  (pvec:persistent-vector)))
  (is (pvec-equal (pvec:persistent-vector 1 2 3)
                  (pvec:persistent-vector 1 2 3)))
  (is (pvec-equal (pvec:persistent-vector 0 1 2 3)
                  (pvec:range 4)))

  ;; test length
  (dotimes (i 150)
    (is (= i (pvec:length (pvec:range i)))))

  ;; test push and pop
  (let* ((o (pvec:range 150))
         (x (pvec:range 150))
         (y (pvec:push-back x 150)))
    (is (pvec-equal o x))
    (is (pvec-equal y (pvec:range 151)))
    (multiple-value-bind (z el)
        (pvec:pop-back y)
      (is (= el 150))
      (is (pvec-equal z x))))

  ;; test prune
  (is (pvec-equal (pvec:range 150)
                  (pvec:prune (pvec:range 300) 150)))

  ;; test for-each
  (let ((i #b0)
        (p 65))
    (flet ((f (n) (setf i (dpb 1 (byte 1 n) i))))
      (pvec:for-each (pvec:range p) #'f)
      (is (= i (1- (expt 2 p))))))

  ;; test reduce
  (is (= 500500 (pvec:reduce #'+ (pvec:range 1001))))

  ;; test to-list and to-vec
  (is (equalp (list-range 150)
              (pvec:to-list (pvec:range 150))))
  (is (equalp (vec-range 150)
              (pvec:to-vector (pvec:range 150)))))

(deftest test-pvec-subseq ()
  (is (pvec-equal (pvec:subseq (pvec:range 300) 5 155)
                  (pvec:map (lambda (x) (+ 5 x)) (pvec:range 150)))))

(deftest test-pvec-push-pop ()
  (let* ((n (+ 3 (* 5 pvec::+branching-factor+)))
         (x (pvec:persistent-vector)))
    (flet ((p+ (i)
             (setf x (pvec:push-back x i))
             (is (= i (1- (pvec:length x))))
             nil)
           (p- (i)
             (multiple-value-bind (nx p) (pvec:pop-back x)
               (setf x nx)
               (is (= n (+ 1 p i)))
               nil)))
      (dotimes (i n)
        (p+ i))
      (dotimes (i n)
        (p- i)))))


(in-package #:coalton-native-tests)

(define-test simple-seq-ops ()
  (let ((s (the (Seq Integer) (seq:new)))
        (t (seq:push 20 (seq:push 10 s))))
    (is (seq:empty? s))
    (is (== 2 (seq:length t)))
    (is (== 10 (seq:index 0 t)))
    (is (== 10 (seq:head-unsafe t)))
    (is (== 20 (seq:last-unsafe t)))
    (is (== 100 (seq:index 0 (seq:set t 0 100))))))
