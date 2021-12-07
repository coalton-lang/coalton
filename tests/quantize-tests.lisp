(in-package #:coalton-tests)

(deftest test-rounding ()
  (is (= 0 (coalton:coalton (coalton-library:floor 0.1))))
  (is (= 1 (coalton:coalton (coalton-library:ceiling 0.1))))
  (is (= 0 (coalton:coalton (coalton-library:round 0.1))))
  
  (is (= -1 (coalton:coalton (coalton-library:floor -0.1))))
  (is (= 0 (coalton:coalton (coalton-library:ceiling -0.1))))
  (is (= 0 (coalton:coalton (coalton-library:round -0.1))))
  
  (is (= 0 (coalton:coalton (coalton-library:floor 0.0))))
  (is (= 0 (coalton:coalton (coalton-library:ceiling 0.0))))
  (is (= 0 (coalton:coalton (coalton-library:round 0.0))))
  
  (is (= 1 (coalton:coalton (coalton-library:round 0.5))))
  (is (= 0 (coalton:coalton (coalton-library:round -0.5)))))
