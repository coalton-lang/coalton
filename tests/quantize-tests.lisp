(in-package #:coalton-tests)

(deftest test-rounding ()
  (is (= 0 (coalton:coalton (coalton-prelude:floor 0.1))))
  (is (= 1 (coalton:coalton (coalton-prelude:ceiling 0.1))))
  (is (= 0 (coalton:coalton (coalton-prelude:round 0.1))))
  
  (is (= -1 (coalton:coalton (coalton-prelude:floor -0.1))))
  (is (= 0 (coalton:coalton (coalton-prelude:ceiling -0.1))))
  (is (= 0 (coalton:coalton (coalton-prelude:round -0.1))))
  
  (is (= 0 (coalton:coalton (coalton-prelude:floor 0.0))))
  (is (= 0 (coalton:coalton (coalton-prelude:ceiling 0.0))))
  (is (= 0 (coalton:coalton (coalton-prelude:round 0.0))))
  
  (is (= 1 (coalton:coalton (coalton-prelude:round 0.5))))
  (is (= 0 (coalton:coalton (coalton-prelude:round -0.5)))))
