(in-package #:coalton-native-tests)

(define-test rbit-test ()

  ;; 01100...001 <= reversed => 100...00110

  (is (==  97 (rbit:rbit (the U8 134))))
  (is (== 134 (rbit:rbit (the U8  97))))

  (is (== 24577 (rbit:rbit (the U16 32774))))
  (is (== 32774 (rbit:rbit (the U16 24577))))

  (is (== 1610612737 (rbit:rbit (the U32 2147483654))))
  (is (== 2147483654 (rbit:rbit (the U32 1610612737))))

  (is (== 6917529027641081857 (rbit:rbit (the U64 9223372036854775814))))
  (is (== 9223372036854775814 (rbit:rbit (the U64 6917529027641081857)))))

(define-test rbitn-test ()

  (is (== 13 (rbit:rbitn 4 (the U8 11))))
  (is (== 11 (rbit:rbitn 4 (the U8 13))))

  (is (== 13 (rbit:rbitn 4 (the U16 11))))
  (is (== 11 (rbit:rbitn 4 (the U16 13))))

  (is (== 13 (rbit:rbitn 4 (the U32 11))))
  (is (== 11 (rbit:rbitn 4 (the U32 13))))

  (is (== 13 (rbit:rbitn 4 (the UFix 11))))
  (is (== 11 (rbit:rbitn 4 (the UFix 13))))

  (is (== 13 (rbit:rbitn 4 (the U64 11))))
  (is (== 11 (rbit:rbitn 4 (the U64 13)))))
