(in-package #:coalton-native-tests)

(define-test reverse-bits-test ()

  ;; 01100...001 <= reversed => 100...00110

  (is (==  97 (bits:reverse-bits (the U8 134))))
  (is (== 134 (bits:reverse-bits (the U8  97))))

  (is (== 24577 (bits:reverse-bits (the U16 32774))))
  (is (== 32774 (bits:reverse-bits (the U16 24577))))

  (is (== 1610612737 (bits:reverse-bits (the U32 2147483654))))
  (is (== 2147483654 (bits:reverse-bits (the U32 1610612737))))

  (is (== 6917529027641081857 (bits:reverse-bits
                               (the U64 9223372036854775814))))
  (is (== 9223372036854775814 (bits:reverse-bits
                               (the U64 6917529027641081857)))))

(define-test reverse-n-bits-test ()

  (is (== 13 (bits:reverse-n-bits 4 (the U8 11))))
  (is (== 11 (bits:reverse-n-bits 4 (the U8 13))))

  (is (== 13 (bits:reverse-n-bits 4 (the U16 11))))
  (is (== 11 (bits:reverse-n-bits 4 (the U16 13))))

  (is (== 13 (bits:reverse-n-bits 4 (the U32 11))))
  (is (== 11 (bits:reverse-n-bits 4 (the U32 13))))

  (is (== 13 (bits:reverse-n-bits 4 (the UFix 11))))
  (is (== 11 (bits:reverse-n-bits 4 (the UFix 13))))

  (is (== 13 (bits:reverse-n-bits 4 (the U64 11))))
  (is (== 11 (bits:reverse-n-bits 4 (the U64 13)))))
