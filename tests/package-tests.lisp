(in-package #:coalton-tests)

(deftest test-parse-package ()
  (is (packagep (check-parse #'coalton-impl/parser/package:parse-package
                             "(package coalton-unit-test/lib-example-simple)")))
  (is (packagep (check-parse #'coalton-impl/parser/package:parse-package
                             "(package coalton-unit-test/lib-example-complex
  (import
   coalton-library/builtin
   coalton-library/classes
   coalton-library/hash)
  (export
   fst
   snd
   Tuple3
   Tuple4
   Tuple5))"))))
