(in-package #:coalton-user)

(coalton-toplevel
  (define-class (Into :a :b)
    (into (:a -> :b)))

  (define-instance (Into :a :a)
    (define into id))

  (define-instance (Into String (List Char))
      (define into unpack-string))

  (define-instance (Into (List Char) String)
      (define into pack-string)))
