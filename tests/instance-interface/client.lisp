(in-package #:coalton-tests/instance-interface)

(coalton-toplevel
  (define-instance (Needs :a => Needs (Wrapper :a))
    (define (tag _) "wrapper"))
  (define (call-outer) (outer (the Integer 1)))

  (define-instance (Badge Integer)
    (define (badge _) "int"))
  (define-instance (Badge String)
    (define (badge _) "str"))
  (define-instance (Badge Boolean)
    (define (badge _) "bool"))

  (declare client-unpack-pair (Badge :b => SomeBadgedPair * :b -> String))
  (define (client-unpack-pair pair _unrelated)
    (match pair
      ((PackBadgedPair x y) (<> (badge x) (<> "/" (badge y))))))

  (declare client-unpack-witness (Badge :b => BadgeWitness :a * :a * :b -> String))
  (define (client-unpack-witness (HasBadge) value _unrelated)
    (badge value))

  (define (exercise-constrained-constructors)
    (let ((p-int (pack-badged-pair (the Integer 10) (the Integer 20)))
          (p-str (PackBadgedPair "left" "right"))
          (w-int (the (BadgeWitness Integer) (make-badge-witness)))
          (w-str (the (BadgeWitness String) HasBadge)))
      (and (== "int:int" (format-badged-pair p-int "unrelated"))
           (== "str:str" (format-badged-pair p-str True))
           (== "int/int" (client-unpack-pair p-int False))
           (== "str/str" (client-unpack-pair p-str (the Integer 0)))
           (== "int" (format-with-badge-witness w-int 42 "unrelated"))
           (== "str" (format-with-badge-witness w-str "hello" 0))
           (== "int" (client-unpack-witness w-int 99 True))
           (== "str" (client-unpack-witness w-str "ok" False))))))

(cl:assert (cl:string= "wrapper" (coalton (call-outer))))
(cl:assert (cl:eq cl:t (coalton (exercise-constrained-constructors))))
