(defpackage #:coalton-tests/instance-interface
  (:use #:coalton #:coalton-prelude)
  (:import-from #:coalton/types #:Proxy #:proxy-of))
(in-package #:coalton-tests/instance-interface)

;; Both tag methods take a Proxy and ignore it. Only the dictionary determines
;; which string the client sees.
(coalton-toplevel
  (define-type (Wrapper :a) (Wrap :a))
  (define-class (Needs :a)
    (tag (Proxy :a -> String)))
  (define-class (Outer :a)
    (outer (:a -> String)))
  (define-instance (Needs Integer)
    (define (tag _) "integer"))
  (define-instance (Needs (Wrapper :a) => Outer :a)
    (define (outer x) (tag (proxy-of (Wrap x)))))

  (define-class (Badge :a)
    (badge (:a -> String)))

  (define-type SomeBadgedPair
    (declare PackBadgedPair (Badge :a => :a * :a -> SomeBadgedPair)))

  (define-type (BadgeWitness :a)
    (declare HasBadge (Badge :a => BadgeWitness :a)))

  (declare pack-badged-pair (Badge :a => :a * :a -> SomeBadgedPair))
  (define (pack-badged-pair x y)
    (PackBadgedPair x y))

  (declare make-badge-witness (Badge :a => Void -> BadgeWitness :a))
  (define (make-badge-witness)
    HasBadge)

  ;; An unrelated Badge :b dictionary is in scope. The consumer must dispatch
  ;; through the dictionary packaged in the constructor, not the caller's :b.
  (declare format-badged-pair (Badge :b => SomeBadgedPair * :b -> String))
  (define (format-badged-pair pair _unrelated)
    (match pair
      ((PackBadgedPair x y) (<> (badge x) (<> ":" (badge y))))))

  (declare format-with-badge-witness (Badge :b => BadgeWitness :a * :a * :b -> String))
  (define (format-with-badge-witness witness value _unrelated)
    (match witness
      ((HasBadge) (badge value)))))
