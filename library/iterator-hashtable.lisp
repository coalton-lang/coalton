(in-package #:coalton/iterator)

(named-readtables:in-readtable coalton:coalton)

(coalton-toplevel
  (declare remove-duplicates! (Hash :elt => Iterator :elt -> Iterator :elt))
  (define (remove-duplicates! iter)
    "Yield unique elements from ITER in order of first appearance."
    (let ((already-seen (the (coalton/hashtable:Hashtable :elt Unit)
                             (coalton/hashtable:new)))
          (unique? (fn (elt)
                     (match (coalton/hashtable:get already-seen elt)
                       ((Some _) False)
                       ((None)
                        (let (values) = (coalton/hashtable:set! already-seen elt Unit))
                        True)))))
      (filter! unique? iter))))
