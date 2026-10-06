(in-package #:coalton-native-tests)

;;; The tests below check the laws documented on `bits:Bits`,
;;; `bits:ldb`, and `bits:dpb` against a reference model: the
;;; operations on unbounded integers, computed directly in Common Lisp,
;;; followed by the reduction `wrap` into the type.

(coalton-toplevel
  (declare %logand (Integer * Integer -> Integer))
  (define (%logand a b) (lisp (-> Integer) (a b) (cl:logand a b)))

  (declare %logior (Integer * Integer -> Integer))
  (define (%logior a b) (lisp (-> Integer) (a b) (cl:logior a b)))

  (declare %logxor (Integer * Integer -> Integer))
  (define (%logxor a b) (lisp (-> Integer) (a b) (cl:logxor a b)))

  (declare %lognot (Integer -> Integer))
  (define (%lognot a) (lisp (-> Integer) (a) (cl:lognot a)))

  ;; Shift counts, field sizes, and positions beyond 1024 cannot change
  ;; a result of 64 or fewer bits, and the `Integer` tests use smaller
  ;; ones, so the model clamps them to keep its computations small.
  (declare %ash (Integer * Integer -> Integer))
  (define (%ash a k)
    (lisp (-> Integer) (a k) (cl:ash a (cl:max -1024 (cl:min 1024 k)))))

  (declare %ldb (UFix * UFix * Integer -> Integer))
  (define (%ldb s p a)
    (lisp (-> Integer) (s p a) (cl:ldb (cl:byte (cl:min s 1024) (cl:min p 1024)) a)))

  (declare %dpb (Integer * UFix * UFix * Integer -> Integer))
  (define (%dpb n s p a)
    (lisp (-> Integer) (n s p a) (cl:dpb n (cl:byte (cl:min s 1024) (cl:min p 1024)) a)))

  (declare %wrap-unsigned (UFix * Integer -> :t))
  (define (%wrap-unsigned w n)
    (lisp (-> :t) (w n) (cl:ldb (cl:byte w 0) n)))

  (declare %wrap-signed (UFix * Integer -> :t))
  (define (%wrap-signed w n)
    (lisp (-> :t) (w n)
      (cl:- (cl:ldb (cl:byte w 0) (cl:+ n (cl:ash 1 (cl:1- w)))) (cl:ash 1 (cl:1- w)))))

  (declare %ufix-width UFix)
  (define %ufix-width (lisp (-> UFix) () (cl:integer-length cl:most-positive-fixnum)))

  (declare %ifix-width UFix)
  (define %ifix-width (lisp (-> UFix) () (cl:1+ (cl:integer-length cl:most-positive-fixnum))))

  (declare %wrap-bit (Integer -> Bit))
  (define (%wrap-bit n) (%wrap-unsigned 1 n))

  (declare %wrap-u8 (Integer -> U8))
  (define (%wrap-u8 n) (%wrap-unsigned 8 n))

  (declare %wrap-u16 (Integer -> U16))
  (define (%wrap-u16 n) (%wrap-unsigned 16 n))

  (declare %wrap-u32 (Integer -> U32))
  (define (%wrap-u32 n) (%wrap-unsigned 32 n))

  (declare %wrap-ufix (Integer -> UFix))
  (define (%wrap-ufix n) (%wrap-unsigned %ufix-width n))

  (declare %wrap-u64 (Integer -> U64))
  (define (%wrap-u64 n) (%wrap-unsigned 64 n))

  (declare %wrap-i8 (Integer -> I8))
  (define (%wrap-i8 n) (%wrap-signed 8 n))

  (declare %wrap-i16 (Integer -> I16))
  (define (%wrap-i16 n) (%wrap-signed 16 n))

  (declare %wrap-i32 (Integer -> I32))
  (define (%wrap-i32 n) (%wrap-signed 32 n))

  (declare %wrap-ifix (Integer -> IFix))
  (define (%wrap-ifix n) (%wrap-signed %ifix-width n))

  (declare %wrap-i64 (Integer -> I64))
  (define (%wrap-i64 n) (%wrap-signed 64 n)))

;;; Sample inputs: boundaries of each width, sign changes, and dense bit
;;; patterns, plus shift counts and field bounds on both sides of the
;;; widths, including counts far too large to compute with `cl:ash`.

(coalton-toplevel
  (declare %integer-samples (List Integer))
  (define %integer-samples
    (make-list 0 1 -1 2 -2 3 5 127 128 -128 255 256 -256
               #x7fffffff #x80000000 #xffffffff #x100000000
               #x3fffffffffffffff #x4000000000000000
               #x7fffffffffffffff #x8000000000000000 #x8000000000000001
               #xfffffffffffffffe #xffffffffffffffff -9223372036854775808
               #x5555555555555555 #xaaaaaaaaaaaaaaaa
               #x0123456789abcdef #xfedcba9876543210))

  (declare %u64-samples (List U64))
  (define %u64-samples (map %wrap-u64 %integer-samples))

  (declare %bounded-shift-counts (List Integer))
  (define %bounded-shift-counts
    (make-list -129 -128 -65 -64 -63 -62 -61 -33 -32 -31 -17 -16 -15 -9 -8 -7 -2 -1
               0 1 2 7 8 9 15 16 17 31 32 33 61 62 63 64 65 128 129))

  (declare %shift-counts (List Integer))
  (define %shift-counts
    (Cons -1000000000000 (Cons 1000000000000 %bounded-shift-counts)))

  (declare %non-negative-shift-counts (List Integer))
  (define %non-negative-shift-counts
    (make-list 0 1 2 7 8 9 15 16 17 31 32 33 61 62 63 64 65 128 129 1000000000000))

  (declare %bounded-field-bounds (List UFix))
  (define %bounded-field-bounds
    (make-list 0 1 2 7 8 9 15 16 17 31 32 33 61 62 63 64 65 128))

  (declare %field-bounds (List UFix))
  (define %field-bounds (Cons 1000000000000 %bounded-field-bounds)))

;;; Searching for counterexamples. Each search returns `None` when the
;;; law holds for every combination of inputs, and otherwise the first
;;; failing inputs, which `is` prints on failure.

(coalton-toplevel
  (declare %pairs (List :a * List :b -> List (Tuple :a :b)))
  (define (%pairs xs ys)
    (list:concatMap (fn (x) (map (fn (y) (Tuple x y)) ys)) xs))

  (declare %find-1 ((:a -> Boolean) * List :a -> Optional :a))
  (define (%find-1 law xs)
    (list:find (fn (x) (not (law x))) xs))

  (declare %find-2 ((:a * :b -> Boolean) * List :a * List :b -> Optional (Tuple :a :b)))
  (define (%find-2 law xs ys)
    (list:find (fn (args)
                 (match args
                   ((Tuple x y) (not (law x y)))))
               (%pairs xs ys)))

  (declare %find-3 ((:a * :b * :c -> Boolean) * List :a * List :b * List :c
                    -> Optional (Tuple :a (Tuple :b :c))))
  (define (%find-3 law xs ys zs)
    (list:find (fn (args)
                 (match args
                   ((Tuple x (Tuple y z)) (not (law x y z)))))
               (%pairs xs (%pairs ys zs))))

  (declare %find-4 ((:a * :b * :c * :d -> Boolean) * List :a * List :b * List :c * List :d
                    -> Optional (Tuple :a (Tuple :b (Tuple :c :d)))))
  (define (%find-4 law ws xs ys zs)
    (list:find (fn (args)
                 (match args
                   ((Tuple w (Tuple x (Tuple y z))) (not (law w x y z)))))
               (%pairs ws (%pairs xs (%pairs ys zs))))))

;;; The laws, written once for every instance. Calling them directly
;;; dispatches the methods through instance dictionaries; the
;;; `monomorphize`d wrappers further below compile them with the methods
;;; inlined, so both ways of calling an instance are tested.

(coalton-toplevel
  (declare %method-laws ((bits:Bits :a) (Integral :a) => (Integer -> :a) * :a * :a * Integer -> Boolean))
  (define (%method-laws wrap a b k)
    "Each method is the unbounded operation followed by `wrap`."
    (let ((ia (math:toInteger a))
          (ib (math:toInteger b)))
      (and (== (bits:and a b) (wrap (%logand ia ib)))
           (== (bits:or a b) (wrap (%logior ia ib)))
           (== (bits:xor a b) (wrap (%logxor ia ib)))
           (== (bits:not a) (wrap (%lognot ia)))
           (== (bits:shift k a) (wrap (%ash ia k))))))

  (declare %field-laws ((bits:Bits :a) (Integral :a) => (Integer -> :a) * :a * :a * (Tuple UFix UFix) -> Boolean))
  (define (%field-laws wrap n a size-and-position)
    "`ldb` and `dpb` are the unbounded operations followed by `wrap`."
    (match size-and-position
      ((Tuple s p)
       (and (== (bits:ldb s p a) (wrap (%ldb s p (math:toInteger a))))
            (== (bits:dpb n s p a) (wrap (%dpb (math:toInteger n) s p (math:toInteger a))))))))

  (declare %boolean-algebra-laws (bits:Bits :a => :a * :a * :a -> Boolean))
  (define (%boolean-algebra-laws a b c)
    (and (== (bits:and a b) (bits:and b a))
         (== (bits:or a b) (bits:or b a))
         (== (bits:xor a b) (bits:xor b a))
         (== (bits:and a (bits:and b c)) (bits:and (bits:and a b) c))
         (== (bits:or a (bits:or b c)) (bits:or (bits:or a b) c))
         (== (bits:xor a (bits:xor b c)) (bits:xor (bits:xor a b) c))
         (== (bits:and a (bits:or b c)) (bits:or (bits:and a b) (bits:and a c)))
         (== (bits:or a (bits:and b c)) (bits:and (bits:or a b) (bits:or a c)))
         (== (bits:not (bits:not a)) a)
         (== (bits:not (bits:and a b)) (bits:or (bits:not a) (bits:not b)))
         (== (bits:and a 0) 0)
         (== (bits:and a (bits:not 0)) a)
         (== (bits:or a 0) a)
         (== (bits:xor a 0) a)
         (== (bits:xor a a) 0)
         (== (bits:xor a (bits:not 0)) (bits:not a))))

  (declare %shift-composition-laws (bits:Bits :a => Integer * Integer * :a -> Boolean))
  (define (%shift-composition-laws j k a)
    "For non-negative `j` and `k`."
    (and (== (bits:shift 0 a) a)
         (== (bits:shift j (bits:shift k a)) (bits:shift (+ j k) a))
         (== (bits:shift (negate j) (bits:shift (negate k) a))
             (bits:shift (negate (+ j k)) a))))

  (declare %signed-shift-laws (bits:Bits :a => Integer * :a -> Boolean))
  (define (%signed-shift-laws k a)
    "For non-negative `k`, in signed types and `Integer`."
    (== (bits:shift (negate k) (bits:not a)) (bits:not (bits:shift (negate k) a))))

  (declare %shift-distribution-laws (bits:Bits :a => Integer * :a * :a -> Boolean))
  (define (%shift-distribution-laws i a b)
    (and (== (bits:shift i (bits:and a b)) (bits:and (bits:shift i a) (bits:shift i b)))
         (== (bits:shift i (bits:or a b)) (bits:or (bits:shift i a) (bits:shift i b)))
         (== (bits:shift i (bits:xor a b)) (bits:xor (bits:shift i a) (bits:shift i b)))))

  (declare %field-lens-laws (bits:Bits :a => UFix * :a * :a * :a * (Tuple UFix UFix) -> Boolean))
  (define (%field-lens-laws w m n a size-and-position)
    "`ldb` and `dpb` behave like a getter and setter of the field."
    (match size-and-position
      ((Tuple s p)
       (and (== (bits:dpb (bits:ldb s p a) s p a) a)
            (== (bits:dpb m s p (bits:dpb n s p a)) (bits:dpb m s p a))
            (or (> (+ s p) w)
                (== (bits:ldb s p (bits:dpb n s p a)) (bits:ldb s 0 n))))))))

;;; For each instance, `define-bits-law-test` defines the laws compiled
;;; with the methods inlined, and a test that checks every law both ways.

(cl:defmacro define-bits-law-test (type wrap width
                                   cl:&key signed
                                     (counts '%shift-counts)
                                     (non-negative-counts '%non-negative-shift-counts)
                                     (fields '%field-bounds))
  (cl:flet ((name (control)
              (cl:intern (cl:format cl:nil control (cl:symbol-name type)))))
    (cl:let ((inlined (name "%~A-INLINED-LAWS"))
             (inlined-fields (name "%~A-INLINED-FIELD-LAWS")))
      `(cl:progn
         (coalton-toplevel
           (monomorphize)
           (declare ,inlined (,type * ,type * Integer -> Boolean))
           (define (,inlined a b k)
             (and (%method-laws ,wrap a b k)
                  (%boolean-algebra-laws a b (bits:shift k a))
                  (%shift-distribution-laws k a b)))

           (monomorphize)
           (declare ,inlined-fields (,type * ,type * (Tuple UFix UFix) -> Boolean))
           (define (,inlined-fields n a size-and-position)
             (and (%field-laws ,wrap n a size-and-position)
                  (%field-lens-laws ,width a n a size-and-position))))

         (define-test ,(name "BITS-~A-LAWS") ()
           (let xs = (map ,wrap %integer-samples))
           (let small = (list:take 6 xs))
           (let fields = (%pairs ,fields ,fields))
           (is (== None (%find-3 (fn (a b k) (%method-laws ,wrap a b k)) xs xs ,counts)))
           (is (== None (%find-3 (fn (n a f) (%field-laws ,wrap n a f)) xs xs fields)))
           (is (== None (%find-3 %boolean-algebra-laws xs xs xs)))
           (is (== None (%find-3 %shift-composition-laws
                                 ,non-negative-counts ,non-negative-counts xs)))
           ,@(cl:when signed
               `((is (== None (%find-2 %signed-shift-laws ,non-negative-counts xs)))))
           (is (== None (%find-3 %shift-distribution-laws ,counts xs xs)))
           (is (== None (%find-4 (fn (m n a f) (%field-lens-laws ,width m n a f))
                                 small small xs fields)))
           (is (== None (%find-3 ,inlined xs xs ,counts)))
           (is (== None (%find-3 ,inlined-fields xs xs fields))))))))

(define-bits-law-test Bit %wrap-bit 1)
(define-bits-law-test U8 %wrap-u8 8)
(define-bits-law-test U16 %wrap-u16 16)
(define-bits-law-test U32 %wrap-u32 32)
(define-bits-law-test U64 %wrap-u64 64)
(define-bits-law-test UFix %wrap-ufix %ufix-width)
(define-bits-law-test I8 %wrap-i8 8 :signed cl:t)
(define-bits-law-test I16 %wrap-i16 16 :signed cl:t)
(define-bits-law-test I32 %wrap-i32 32 :signed cl:t)
(define-bits-law-test I64 %wrap-i64 64 :signed cl:t)
(define-bits-law-test IFix %wrap-ifix %ifix-width :signed cl:t)

;; `Integer` is the reference model itself, so its `wrap` is the
;; identity and it has no width. Shift counts and field bounds stay
;; small, since shifting an unbounded integer by 10^12 bits really does
;; build a number of that size.
(coalton-toplevel
  (declare %wrap-integer (Integer -> Integer))
  (define (%wrap-integer n) n))

(define-bits-law-test Integer %wrap-integer 1000000000000
  :signed cl:t
  :counts %bounded-shift-counts
  :non-negative-counts (list:filter (fn (k) (>= k 0)) %bounded-shift-counts)
  :fields %bounded-field-bounds)

(define-test bits-fixed-width-edge-cases ()
  ;; Results always fit the width, including where earlier versions
  ;; returned out-of-range values or failed.
  (is (== -1 (bits:ldb 64 0 (the I64 -1))))
  (is (== 0 (bits:dpb (the U64 1) 1 64 (the U64 0))))
  (is (== -9223372036854775808 (bits:dpb (the I64 1) 1 63 (the I64 0))))
  (is (== -9223372036854775808 (bits:shift 63 (the I64 1))))
  (is (== -9223372036854775808 (math:lsh (the I64 1) (the UFix 63))))
  (is (== -1 (math:rsh (the I64 -9223372036854775808) (the UFix 63))))
  (is (== 0 (bits:shift 1000000000000 (the U64 1))))
  (is (== 0 (math:lsh (the U64 1) (the UFix 1000000000000))))
  (is (== -1 (bits:shift -1000000000000 (the I64 -5))))
  (is (== -128 (bits:shift 1 (the I8 64))))
  (is (== -1 (bits:ldb 8 0 (the I8 -1))))
  (is (== 0 (bits:dpb (the U8 1) 1 8 (the U8 0))))
  (is (== 0 (bits:dpb (the Bit 1) 1 1 (the Bit 0))))
  (is (== 0 (bits:dpb (the UFix 1) 1 %ufix-width (the UFix 0))))
  (is (== -2 (bits:shift 1 (lisp (-> IFix) () cl:most-positive-fixnum)))))

;;; A `Bits` instance whose representation is not a Lisp integer: `ldb`
;;; and `dpb` are defined in terms of the methods, so they work for it.

(coalton-toplevel
  (derive Eq)
  (define-type %Flags (%Flags U64))

  (declare %flags-bits (%Flags -> U64))
  (define (%flags-bits f)
    (match f ((%Flags x) x)))

  (define-instance (Num %Flags)
    (define (+ a b) (%Flags (+ (%flags-bits a) (%flags-bits b))))
    (define (- a b) (%Flags (- (%flags-bits a) (%flags-bits b))))
    (define (* a b) (%Flags (* (%flags-bits a) (%flags-bits b))))
    (define (fromInt n) (%Flags (fromInt n))))

  (define-instance (bits:Bits %Flags)
    (define (bits:and a b) (%Flags (bits:and (%flags-bits a) (%flags-bits b))))
    (define (bits:or a b) (%Flags (bits:or (%flags-bits a) (%flags-bits b))))
    (define (bits:xor a b) (%Flags (bits:xor (%flags-bits a) (%flags-bits b))))
    (define (bits:not a) (%Flags (bits:not (%flags-bits a))))
    (define (bits:shift k a) (%Flags (bits:shift k (%flags-bits a))))))

(define-test bits-ldb-dpb-on-user-instance ()
  (is (== (%Flags #b101) (bits:ldb 3 4 (%Flags #b1010110))))
  (is (== (%Flags #b0111110) (bits:dpb (%Flags #b011) 3 4 (%Flags #b1001110)))))

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
