(coalton-library/utils:defstdlib-package #:coalton-library/string-builder
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes)
  (:local-nicknames (#:addr #:coalton-library/addressable)
                    (#:str #:coalton-library/string)
                    (#:real #:coalton-library/math/real)
                    (#:iter #:coalton-library/iterator))
  (:export
   #:new
   #:with-capacity
   #:length
   #:capacity
   #:remaining-capacity
   #:push-char!
   #:push-str!
   #:reserve!
   #:empty!
   #:finalize
   #:non-copying-finalize!))

(cl:in-package #:coalton-library/string-builder)

#+coalton-release
(cl:declaim #.coalton-impl:*coalton-optimize-library*)

(coalton-toplevel
  (repr :native (cl:and cl:string (cl:satisfies cl:adjustable-array-p) (cl:satisfies cl:array-has-fill-pointer-p)))
  (define-type StringBuilder
    "A mutable buffer of characters to be used when constructing strings while minimizing copying.")

  (define-instance (addr:Addressable StringBuilder)
    (define (addr:eq? a b)
      (addr::unsafe-internal-eq? a b)))

  (declare with-capacity (UFix -> StringBuilder))
  (define (with-capacity cap)
    "Construct a `StringBuffer' with enough space to write at least CAP characters without reallocating."
    (let actual-cap = (upgrade-reserve-size cap))
    (lisp StringBuilder (actual-cap)
      (cl:make-array actual-cap
                     :element-type 'cl:character
                     :fill-pointer 0
                     :adjustable cl:t
                     :initial-element (cl:code-char 0))))

  (declare length (StringBuilder -> UFix))
  (define (length sb)
    "The number of characters already written to SB."
    (lisp UFix (sb) (cl:length sb)))

  (declare capacity (StringBuilder -> UFix))
  (define (capacity sb)
    "The total number of characters for which space is reserved in SB, including characters already written."
    (lisp UFix (sb) (cl:array-dimension sb 0)))

  (declare remaining-capacity (StringBuilder -> UFix))
  (define (remaining-capacity sb)
    "The number of characters which can be written to SB without reallocating."
    (- (capacity sb) (length sb)))

  (declare default-initial-capacity UFix)
  (define default-initial-capacity 128) ; a nice round number

  (declare new (Unit -> StringBuilder))
  (define (new)
    "Construct a `StringBuilder' with the default initial capacity."
    (with-capacity default-initial-capacity))

  (declare grow-size UFix)
  (define grow-size
    "The minimum number of characters for which to allocate space when extending a `StringBuilder'."
    default-initial-capacity) ; for lack of a better idea

  (declare upgrade-reserve-size (UFix -> UFix))
  (define (upgrade-reserve-size base)
    "The smallest multiple of `grow-size' larger than BASE.

When growing a `StringBuilder', we'd prefer to make fewer, larger allocations, to avoid the cost of
copying. As such, each allocation for a `StringBuilder' will be at least `grow-size' chars, and will always be
a multiple of `grow-size'."
    (* grow-size (into (real:ceiling/ (into base) (into grow-size)))))

  (declare reserve! (UFix -> StringBuilder -> Unit))
  (define (reserve! count sb)
    "Ensure that SB has space for at least COUNT more characters"
    (when (< (remaining-capacity sb) count)
      (let new-capacity = (upgrade-reserve-size (+ count (length sb))))
      (lisp :any (sb new-capacity)
        (cl:adjust-array sb new-capacity
                         :initial-element (cl:code-char 0)))))

  (declare empty! (StringBuilder -> Unit))
  (define (empty! sb)
    "Remove the contents of SB, leaving it with a `length' of 0.

This operation is quite efficient, so mutable string-wrangling algorithms may find it desirable to keep a
single `StringBuffer' with a large capacity sitting around and `empty!' it when starting a new operation,
rather than using a new `StringBuffer' for each new operation."
    (lisp :any (sb)
      (cl:setf (cl:fill-pointer sb) 0))
    Unit)

  ;; the various `push-*!' functions return the `StringBuilder' so they can be passed to `fold' and friends.
  (declare push-char! (StringBuilder -> Char -> StringBuilder))
  (define (push-char! sb ch)
    "Append CH to the end of SB."
    (lisp :any (sb ch)
      (cl:vector-push-extend ch sb (coalton grow-size)))
    sb)

  (declare push-str! (StringBuilder -> String -> StringBuilder))
  (define (push-str! sb str)
    "Append STR to the end of SB."
    (let needed-cap = (into (str:length str)))
    (when (< (remaining-capacity sb) needed-cap)
      (reserve! needed-cap sb))
    (iter:fold! push-char! sb (iter:string-chars str)))

  (declare non-copying-finalize! (StringBuilder -> String))
  (define (non-copying-finalize! sb)
    "Convert SB into a `String', reusing its memory.

This function consumes its argument; operating on SB after invoking `non-copying-finalize!' on it invokes
undefined behavior."
    (lisp String (sb) sb))
  

  (declare finalize (StringBuilder -> String))
  (define (finalize sb)
    "Copy the contents of SB into a fresh immutable `String'.

Unlike `non-copying-finalize!', `finalize' will allocate a new string and copy SB's contents into it. This
way, it remains safe to reference SB after finalization."
    (let len = (length sb))
    (lisp String (sb len)
      (cl:make-array len
                     :element-type 'cl:character
                     :initial-contents sb)))

  (define-instance (Into StringBuilder String)
    (define into finalize)))
