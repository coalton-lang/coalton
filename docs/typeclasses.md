# TypeClasses

## Syntax

```lisp
;; Defining a class
(define-class (Eq :a)
  (= (fn :a -> :a -> Boolean)))

;; Defining a class with a context
(define-class ((Eq :a) => (Ord :a))
  (< (fn :a -> :a -> Boolean))
  ...)

;; Defining an instance of a class
(define-instance (Eq Boolean)
  (define (== a b)
    (not (xor a b))))

;; Defining an instance with a context
(define-instance ((Eq :a) => (Eq (Optional :a)))
  (define (== a b)
    (match a
      ((Some x)
        (match b
          ((Some y) (== x y))
          ((None) False)))
        ((None)
          (match b
            ((Some _) False)
            ((None) True))))))

;; Declaring a function with type constraints
(declare (fn (Eq :a) => :a -> :a -> Boolean))
(define (f a b)
  (== a b))
```

## Design Decisions

### Higher Kinded Types

Higher kinded types are outside of scope of the initial typeclass implementation.

### Constraints in Class Declarations

Referred to as Decision 9 in Jones 1997.

```lisp
;; Is not allowed
(define-class (Test :a)
  (test (fn (Eq :a) => :a -> :a -> Bool)))

;; Is allowed
(define-class (Test :a)
  (test (fn (Eq :b) => :b -> :b -> Bool)))
```

Constraints of class variables in class methods are not allowed since this complicates code and will result in the constraint being propogated up to the class, which is already supported by the class defintion syntax.

### Self Referential Class Definitions

```lisp
;; Is allowed
(define-class ((Iso :b :a) => (Iso :a :b))
  ...)
```

### Polymorphic Recursion

Contained in Decision 2 in Jones 1997.

```lisp
(define (f x y) ; f is of type (fn (Eq :a)) => :a -> :a -> Boolean)
  (if (== x y)  ; Here we use Eq on :a
    True
    (f (singleton x) (singleton y)))) ; Here we recurse now on (List :a)
                                      ; This creates the constraint (Eq (List :a))
                                      ; which can be simplified to (Eq :a).
```

In order to allow polymorphic recursion, context reduction cannot be deferred to the toplevel since context reduction must be done to ensure finite instance dictionaries.

To allow for polymorphic recursion, context reduction is done before generalization but the _inst_ rule is avoided except in the case of tautological constraints (`Num Int`, etc.). Additionally, inferred type signatures must be entailed by the context given in the type signature.

### Overlapping Instances

Referred to as Decision 3 in Jones 1997.

```lisp
;; We first define a show instance for any type :a
(define-instance (Show :a)
  ...)

;; Then, we define a show for a list of :a
;; This is disallowed as it overlaps with
;; the existing definiton of (Show :a).
(define-instance (Show (List :a))
  ...)
```

Instances are not allowed to overlap. Adding overlapping instances requires careful consideration of when context reduction is allowed to act on constraints since an overly-eager reduction can result in the wrong instance being called. Additionally, there needs to be specific rules for which instances should be called when multiple instances match equally.

In summary, this requires the formalisation of context reduction and defined resolution semantics which are beyond the scope of this implementation.

### Flexible Instances

Referred to as Decision 4 in Jones 1997.

```lisp
;; We can define instances for types beyond just a type constructor with type varaibles.
(define-instance (Eq (Optional Int))
  (define (== a b)
    False))

(define-instance (Eq (Optional String))
  (define (== a b)
    True))
```

In order to allow for non-simple instance types and ensure that context reduction terminates, at least one of the instance types must not be a type variable (optionally, all types must be simple). 

### Repeated Type Variables in Instance Head

Referred to as Decision 5 in Jones 1997.

```lisp
;; The instance of Iso contains :a twice
(define-instance (Iso :a :a)
  (define iso id))
```

In order to allow for repeating type variables in an instance head, we must perform one-way unification to match candidates with constraints.

### Flexible Contexts

Referred to as Decision 6 in Jones 1997.

```lisp
;; This is not allowed because the context of the instance
;; is more complex than the resulting constraint.
(define-instance ((C (List (List :a))) => (C (List :a)))
  (define c undefined)) 
```

With flexible contexts (context allowed to be more complex than resulting instance), the context reduction is not guaranteed to result in a simpler context, leading to non-terminating type checking.

To avoid non-termination, instance contexts must be simpler than the resulting instance context.

# References

Jones 1997, Type classes: an exploration of the design space (https://www.microsoft.com/en-us/research/wp-content/uploads/1997/01/multi.pdf)
