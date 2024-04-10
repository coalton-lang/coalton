# How Type Classes are Compiled in Coalton

## Function Calls
 
Type class constraints on functions are turned into additional parameters. These additional parameters are used to pass type class dictionaries. For example the following type `f :: (Num :a) (Ord :a) => :a -> :a -> :a` would be compiled to the following lisp function definition.

```lisp
(defun f (G103 G104 A-27 B-28)
  ...)
```

Here `G103` and `G104` will hold the `Num` and `Ord` instance dictionaries for whichever `:a` is chosen by the caller. Variables with constraints are wrapped in a function.

## Classes

Each class defines a struct with fields for methods and superclasses. For instance `Num` looks like:

```lisp
(defstruct CLASS/NUM
  SUPER-0 ; holds CLASS/EQ
  +       ; remaining fields hold methods
  -
  *
  FROMINT)
```

Classes also define stub functions for dispatching methods on a dictionary. Each stub function selects the correct field from a dict and forwards the remaining arguments. See the definition of the `+` stub function below:

```lisp
(defun + (dict a b)
  (funcall (CLASS/NUM-+ dict) a b))
```

## Instances

Instances without constraints are compiled to variables.

```lisp
(defvar |INSTANCE/NUM INTEGER|
  (make-CLASS/NUM
   :SUPER-0 |INSTANCE/EQ INTEGER|
   :+ (lambda (a b)
        (cl:+ a b))
   :- (lambda (a b)
        (cl:- a b))
   :* (lambda (a b)
        (cl:* a b))
   :FROMINT (lambda (a)
              a)))
```

Instances with constraints such as such as `Eq :a => (Eq (Optional :a))` are compiled to functions.

```lisp
(defun |INSTANCE/EQ OPTIONAL :A| (G105)
  (make-CLASS/NUM ...))
```

Each method definion in an instance is also compiled to a seperate function. These functions are used for static method calls. The `==` method on `Eq :a => (Eq (Optional :a))` would be compiled to:

```lisp
(defun |INSTANCE/EQ OPTIONAL :A-=| (G106 a b)
  ...)
```

## Static Method Inlining

Coalton maintains a mapping of `(method, instance) -> static method`. These mappings are used to rewrite method calls on instances known at compile time.

* `(+ |INSTANCE/NUM INTEGER| 1 2)` becomes `(|INSTANCE/NUM INTEGER-+| 1 2)`
* `(== (|INSTANCE/EQ OPTIONAL :A| G106) (Some a) (Some b))` becomes `(|INSTANCE/EQ OPTIONAL :A-==| G106 (Some a) (Some b))`

Note that in the second example, the context variable `G106` become a parameter to `|INSTANCE/EQ OPTIONAL :A-==|`.
