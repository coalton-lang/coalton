;;;; hyperdual.lisp
;;;;
;;;; An implementation of Hyperdual numbers for computing second-order
;;;; derivatives of compositions of built-in Coalton functions.

(coalton-library/utils::defstdlib-package #:coalton-library/math/hyperdual
  (:use
   #:coalton
   #:coalton-library/builtin
   #:coalton-library/classes
   #:coalton-library/functions
   #:coalton-library/math/arith
   #:coalton-library/math/elementary
   #:coalton-library/math/integral
   #:coalton-library/math/complex
   #:coalton-library/hash)
  (:local-nicknames
   (#:complex #:coalton-library/math/complex))
  (:export
   #:Hyperdual
   #:d-x
   #:d-xx
   #:partial-x
   #:partial-y
   #:gradient
   #:partial-xx
   #:partial-xy
   #:partial-yy
   #:laplacian
   #:hessian)
  (:documentation
   "An implementation of hyperdual numbers for second-order and
multivariate automatic differentiation.

 ——————————————————————————————————————————————–

For univariate differentiation of a function `f` at a point `x`, apply
`f` to `(Hyperdual x 1 1 0)`. The result will be
`(Hyperdual f(x) df/dx(x) df/dx(x) d²f/dx²(x))`.

You may also use the convenience functions `d-x` and `d-xx` to compute
the first and second derivatives as `(d-x f x)` and `(d-xx f x)`.

 ——————————————————————————————————————————————–

For multivariate differentiation of a function `f` at a point `(x,
y)`, an application of `f` to `(Hyperdual x 1 0 0)` and 
`(Hyperdual y 0 1 0)` will result in  
`(Hyperdual f(x, y) ∂f/∂x(x, y) ∂f/∂y(x, y) ∂²f/∂x∂y(x, y))`. 

Second derivatives of a single argument `xi` of `f` are computed in
the same manner as the univariate case, except the values 
`(Hyperdual xj 0 0 0)` are passed for the remaining arguments, j ≠ i.

You may also use the convenience functions `partial-x`, `partial-y`,
`gradient`, `partial-xx`, `partial-xy`, `partial-yy`, `hessian`, and
`laplacian`, to compute partials of bivariate functions.

 ——————————————————————————————————————————————–

The following list of identities describe the theory of hyperdual numbers.

 :: given (∀i∀j((i ≠ j) → (εᵢεⱼ ≠ 0)) ∧ ∀i(εᵢ² = 0))

 ——————————————————————————————————————————————–

 :: univariate identities

 (1) f(a + bε₁ + cε₂ + dε₁ε₂)
      = f(a) + (bε₁+cε₂+dε₁ε₂)f'(a) + bcε₁ε₂f''(a)
      = f(a) + bf'(a)ε₁ + cf'(a)ε₂ + [df'(a) + bcf''(a)]ε₁ε₂

 (2) f(x + ε₁ + ε₂)
      = f(x) + f'(x)ε₁ + f'(x)ε₂ + f''(x)ε₁ε₂

 :: multivariate identities

 (3) f(a₁ + b₁ε₁ + c₁ε₂ + d₁ε₁ε₂, a₂ + b₂ε₁ + c₂ε₂ + d₂ε₁ε₂)
      = f(a₁, a₂) + (b₁ε₁ + c₁ε₂ + d₁ε₁ε₂)∂f/∂x(a₁, a₂) + b₁c₁ε₁ε₂∂²f/∂x²(a₁, a₂)
        + (b₂ε₁ + c₂ε₂ + d₂ε₁ε₂)∂f/∂y(a₁, a₂) + b₂c₂ε₁ε₂∂²f/∂y² 
        + (b₁c₂ + b₂c₁)ε₁ε₂∂²f/∂x∂y(a₁, a₂)
      = f(a₁, a₂) 
        + (b₁∂f/∂x(a₁, a₂) + b₂∂f/∂y(a₁, a₂))ε₁ 
        + (c₁∂f/∂x(a₁, a₂) + c₂∂f/∂y(a₁, a₂))ε₂
        + (d₁∂f/∂x(a₁, a₂) + d₂∂f/∂y(a₁, a₂) 
           + b₁c₁∂²f/∂x²(a₁, a₂) + b₂c₂∂²f/∂x²(a₁, a₂) 
           + (b₁c₂ + b₂c₁)∂²f/∂x∂y(a₁, a₂))ε₁ε₂

 (4) f(x + ε₁ + ε₂, y)
      = f(x, y) + ∂f/∂x(x, y)ε₁ + ∂f/∂x(x, y)ε₂ + ∂²f/∂x²(x, y)ε₁ε₂

 (5) f(x + ε₁, y + ε₂)
      = f(x, y) + ∂f/∂x(x, y)ε₁ + ∂f/∂y(x, y)ε₂ + ∂²f/∂x∂y(x, y)ε₁ε₂

 (6) f(x, y + ε₁ + ε₂)
      = f(x, y) + ∂f/∂y(x, y)ε₁ + ∂f/∂y(x, y)ε₂ + ∂²f/∂y²(x, y)ε₁ε₂

 :: equivalently

 (1) (f (Hyperdual a b c d))
      = (Hyperdual
         (f a)
         (* b (f' a))
         (* c (f' a))
         (+ (* d (f' a)) (* (* b c) (f'' a))))

 (2) (f (Hyperdual x 1 1 0))
      = (Hyperdual (f x) (f' x) (f' x) (f'' x))

 (3) (f (Hyperdual a1 b1 c1 d1) (Hyperdual a2 b2 c2 d2))
      = (Hyperdual
         (f a1 a2)
         (+ (* b1 (∂f/∂x a1 a2)) (* b2 (∂f/∂y a1 a2)))
         (+ (* c1 (∂f/∂x a1 a2)) (* c2 (∂f/∂y a1 a2)))
         (+ (+ (* d1 (∂f/∂x a1 a2)) (* d2 (∂f/∂y a1 a2)))
            (+ (* (* b1 c1) (∂²f/∂x² a1 a2)) (* (* b2 c2) (∂²f/∂y² a1 a2)))
            (* (+ (* b1 c2) (* b2 c1)) (∂²f/∂x∂y a1 a2)))

 (4) (f (Hyperdual x 1 1 0) (Hyperdual y 0 0 0))
      = (Hyperdual (f x y) (∂f/∂x x y) (∂f/∂x x y) (∂²f/∂x² x y))

 (5) (f (Hyperdual x 1 0 0) (Hyperdual y 0 1 0))
      = (Hyperdual (f x y) (∂f/∂x x y) (∂f/∂y x y) (∂²f/∂x∂y x y))

 (6) (f (Hyperdual x 0 0 0) (Hyperdual y 1 1 0))
      = (Hyperdual (f x y) (∂f/∂x x y) (∂f/∂x x y) (∂²f/∂x² x y))"))

(in-package #:coalton-library/math/hyperdual)

(named-readtables:in-readtable coalton:coalton)

#+coalton-release
(cl:declaim #.coalton-impl/settings:*coalton-optimize-library*)

(coalton-toplevel

  (define-struct (Hyperdual :t)
    "Representation of a hyperdual number in the form `a + bε₁ + cε₂ + dε₁ε₂` where `a`, `b`, `c`, and `d` are real numbers and `ε₁` and `ε₂` satisfy `εᵢ² = 0` and `ε₁ε₂ != 0`.

Note: `Eq`, and `Ord` and `Hash` only make use of the primal component."
    (a "The primal part." :t)
    (b "The coefficient of `ε₁`." :t)
    (c "The coefficient of `ε₂`." :t)
    (d "The coefficient of `ε₁ε₂`." :t))

  ;; utilities

  (declare h (Num :t => Hyperdual :t -> :t -> :t -> :t -> Hyperdual :t))
  (define (h x f0 f1 f2)
    "Compute (f x) given f0 := (f a), f1 := (f' a), and f2 := (f'' a).

Note: See identity (1) in the description of this package (`coalton-library/math/hyperdual`)."
    (let (Hyperdual _ b c d) = x)
    (Hyperdual f0 (* b f1) (* c f1) (+ (* d f1) (* (* b c) f2))))

  (inline)
  (declare square (Num :t => :t -> :t))
  (define (square x) (* x x))
  (inline)
  (declare cube (Num :t => :t -> :t))
  (define (cube x) (* (* x x) x))

  ;; type class instances

  (define-instance (Eq :t => Eq (Hyperdual :t))
    (define (== x y) (== (.a x) (.a y))))

  (define-instance (Ord :t => Ord (Hyperdual :t))
    (define (<=> x y) (<=> (.a x) (.a y))))

  (define-instance (Hash :t => Hash (Hyperdual :t))
    (define (hash x) (hash (.a x))))

  (define-instance (Num :t => Into :t (Hyperdual :t))
    (define (into x) (Hyperdual x 0 0 0)))

  (define-instance (Num :t => Num (Hyperdual :t))
    (define (+ (Hyperdual ax bx cx dx) (Hyperdual ay by cy dy))
      (Hyperdual (+ ax ay) (+ bx by) (+ cx cy) (+ dx dy)))
    (define (- (Hyperdual ax bx cx dx) (Hyperdual ay by cy dy))
      (Hyperdual (- ax ay) (- bx by) (- cx cy) (- dx dy)))
    (define (* (Hyperdual ax bx cx dx) (Hyperdual ay by cy dy))
      (Hyperdual (* ax ay)
                 (+ (* ax by) (* bx ay))
                 (+ (* ax cy) (* cx ay))
                 (+ (+ (* ax dy) (* dx ay)) (+ (* bx cy) (* cx by)))))
    (define (fromInt x) (Hyperdual (fromInt x) 0 0 0)))

  (define-instance (Complex :t => Complex (Hyperdual :t))
    (define (complex a b)
      (complex::%Complex a b))
    (define (real-part a)
      (match a
        ((complex::%Complex a _) a)))
    (define (imag-part a)
      (match a
        ((complex::%Complex _ b) b))))

  (define-instance ((Complex :t) (Into :t (Hyperdual :t)) => Into (Complex :t) (Complex (Hyperdual :t)))
    (define (into z) (complex (into (real-part z)) (into (imag-part z)))))

  (define-instance (Reciprocable :t => Reciprocable (Hyperdual :t))
    (define (/ x y)
      ;; x / y = x * y⁻¹
      (* x (reciprocal y)))
    (define (reciprocal x)
      (let ((ax (.a x))
            ;; f(a) = a⁻¹
            (f0 (reciprocal ax))
            ;; f'(a) = -a⁻² = -(f(a))²
            (f1 (negate (square f0)))
            ;; f''(a) = 2a⁻3 = 2(f(a))³
            (f2 (* 2 (cube f0))))
        (h x f0 f1 f2))))

  (define-instance ((Trigonometric :t) (Reciprocable :t) (Radical :t) => Trigonometric (Hyperdual :t))
    (define (sin x)
      (let ((ax (.a x))
            ;; f(a) = sin(a)
            (f0 (sin ax))
            ;; f'(a) = cos(a)
            (f1 (cos ax))
            ;; f''(a) = -sin(a) = -f(a)
            (f2 (negate f0)))
        (h x f0 f1 f2)))
    (define (cos x)
      (let ((ax (.a x))
            ;; f(a) = cos(a)
            (f0 (cos ax))
            ;; f'(a) = -sin(a)
            (f1 (negate (sin ax)))
            ;; f''(a) = -cos(a) = -f(a)
            (f2 (negate f0)))
        (h x f0 f1 f2)))
    (define (tan x)
      (let ((ax (.a x))
            ;; f(a) = tan(a)
            (f0 (tan ax))
            ;; f'(a) = sec²(a) = ((cos(a))⁻¹)²
            (f1 (square (reciprocal (cos ax))))
            ;; f''(a) = 2sec²(a)tan(a) = 2f(a)f'(a)
            (f2 (* 2 (* f0 f1))))
        (h x f0 f1 f2)))
    (define (asin x)
      (let ((ax (.a x))
            ;; f(a) = arcsin(a)
            (f0 (asin ax))
            ;; f'(a) = (√(1 - a²))⁻¹
            (f1 (reciprocal (sqrt (- 1 (square ax)))))
            ;; f''(a) = x/(√(1 - a²))³
            (f2 (/ ax (sqrt (cube (- 1 (square ax)))))))
        (h x f0 f1 f2)))
    (define (acos x)
      (let ((ax (.a x))
            ;; f(a) = arccos(a)
            (f0 (acos ax))
            ;; f'(a) = -(√(1 - a²))⁻¹
            (f1 (negate (reciprocal (sqrt (- 1 (square ax))))))
            ;; f''(a) = -x/(√(1 - a²))³
            (f2 (negate (/ ax (sqrt (cube (- 1 (square ax))))))))
        (h x f0 f1 f2)))
    (define (atan x)
      (let ((ax (.a x))
            ;; f(a) = arctan(a)
            (f0 (atan ax))
            ;; f'(a) = (1 + a²)⁻¹
            (f1 (reciprocal (1+ (square ax))))
            ;; f''(a) = -2a / (1 + a²)²
            (f2 (* -2 (/ ax (square (1+ (square ax)))))))
        (h x f0 f1 f2)))
    (define pi (Hyperdual pi 0 0 0)))

  (define-instance ((Exponentiable :t) (Reciprocable :t) => Exponentiable (Hyperdual :t))
    (define (exp x)
      (let ((ax (.a x))
            ;; f(a) = eᵃ
            (f0 (exp ax))
            ;; f'(a) = eᵃ = f(a)
            (f1 f0)
            ;; f'(a) = eᵃ = f(a)
            (f2 f0))
        (h x f0 f1 f2)))
    (define (ln x)
      (let ((ax (.a x))
            ;; f(a) = ln(a)
            (f0 (ln ax))
            ;; f'(a) = a⁻¹
            (f1 (reciprocal ax))
            ;; f''(a) = -a⁻² = -(f'(a))²
            (f2 (negate (square f1))))
        (h x f0 f1 f2)))
    (define (pow x y)
      ;; xʸ = exp(ln(x)y)
      (exp (* (ln x) y)))
    (define (log x y)
      ;; logₓy = ln(y) / ln(x)
      (/ (ln x) (ln y)))
    (define ee (Hyperdual ee 0 0 0)))

  (define-instance ((Radical :t) (Reciprocable :t) (Exponentiable :t) => Radical (Hyperdual :t))
    (define (nth-root n x)
      (let ((ax (.a x))
            ;; f(a) = ⁿ√a
            (f0 (nth-root n ax))
            ;; f'(a) = (1/n)(a ^ (1/n - 1)) = f(a) / na
            (f1 (/ f0 (* (fromInt n) ax)))
            ;; f''(a) = (naf'(a) - nf(a)) / (na)²
            ;;        = (f'(a) - f(a)/a) / na
            (f2 (/ (- f1 (/ f0 ax)) (* (fromInt n) ax))))
        (h x f0 f1 f2)))
    (define (sqrt x)
      (let ((ax (.a x))
            ;; f(a) = √a
            (f0 (sqrt ax))
            ;; f'(a) = (2√a)⁻¹ = (2f(a))⁻¹
            (f1 (reciprocal (* 2 f0)))
            ;; f''(a) = -(4(√a)³)⁻¹ = f'(a) / -2a
            (f2 (/ f1 (* -2 ax))))
        (h x f0 f1 f2))))

  ;; functions

  (define-type-alias (UnOp :t) (:t -> :t))
  (define-type-alias (BinOp :t) (:t -> :t -> :t))

  (declare d-x (Num :t => UnOp (Hyperdual :t)  -> :t -> :t))
  (define (d-x f x)
    "Compute f'(x)."
    (.b (f (Hyperdual x 1 1 0))))

  (declare d-xx (Num :t => UnOp (Hyperdual :t)  -> :t -> :t))
  (define (d-xx f x)
    "Compute f''(x)."
    (.d (f (Hyperdual x 1 1 0))))

  (declare partial-x (Num :t => BinOp (Hyperdual :t)  -> :t -> :t -> :t))
  (define (partial-x f x y)
    "Compute ∂f/∂x(x, y)."
    (.b (f (Hyperdual x 1 0 0) (Hyperdual y 0 1 0))))

  (declare partial-y (Num :t => BinOp (Hyperdual :t)  -> :t -> :t -> :t))
  (define (partial-y f x y)
    "Compute ∂f/∂y(x, y)."
    (.c (f (Hyperdual x 1 0 0) (Hyperdual y 0 1 0))))

  (declare gradient (Num :t => BinOp (Hyperdual :t) -> :t -> :t -> List :t))
  (define (gradient f x y)
    "Compute the gradient (∂f/∂x, ∂f/∂y) at the point (x, y)."
    (match (f (Hyperdual x 1 0 0) (Hyperdual y 0 1 0))
      ((Hyperdual _ b c _) (Cons b (Cons c Nil)))))

  (declare partial-xx (Num :t => BinOp (Hyperdual :t)  -> :t -> :t -> :t))
  (define (partial-xx f x y)
    "Compute ∂²f/∂x²(x, y)."
    (.d (f (Hyperdual x 1 1 0) (Hyperdual y 0 0 0))))

  (declare partial-xy (Num :t => BinOp (Hyperdual :t) -> :t -> :t -> :t))
  (define (partial-xy f x y)
    "Compute ∂²f/∂x∂y(x, y)."
    (.d (f (Hyperdual x 1 0 0) (Hyperdual y 0 1 0))))

  (declare partial-yy (Num :t => BinOp (Hyperdual :t) -> :t -> :t -> :t))
  (define (partial-yy f x y)
    "Compute ∂²f/∂y²(x, y)."
    (.d (f (Hyperdual x 0 0 0) (Hyperdual y 1 1 0))))

  (declare laplacian (Num :t => BinOp (Hyperdual :t) -> :t -> :t -> :t))
  (define (laplacian f x y)
    "Compute the Laplacian ∂²f/∂x² + ∂²f/∂y² at the point (x, y)."
    (+ (partial-xx f x y) (partial-yy f x y)))

  (declare hessian (Num :t => BinOp (Hyperdual :t) -> :t -> :t -> List :t))
  (define (hessian f x y)
    "Compute the flat Hessian (∂²f/∂x², ∂²f/∂x∂y, ∂²f/∂y∂x, ∂²f/∂y²) at the point (x, y)."
    (let ((xx (partial-xx f x y))
          (xy (partial-xy f x y))
          (yx xy)
          (yy (partial-yy f x y)))
      (Cons xx (Cons xy (Cons yx (Cons yy Nil)))))))

#+sb-package-locks
(sb-ext:lock-package "COALTON-LIBRARY/MATH/HYPERDUAL")

