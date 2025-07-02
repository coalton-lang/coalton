(cl:defpackage #:fractal-coalton
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:math #:coalton-library/math)
                    (#:bf #:coalton-library/big-float))
  (:export #:lisp-mandel
           #:max-iter))

(cl:in-package #:fractal-coalton)

(coalton-toplevel
  (declare max-iter UFix)
  (define max-iter 200)

  (define (mandel limit x0 y0)
    (rec % ((x 0)
            (y 0)
            (x2 0)
            (y2 0)
            (iter (the UFix 0)))
      (if (not (and (<= (+ x2 y2) 4)
                    (< iter limit)))
          (Tuple3 iter x y)
          (let ((y-next (+ y0 (* 2 (* x y))))
                (x-next (+ x0 (- x2 y2))))
            (% x-next y-next (* x-next x-next) (* y-next y-next) (1+ iter))))))

  (define (smooth x y)
    (let ((log-zn (/ (ln (+ (* x x) (* y y))) 2))
          (nu (/ (ln (/ log-zn (ln 2)))
                 (ln 2))))
      (- 1 nu)))

  (declare lisp-mandel (UFix -> Fraction -> Fraction -> Single-Float))
  (define (lisp-mandel prec x y)
    (bf:set-precision! (+ 64 prec))
    (let ((x-bf (the bf:Big-Float (into x)))
          (y-bf (the bf:Big-Float (into y))))
      (match (mandel max-iter x-bf y-bf)
        ((Tuple3 iter x-out y-out)
         (if (== iter max-iter)
             (lisp Single-Float (iter) (cl:coerce iter 'cl:single-float))
             (let ((r (math:to-fraction (+ (into (the Integer (into iter))) (smooth x-out y-out)))))
               (lisp Single-Float (r)
                 (cl:coerce (cl:/ r max-iter) 'cl:single-float)))))))))

(cl:defpackage #:fractal
  (:use #:cl)
  (:export #:main))

(cl:in-package #:fractal)

(defun scale (x from-min from-max to-min to-max)
  (let ((unit-coord (/ (- x from-min) (- from-max from-min))))
    (+ to-min (* unit-coord (- to-max to-min)))))

(defun clear (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))

(defun color (h)
  (setf h (mod (+ h 0.5) 1.0))
  (let ((kr (mod (+ 5.0 (* 6.0 h)) 6.0))
        (kg (mod (+ 3.0 (* 6.0 h)) 6.0))
        (kb (mod (+ 1.0 (* 6.0 h)) 6.0)))
    (values
     (- 1.0 (max 0 (min kr (- 4.0 kr) 1.0)))
     (- 1.0 (max 0 (min kg (- 4.0 kg) 1.0)))
     (- 1.0 (max 0 (min kb (- 4.0 kb) 1.0))))))

(defun mandel-app ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "Fractal" :w 400 :h 400 :flags '(:shown))
      (multiple-value-bind (width height) (sdl2:get-window-size win)
        (let ((change? t)
              (additional-prec 0)
              down-x down-y
              (target-x-min -200/100)
              (target-x-max   47/100)
              (target-y-min -112/100)
              (target-y-max  112/100))
          (sdl2:with-renderer (renderer win :flags '(:accelerated))
            (sdl2:with-event-loop (:method :poll)
              (:keyup
               (:keysym keysym)
               (when (sdl2:scancode= (sdl2:scancode-value keysym) ':scancode-escape)
                 (sdl2:push-event ':quit)))

              (:mousebuttondown
               (:x x :y y)
               (setf down-x x
                     down-y y))

              (:mousebuttonup
               (:x x :y y)
               (psetq target-x-min (scale (min down-x x) 0 width target-x-min target-x-max)
                      target-x-max (scale (max down-x x) 0 width target-x-min target-x-max)
                      target-y-min (scale (min down-y y) 0 height target-y-min target-y-max)
                      target-y-max (scale (max down-y y) 0 height target-y-min target-y-max))
               (incf additional-prec
                     (integer-length (max (ceiling width (abs (- down-x x)))
                                          (ceiling height (abs (- down-y y))))))
               (format t "additional-prec=~A [~A, ~A] x [~A, ~A]~%"
                       additional-prec
                       target-x-min target-x-max
                       target-y-min target-y-max)
               (setf change? t))

              (:idle
               ()
               (when change?
                 (setf change? nil)
                 (fresh-line)
                 (clear renderer)
                 
                 (dotimes (px width)
                   ;; Print some progress to the console.
                   (when (zerop (mod px 8))
                     (write-char #\.) (finish-output))
                   (dotimes (py height)
                     (let ((escape
                             (fractal-coalton:lisp-mandel
                              additional-prec
                              (scale px 0 width target-x-min target-x-max)
                              (scale py 0 height target-y-min target-y-max))))
                       (cond
                         ((= escape fractal-coalton:max-iter)
                          (sdl2:set-render-draw-color renderer 0 0 0 0))
                         (t
                          (multiple-value-bind (r g b) (color escape)
                            (setf r (floor (* 255 r))
                                  g (floor (* 255 g))
                                  b (floor (* 255 b)))
                            (sdl2:set-render-draw-color renderer r g b 0)))))
                     (sdl2:render-draw-point renderer px py)))
                 (sdl2:render-present renderer))
	       (sdl2:delay 1000))

              (:quit
               ()
               t))))))))

(defun main ()
  (sdl2:make-this-thread-main #'mandel-app))
