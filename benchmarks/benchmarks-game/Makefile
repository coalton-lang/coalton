.PHONY: all clean

all: pidigits

pidigits:
	sbcl --eval '(require :sb-gmp)' \
	     --eval '(setf (get :coalton-config :compiler-mode) "release")' \
	     --eval '(setf (get :coalton-config :perform-specialization) t)' \
	     --eval '(setf (get :coalton-config :emit-type-annotations) t)' \
	     --eval '(asdf:load-system "coalton" :force t)' \
	     --load 'pidigits.lisp' \
             --eval '(sb-ext:save-lisp-and-die "pidigits" :executable t :toplevel (function benchmarks-game/pidigits:main))' \
	     --quit

clean:
	rm -f pidigits
