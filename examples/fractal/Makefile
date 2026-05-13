SBCL ?= sbcl
SBCL_DYNAMIC_SPACE_SIZE ?= 4096
QUICKLISP_HOME ?= $(HOME)/quicklisp
QUICKLISP_SETUP ?= $(QUICKLISP_HOME)/setup.lisp
REPO_ROOT := $(abspath ../..)
LOCAL_SOURCE_REGISTRY := $(REPO_ROOT)//:
FRACTAL_CACHE_HOME ?= /tmp/coalton-fractal-release-cache
COALTON_ENV := release
COALTON_DISABLE_SPECIALIZATION := 0
COALTON_HEURISTIC_INLINING ?= 1

.PHONY: run

run:
	env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
		XDG_CACHE_HOME="$(FRACTAL_CACHE_HOME)" \
		COALTON_ENV="$(COALTON_ENV)" \
		COALTON_DISABLE_SPECIALIZATION="$(COALTON_DISABLE_SPECIALIZATION)" \
		COALTON_HEURISTIC_INLINING="$(COALTON_HEURISTIC_INLINING)" \
		$(SBCL) --dynamic-space-size "$(SBCL_DYNAMIC_SPACE_SIZE)" \
		--noinform --no-userinit --no-sysinit \
		--load "$(QUICKLISP_SETUP)" \
		--eval '(asdf:load-system "fractal")' \
		--eval '(fractal:main)' \
		--quit
