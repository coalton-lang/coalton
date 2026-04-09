SBCL_BIN ?= sbcl
SBCL = $(SBCL_BIN) --noinform --no-userinit --no-sysinit --non-interactive
QUICKLISP_HOME ?= $(HOME)/quicklisp
QUICKLISP_SETUP = $(QUICKLISP_HOME)/setup.lisp
REPO_ROOT = $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/..)
LOCAL_SOURCE_REGISTRY = $(REPO_ROOT)//

.PHONY: mine clean

mine:
	env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
		$(SBCL) \
		--load $(QUICKLISP_SETUP) \
		--eval '(asdf:load-system "mine" :verbose t)' \
		--eval '(mine/app/executable:build)'

clean:
	find . -type f -name '*.fasl' -print -delete
	rm -f mine
