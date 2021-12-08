LISP_CACHE ?= $(HOME)/.cache/common-lisp
RIGETTI_LISP_LIBRARY_HOME=../
SBCL_BIN=sbcl
SBCL=$(SBCL_BIN) --noinform --no-userinit --no-sysinit --non-interactive
QUICKLISP_HOME=$(HOME)/quicklisp
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
QUICKLISP=$(SBCL) --load $(QUICKLISP_HOME)/setup.lisp \
	--eval '(push (truename ".") asdf:*central-registry*)' \
	--eval "(push (truename \"$(RIGETTI_LISP_LIBRARY_HOME)\") ql:*local-project-directories*)"

.PHONY: test test-safe
test:
	sbcl --non-interactive \
	     --eval "(asdf:test-system :coalton)"

test-safe:
	sbcl --non-interactive \
	     --eval "(sb-ext:restrict-compiler-policy 'safety 3)" \
	     --eval "(asdf:test-system :coalton)"

.PHONY: docs
docs:
	sbcl --non-interactive \
	     --eval "(ql:quickload :coalton)" \
	     --eval "(with-open-file (out \"docs/reference.md\" :direction :output :if-exists :supersede) \
	               (coalton-impl/doc::write-documentation-for-package :env coalton-impl::*global-environment* :stream out :file-link-prefix \"../src/library/\"))"

###############################################################################
# CLEAN
###############################################################################
.PHONY: clean-quicklisp clean-cache cleanall

clean-quicklisp:
	@echo "Cleaning up old projects in Quicklisp"
	$(QUICKLISP) \
             --eval '(ql-dist:clean (ql-dist:dist "quicklisp"))'

clean-cache:
	@echo "Deleting $(LISP_CACHE)"
	rm -rf "$(LISP_CACHE)"

cleanall: clean-cache clean-quicklisp
	@echo "All cleaned and reindexed."
