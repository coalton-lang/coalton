LISP_CACHE ?= $(HOME)/.cache/common-lisp
SBCL_BIN=sbcl
SBCL=$(SBCL_BIN) --noinform --no-userinit --no-sysinit --non-interactive
QUICKLISP_HOME=$(HOME)/quicklisp
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
QUICKLISP=$(SBCL) --load $(QUICKLISP_HOME)/setup.lisp \
	--eval '(push (truename ".") asdf:*central-registry*)' \
	--eval "(push (truename \"../\") ql:*local-project-directories*)"

.PHONY: test test-safe
test:
	sbcl --noinform \
		--non-interactive \
		--eval "(asdf:test-system :coalton)"

test-safe:
	sbcl --noinform \
		 --non-interactive \
		 --eval "(sb-ext:restrict-compiler-policy 'safety 3)" \
		 --eval "(asdf:test-system :coalton)"

.PHONY: docs
docs:
	sbcl --noinform \
		 --non-interactive \
		 --eval "(ql:quickload :coalton/doc :silent t)" \
		 --eval "(coalton-doc:write-stdlib-documentation-to-file \"docs/reference.md\")"

.PHONY: web-docs
web-docs:
	sbcl --noinform \
		 --non-interactive \
		 --eval "(ql:quickload :coalton/doc :silent t)" \
		 --eval "(coalton-doc:write-stdlib-documentation-to-file \"../coalton-website/content/reference.md\" :backend :hugo)"


.PHONY: bench
bench:
	COALTON_ENV=release sbcl --noinform \
		 --non-interactive \
		 --eval "(ql:quickload :coalton/benchmarks :silent t)" \
		 --eval "(sb-ext::without-gcing (coalton-benchmarks:run-benchmarks))"

.PHONY: parser-coverage
parser-coverage:
	mkdir coverage-report || true
	sbcl --noinform \
		--non-interactive \
		--eval "(require :sb-cover)" \
		--eval "(declaim (optimize sb-cover:store-coverage-data))" \
		--eval "(asdf:test-system :coalton :force '(:coalton :coalton/tests))" \
		--eval "(sb-cover:report \
	              \"coverage-report/\" \
	              :if-matches (lambda (pathname) \
                                (string= (directory-namestring pathname) \
                                         (directory-namestring (merge-pathnames \"src/parser/\" (asdf:system-source-directory \"coalton\"))))))"
	open coverage-report/cover-index.html


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
