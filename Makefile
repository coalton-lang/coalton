QUICKLISP_HOME=$(HOME)/quicklisp
COALTON_HOME ?= $(shell test -f ./coalton.asd && pwd || echo $(QUICKLISP_HOME)/local-projects/coalton)
LISP ?= sbcl
## run tests sequentially (t) or concurrently (nil) ?
SEQp ?= nil
LISP_CACHE ?= $(HOME)/.cache/common-lisp
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
EXTRA_LOCAL_PROJECTS=$(QUICKLISP_HOME)/local-projects
## Directory where test output is stored.
TEMP=zz-temp
VERSION=$(shell cat VERSION.txt | tr -d \" )

## Supported lisps:
SBCL_BIN ?= sbcl
SBCL=$(SBCL_BIN) --noinform --non-interactive
# SBCLCLEAN=$(SBCL)
SBCLCLEAN=$(SBCL) --no-userinit --no-sysinit
SBCL_COMP=$(SBCLCLEAN)
CCL_COMP=ccl --no-init --batch --quiet
## **Unsupported** lisps:
ABCL_COMP=abcl --noinit --nosystem --batch
ECL_COMP=ecl --norc -q
ALLEGRO_COMP=alisp --batch
# # do read ql's setup
# SBCL_COMP=$(SBCL)

## Different kinds of configurations for Coalton
## taken from https://github.com/coalton-lang/coalton/blob/main/.github/workflows/main.yml
CENV ?="release" "development"
HAS_SAFETY=no
ifeq ($(LISP),sbcl)
  CSAFETY ?="0" "3"
  HAS_SAFETY=yes
endif
ifeq ($(LISP),abcl)
  CSAFETY ?="0" "3"
  HAS_SAFETY=yes
endif
## only sbcl & abcl have a safety level?
CSAFETY ?="0"
CDISABLE_SPECIALIZATION ?="1" "0"
CHEURISTIC_INLINING ?="1" "0"

## quick - just check these values
# CENV ="release"
# CENV ="development"
# CSAFETY ="3"
# CSAFETY ="0"
# CDISABLE_SPECIALIZATION ="0"
# CDISABLE_SPECIALIZATION ="1"
# CHEURISTIC_INLINING ="1"

CCL=$(shell which -s ccl && echo "$(CCL_COMP)" || echo "echo $(CCL_COMP)")
ECL=$(shell which -s ccl && echo "$(ECL_COMP)" || echo "echo $(ECL_COMP)")
ABCL=$(shell which -s ccl && echo "$(ABCL_COMP)" || echo "echo $(ABCL_COMP)")
ALLEGRO=$(shell which -s ccl && echo "$(ALLEGRO_COMP)" || echo "echo $(ALLEGRO_COMP)")

## COMP is the actual compiler to be used - executable with specific arguments
COMP=none
## How to load a file using COMP - default:
runOnFile = $(1) --load $(2)
## How to eval an expression using COMP - default:
runOnExpr = $(1) --eval $(2)
LISPEXEC=$(LISP)
ifeq ($(LISP),sbcl)
  COMP=$(SBCLCLEAN)
endif
ifeq ($(LISP),allegro)
  COMP=$(ALLEGRO)
  runOnFile = cat $(2) | $(1)
  runOnExpr = echo $(2) | $(1)
  LISPEXEC=alisp
endif
ifeq ($(LISP),clozure)
  COMP=$(CCL)
  runOnFile = cat /dev/null | $(1) --load $(2)
  LISPEXEC=ccl
endif
ifeq ($(LISP),abcl)
  COMP=$(ABCL)
  runOnFile = cat /dev/null | $(1) --load $(2)
  runOnExpr = cat /dev/null | $(1) --eval $(2)
endif
ifeq ($(LISP),ecl)
  COMP=$(ECL)
  runOnFile = cat /dev/null | $(1) --load $(2)
  runOnExpr = cat /dev/null | $(1) --eval $(2)
endif
ifeq ($(COMP),none)
  $(warning Lisp choice "$(LISP)" is unknown. Supported choices are:)
  $(warning     sbcl (default), clozure )
  $(warning Unsupported choices are (porting to them would be highly appreciated!):)
  $(warning     abcl, ecl, allegro )
  $(error Unknown lisp "$(LISP)" requested)
endif

## 1: compiler command, 2: what to eval, 3: output file
runOnFileWithOutputForeground = ( ( set -x ; cat /dev/null | $(call runOnFile,$(1),$(2)) ) 2>&1 | tee $(3) )
runOnFileWithOutputBackground = ( ( set -x ; cat /dev/null | $(call runOnFile,$(1),$(2)) ) > $(3) 2>&1 & ) ; sleep 2
## If makefile is run with SEQp=t, then execute the tests in the foreground
## (sequentially), otherwise, execute them in the background (concurrently).
ifeq ($(SEQp),t)
  runOnFileWithOutput = $(call runOnFileWithOutputForeground,$(1),$(2),$(3))
else
  runOnFileWithOutput = $(call runOnFileWithOutputBackground,$(1),$(2),$(3))
endif

.PHONY: test test-release test-safe show-dir testall lisps-running

show-dir:
	@echo $(COALTON_HOME)
	@echo $(EXTRA_LOCAL_PROJECTS)

## Use whatever variables the user might have defined (first four), then run a
## make testall on two different configurations (just to show how it can be
## done).
test:
	LISP=$(LISP) \
	LISP_CACHE=$(LISP_CACHE) \
	SBCL_BIN=$(SBCL_BIN) \
	COALTON_HOME=$(COALTON_HOME) \
	CENV="development" \
	CSAFETY="3" \
	CDISABLE_SPECIALIZATION="0" \
	CHEURISTIC_INLINING="0 1" \
	make testall

testall:
	mkdir -p $(TEMP)
	export COALTON_HOME=$(COALTON_HOME) ; \
	for safety in $(CSAFETY) ; do export SAFETY=$${safety} ; echo SAFETY=$${SAFETY} ; \
	for env in $(CENV) ; do export COALTON_ENV=$${env} ; echo COALTON_ENV=$${COALTON_ENV} ; \
	for no_specialisation in $(CDISABLE_SPECIALIZATION); do export COALTON_DISABLE_SPECIALIZATION=$${no_specialisation} ; echo COALTON_DISABLE_SPECIALIZATION=$${COALTON_DISABLE_SPECIALIZATION} ; \
	for heuristic_inlining in $(CHEURISTIC_INLINING) ; do export COALTON_HEURISTIC_INLINING=$${heuristic_inlining} ; echo COALTON_HEURISTIC_INLINING=$${COALTON_HEURISTIC_INLINING} ; \
	  caseNow=coalton-$(VERSION)-$${env}-$${safety}-$${no_specialisation}-$${heuristic_inlining}-$(LISPEXEC) ; \
	  echo Storing fasls in ~/.cache/common-lisp/$${caseNow}\* ; \
	  test -d ~/.cache/common-lisp/$${caseNow}* \
	  && echo Which already exists \
	  || echo Which does not exist already ; \
	  $(call runOnFileWithOutput,$(COMP),compat/x-build.lisp,$(TEMP)/z-out-$${caseNow}.txt) \
	; done \
	; done \
	; done \
	; done

## We don't know which configuration was requested in the tests, so showing the
## last lines of all test outputs.
lisps-running:
	tail -n 1 $(TEMP)/z-out-*
	ps aux | egrep '[0-9] (((((s|a)b)|c|e)cl)|alisp)'

ifeq ($(HAS_SAFETY),yes)
test-safe:
	LISP=$(LISP) \
	LISP_CACHE=$(LISP_CACHE) \
	SBCL_BIN=$(SBCL_BIN) \
	COALTON_HOME=$(COALTON_HOME) \
	CENV="$(CENV)" \
	CSAFETY="3" \
	CDISABLE_SPECIALIZATION="$(CDISABLE_SPECIALIZATION)" \
	CHEURISTIC_INLINING="$(CHEURISTIC_INLINING)" \
	make testall
else
test-safe:
	echo "$(LISP) has no way to set safety - aborting" >&2 ; exit 1
endif

## Run all tests in release mode
test-release:
	LISP=$(LISP) \
	LISP_CACHE=$(LISP_CACHE) \
	SBCL_BIN=$(SBCL_BIN) \
	COALTON_HOME=$(COALTON_HOME) \
	CENV="release" \
	CSAFETY=$(CSAFETY) \
	CDISABLE_SPECIALIZATION="$(CDISABLE_SPECIALIZATION)" \
	CHEURISTIC_INLINING="$(CHEURISTIC_INLINING)" \
	make testall

.PHONY: docs
docs:
	$(SBCL) \
		 --eval "(ql:quickload :coalton/doc :silent t)" \
		 --eval "(coalton/doc:write-stdlib-documentation-to-file \"docs/reference.md\")"

.PHONY: web-docs
web-docs:
	$(SBCL) \
		 --eval "(ql:quickload :coalton/doc :silent t)" \
		 --eval "(coalton/doc:write-stdlib-documentation-to-file \"../coalton-website/content/reference.md\" :backend :hugo :revision \"main\")"


.PHONY: bench
bench:
	COALTON_ENV=release $(SBCL) \
		 --eval "(ql:quickload :coalton/benchmarks :silent t)" \
		 --eval "(sb-ext::without-gcing (coalton-benchmarks:run-benchmarks))"

.PHONY: parser-coverage
parser-coverage:
	mkdir coverage-report || true
	$(SBCL) \
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

## Not really needed anymore - we don't use quicklisp in the building of the
## tests.
$(TEMP)/cleanql-$(LISP).lisp:	Makefile
	mkdir -p $(TEMP)
	( echo '(load "'$(QUICKLISP_HOME)/setup.lisp'")' ; \
	  echo '(push (truename "'$(COALTON_HOME)'") asdf:*central-registry*)' ; \
	  echo '(push (truename "'$(EXTRA_LOCAL_PROJECTS)'") ql:*local-project-directories*)' ; \
	  echo '(ql-dist:clean (ql-dist:dist "quicklisp"))' ; \
	  echo '(format t "cleanql-'$(LISP)' asdf:*central-registry*: ~A~%" asdf:*central-registry*)' \
	) > $(TEMP)/cleanql-$(LISP).lisp

## Not really needed anymore - we don't use quicklisp in the building of the
## tests.
clean-quicklisp: $(TEMP)/cleanql-$(LISP).lisp
	@echo "Cleaning up old projects in Quicklisp"
	$(call runOnFile,$(COMP),$(TEMP)/cleanql-$(LISP).lisp)

clean-cache:
	@echo "Deleting $(LISP_CACHE)"
	-rm -rf "$(LISP_CACHE)"
	-rm $(TEMP)/cleanql-$(LISP).lisp

cleanall: clean-cache clean-quicklisp
	@echo "All cleaned and reindexed."
	ls "$(LISP_CACHE)"
