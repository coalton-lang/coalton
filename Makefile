LISP ?= sbcl
TIME ?= /bin/time
## run tests sequentially (t) or concurrently (nil) ?
SEQp ?= nil
  # Is package :compatibility being used?
  ORIGINALp=$(shell grep compat: library/classes.ct && echo nil || echo t)
  ifeq ($(ORIGINALp),t)
    COALTON_NAME = original-coalton
    $(warning Using the original Coalton code)
  else
    COALTON_NAME = coalton-compat
    $(warning Using the Coalton-compat fork code)
  endif
$(warning         With BLDDIR="$(BLDDIR)")
$(warning         With QUICKLISP_HOME="$(QUICKLISP_HOME)")
QUICKLISP_HOME ?= $(HOME)/quicklisp-$(COALTON_NAME)
COALTON_HOME ?= $(shell test -f ./coalton.asd && pwd || echo $(QUICKLISP_HOME)/local-projects/coalton)
## Directory where test output is stored.
TEMP ?= zz-temp
TMPDIR ?= $(HOME)/tmp
BLDDIR = $(HOME)/.cache/$(COALTON_NAME)

## quick - just check these values
# SEQp = t
# CENV ="release"
# CENV ="development"
# CSAFETY = "0"
# CDISABLE_SPECIALIZATION ="0"
# CHEURISTIC_INLINING ="1"

LISP_CACHE = $(BLDDIR)
EXTRA_LOCAL_PROJECTS=$(QUICKLISP_HOME)/local-projects
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
REPO_ROOT=$(abspath $(CURDIR))
LOCAL_SOURCE_REGISTRY=$(REPO_ROOT)//
TEST_FORMS=--load $(QUICKLISP_SETUP) \
	--eval "(ql:quickload :coalton/tests)" \
	--eval "(asdf:test-system :coalton/tests)" \
	--eval "(asdf:load-system :small-coalton-programs)"
VERSION=$(shell cat VERSION.txt | tr -d \" )
### ****** EXTERNAL LIBRARIES NEEDED ******
FSET_REPO=https://github.com/slburson/fset.git
MISC_EXTENSIONS_REPO=https://gitlab.common-lisp.net/misc-extensions/misc-extensions.git
NAMED_READTABLES_REPO=https://github.com/melisgl/named-readtables.git

## Supported lisps:
SBCL_BIN ?= sbcl --dynamic-space-size 2048
SBCL=$(SBCL_BIN) --noinform --non-interactive
# SBCLCLEAN=$(SBCL)
SBCLCLEAN=$(SBCL) --no-userinit --no-sysinit
SBCL_COMP=$(SBCLCLEAN)
CCL_COMP=ccl --no-init --batch --quiet
## **Unsupported** lisps:
ABCL_COMP=abcl --noinit --nosystem --batch
ECL_COMP=ecl --norc -q
ALLEGRO_COMP=alisp --batch
CLASP_COMP=clasp --non-interactive --norc
# # do read ql's setup
# SBCL_COMP=$(SBCL)

QUICKLISP=env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
	$(SBCL) --load $(QUICKLISP_SETUP)

# deal with synonyms
ifeq ($(LISP),clozure)
  LISP=ccl
endif
ifeq ($(LISP),allegro)
  LISP=alisp
endif
## Different kinds of configurations for Coalton
## taken from https://github.com/coalton-lang/coalton/blob/main/.github/workflows/main.yml
CENV ?= "release" "development"
HAS_SAFETY=no
## only sbcl & abcl have a safety level?
ifeq ($(LISP),sbcl)
  HAS_SAFETY=yes
endif
ifeq ($(LISP),abcl)
  HAS_SAFETY=yes
endif
ifeq ($(HAS_SAFETY),yes)
  CSAFETY ?= "3" "0"
else
  CSAFETY ?= "0"
endif
CDISABLE_SPECIALIZATION ?="1" "0"
CHEURISTIC_INLINING ?="1" "0"

CCL=$(shell which -s ccl && echo "$(CCL_COMP)" || echo "echo $(CCL_COMP)")
ECL=$(shell which -s ecl && echo "$(ECL_COMP)" || echo "echo $(ECL_COMP)")
ABCL=$(shell which -s abcl && echo "$(ABCL_COMP)" || echo "echo $(ABCL_COMP)")
ALLEGRO=$(shell which -s alisp && echo "$(ALLEGRO_COMP)" || echo "echo $(ALLEGRO_COMP)")
CLASP=$(shell which -s clasp && echo "$(CLASP_COMP)" || echo "echo $(CLASP_COMP)")

## COMP is the actual compiler to be used - executable with specific arguments
COMP=none
## How to load a file using COMP - minimal default:
runOnFileDefault = env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" $(TIME) $(1) --load $(QUICKLISP_HOME)/asdf.lisp --load $(2)
## How to eval an expression using COMP - minimal default:
runOnExprDefault = env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" $(TIME) $(1) --load $(QUICKLISP_HOME)/asdf.lisp --eval $(2)
## How to eval and load using COMP - minimal default:
runOnExprAndFileDefault = env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" $(TIME) $(1) --load $(QUICKLISP_HOME)/asdf.lisp --eval $(2) --load $(3)
## How to load and eval using COMP - minimal default:
runOnFileAndExprNoEnvDefault = $(TIME) $(1) --load $(QUICKLISP_HOME)/asdf.lisp --load $(2) --eval $(3)
runOnFileAndExprDefault = env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" $(call runOnFileAndExprNoEnvDefault,$(1),$(2),$(3))

LISPEXEC=$(LISP)
  runOnFile = cat /dev/null | $(call runOnFileDefault,$(1),$(2))
  runOnExpr = cat /dev/null | $(call runOnExprDefault,$(1),$(2))
  runOnExprAndFile = cat /dev/null | $(call runOnExprAndFileDefault,$(1),$(2),$(3))
  runOnFileAndExpr = cat /dev/null | $(call runOnFileAndExprDefault,$(1),$(2),$(3))
  runOnFileAndExprNoEnv = cat /dev/null | $(call runOnFileAndExprNoEnvDefault,$(1),$(2),$(3))

ifeq ($(LISP),sbcl)
  COMP=$(SBCLCLEAN)
  runOnFile = $(call runOnFileDefault,$(1),$(2))
  runOnExpr = $(call runOnExprDefault,$(1),$(2))
  runOnExprAndFile = $(call runOnExprAndFileDefault,$(1),$(2),$(3))
  runOnFileAndExpr = $(call runOnFileAndExprDefault,$(1),$(2),$(3))
  runOnFileAndExprNoEnv = $(call runOnFileAndExprNoEnvDefault,$(1),$(2),$(3))
endif
ifeq ($(LISP),alisp)
  COMP=$(ALLEGRO)
  runOnFile = cat $(QUICKLISP_HOME)/asdf.lisp $(2) | env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" $(TIME) $(1)
  runOnExpr = (cat $(QUICKLISP_HOME)/asdf.lisp ; echo $(2)) | env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" $(TIME) $(1)
  runOnExprAndFile = (cat $(QUICKLISP_HOME)/asdf.lisp ; echo $(2) ; cat $(3)) | env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" $(TIME) $(1) 
  runOnFileAndExpr = (cat $(QUICKLISP_HOME)/asdf.lisp $(2) ; echo $(3)) | env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" $(TIME) $(1)
  runOnFileAndExprNoEnv = (cat $(QUICKLISP_HOME)/asdf.lisp $(2) ; echo $(3)) | $(TIME) $(1)
  LISPEXEC=alisp
endif
ifeq ($(LISP),ccl)
  COMP=$(CCL)
  LISPEXEC=ccl
endif
ifeq ($(LISP),abcl)
  COMP=$(ABCL)
endif
ifeq ($(LISP),ecl)
  COMP=$(ECL)
endif
ifeq ($(LISP),clasp)
  COMP=$(CLASP)
  runOnExprAndFile = $(call runOnExprAndFileDefault,$(1),$(2),$(3))
  runOnFileAndExpr = $(call runOnFileAndExprDefault,$(1),$(2),$(3))
  runOnFileAndExprNoEnv = $(call runOnFileAndExprNoEnvDefault,$(1),$(2),$(3))
endif
ifeq ($(COMP),none)
  $(warning Lisp choice "$(LISP)" is unknown. Supported choices are:)
  $(warning     sbcl (default), ccl (aka clozure) )
  $(warning Unsupported choices are (porting to them would be highly appreciated!):)
  $(warning     abcl, ecl, alisp (aka allegro), clasp )
  $(error Unknown lisp "$(LISP)" requested)
endif

## 1: compiler command, 2: what to eval, 3: output file
runOnFileWithOutputForeground = ( ( set -x ; $(call runOnFile,$(1),$(2)) ) 2>&1 | tee $(3) )
runOnFileWithOutputBackground = ( ( set -x ; $(call runOnFile,$(1),$(2)) ) > $(3) 2>&1 & ) ; sleep 2
## If makefile is run with SEQp=t, then execute the tests in the foreground
## (sequentially), otherwise, execute them in the background (concurrently).
ifeq ($(SEQp),t)
  runOnFileWithOutput = $(call runOnFileWithOutputForeground,$(1),$(2),$(3))
else
  runOnFileWithOutput = $(call runOnFileWithOutputBackground,$(1),$(2),$(3))
endif

.PHONY: test test-release test-safe testall

.PHONY: all show-dir lisps-running clean-blddir
.PHONY: get-ql clone-repos setup-ql clean
.PHONY: setup-coalton-sources
.PHONY: test-external-libraries test-external-libraries-all

all:	install-libraries~ testall

install-libraries~: clean-blddir get-ql setup-ql update-repos~
	@touch install-libraries~

get-ql: $(QUICKLISP_HOME)/quicklisp.lisp

$(QUICKLISP_HOME)/quicklisp.lisp:
	mkdir -p $(EXTRA_LOCAL_PROJECTS) && test -d $(EXTRA_LOCAL_PROJECTS)
	test -f $(QUICKLISP_HOME)/quicklisp.lisp \
		|| (cd $(QUICKLISP_HOME) ; curl -O https://beta.quicklisp.org/quicklisp.lisp)

gitClone = cd $(EXTRA_LOCAL_PROJECTS) ; git clone $(2)
gitPull = cd $(1) ; git pull $(2)
gitPullOrClone = d=`dirname $(1)` ; if [ -f "$(1)" ] ; then $(call gitPull,$${d},$(2)) ; else $(call gitClone,$${d},$(2)) ; fi

clone-repos: setup-coalton-sources $(EXTRA_LOCAL_PROJECTS)/fset/fset.asd $(EXTRA_LOCAL_PROJECTS)/misc-extensions/misc-extensions.asd $(EXTRA_LOCAL_PROJECTS)/named-readtables/named-readtables.asd

setup-coalton-sources:
	cd $(EXTRA_LOCAL_PROJECTS) ; rm -rf coalton* ; cp -rp $(REPO_ROOT)/. coalton/

$(EXTRA_LOCAL_PROJECTS)/fset/fset.asd:	update-repos~
	$(call gitPullOrClone,$(EXTRA_LOCAL_PROJECTS)/fset/fset.asd,$(FSET_REPO))

$(EXTRA_LOCAL_PROJECTS)/misc-extensions/misc-extensions.asd:	update-repos~
	$(call gitPullOrClone,$(EXTRA_LOCAL_PROJECTS)/misc-extensions/misc-extensions.asd,$(MISC_EXTENSIONS_REPO))

$(EXTRA_LOCAL_PROJECTS)/named-readtables/named-readtables.asd:	update-repos~
	$(call gitPullOrClone,$(EXTRA_LOCAL_PROJECTS)/named-readtables/named-readtables.asd,$(NAMED_READTABLES_REPO))

update-repos~:
	LISP=$(LISP) \
	  TIME=$(TIME) \
	  BLDDIR="$(BLDDIR)" \
	  QUICKLISP_HOME="$(QUICKLISP_HOME)" \
	  SBCL_BIN="$(SBCL_BIN)" \
	  COALTON_HOME="$(COALTON_HOME)" \
	  TEMP="$(TEMP)" \
	  CENV="$(CENV)" \
	  CSAFETY="$(CSAFETY)" \
	  CDISABLE_SPECIALIZATION="$(CDISABLE_SPECIALIZATION)" \
	  CHEURISTIC_INLINING="$(CHEURISTIC_INLINING)" \
	    make clone-repos
	touch update-repos~

setup-ql: get-ql $(QUICKLISP_SETUP)

$(QUICKLISP_SETUP):
	test -f $(QUICKLISP_SETUP) \
		|| (echo Installing Quicklisp at $(QUICKLISP_HOME) ; \
		    cd $(QUICKLISP_HOME) ; \
	            $(SBCL_COMP) --load "quicklisp.lisp" --eval "(quicklisp-quickstart:install :path \"$(QUICKLISP_HOME)/\")")

got-ql-libs~: update-repos~
	@echo Retrieving further required external libraries for coalton and its tests
	$(call runOnFileAndExprNoEnv,$(COMP),"$(QUICKLISP_SETUP)","(progn (ql:quickload :coalton) (ql:quickload :coalton/tests))")
	touch got-ql-libs~

test-external-libraries:	install-libraries~
	for f in compat/test-external-libraries/*.lisp ; do \
	  $(call runOnExprAndFile,$(COMP),"(load \"$(QUICKLISP_SETUP)\")",$${f}) ; \
	done

test-external-libraries-all:
	for lisp in sbcl ccl alisp ecl abcl ; do \
	  LISP=$${lisp} QUICKLISP_HOME=$(QUICKLISP_HOME) make test-external-libraries ; \
	  res=$$? ; echo '' ; echo Libraries returned $${res} for $${lisp} ; \
	done

show-dir:
	@echo $(COALTON_HOME)
	@echo $(EXTRA_LOCAL_PROJECTS)

## Use whatever variables the user might have defined (first four), then run a
## make testall on two different configurations (just to show how it can be
## done).
test:
	LISP=$(LISP) \
	  TIME=$(TIME) \
	BLDDIR=$(BLDDIR) \
	QUICKLISP_HOME=$(QUICKLISP_HOME) \
	SBCL_BIN=$(SBCL_BIN) \
	COALTON_HOME=$(COALTON_HOME) \
	TEMP=$(TEMP) \
	CENV="development" \
	CSAFETY="3" \
	CDISABLE_SPECIALIZATION="0" \
	CHEURISTIC_INLINING="0 1" \
	make testall

clean-blddir:
	-rm -rf $(BLDDIR)/*$(LISP)* ~/.cache/common-lisp/*

testall:	clean-blddir install-libraries~
	mkdir -p $(TEMP) && test -d $(TEMP)
	mkdir -p $(TMPDIR) && test -d $(TMPDIR) ; \
	export SEQp=$(SEQp) ; \
	export TMPDIR=$(TMPDIR) ; rm $(TMPDIR)/* ; \
	  \
	export COALTON_NAME=$(COALTON_NAME) ; \
	export BLDDIR=$(BLDDIR) ; \
	export QUICKLISP_HOME=$(QUICKLISP_HOME) ; \
	export COALTON_HOME=$(COALTON_HOME) ; \
	for env in $(CENV) ; do export COALTON_ENV=$${env} ; echo COALTON_ENV=$${COALTON_ENV} ; \
	for safety in $(CSAFETY) ; do export SAFETY=$${safety} ; echo SAFETY=$${SAFETY} ; \
	for no_specialisation in $(CDISABLE_SPECIALIZATION); do export COALTON_DISABLE_SPECIALIZATION=$${no_specialisation} ; echo COALTON_DISABLE_SPECIALIZATION=$${COALTON_DISABLE_SPECIALIZATION} ; \
	for heuristic_inlining in $(CHEURISTIC_INLINING) ; do export COALTON_HEURISTIC_INLINING=$${heuristic_inlining} ; echo COALTON_HEURISTIC_INLINING=$${COALTON_HEURISTIC_INLINING} ; \
	  caseNow=coalton-$(VERSION)-$${env}-$${safety}-$${no_specialisation}-$${heuristic_inlining}-$(LISPEXEC) ; \
	  echo Storing fasls in $(BLDDIR)/$${caseNow}\* ; \
	  test -d $(BLDDIR)/$${caseNow}* \
	  && echo Which already exists \
	  || echo Which does not exist already ; \
	  echo Running in the foreground is ${SEQp} ; \
	  echo '' > $(TEMP)/z-out-$${caseNow}.txt ; \
	  $(call runOnFileWithOutput,$(COMP),compat/build-test-configuration.lisp,$(TEMP)/z-out-$${caseNow}.txt) \
	; done \
	; done \
	; done \
	; done

## Show last lines of outputs of different configurations for current lisp and
## its instances that are still running.
## Some extra parentheses added to not match egrep itself.
lisps-running:
	-ls -rt $(TEMP)/z-out-*.txt | egrep "$(LISPEXEC)" | xargs tail -n 4 ; echo ''
	-ps aux | egrep '[0-9] (((((s|a)b)|c|e)cl)|(al)isp|(cl)asp)'

ifeq ($(HAS_SAFETY),yes)
test-safe:
	LISP=$(LISP) \
	  TIME=$(TIME) \
	BLDDIR=$(BLDDIR) \
	QUICKLISP_HOME=$(QUICKLISP_HOME) \
	SBCL_BIN=$(SBCL_BIN) \
	COALTON_HOME=$(COALTON_HOME) \
	TEMP=$(TEMP) \
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
	  TIME=$(TIME) \
	BLDDIR=$(BLDDIR) \
	QUICKLISP_HOME=$(QUICKLISP_HOME) \
	SBCL_BIN=$(SBCL_BIN) \
	COALTON_HOME=$(COALTON_HOME) \
	TEMP=$(TEMP) \
	CENV="release" \
	CSAFETY=$(CSAFETY) \
	CDISABLE_SPECIALIZATION="$(CDISABLE_SPECIALIZATION)" \
	CHEURISTIC_INLINING="$(CHEURISTIC_INLINING)" \
	make testall

.PHONY: docs
docs:
	env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
	  $(SBCL) \
		--load "$(QUICKLISP_SETUP)" \
		--eval "(ql:quickload :coalton/doc :silent t)" \
		--eval "(coalton/doc:write-stdlib-documentation-to-file \"docs/reference.md\")"

.PHONY: web-docs
web-docs:
	env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
		$(SBCL) \
		--load $(QUICKLISP_SETUP) \
		--eval "(ql:quickload :coalton/doc :silent t)" \
		--eval "(coalton/doc:write-stdlib-documentation-to-file \"../coalton-website/content/reference.md\" :backend :hugo :revision \"main\")"


.PHONY: bench
	env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
		COALTON_ENV=release \
		$(SBCL) \
		--load $(QUICKLISP_SETUP) \
		--eval "(ql:quickload :coalton/benchmarks :silent t)" \
		--eval "(sb-ext::without-gcing (coalton-benchmarks:run-benchmarks))"

.PHONY: parser-coverage
parser-coverage:
	mkdir -p coverage-report && test -d coverage-report
	env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
		$(SBCL) \
		--load $(QUICKLISP_SETUP) \
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

$(TEMP)/cleanql-$(LISP).lisp:	Makefile
	mkdir -p $(TEMP) && test -d $(TEMP)
	( echo '(load "'$(QUICKLISP_SETUP)'")' ; \
	  echo '(push (truename "'$(COALTON_HOME)'") asdf:*central-registry*)' ; \
	  echo '(push (truename "'$(EXTRA_LOCAL_PROJECTS)'") ql:*local-project-directories*)' ; \
	  echo '(ql:register-local-projects)' ; \
	  echo '(ql-dist:clean (ql-dist:dist "quicklisp"))' ; \
	  echo '(format t "cleanql-'$(LISP)' asdf:*central-registry*: ~A~%" asdf:*central-registry*)' \
	) > $(TEMP)/cleanql-$(LISP).lisp

clean-quicklisp: $(TEMP)/cleanql-$(LISP).lisp setup-ql
	@echo "Cleaning up old projects in Quicklisp"
	$(call runOnFile,$(COMP),$(TEMP)/cleanql-$(LISP).lisp)

clean-cache:
	@echo "Deleting $(LISP_CACHE)"
	-rm -rf "$(LISP_CACHE)"
	-rm $(TEMP)/cleanql-$(LISP).lisp

cleanall: clean-cache clean-quicklisp
	@echo "All cleaned and reindexed."
	-ls "$(LISP_CACHE)"

clean-n-update-quicklisp: $(TEMP)/cleanql-$(LISP).lisp
	( echo '(ql:update-dist "quicklisp")' ; \
	  echo '(ql:update-client)' \
	) >> $(TEMP)/cleanql-$(LISP).lisp

clean:	clean-cache clean-n-update-quicklisp clean-quicklisp
	-rm -f update-repos~

