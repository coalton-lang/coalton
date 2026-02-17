LISP ?= sbcl
## run tests sequentially (t) or concurrently (nil) ?
SEQp ?= nil
  ORIGINALp=$(shell test -f ./compat/compatibility-layer.lisp && echo nil || echo t)
  ifeq ($(ORIGINALp),t)
    BLDDIR ?= $(HOME)/.cache/coalton-common-lisp
    QUICKLISP_HOME ?= $(HOME)/quicklisp-coalton-common-lisp
    $(warning Using the original Coalton code)
  else
    BLDDIR ?= $(HOME)/.cache/compat-coalton-common-lisp
    QUICKLISP_HOME ?= $(HOME)/quicklisp-compat-coalton-common-lisp
    $(warning Using the Coalton-compat fork code)
  endif
$(warning         With BLDDIR="$(BLDDIR)")
$(warning         With QUICKLISP_HOME="$(QUICKLISP_HOME)")
COALTON_HOME ?= $(shell test -f ./coalton.asd && pwd || echo $(QUICKLISP_HOME)/local-projects/coalton)
## Directory where test output is stored.
TEMP ?= zz-temp

## quick - just check these values
# CENV ="release"
# CENV ="development"
# CSAFETY = "3"
# CDISABLE_SPECIALIZATION ="0"
# CHEURISTIC_INLINING ="1"

LISP_CACHE = $(BLDDIR)
EXTRA_LOCAL_PROJECTS=$(QUICKLISP_HOME)/local-projects
QUICKLISP_SETUP=$(QUICKLISP_HOME)/setup.lisp
VERSION=$(shell cat VERSION.txt | tr -d \" )
### ****** EXTERNAL LIBRARIES NEEDED ******
## Latest official FSet version is broken on abcl (and doesn't pass the
## self-test in ecl).
# FSET_REPO=https://github.com/slburson/fset.git
## This fork of a previous FSet version seems to work on abcl (though it doesn't
## pass FSet's self-test on either of abcl or ecl).
FSET_REPO=https://github.com/c-kloukinas/fset.git
MISC_EXTENSIONS_REPO=https://gitlab.common-lisp.net/misc-extensions/misc-extensions.git
NAMED_READTABLES_REPO=https://github.com/melisgl/named-readtables.git

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
CLASP_COMP=clasp --non-interactive
# # do read ql's setup
# SBCL_COMP=$(SBCL)

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
# not fully supported but we can proclaim it globally
ifeq ($(LISP),ecl)
  HAS_SAFETY=yes
endif
ifeq ($(LISP),ccl)
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
runOnFileDefault = $(1) --load $(2)
## How to eval an expression using COMP - minimal default:
runOnExprDefault = $(1) --eval $(2)
## How to eval and load using COMP - minimal default:
runOnExprAndFileDefault = $(1) --eval $(2) --load $(3)
LISPEXEC=$(LISP)
  runOnFile = cat /dev/null | $(call runOnFileDefault,$(1),$(2))
  runOnExpr = cat /dev/null | $(call runOnExprDefault,$(1),$(2))
  runOnExprAndFile = cat /dev/null | $(call runOnExprAndFileDefault,$(1),$(2),$(3))

ifeq ($(LISP),sbcl)
  COMP=$(SBCLCLEAN)
  runOnFile = $(call runOnFileDefault,$(1),$(2))
  runOnExpr = $(call runOnExprDefault,$(1),$(2))
  runOnExprAndFile = $(call runOnExprAndFileDefault,$(1),$(2),$(3))
endif
ifeq ($(LISP),alisp)
  COMP=$(ALLEGRO)
  runOnFile = cat $(2) | $(1)
  runOnExpr = echo $(2) | $(1)
  runOnExprAndFile = (echo $(2) ; cat $(3)) | $(1) 
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
.PHONY: install-libraries get-ql clone-repos setup-ql get-ql-libs clean
.PHONY: test-external-libraries test-external-libraries-all

all:	install-libraries testall

install-libraries:	get-ql clone-repos setup-ql get-ql-libs

get-ql: $(QUICKLISP_HOME)/quicklisp.lisp

$(QUICKLISP_HOME)/quicklisp.lisp:
	mkdir -p $(EXTRA_LOCAL_PROJECTS) && test -d $(EXTRA_LOCAL_PROJECTS)
	test -f $(QUICKLISP_HOME)/quicklisp.lisp \
		|| (cd $(QUICKLISP_HOME) ; curl -O https://beta.quicklisp.org/quicklisp.lisp)

gitClone = cd $(EXTRA_LOCAL_PROJECTS) ; git clone $(2)
gitPull = cd $(1) ; git pull $(2)
gitPullOrClone = d=`dirname $(1)` ; if [ -f "$(1)" ] ; then $(call gitPull,$${d},$(2)) ; else $(call gitClone,$${d},$(2)) ; fi

clone-repos: $(EXTRA_LOCAL_PROJECTS)/fset/fset.asd $(EXTRA_LOCAL_PROJECTS)/misc-extensions/misc-extensions.asd $(EXTRA_LOCAL_PROJECTS)/named-readtables/named-readtables.asd

$(EXTRA_LOCAL_PROJECTS)/fset/fset.asd:	update-repos~
	$(call gitPullOrClone,$(EXTRA_LOCAL_PROJECTS)/fset/fset.asd,$(FSET_REPO))

$(EXTRA_LOCAL_PROJECTS)/misc-extensions/misc-extensions.asd:	update-repos~
	$(call gitPullOrClone,$(EXTRA_LOCAL_PROJECTS)/misc-extensions/misc-extensions.asd,$(MISC_EXTENSIONS_REPO))

$(EXTRA_LOCAL_PROJECTS)/named-readtables/named-readtables.asd:	update-repos~
	$(call gitPullOrClone,$(EXTRA_LOCAL_PROJECTS)/named-readtables/named-readtables.asd,$(NAMED_READTABLES_REPO))

update-repos~:
	touch update-repos~

setup-ql: $(QUICKLISP_HOME)/setup.lisp

$(QUICKLISP_HOME)/setup.lisp:
	test -f $(QUICKLISP_HOME)/setup.lisp \
		|| (echo Installing Quicklisp at $(QUICKLISP_HOME) ; \
		    cd $(QUICKLISP_HOME) ; \
	            $(SBCL_COMP) --load "quicklisp.lisp" --eval "(quicklisp-quickstart:install :path \"$(QUICKLISP_HOME)/\")")

get-ql-libs:
	@echo Retrieving further required external libraries for coalton and its tests
	$(SBCL_COMP) --load "$(QUICKLISP_HOME)/setup.lisp" \
			--eval "(ql:quickload :coalton)" \
			--eval "(ql:quickload :coalton/doc :silent t)" \
			--eval "(ql:quickload :coalton/benchmarks :silent t)" \
			--eval "(ql:quickload :coalton/tests)"

test-external-libraries:	install-libraries
	for f in compat/test-external-libraries/*.lisp ; do \
	  $(call runOnExprAndFile,$(COMP),"(load \"$(QUICKLISP_HOME)/setup.lisp\")",$${f}) ; \
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
	rm -rf $(BLDDIR)/*$(LISP)* $(TEMP)/z-out-*$(LISP)*

testall:	clean-blddir
	test -f $(QUICKLISP_HOME)/setup.lisp \
		|| QUICKLISP_HOME=$(QUICKLISP_HOME) make install-libraries
	mkdir -p $(TEMP) && test -d $(TEMP)
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
	  $(call runOnFileWithOutput,$(COMP),compat/build-test-configuration.lisp,$(TEMP)/z-out-$${caseNow}.txt) \
	; done \
	; done \
	; done \
	; done

## Show last lines of outputs of different configurations for current lisp and
## its instances that are still running.
## Some extra parentheses added to not match egrep itself.
lisps-running:
	-ls -rt $(TEMP)/z-out-*.txt | egrep "$(LISPEXEC)" | xargs tail -n 1
	-ps aux | egrep '[0-9] (((((s|a)b)|c|e)cl)|(al)isp|(cl)asp)'

ifeq ($(HAS_SAFETY),yes)
test-safe:
	LISP=$(LISP) \
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
	$(SBCL) \
		 --eval "(load \"$(QUICKLISP_HOME)/setup.lisp\")" \
		 --eval "(ql:quickload :coalton/doc :silent t)" \
		 --eval "(coalton/doc:write-stdlib-documentation-to-file \"docs/reference.md\")"

.PHONY: web-docs
web-docs:
	$(SBCL) \
		 --eval "(load \"$(QUICKLISP_HOME)/setup.lisp\")" \
		 --eval "(ql:quickload :coalton/doc :silent t)" \
		 --eval "(coalton/doc:write-stdlib-documentation-to-file \"../coalton-website/content/reference.md\" :backend :hugo :revision \"main\")"


.PHONY: bench
bench:
	COALTON_ENV=release $(SBCL) \
		 --eval "(load \"$(QUICKLISP_HOME)/setup.lisp\")" \
		 --eval "(ql:quickload :coalton/benchmarks :silent t)" \
		 --eval "(sb-ext::without-gcing (coalton-benchmarks:run-benchmarks))"

.PHONY: parser-coverage
parser-coverage:
	mkdir -p coverage-report && test -d coverage-report
	$(SBCL) \
		--eval "(load \"$(QUICKLISP_HOME)/setup.lisp\")" \
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
	( echo '(load "'$(QUICKLISP_HOME)/setup.lisp'")' ; \
	  echo '(push (truename "'$(COALTON_HOME)'") asdf:*central-registry*)' ; \
	  echo '(push (truename "'$(EXTRA_LOCAL_PROJECTS)'") ql:*local-project-directories*)' ; \
	  echo '(ql-dist:clean (ql-dist:dist "quicklisp"))' ; \
	  echo '(format t "cleanql-'$(LISP)' asdf:*central-registry*: ~A~%" asdf:*central-registry*)' \
	) > $(TEMP)/cleanql-$(LISP).lisp

clean-quicklisp: $(TEMP)/cleanql-$(LISP).lisp
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

