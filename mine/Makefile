SBCL_BIN ?= sbcl
SBCL = $(SBCL_BIN) --noinform --no-userinit --no-sysinit --non-interactive
QUICKLISP_HOME ?= $(subst \,/,$(HOME))/quicklisp
QUICKLISP_SETUP = $(QUICKLISP_HOME)/setup.lisp
REPO_ROOT = $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/..)
LOCAL_SOURCE_REGISTRY = $(REPO_ROOT)//

ifeq ($(OS),Windows_NT)
    PLATFORM := windows
    EXE_SUFFIX := .exe
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Darwin)
        PLATFORM := macos
    else
        PLATFORM := linux
    endif
    EXE_SUFFIX :=
endif

MINE_BIN := mine$(EXE_SUFFIX)

.PHONY: mine mine-app mine-app-nobundle mine-app-fonts mine-app-deps clean

# On Linux we ship a separate SBCL runtime + saved core (mine.core)
# instead of an :executable t standalone image.  This avoids the
# linuxdeploy patchelf step corrupting the appended core, and removes
# the need for a custom SBCL with the embedded-core branch.
ifeq ($(PLATFORM),linux)
MINE_BUILD_ENV = MINE_SAVE_CORE=1
endif

mine:
	env $(MINE_BUILD_ENV) CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
		$(SBCL) \
		--load $(QUICKLISP_SETUP) \
		--eval '(pushnew :coalton-portable-bigfloat *features*)' \
		--load coalton-config.lisp \
		--eval '(asdf:load-system "mine" :verbose t)' \
		--eval '(mine/app/executable:build)'

TAURI_TARGET_TRIPLE := $(shell rustc -vV | grep host | cut -d' ' -f2)
IOSEVKA_VERSION := 34.3.0
IOSEVKA_URL := https://github.com/be5invis/Iosevka/releases/download/v$(IOSEVKA_VERSION)/PkgWebFont-IosevkaFixedSlab-$(IOSEVKA_VERSION).zip

ifeq ($(PLATFORM),windows)
mine-app: mine mine-app-fonts mine-app-deps
	mkdir -p mine-app/src-tauri/binaries
	cp $(MINE_BIN) mine-app/src-tauri/binaries/mine-core-$(TAURI_TARGET_TRIPLE)$(EXE_SUFFIX)
	cd mine-app && npx tauri build --bundles nsis
else ifeq ($(PLATFORM),macos)
mine-app: mine mine-app-fonts mine-app-deps
	mkdir -p mine-app/src-tauri/binaries
	cp $(MINE_BIN) mine-app/src-tauri/binaries/mine-core-$(TAURI_TARGET_TRIPLE)
	cd mine-app && npx tauri build
	mkdir -p /tmp/mine-dmg
	cp -R mine-app/src-tauri/target/release/bundle/macos/mine.app /tmp/mine-dmg/
	cp README.md /tmp/mine-dmg/
	hdiutil create -volname "mine" \
		-srcfolder /tmp/mine-dmg \
		-ov -format UDZO mine.dmg
	rm -rf /tmp/mine-dmg
else ifeq ($(PLATFORM),linux)
mine-app: mine
	MINE_CORE_SOURCE=mine.core ./ci/build-mine-app-linux.sh
else
mine-app:
	@echo "Unsupported platform: $(PLATFORM) -- mine-app only supports macOS, Linux, and Windows"; exit 1
endif

mine-app-nobundle: mine mine-app-fonts mine-app-deps
	mkdir -p mine-app/src-tauri/binaries
	cp $(MINE_BIN) mine-app/src-tauri/binaries/mine-core-$(TAURI_TARGET_TRIPLE)$(EXE_SUFFIX)
	cd mine-app && npx tauri build --no-bundle

mine-app-deps: mine-app/node_modules

mine-app/node_modules: mine-app/package.json mine-app/package-lock.json
	cd mine-app && npm install
	touch mine-app/node_modules

mine-app-fonts: mine-app/dist/fonts/IosevkaFixedSlab-Extended.woff2

mine-app/dist/fonts/IosevkaFixedSlab-Extended.woff2:
	mkdir -p mine-app/dist/fonts
	curl -L -o /tmp/iosevka-fixed.zip "$(IOSEVKA_URL)"
	unzip -o -j /tmp/iosevka-fixed.zip \
		"WOFF2/IosevkaFixedSlab-Extended.woff2" \
		"WOFF2/IosevkaFixedSlab-ExtendedBold.woff2" \
		"WOFF2/IosevkaFixedSlab-ExtendedItalic.woff2" \
		"WOFF2/IosevkaFixedSlab-ExtendedBoldItalic.woff2" \
		-d mine-app/dist/fonts
	rm -f /tmp/iosevka-fixed.zip

clean:
	find . -type f -name '*.fasl' -print -delete
	rm -f mine mine.exe mine.core mine.dmg mine.AppImage
	cd mine-app/src-tauri && cargo clean
