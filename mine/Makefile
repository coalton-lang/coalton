SBCL_BIN ?= sbcl
SBCL = $(SBCL_BIN) --noinform --no-userinit --no-sysinit --non-interactive
QUICKLISP_HOME ?= $(HOME)/quicklisp
QUICKLISP_SETUP = $(QUICKLISP_HOME)/setup.lisp
REPO_ROOT = $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/..)
LOCAL_SOURCE_REGISTRY = $(REPO_ROOT)//

.PHONY: mine mine-app clean

mine:
	env CL_SOURCE_REGISTRY="$(LOCAL_SOURCE_REGISTRY)" \
		$(SBCL) \
		--load $(QUICKLISP_SETUP) \
		--eval '(pushnew :coalton-portable-bigfloat *features*)' \
		--load coalton-config.lisp \
		--eval '(asdf:load-system "mine" :verbose t)' \
		--eval '(mine/app/executable:build)'

TAURI_TARGET_TRIPLE := $(shell rustc -vV | grep host | cut -d' ' -f2)
IOSEVKA_VERSION := 34.3.0
IOSEVKA_URL := https://github.com/be5invis/Iosevka/releases/download/v$(IOSEVKA_VERSION)/PkgWebFont-IosevkaFixedSlab-$(IOSEVKA_VERSION).zip

mine-app: mine mine-app-fonts
	mkdir -p mine-app/src-tauri/binaries
	cp mine mine-app/src-tauri/binaries/mine-$(TAURI_TARGET_TRIPLE)
	cd mine-app && npx tauri build
	mkdir -p /tmp/mine-dmg
	cp -R mine-app/src-tauri/target/release/bundle/macos/mine.app /tmp/mine-dmg/
	cp README.md /tmp/mine-dmg/
	hdiutil create -volname "mine" \
		-srcfolder /tmp/mine-dmg \
		-ov -format UDZO mine.dmg
	rm -rf /tmp/mine-dmg

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
	rm -f mine mine.dmg
	cd mine-app/src-tauri && cargo clean
