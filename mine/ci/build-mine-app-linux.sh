#!/usr/bin/env bash
#
# Linux AppImage bundler.  Counterpart to build-mine-app.sh, which
# handles the macOS .app and Windows .exe paths.  Linux ships an
# SBCL-runtime + saved-core sidecar trio rather than a single binary,
# which is invisible to Tauri's externalBin model -- so we stage the
# files under non-conflicting names and override externalBin via
# --config to pull all three into usr/bin/ of the AppImage.
#
# Inputs (env):
#   MINE_CORE_SOURCE  -- path to mine.core (required; produced by
#                        ci/build-mine.sh with MINE_AS_CORE=1)
#   MINE_VERSION      -- optional; injected as bundle.version override
#
# Output: $PROJECT_ROOT/mine.AppImage

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
PROJECT_ROOT=$(cd -- "$SCRIPT_DIR/.." && pwd)
APP_ROOT="$PROJECT_ROOT/mine-app"
IOSEVKA_VERSION=34.3.0

: "${MINE_CORE_SOURCE:?MINE_CORE_SOURCE must be set (path to mine.core)}"
MINE_CORE_SOURCE=$(cd "$(dirname "$MINE_CORE_SOURCE")" && pwd)/$(basename "$MINE_CORE_SOURCE")
[ -f "$MINE_CORE_SOURCE" ] || { echo "MINE_CORE_SOURCE not found: $MINE_CORE_SOURCE" >&2; exit 1; }

TARGET_TRIPLE=$(rustc -vV | awk '/^host:/ { print $2 }')

TMPDIR_ROOT=$(mktemp -d)
trap 'rm -rf "$TMPDIR_ROOT"' EXIT

# Iosevka fonts (cached by file -- skip if already present)
if [ ! -f "$APP_ROOT/dist/fonts/IosevkaFixedSlab-Extended.woff2" ]; then
  mkdir -p "$APP_ROOT/dist/fonts"
  curl -L -o "$TMPDIR_ROOT/iosevka.zip" \
    "https://github.com/be5invis/Iosevka/releases/download/v${IOSEVKA_VERSION}/PkgWebFont-IosevkaFixedSlab-${IOSEVKA_VERSION}.zip"
  unzip -o -j "$TMPDIR_ROOT/iosevka.zip" \
    "WOFF2/IosevkaFixedSlab-Extended.woff2" \
    "WOFF2/IosevkaFixedSlab-ExtendedBold.woff2" \
    "WOFF2/IosevkaFixedSlab-ExtendedItalic.woff2" \
    "WOFF2/IosevkaFixedSlab-ExtendedBoldItalic.woff2" \
    -d "$APP_ROOT/dist/fonts"
fi

# Stage sidecars.  All three land in usr/bin/ of the AppImage:
#   mine-core         -- shell-script launcher (Tauri spawns this)
#   sbcl-runtime      -- the SBCL runtime that loads the saved core
#   mine-core-data    -- mine.core renamed (Tauri's externalBin
#                        forbids '.' in sidecar names)
# patchelf is harmless on each: launcher is a script, runtime has no
# appended-trailer data, mine-core-data isn't ELF.
SBCL_BIN_PATH=$(command -v "${SBCL_BIN:-sbcl}")
[ -x "$SBCL_BIN_PATH" ] || { echo "sbcl not found on PATH" >&2; exit 1; }

mkdir -p "$APP_ROOT/src-tauri/binaries"
install -m 0755 "$PROJECT_ROOT/ci/linux-mine-core-launcher.sh" \
  "$APP_ROOT/src-tauri/binaries/mine-core-${TARGET_TRIPLE}"
install -m 0755 "$SBCL_BIN_PATH" \
  "$APP_ROOT/src-tauri/binaries/sbcl-runtime-${TARGET_TRIPLE}"
install -m 0644 "$MINE_CORE_SOURCE" \
  "$APP_ROOT/src-tauri/binaries/mine-core-data-${TARGET_TRIPLE}"

cd "$APP_ROOT"
npm install

CONFIG_JSON='{"bundle":{"externalBin":["binaries/mine-core","binaries/sbcl-runtime","binaries/mine-core-data"]}}'
if [ -n "${MINE_VERSION:-}" ]; then
  CONFIG_JSON=$(echo "$CONFIG_JSON" | jq --arg v "$MINE_VERSION" '.version = $v')
fi
CONFIG_FILE="$TMPDIR_ROOT/tauri-config.json"
echo "$CONFIG_JSON" > "$CONFIG_FILE"

npx tauri build --bundles appimage --config "$CONFIG_FILE"

cp "$APP_ROOT"/src-tauri/target/release/bundle/appimage/mine_*.AppImage \
   "$PROJECT_ROOT/mine.AppImage"
