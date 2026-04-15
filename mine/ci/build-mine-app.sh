#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
PROJECT_ROOT=$(cd -- "$SCRIPT_DIR/.." && pwd)
APP_ROOT="$PROJECT_ROOT/mine-app"
README_SOURCE=${MINE_README_SOURCE:-"$PROJECT_ROOT/README.md"}
IOSEVKA_VERSION=34.3.0

: "${MINE_SIDECAR_SOURCE:?MINE_SIDECAR_SOURCE must be set}"
: "${MINE_SIDECAR_NAME:?MINE_SIDECAR_NAME must be set}"
: "${MINE_APP_BUNDLES:?MINE_APP_BUNDLES must be set}"

resolve_path() {
  case "$1" in
    /*) printf '%s\n' "$1" ;;
    *) printf '%s/%s\n' "$PWD" "$1" ;;
  esac
}

README_SOURCE=$(resolve_path "$README_SOURCE")
MINE_SIDECAR_SOURCE=$(resolve_path "$MINE_SIDECAR_SOURCE")

TMPDIR_ROOT=$(mktemp -d)
trap 'rm -rf "$TMPDIR_ROOT"' EXIT

mkdir -p "$APP_ROOT/dist/fonts"
curl -L -o "$TMPDIR_ROOT/iosevka.zip" \
  "https://github.com/be5invis/Iosevka/releases/download/v${IOSEVKA_VERSION}/PkgWebFont-IosevkaFixedSlab-${IOSEVKA_VERSION}.zip"
unzip -o -j "$TMPDIR_ROOT/iosevka.zip" \
  "WOFF2/IosevkaFixedSlab-Extended.woff2" \
  "WOFF2/IosevkaFixedSlab-ExtendedBold.woff2" \
  "WOFF2/IosevkaFixedSlab-ExtendedItalic.woff2" \
  "WOFF2/IosevkaFixedSlab-ExtendedBoldItalic.woff2" \
  -d "$APP_ROOT/dist/fonts"

mkdir -p "$APP_ROOT/src-tauri/binaries"
cp "$MINE_SIDECAR_SOURCE" "$APP_ROOT/src-tauri/binaries/$MINE_SIDECAR_NAME"
chmod +x "$APP_ROOT/src-tauri/binaries/$MINE_SIDECAR_NAME" || true

cd "$APP_ROOT"
npm install

# Build TAURI_CONFIG JSON from optional env vars:
#   MINE_VERSION      → sets top-level "version"
#   MINE_SIGN_COMMAND → sets bundle.windows.signCommand (Tauri calls
#                       this for every signable binary + the installer)
CONFIG_JSON="{}"
if [ -n "${MINE_VERSION:-}" ]; then
  CONFIG_JSON=$(echo "$CONFIG_JSON" | jq --arg v "$MINE_VERSION" '.version = $v')
fi
if [ -n "${MINE_SIGN_COMMAND:-}" ]; then
  CONFIG_JSON=$(echo "$CONFIG_JSON" | jq --arg s "$MINE_SIGN_COMMAND" '.bundle.windows.signCommand = $s')
fi

if [ "$CONFIG_JSON" != "{}" ]; then
  CONFIG_FILE="$TMPDIR_ROOT/tauri-config-override.json"
  echo "$CONFIG_JSON" > "$CONFIG_FILE"
  echo "Tauri config override: $(cat "$CONFIG_FILE")"
  npx tauri build --bundles "$MINE_APP_BUNDLES" --config "$CONFIG_FILE"
else
  npx tauri build --bundles "$MINE_APP_BUNDLES"
fi

if [[ "${MINE_CREATE_DMG:-0}" == "1" ]]; then
  mkdir -p "$TMPDIR_ROOT/mine-dmg"
  cp -R "$APP_ROOT/src-tauri/target/release/bundle/macos/mine.app" "$TMPDIR_ROOT/mine-dmg/"
  cp "$README_SOURCE" "$TMPDIR_ROOT/mine-dmg/"
  hdiutil create -volname "mine" \
    -srcfolder "$TMPDIR_ROOT/mine-dmg" \
    -ov -format UDZO \
    "$APP_ROOT/src-tauri/target/release/bundle/macos/mine.dmg"
fi
