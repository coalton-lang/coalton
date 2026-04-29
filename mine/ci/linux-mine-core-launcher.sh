#!/bin/sh
# Sidecar shim shipped as `mine-core` in the Linux AppImage.  mine-app
# spawns this script via portable-pty as if it were a single-file
# executable.  We exec the bundled SBCL runtime and load the saved
# Lisp image (mine.core, renamed to `mine-core-data` so Tauri's
# externalBin, which forbids "." in names, accepts it).
DIR=$(dirname "$(readlink -f "$0")")
# Mine spawns its own REPL runtime by re-executing argv[0] with extra
# args.  In the AppImage, argv[0] of the running process is sbcl-runtime
# (the bare SBCL ELF, exec'd below) -- which has no notion of mine.core
# and would start a vanilla REPL.  Export this script's path so mine
# can re-exec the launcher (which then re-exec's sbcl-runtime with the
# right --core) instead.
export MINE_LAUNCHER="$(readlink -f "$0")"
# SBCL's argument parser is order-sensitive: runtime options (--core,
# --noinform, --disable-ldb, --dynamic-space-size, ...) must precede
# toplevel options (--no-userinit, --no-sysinit, ...).  Mine's REPL
# spawn passes `--disable-ldb --dynamic-space-size N --runtime-server`
# in "$@" -- all runtime/user options.  Keeping our prefix runtime-only
# (no --no-userinit/--no-sysinit) lets SBCL stay in runtime-options
# mode through the whole command line.  Init files are not loaded in
# the first place because mine.core was saved with :toplevel mine-main,
# which replaces SBCL's default toplevel.
exec "$DIR/sbcl-runtime" \
  --core "$DIR/mine-core-data" \
  --noinform \
  "$@"
