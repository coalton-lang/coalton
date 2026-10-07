#!/bin/sh
# Sidecar shim shipped as `mine-core` in the Linux AppImage.  mine-app
# spawns this script via portable-pty as if it were a single-file
# executable.  We exec the bundled SBCL runtime and load the saved
# Lisp image (mine.core, renamed to `mine-core-data` so Tauri's
# externalBin, which forbids "." in names, accepts it).
DIR=$(dirname "$(readlink -f "$0")")
# The AppImage's AppRun prepends its own directories to LD_LIBRARY_PATH,
# PATH, and other search paths, and its GTK hook sets GTK and GIO
# variables, all for mine-app's webview.  Every program mine starts would
# inherit them: the setup wizard's curl, the REPL runtime, and programs run
# from the REPL then load the AppImage's older libraries instead of the
# system's and can fail, as curl does with "symbol lookup error".  mine-core
# needs none of them; restore the user's environment before starting it.
if [ -n "${APPDIR:-}" ]; then
  # Print the colon-separated list $1 without entries inside $APPDIR.
  without_appdir() {
    kept=
    saved_ifs=$IFS
    IFS=:
    set -f
    for entry in $1; do
      case "$entry" in
        "$APPDIR"|"$APPDIR"/*) ;;
        *) kept="${kept:+$kept:}$entry" ;;
      esac
    done
    set +f
    IFS=$saved_ifs
    printf '%s' "$kept"
  }
  LD_LIBRARY_PATH=$(without_appdir "${LD_LIBRARY_PATH:-}")
  PATH=$(without_appdir "${PATH:-}")
  XDG_DATA_DIRS=$(without_appdir "${XDG_DATA_DIRS:-}")
  PYTHONPATH=$(without_appdir "${PYTHONPATH:-}")
  PERLLIB=$(without_appdir "${PERLLIB:-}")
  QT_PLUGIN_PATH=$(without_appdir "${QT_PLUGIN_PATH:-}")
  GST_PLUGIN_SYSTEM_PATH=$(without_appdir "${GST_PLUGIN_SYSTEM_PATH:-}")
  GST_PLUGIN_SYSTEM_PATH_1_0=$(without_appdir "${GST_PLUGIN_SYSTEM_PATH_1_0:-}")
  export LD_LIBRARY_PATH PATH XDG_DATA_DIRS PYTHONPATH PERLLIB QT_PLUGIN_PATH \
         GST_PLUGIN_SYSTEM_PATH GST_PLUGIN_SYSTEM_PATH_1_0
  [ -n "$LD_LIBRARY_PATH" ] || unset LD_LIBRARY_PATH
  [ -n "$XDG_DATA_DIRS" ] || unset XDG_DATA_DIRS
  [ -n "$PYTHONPATH" ] || unset PYTHONPATH
  [ -n "$PERLLIB" ] || unset PERLLIB
  [ -n "$QT_PLUGIN_PATH" ] || unset QT_PLUGIN_PATH
  [ -n "$GST_PLUGIN_SYSTEM_PATH" ] || unset GST_PLUGIN_SYSTEM_PATH
  [ -n "$GST_PLUGIN_SYSTEM_PATH_1_0" ] || unset GST_PLUGIN_SYSTEM_PATH_1_0
  # AppRun sets these two outright, replacing any value of the user's.
  case "${PYTHONHOME:-}" in
    "$APPDIR"|"$APPDIR"/*) unset PYTHONHOME ;;
  esac
  unset PYTHONDONTWRITEBYTECODE
  unset GTK_DATA_PREFIX GTK_THEME GDK_BACKEND GSETTINGS_SCHEMA_DIR GTK_EXE_PREFIX \
        GTK_PATH GTK_IM_MODULE_FILE GDK_PIXBUF_MODULE_FILE GIO_EXTRA_MODULES
fi
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
