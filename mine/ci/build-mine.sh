#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
PROJECT_ROOT=$(cd -- "$SCRIPT_DIR/.." && pwd)
WORKSPACE_ROOT=$(cd -- "$PROJECT_ROOT/.." && pwd)

SBCL_BIN=${SBCL_BIN:-sbcl}
CL_SOURCE_REGISTRY_VALUE=${CL_SOURCE_REGISTRY:-${WORKSPACE_ROOT}//}

cd "$PROJECT_ROOT"

# Signal the Lisp build to save a bare .core instead of a standalone
# executable when the embedding infrastructure is available.
if [ -n "${SBCL_SRC_DIR:-}" ] && \
   [ -f "${SBCL_SRC_DIR}/tools-for-build/make-embedded-core-executable.lisp" ] && \
   [ -f "${SBCL_SRC_DIR}/src/runtime/libsbcl.a" ]; then
  export MINE_SAVE_CORE=1
fi

# MINE_AS_CORE=1 explicitly requests the saved-core path without the
# embedding step that follows.  Used by the Linux AppImage build, which
# ships the SBCL runtime + saved core as a sidecar trio rather than a
# standalone executable, and so doesn't need (or want) the core embedded.
if [ -n "${MINE_AS_CORE:-}" ]; then
  export MINE_SAVE_CORE=1
fi

COALTON_PORTABLE_BIGFLOAT=1 \
CL_SOURCE_REGISTRY="$CL_SOURCE_REGISTRY_VALUE" \
"$SBCL_BIN" \
  --dynamic-space-size 4096 \
  --noinform --non-interactive \
  --load coalton-config.lisp \
  --eval '(ql:quickload "mine" :verbose t)' \
  --eval '(mine/app/executable:build)'

# When SBCL was built with --with-sb-linkable-runtime, the Lisp build
# saves mine.core instead of a standalone executable.  Embed the core
# into a proper Mach-O binary so it can be code-signed.  Skip embedding
# when MINE_AS_CORE=1 is set: the caller wants the bare core file.
if [ -f "$PROJECT_ROOT/mine.core" ] && [ -z "${MINE_AS_CORE:-}" ]; then
  EMBED_SCRIPT="${SBCL_SRC_DIR}/tools-for-build/make-embedded-core-executable.lisp"
  if [ ! -f "$EMBED_SCRIPT" ]; then
    echo "ERROR: make-embedded-core-executable.lisp not found at $EMBED_SCRIPT"
    echo "       Set SBCL_SRC_DIR to the SBCL source directory"
    exit 1
  fi
  echo "==> Embedding mine.core into signable executable"
  "$SBCL_BIN" --noinform \
    --script "$EMBED_SCRIPT" -- \
    --core "$PROJECT_ROOT/mine.core" \
    --output "$PROJECT_ROOT/mine" \
    --sbcl-home "$SBCL_SRC_DIR/src/runtime"
  if [ ! -f "$PROJECT_ROOT/mine" ]; then
    echo "ERROR: Embedding failed — mine binary was not produced"
    exit 1
  fi
  rm "$PROJECT_ROOT/mine.core"
fi
