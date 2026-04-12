#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
PROJECT_ROOT=$(cd -- "$SCRIPT_DIR/.." && pwd)
WORKSPACE_ROOT=$(cd -- "$PROJECT_ROOT/.." && pwd)

SBCL_BIN=${SBCL_BIN:-sbcl}
CL_SOURCE_REGISTRY_VALUE=${CL_SOURCE_REGISTRY:-${WORKSPACE_ROOT}//}

cd "$PROJECT_ROOT"

COALTON_PORTABLE_BIGFLOAT=1 \
CL_SOURCE_REGISTRY="$CL_SOURCE_REGISTRY_VALUE" \
"$SBCL_BIN" \
  --dynamic-space-size 4096 \
  --noinform --non-interactive \
  --load coalton-config.lisp \
  --eval '(ql:quickload "mine" :verbose t)' \
  --eval '(mine/app/executable:build)'
