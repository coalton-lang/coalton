$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Resolve-Path (Join-Path $ScriptDir "..")).Path
$WorkspaceRoot = (Resolve-Path (Join-Path $ProjectRoot "..")).Path

$sbclBin = if ($env:SBCL_BIN) { $env:SBCL_BIN } else { "sbcl" }

$env:COALTON_PORTABLE_BIGFLOAT = "1"
if (-not $env:CL_SOURCE_REGISTRY) {
  $env:CL_SOURCE_REGISTRY = "$WorkspaceRoot//"
}

Push-Location $ProjectRoot
try {
  & $sbclBin `
    --dynamic-space-size 4096 `
    --noinform --non-interactive `
    --load coalton-config.lisp `
    --eval '(ql:quickload "mine" :verbose t)' `
    --eval '(mine/app/executable:build)'

  if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
  }
} finally {
  Pop-Location
}
