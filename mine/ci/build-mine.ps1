$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ProjectRoot = (Resolve-Path (Join-Path $ScriptDir "..")).Path
$WorkspaceRoot = (Resolve-Path (Join-Path $ProjectRoot "..")).Path

$sbclBin = if ($env:SBCL_BIN) { $env:SBCL_BIN } else { "sbcl" }

$env:COALTON_PORTABLE_BIGFLOAT = "1"
if (-not $env:CL_SOURCE_REGISTRY) {
  $env:CL_SOURCE_REGISTRY = "$WorkspaceRoot//"
}

# Signal the Lisp build to save a bare .core instead of a standalone
# executable when the embedding infrastructure is available.
if ($env:SBCL_SRC_DIR) {
  $embedCheck = Join-Path $env:SBCL_SRC_DIR "tools-for-build\make-embedded-core-executable.lisp"
  $libCheck = Join-Path $env:SBCL_SRC_DIR "src\runtime\libsbcl.a"
  if ((Test-Path $embedCheck) -and (Test-Path $libCheck)) {
    $env:MINE_SAVE_CORE = "1"
  }
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

  # When SBCL was built with --with-sb-linkable-runtime, the Lisp build
  # saves mine.core instead of a standalone executable.  Embed the core
  # into a proper PE binary so it can be code-signed.
  $corePath = Join-Path $ProjectRoot "mine.core"
  if (Test-Path $corePath) {
    $embedScript = Join-Path $env:SBCL_SRC_DIR "tools-for-build\make-embedded-core-executable.lisp"
    if (-not (Test-Path $embedScript)) {
      Write-Error "make-embedded-core-executable.lisp not found at $embedScript`nSet SBCL_SRC_DIR to the SBCL source directory"
      exit 1
    }
    Write-Host "==> Embedding mine.core into signable executable"
    $outputExePath = Join-Path $ProjectRoot "mine.exe"
    & $sbclBin --noinform `
      --script $embedScript -- `
      --core $corePath `
      --output $outputExePath `
      --sbcl-home (Join-Path $env:SBCL_SRC_DIR "src\runtime") `
      --application-type console

    if ($LASTEXITCODE -ne 0) {
      exit $LASTEXITCODE
    }
    if (-not (Test-Path $outputExePath)) {
      Write-Error "Embedding failed - mine.exe was not produced"
      exit 1
    }
    Remove-Item $corePath
  }
} finally {
  Pop-Location
}
