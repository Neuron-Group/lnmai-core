#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
TREE_SITTER_DIR="$ROOT_DIR/tools/tree-sitter-lean"
CODEGRAPH_DIR="$ROOT_DIR/tools/codegraph"
LEAN_WASM_SRC="$TREE_SITTER_DIR/tree-sitter-lean.wasm"
LEAN_WASM_DST="$CODEGRAPH_DIR/src/extraction/wasm/tree-sitter-lean.wasm"

if ! command -v nix-shell >/dev/null 2>&1; then
  echo "nix-shell is required to bootstrap the local CodeGraph toolchain." >&2
  exit 1
fi

nix-shell "$ROOT_DIR/shell.nix" --run "
  set -euo pipefail
  cd \"$TREE_SITTER_DIR\"
  tree-sitter build --wasm
  cp \"$LEAN_WASM_SRC\" \"$LEAN_WASM_DST\"
  cd \"$CODEGRAPH_DIR\"
  npm ci
  npm run build
"
