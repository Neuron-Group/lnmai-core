#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
CODEGRAPH_DIR="$ROOT_DIR/tools/codegraph"
CODEGRAPH_BIN="$CODEGRAPH_DIR/dist/bin/codegraph.js"

if [ ! -f "$CODEGRAPH_BIN" ]; then
  echo "CodeGraph is not built yet. Run: bash tools/bootstrap_codegraph.sh" >&2
  exit 1
fi

cd "$ROOT_DIR"
exec node "$CODEGRAPH_BIN" "$@"
