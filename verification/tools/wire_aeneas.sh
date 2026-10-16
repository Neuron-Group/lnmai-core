#!/usr/bin/env bash
# Point the verification package at a local Aeneas Lean backend.
#
#   AENEAS_HOME=/path/to/aeneas verification/tools/wire_aeneas.sh
#
# Creates an ignored local symlink; committed configuration stays portable.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

: "${AENEAS_HOME:?set AENEAS_HOME to your aeneas checkout}"
backend="$AENEAS_HOME/backends/lean"
if [ ! -f "$backend/lakefile.lean" ]; then
  echo "no Aeneas Lean backend at $backend" >&2
  exit 1
fi

mkdir -p "$here/.lake/local"
ln -sfn "$backend" "$here/.lake/local/aeneas"
echo "wired aeneas -> $backend"
