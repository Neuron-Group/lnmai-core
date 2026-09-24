#!/usr/bin/env bash
# Point the verification package at a local Aeneas Lean backend.
#
#   AENEAS_HOME=/path/to/aeneas verification/tools/wire_aeneas.sh
#
# Rewrites the `aeneas` [[require]] path in verification/lakefile.toml.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
lakefile="$here/lakefile.toml"

: "${AENEAS_HOME:?set AENEAS_HOME to your aeneas checkout}"
backend="$AENEAS_HOME/backends/lean"
if [ ! -f "$backend/lakefile.lean" ]; then
  echo "no Aeneas Lean backend at $backend" >&2
  exit 1
fi

python3 - "$lakefile" "$backend" <<'PY'
import re, sys
path, backend = sys.argv[1], sys.argv[2]
s = open(path).read()
# Replace the `path = "..."` line that follows `name = "aeneas"`.
s = re.sub(
    r'(name = "aeneas"\npath = ")[^"]*(")',
    lambda m: m.group(1) + backend + m.group(2),
    s,
)
open(path, "w").write(s)
print(f"wired aeneas -> {backend}")
PY
