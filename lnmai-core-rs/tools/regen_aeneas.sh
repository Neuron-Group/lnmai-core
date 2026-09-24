#!/usr/bin/env bash
# Regenerate verification/Verification/Generated.lean from the Rust
# verification-facing crate.
#
# Pipeline: cargo -> charon (LLBC) -> aeneas (Lean model) -> lake package.
#
# Toolchain locations can be overridden via environment variables:
#   AENEAS_HOME  (default: /Users/pingfanh/bin/aeneas)
#   CHARON_HOME  (default: /Users/pingfanh/bin/charon)
#
# The generated namespace is the crate name (`lnmai_core_verify`).
set -euo pipefail

AENEAS_HOME="${AENEAS_HOME:-/Users/pingfanh/bin/aeneas}"
CHARON_HOME="${CHARON_HOME:-/Users/pingfanh/bin/charon}"

here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
repo="$(cd "$here/.." && pwd)"
crate_dir="$here/crates/lnmai-core-verify"
dest="$repo/verification/Verification/Generated.lean"

export PATH="$CHARON_HOME:$PATH"

echo "[1/3] charon: extracting LLBC from lnmai-core-verify"
(cd "$crate_dir" && charon cargo --preset=aeneas)

llbc="$here/lnmai_core_verify.llbc"
test -f "$llbc" || { echo "missing $llbc" >&2; exit 1; }

out="$(mktemp -d)"
trap 'rm -rf "$out"' EXIT

echo "[2/3] aeneas: generating Lean model"
"$AENEAS_HOME/bin/aeneas" -backend lean -dest "$out" "$llbc"

gen="$out/LnmaiCoreVerify.lean"
test -f "$gen" || { echo "missing $gen" >&2; exit 1; }

echo "[3/3] installing $dest"
cp "$gen" "$dest"
echo "done: $(wc -l < "$dest") lines"
