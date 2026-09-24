#!/usr/bin/env bash
# Full Lean verification build.
#
# WARNING: every proof module imports `LnmaiCore.*`, which imports Mathlib.
# A cold check therefore loads the Mathlib + Aeneas + Generated oleans and can
# take several minutes on this machine. Run it in the background and inspect
# the log rather than blocking on it:
#
#   verification/tools/build.sh > /tmp/verify.log 2>&1 &
#   tail -f /tmp/verify.log
set -euo pipefail
cd "$(dirname "$0")/.."
exec "${LAKE:-$HOME/.elan/bin/lake}" build "$@"
