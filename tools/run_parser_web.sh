#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")/.." && pwd)"
PORT="${1:-8080}"

echo "[1/2] Building simai-parser-cli..."
cd "$ROOT_DIR"
lake build simai-parser-cli

echo "[2/2] Starting parser web UI on http://127.0.0.1:${PORT}"
exec python3 tools/parser_web_server.py "$PORT"
