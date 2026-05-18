#!/usr/bin/env python3
import argparse
import json
import os
import socketserver
import subprocess
import sys
import threading


class LeanParserProcess:
    def __init__(self, repo_root: str):
        self.repo_root = repo_root
        self.proc = subprocess.Popen(
            ["lake", "exe", "simai-parser-cli"],
            cwd=repo_root,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=sys.stderr,
            text=True,
            bufsize=1,
        )
        self.lock = threading.Lock()

    def request(self, line: str) -> str:
        with self.lock:
            assert self.proc.stdin is not None
            assert self.proc.stdout is not None
            self.proc.stdin.write(line + "\n")
            self.proc.stdin.flush()
            response = self.proc.stdout.readline()
            if response == "":
                raise RuntimeError("simai-parser-cli exited unexpectedly")
            return response.rstrip("\n")

    def close(self) -> None:
        if self.proc.poll() is None:
            self.proc.terminate()
            try:
                self.proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.proc.kill()


class ThreadedTCPServer(socketserver.ThreadingMixIn, socketserver.TCPServer):
    allow_reuse_address = True
    daemon_threads = True


class ParserRequestHandler(socketserver.StreamRequestHandler):
    def handle(self) -> None:
        parser: LeanParserProcess = self.server.parser  # type: ignore[attr-defined]
        while True:
            raw = self.rfile.readline()
            if not raw:
                return
            line = raw.decode("utf-8").strip()
            if not line:
                continue
            try:
                response = parser.request(line)
            except Exception as exc:
                response = json.dumps({
                    "ok": False,
                    "error": {
                        "code": "server_error",
                        "message": str(exc),
                    },
                }, separators=(",", ":"))
            self.wfile.write(response.encode("utf-8") + b"\n")
            self.wfile.flush()


def main() -> int:
    argp = argparse.ArgumentParser()
    argp.add_argument("port", nargs="?", type=int, default=8765)
    args = argp.parse_args()

    repo_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    parser = LeanParserProcess(repo_root)
    try:
        with ThreadedTCPServer(("127.0.0.1", args.port), ParserRequestHandler) as server:
            server.parser = parser  # type: ignore[attr-defined]
            print(f"simai parser server listening on 127.0.0.1:{args.port}", file=sys.stderr)
            server.serve_forever()
    finally:
        parser.close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
