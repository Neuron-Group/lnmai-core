#!/usr/bin/env python3
import argparse
import http.server
import json
import subprocess
import sys
import threading
from pathlib import Path


class LeanParserProcess:
    def __init__(self, repo_root: Path):
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

    def request(self, payload: dict) -> dict:
        line = json.dumps(payload, separators=(",", ":"))
        with self.lock:
            assert self.proc.stdin is not None
            assert self.proc.stdout is not None
            self.proc.stdin.write(line + "\n")
            self.proc.stdin.flush()
            response = self.proc.stdout.readline()
            if response == "":
                raise RuntimeError("simai-parser-cli exited unexpectedly")
            return json.loads(response)

    def close(self) -> None:
        if self.proc.poll() is None:
            self.proc.terminate()
            try:
                self.proc.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.proc.kill()


class ParserWebHandler(http.server.SimpleHTTPRequestHandler):
    parser_process: LeanParserProcess
    static_root: Path

    def __init__(self, *args, **kwargs):
        super().__init__(*args, directory=str(self.static_root), **kwargs)

    def do_POST(self):
        if self.path != "/api/parse":
            self.send_error(404, "Not Found")
            return

        try:
            content_length = int(self.headers.get("Content-Length", "0"))
            raw = self.rfile.read(content_length)
            payload = json.loads(raw.decode("utf-8"))
            response = self.parser_process.request(payload)
            body = json.dumps(response, indent=2).encode("utf-8")
            self.send_response(200)
            self.send_header("Content-Type", "application/json; charset=utf-8")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        except Exception as exc:
            body = json.dumps({
                "ok": False,
                "error": {
                    "code": "web_bridge_error",
                    "message": str(exc),
                },
            }, indent=2).encode("utf-8")
            self.send_response(500)
            self.send_header("Content-Type", "application/json; charset=utf-8")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)


def main() -> int:
    argp = argparse.ArgumentParser()
    argp.add_argument("port", nargs="?", type=int, default=8080)
    args = argp.parse_args()

    repo_root = Path(__file__).resolve().parent.parent
    static_root = repo_root / "tools" / "parser_web"
    parser = LeanParserProcess(repo_root)

    handler = ParserWebHandler
    handler.parser_process = parser
    handler.static_root = static_root

    server = http.server.ThreadingHTTPServer(("127.0.0.1", args.port), handler)
    print(f"parser web UI listening on http://127.0.0.1:{args.port}", file=sys.stderr)
    try:
        server.serve_forever()
    finally:
        parser.close()
        server.server_close()
    return 0


if __name__ == "__main__":
  raise SystemExit(main())
