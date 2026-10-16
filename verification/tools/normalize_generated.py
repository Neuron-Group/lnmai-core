#!/usr/bin/env python3
"""Remove Aeneas's trailing layout without changing Lean literal bytes.

Aeneas can emit actual CR/LF characters inside Char literals. Text-mode
universal-newline decoding and str.splitlines() both corrupt those values.
Read/write bytes, preserve all line terminators, and only trim horizontal
whitespace outside strings, character literals, and quoted identifiers.
"""

from pathlib import Path
import re
import sys


CHAR = re.compile(r"'(?:[^'\\]|\\(?:u\{[0-9a-fA-F]+\}|x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|[^\r\n]))'")


def literal_spans(source: str) -> list[tuple[int, int]]:
    """Locate literals in Aeneas output, skipping nested Lean comments."""
    spans = []
    index = 0
    while index < len(source):
        if source.startswith("--", index):
            end = source.find("\n", index + 2)
            index = len(source) if end < 0 else end + 1
        elif source.startswith("/-", index):
            depth = 1
            index += 2
            while depth and index < len(source):
                if source.startswith("/-", index):
                    depth += 1
                    index += 2
                elif source.startswith("-/", index):
                    depth -= 1
                    index += 2
                else:
                    index += 1
            if depth:
                raise ValueError("Unterminated generated Lean block comment")
        elif source[index] in ('"', '«'):
            start = index
            closer = '"' if source[index] == '"' else '»'
            index += 1
            while index < len(source):
                if closer == '"' and source[index] == '\\':
                    index += 2
                elif source[index] == closer:
                    index += 1
                    spans.append((start, index))
                    break
                else:
                    index += 1
            else:
                raise ValueError("Unterminated generated Lean literal")
        elif source[index] == "'":
            literal = CHAR.match(source, index)
            if literal:
                spans.append(literal.span())
                index = literal.end()
            else:
                # Apostrophes also occur in generated identifier names.
                index += 1
        else:
            index += 1
    return spans


def normalize(data: bytes) -> bytes:
    source = data.decode("utf-8")
    spans = literal_spans(source)
    protected = bytearray(len(source))
    for start, end in spans:
        protected[start:end] = b"\x01" * (end - start)
    pieces = []
    previous = 0
    for layout in re.finditer(r"[ \t]+(?=\n|$)", source):
        start, end = layout.span()
        if not any(protected[start:end]):
            pieces.append(source[previous:start])
            previous = end
    pieces.append(source[previous:])
    result = "".join(pieces)
    if not result.endswith("\n"):
        result += "\n"
    # Fail closed if lexing/normalization ever changes literal contents.
    before = [source[start:end] for start, end in spans]
    after = [result[start:end] for start, end in literal_spans(result)]
    if before != after:
        raise ValueError("Generated Lean literal changed during normalization")
    return result.encode("utf-8")


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("usage: normalize_generated.py GENERATED.lean")
    path = Path(sys.argv[1])
    path.write_bytes(normalize(path.read_bytes()))


if __name__ == "__main__":
    main()
