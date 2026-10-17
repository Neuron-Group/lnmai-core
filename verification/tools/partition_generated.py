#!/usr/bin/env python3
"""Partition actual Aeneas output without changing any declaration bytes.

Keep each existing module's order when its blocks are unchanged. Source-tagged
extensions import the core and are emitted only when present in this extraction,
so adding an extension does not invalidate every previously compiled proof.
The manifest reconstructs the complete normalized extraction byte for byte;
neither comments nor literal CR/LF characters are discarded for comparison.
"""

import argparse
from collections import Counter, defaultdict, deque
import hashlib
import json
from pathlib import Path
import re


CORE = "Generated.lean"
EXTRA = "GeneratedTouchHoldBodyAdapter.lean"
EXTRA_SOURCES = {
    EXTRA: b"    Source: 'crates/lnmai-core-verify/src/touch_hold_body_adapter.rs',",
    "GeneratedMaidataLineAdapter.lean":
        b"    Source: 'crates/lnmai-core-verify/src/maidata_line_adapter.rs',",
    "GeneratedQueueRemaining.lean":
        b"    Source: 'crates/lnmai-core-verify/src/queue_remaining.rs',",
}
MODULES = (CORE, *EXTRA_SOURCES)
FOOTER = b"end lnmai_core_verify\n"


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def split(source: bytes) -> tuple[bytes, list[bytes]]:
    if not source.endswith(FOOTER):
        raise ValueError("Unexpected generated namespace footer")
    prefix, *blocks = re.split(rb"(?m)(?=^/--)", source[:-len(FOOTER)])
    if not blocks or b"namespace lnmai_core_verify\n" not in prefix:
        raise ValueError("Unexpected generated module layout")
    for block in blocks:
        if b"-/" not in block:
            raise ValueError("Unterminated generated declaration comment")
    return prefix, blocks


def module_for(block: bytes) -> str:
    comment = block[:block.index(b"-/")]
    matches = [name for name, source in EXTRA_SOURCES.items() if source in comment]
    if len(matches) > 1:
        raise ValueError("Ambiguous generated Source routing")
    return matches[0] if matches else CORE


def module_header(prefix: bytes, name: str) -> bytes:
    if name == CORE:
        return prefix
    return prefix.replace(b"import Aeneas\n", b"import Verification.Generated\n")


def preserve_order(header: bytes, blocks: list[bytes], previous: bytes | None) -> list[bytes]:
    if previous is not None:
        old_header, old_blocks = split(previous)
        if old_header == header and Counter(old_blocks) == Counter(blocks):
            return old_blocks
    return blocks


def reject_unlisted(directory: Path, names: set[str]) -> None:
    stale = [str(directory / name) for name in EXTRA_SOURCES
             if name not in names and (directory / name).exists()]
    if stale:
        raise ValueError("Generated modules absent from this extraction; explicitly move "
                         "or remove these obsolete artifacts before retrying: " + ", ".join(stale))


def partition(full: bytes, previous_core: bytes | None,
              previous_extras: dict[str, bytes] | None = None) -> dict[str, bytes]:
    prefix, blocks = split(full)
    if prefix.count(b"import Aeneas\n") != 1:
        raise ValueError("Unexpected generated imports")
    routed = {name: [] for name in MODULES}
    for block in blocks:
        routed[module_for(block)].append(block)
    if not routed[EXTRA]:
        raise ValueError("No extracted TouchHold adapter declarations found")
    pieces = {}
    outputs = {}
    for name in MODULES:
        if name != CORE and not routed[name]:
            continue
        header = module_header(prefix, name)
        previous = previous_core if name == CORE else (previous_extras or {}).get(name)
        pieces[name] = preserve_order(header, routed[name], previous)
        outputs[name] = header + b"".join(pieces[name]) + FOOTER

    # Retain the full extraction order, including every metadata-only block.
    # Indices identify exact bytes in every generated module present.
    locations = defaultdict(deque)
    for module, parts in pieces.items():
        for index, block in enumerate(parts):
            locations[block].append([module, index])
    order = [locations[block].popleft() for block in blocks]
    if any(locations.values()):
        raise ValueError("Unconsumed generated declaration blocks")
    reconstructed = prefix + b"".join(pieces[m][i] for m, i in order) + FOOTER
    if reconstructed != full:
        raise ValueError("Partition does not reproduce the full extraction")
    manifest = {
        "format": 1,
        "kind": "byte-exact partition of normalized Aeneas output; not proof acceptance",
        "full_sha256": sha256(full),
        "modules": {name: sha256(data) for name, data in outputs.items()},
        "blocks": {name: len(parts) for name, parts in pieces.items()},
        "full_order": order,
    }
    outputs["extraction-manifest.json"] = (json.dumps(manifest, indent=2) + "\n").encode()
    return outputs


def verify(full: bytes, directory: Path, manifest_path: Path) -> dict:
    """Check installed modules and reconstruct the actual extraction bytes."""
    manifest = json.loads(manifest_path.read_bytes())
    if manifest["format"] != 1:
        raise ValueError("Unknown extraction partition manifest format")
    names = set(manifest["modules"])
    if not {CORE, EXTRA} <= names <= set(MODULES):
        raise ValueError("Unexpected generated module set")
    if set(manifest["blocks"]) != names:
        raise ValueError("Generated block counts do not match the module set")
    reject_unlisted(directory, names)
    pieces = {}
    prefix = b""
    for name in MODULES:
        if name not in names:
            continue
        data = (directory / name).read_bytes()
        if sha256(data) != manifest["modules"][name]:
            raise ValueError("Generated module hash mismatch: " + name)
        header, pieces[name] = split(data)
        if name == CORE:
            prefix = header
        elif header != module_header(prefix, name):
            raise ValueError("Generated extension header mismatch: " + name)
        if any(module_for(block) != name for block in pieces[name]):
            raise ValueError("Generated Source routing mismatch: " + name)
        if name != CORE and not pieces[name]:
            raise ValueError("Empty generated extension: " + name)
        if len(pieces[name]) != manifest["blocks"][name]:
            raise ValueError("Generated block count mismatch")
    expected = Counter((name, i) for name, parts in pieces.items() for i in range(len(parts)))
    order = manifest["full_order"]
    if Counter(tuple(item) for item in order) != expected:
        raise ValueError("Every generated block must occur exactly once")
    reconstructed = prefix + b"".join(pieces[m][i] for m, i in order) + FOOTER
    if reconstructed != full or sha256(full) != manifest["full_sha256"]:
        raise ValueError("Installed modules do not match the full actual extraction")
    return manifest


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("full", type=Path)
    parser.add_argument("directory", type=Path)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    full = args.full.read_bytes()
    core_path = args.directory / CORE
    previous = core_path.read_bytes() if core_path.is_file() else None
    previous_extras = {name: (args.directory / name).read_bytes()
                       for name in EXTRA_SOURCES if (args.directory / name).is_file()}
    outputs = partition(full, previous, previous_extras)
    # Reject leftovers before writing anything, including on a stage rollback.
    reject_unlisted(args.directory, set(outputs))
    manifest_path = args.directory.parent / "extraction-manifest.json"
    for name, data in outputs.items():
        path = manifest_path if name == manifest_path.name else args.directory / name
        if path.is_file() and path.read_bytes() == data:
            continue
        if args.check:
            raise SystemExit(str(path) + " is stale; regenerate and recheck proofs")
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(data)
    manifest = verify(full, args.directory, manifest_path)
    print(json.dumps({"full_sha256": manifest["full_sha256"],
                      "blocks": manifest["blocks"], "byte_exact": True}))


if __name__ == "__main__":
    main()
