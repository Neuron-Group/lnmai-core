#!/usr/bin/env python3
"""Partition actual Aeneas output without changing any declaration bytes.

Keep the existing core order when its blocks are unchanged. New concrete
TouchHold adapter declarations import that core in a separate generated module,
so adding the adapter does not invalidate every previously compiled proof.
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
ADAPTER_SOURCE = (
    b"    Source: 'crates/lnmai-core-verify/src/touch_hold_body_adapter.rs',"
)
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


def is_adapter(block: bytes) -> bool:
    comment = block[:block.index(b"-/")]
    return ADAPTER_SOURCE in comment


def partition(full: bytes, previous_core: bytes | None) -> dict[str, bytes]:
    prefix, blocks = split(full)
    if prefix.count(b"import Aeneas\n") != 1:
        raise ValueError("Unexpected generated imports")
    core_blocks = [block for block in blocks if not is_adapter(block)]
    extra_blocks = [block for block in blocks if is_adapter(block)]
    if not extra_blocks:
        raise ValueError("No extracted TouchHold adapter declarations found")
    if previous_core is not None:
        old_prefix, old_blocks = split(previous_core)
        if old_prefix == prefix and Counter(old_blocks) == Counter(core_blocks):
            core_blocks = old_blocks
    core = prefix + b"".join(core_blocks) + FOOTER
    extra_prefix = prefix.replace(
        b"import Aeneas\n", b"import Verification.Generated\n"
    )
    extra = extra_prefix + b"".join(extra_blocks) + FOOTER

    # Retain the full extraction order, including every metadata-only block.
    # Indices identify exact bytes in the two generated modules.
    locations = defaultdict(deque)
    for module, parts in ((CORE, core_blocks), (EXTRA, extra_blocks)):
        for index, block in enumerate(parts):
            locations[block].append([module, index])
    order = [locations[block].popleft() for block in blocks]
    if any(locations.values()):
        raise ValueError("Unconsumed generated declaration blocks")
    pieces = {CORE: core_blocks, EXTRA: extra_blocks}
    reconstructed = prefix + b"".join(pieces[m][i] for m, i in order) + FOOTER
    if reconstructed != full:
        raise ValueError("Partition does not reproduce the full extraction")
    manifest = {
        "format": 1,
        "kind": "byte-exact partition of normalized Aeneas output; not proof acceptance",
        "full_sha256": sha256(full),
        "modules": {CORE: sha256(core), EXTRA: sha256(extra)},
        "blocks": {CORE: len(core_blocks), EXTRA: len(extra_blocks)},
        "full_order": order,
    }
    return {
        CORE: core,
        EXTRA: extra,
        "extraction-manifest.json": (json.dumps(manifest, indent=2) + "\n").encode(),
    }


def verify(full: bytes, directory: Path, manifest_path: Path) -> dict:
    """Check installed modules and reconstruct the actual extraction bytes."""
    manifest = json.loads(manifest_path.read_bytes())
    if manifest["format"] != 1:
        raise ValueError("Unknown extraction partition manifest format")
    if set(manifest["modules"]) != {CORE, EXTRA}:
        raise ValueError("Unexpected generated module set")
    pieces = {}
    prefix = b""
    for name in (CORE, EXTRA):
        data = (directory / name).read_bytes()
        if sha256(data) != manifest["modules"][name]:
            raise ValueError("Generated module hash mismatch: " + name)
        header, pieces[name] = split(data)
        if name == CORE:
            prefix = header
        elif header != prefix.replace(
            b"import Aeneas\n", b"import Verification.Generated\n"
        ):
            raise ValueError("Generated adapter header mismatch")
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
    outputs = partition(full, previous)
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
