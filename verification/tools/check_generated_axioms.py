#!/usr/bin/env python3
"""Reject newly opaque runtime operations before installing an extraction.

Debug formatting is outside the equivalence target. The separate Lean audit
still rejects any theorem that transitively depends on these declarations.
"""

from pathlib import Path
import re
import sys


FORMATTER_AXIOMS = {
    "core.fmt.Formatter.debug_struct_field2_finish",
    "core.fmt.Formatter.debug_struct_field3_finish",
    "core.fmt.Formatter.debug_struct_field5_finish",
    "core.fmt.Formatter.debug_tuple_field2_finish",
    "core.fmt.Formatter.debug_tuple_field3_finish",
    "Char.Insts.CoreFmtDebug.fmt",
}


def main() -> None:
    if len(sys.argv) != 2:
        raise SystemExit("usage: check_generated_axioms.py GENERATED.lean")
    source = Path(sys.argv[1]).read_bytes().decode("utf-8")
    declared = set(re.findall(r"^axiom\s+([A-Za-z_][\w.]*)", source, re.MULTILINE))
    unexpected = declared - FORMATTER_AXIOMS
    if unexpected:
        raise SystemExit("Unexpected extracted axioms: " + ", ".join(sorted(unexpected)))
    print(f"Extracted axioms: {len(declared)} known formatter declarations; no runtime axioms.")


if __name__ == "__main__":
    main()
