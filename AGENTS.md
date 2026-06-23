# Agent Notes

Prefer the local CodeGraph checkout in `tools/codegraph` when exploring this repo semantically.

## Bootstrap

```bash
bash tools/bootstrap_codegraph.sh
```

That builds the Lean tree-sitter wasm, copies it into the local CodeGraph checkout, installs Node dependencies, and builds the CLI.

## Use

```bash
bash tools/run_codegraph.sh init
bash tools/run_codegraph.sh explore "scheduler activation logic"
bash tools/run_codegraph.sh node LnmaiCore/Scheduler.lean
```

## Scope

This local fork is patched for first-pass Lean support:

- declarations: `def`, `theorem`, `abbrev`, `axiom`
- type-like declarations: `structure`, Lean `class`, `inductive`
- imports: `import`
- call sites: `app`

If the local CodeGraph build is missing, fall back to normal file reads and `rg`.
