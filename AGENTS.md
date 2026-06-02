# AGENTS.md

## Urgent

Before changing slide parser/runtime/prover/FFI behavior, read:

- `doc/slide-head-body-refactor-proposal.md`
- `doc/refactor-roadmap-semantic-parity.md`
- `doc/refactor-task-list-semantic-parity.md`

Current urgent semantic-parity target:

- split slide head and slide body into separate lowered objects before runtime

This is now the preferred architecture for supporting:

- normal slides
- singleton no-head slides
- connected body-only slide parts
- eventual head-only artifacts

Before implementing this refactor, first inspect the corresponding runtime semantics in `./reference/MajdataPlay` carefully.
Do not implement directly from the proposal doc alone when the relevant MajdataPlay behavior can still be checked in source.

Do not implement ad hoc new slide-head exceptions in runtime until this refactor direction has been considered against the proposal document above.

## Documentation Language

- Write docs in English.
- If an external file, chart title, or source artifact uses Chinese or Japanese in its literal name, keep the literal name unchanged and explain it in English around it.
