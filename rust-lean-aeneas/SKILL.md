# Rust + Lean 4 Formal Verification via Aeneas

## Overview

This skill provides guidance for formally verifying Rust programs using Lean 4 and Aeneas. The workflow is: write normal Rust → translate with Charon + Aeneas → prove properties in Lean.

## Key Tools

- **Rust** - Source code with memory safety guarantees
- **Charon** - Extracts Rust MIR to `.llbc` intermediate format
- **Aeneas** - Translates `.llbc` to pure functional Lean 4 code
- **Lean 4** - Theorem prover for writing mathematical proofs

## Installation

```bash
# Install Lean 4
curl https://elan.lean-lang.org/elan-init.sh -sSf | sh

# Install Charon (from source)
git clone https://github.com/AeneasVerif/charon.git
cd charon && cargo build --release

# Install Aeneas (from source, requires OCaml 5)
opam switch create 5.3.0
git clone https://github.com/AeneasVerif/aeneas.git
cd aeneas && make
```

## Workflow

### Step 1: Write Aeneas-Friendly Rust

Follow these guidelines:

1. **Use `Vec<u8>` instead of `String`** - String has complex internals Aeneas doesn't model
2. **Use `while` loops with explicit indices** - Avoid iterator chains with closures
3. **Avoid `unwrap()` and `expect()`** - Use `Option`/`Result` with explicit `match`
4. **Use explicit pattern matching** - Avoid combinators like `map`, `and_then`
5. **Use enums for dispatch** - Avoid `dyn Trait`
6. **Pre-allocate vectors** - When size is known upfront
7. **Consider functional data structures** - Linked lists translate cleanly to Lean inductive types
8. **Keep functions small** - Easier to verify individually
9. **Use newtypes** - Makes Lean translation more readable
10. **Avoid `break`/`continue` to outer loops** - Use flag variables instead

### Step 2: Run Charon

```bash
# From your Rust project directory
charon cargo --preset=aeneas
```

This generates `your_crate.llbc` file.

### Step 3: Run Aeneas

```bash
aeneas -backend lean -dest output_dir your_crate.llbc
```

This generates Lean 4 files with pure functional translations.

### Step 4: Write Proofs in Lean

```lean
import Aeneas
open Aeneas Aeneas.Std Result

-- Your generated Lean code will be here
-- Write proofs about the generated functions

theorem my_function_correct (x : U32) :
  my_function x = ok (x + 1) := by
  simp [my_function]
```

## Supported Rust Features

| Feature | Support Level |
|---------|--------------|
| Basic arithmetic | ✅ Full |
| `if`/`else` | ✅ Full |
| `match` | ✅ Full |
| `while` loops | ✅ Full (with fuel) |
| `Vec<T>` | ✅ Full |
| `Option<T>` | ✅ Full |
| `Result<T, E>` | ✅ Full |
| Structs | ✅ Full |
| Enums | ✅ Full |
| References (`&T`, `&mut T`) | ✅ Full |
| Closures | ⚠️ Limited |
| Traits | ⚠️ Limited |
| `String` | ❌ Use `Vec<u8>` |
| Iterator chains | ❌ Use `while` loops |
| `dyn Trait` | ❌ Use enum dispatch |

## Example: Verified Addition

### Rust Source
```rust
pub fn checked_add(x: u32, y: u32) -> Option<u32> {
    if y <= u32::MAX - x {
        Some(x + y)
    } else {
        None
    }
}
```

### Generated Lean
```lean
def checked_add (x : U32) (y : U32) : Option U32 :=
  if y <= U32.max - x then
    some (x + y)
  else
    none
```

### Lean Proof
```lean
theorem checked_add_no_panic (x y : U32) :
  ∃ result, checked_add x y = ok result := by
  simp [checked_add]
  split
  · exact ⟨some (x + y), rfl⟩
  · exact ⟨none, rfl⟩
```

## Proof Patterns

### No Panics
```lean
theorem fn_no_panic (args...) :
  ∃ result, fn args = ok result := by
  simp [fn]
  -- Use split, omega, etc.
```

### Functional Correctness
```lean
theorem fn_correct (args...) :
  fn args = ok expected := by
  simp [fn]
  -- Prove output matches specification
```

### Invariant Preservation
```lean
theorem step_preserves_inv (state : State) (input : Input) :
  inv state → inv (step state input) := by
  intro h
  simp [step]
  -- Prove invariant holds after step
```

## Common Pitfalls

1. **Name clashes**: Enum variants with same name as type path cause issues
   - Solution: Use unique variant names like `ButtonZonePos` instead of `Button`

2. **`trait_decl_id` errors**: Caused by `String` or complex trait usage
   - Solution: Replace `String` with `Vec<u8>`, avoid complex traits

3. **Overflow panics**: Arithmetic can overflow
   - Solution: Use checked arithmetic or prove bounds

4. **Missing fuel**: Loops need fuel parameter
   - Solution: Use `--loops-to-rec` option or set `maxHeartbeats`

## Resources

- [Aeneas GitHub](https://github.com/AeneasVerif/aeneas)
- [Charon GitHub](https://github.com/AeneasVerif/charon)
- [Lean 4 Documentation](https://lean-lang.org/doc/)
- [Aeneas Tutorial](https://github.com/AeneasVerif/aeneas/tree/main/tests/lean/Tutorial)

## Quick Reference

```bash
# Check Charon version
charon version

# Run Charon with Aeneas preset
charon cargo --preset=aeneas

# Run Aeneas to generate Lean
aeneas -backend lean -dest lean_output crate.llbc

# Run Aeneas with options
aeneas -backend lean -split-files -all-computable crate.llbc

# Build Lean project
lake build
```
