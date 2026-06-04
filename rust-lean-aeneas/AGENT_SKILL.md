# Rust + Lean 4 Formal Verification via Aeneas - Agent Skill

## Skill Definition

**Name:** `rust-lean-aeneas-verification`
**Version:** 1.0.0
**Description:** Formally verify Rust programs using Lean 4 and Aeneas
**Tags:** rust, lean, formal-verification, aeneas, theorem-proving

---

## Core Workflow

```
Rust Code → Charon (MIR extraction) → Aeneas (Lean translation) → Lean Proofs
```

### Step-by-Step Process

1. **Write Aeneas-Friendly Rust** (see guidelines below)
2. **Run Charon:** `charon cargo --preset=aeneas`
3. **Run Aeneas:** `aeneas -backend lean -dest output crate.llbc`
4. **Write Lean Proofs** about generated code
5. **Verify:** `lake build` to check proofs

---

## Aeneas-Friendly Rust Guidelines

### DO:
- Use `Vec<u8>` instead of `String`
- Use `while` loops with explicit indices
- Use `match` on `Option`/`Result` instead of `unwrap()`
- Use enum dispatch instead of `dyn Trait`
- Keep functions small and composable
- Use newtypes for clarity

### DON'T:
- Use `String` (complex internals)
- Use iterator chains (`map`, `filter`, `fold`)
- Use `unwrap()` or `expect()`
- Use `dyn Trait`
- Use closures extensively
- Use `break`/`continue` to outer loops

### Examples

```rust
// GOOD: Simple, verifiable
pub fn checked_add(x: u32, y: u32) -> Option<u32> {
    if y <= u32::MAX - x {
        Some(x + y)
    } else {
        None
    }
}

// GOOD: Explicit loop
fn sum_even(v: &Vec<i32>) -> i32 {
    let mut i: usize = 0;
    let mut sum: i32 = 0;
    while i < v.len() {
        if v[i] % 2 == 0 {
            sum += v[i];
        }
        i += 1;
    }
    sum
}

// BAD: Iterator chain (Aeneas can't handle)
fn sum_even_bad(v: &Vec<i32>) -> i32 {
    v.iter().filter(|x| **x % 2 == 0).sum()
}
```

---

## Installation

```bash
# 1. Install Lean 4
curl https://elan.lean-lang.org/elan-init.sh -sSf | sh

# 2. Install Charon
git clone https://github.com/AeneasVerif/charon.git
cd charon && cargo build --release
# Add charon/target/release to PATH

# 3. Install Aeneas (requires OCaml 5)
opam switch create 5.3.0
opam install ppx_deriving visitors easy_logging zarith yojson
git clone https://github.com/AeneasVerif/aeneas.git
cd aeneas && make
# Add aeneas/bin to PATH
```

---

## Commands Reference

### Charon Commands
```bash
charon version                           # Check version
charon cargo --preset=aeneas             # Extract with Aeneas preset
charon cargo --start-from crate::module  # Extract specific module
charon cargo --include crate::module::_  # Include specific items
charon cargo --exclude crate::module     # Exclude items
```

### Aeneas Commands
```bash
aeneas -backend lean file.llbc           # Generate Lean
aeneas -backend lean -split-files file.llbc  # Split into files
aeneas -backend lean -all-computable file.llbc  # No noncomputable
aeneas -backend lean -dest dir file.llbc # Specify output directory
```

### Lean Commands
```bash
lake build                               # Build project
lake test                                # Run tests
lean --version                           # Check version
```

---

## Proof Patterns

### 1. No Panic Proof
```lean
theorem fn_no_panic (x y : U32) :
  ∃ result, fn x y = ok result := by
  simp [fn]
  split
  · exact ⟨some_result, rfl⟩
  · exact ⟨none, rfl⟩
```

### 2. Correctness Proof
```lean
theorem fn_correct (x y : U32) (h : y <= U32.max - x) :
  fn x y = ok (x + y) := by
  simp [fn, h]
```

### 3. Invariant Preservation
```lean
theorem step_preserves_inv (s : State) (i : Input) :
  inv s → inv (step s i) := by
  intro h
  simp [step]
  -- Prove invariant holds
  exact h
```

### 4. Termination Proof
```lean
theorem loop_terminates (n : Nat) :
  ∃ result, loop n = ok result := by
  induction n with
  | zero => exact ⟨base_case, rfl⟩
  | succ n ih =>
    simp [loop]
    exact ih
```

---

## Supported Rust Features

| Feature | Support | Notes |
|---------|---------|-------|
| Basic arithmetic | ✅ Full | u32, i32, u64, etc. |
| if/else | ✅ Full | |
| match | ✅ Full | |
| while loops | ✅ Full | With fuel parameter |
| Vec<T> | ✅ Full | |
| Option<T> | ✅ Full | |
| Result<T, E> | ✅ Full | |
| Structs | ✅ Full | |
| Enums | ✅ Full | |
| References (&T, &mut T) | ✅ Full | |
| Closures | ⚠️ Limited | Simple closures only |
| Traits | ⚠️ Limited | Basic traits only |
| String | ❌ No | Use Vec<u8> |
| Iterator chains | ❌ No | Use while loops |
| dyn Trait | ❌ No | Use enum dispatch |

---

## Common Issues & Solutions

### Issue 1: Name Clashes
**Error:** `Name clash detected: types.RuntimePos.Sensor`
**Solution:** Use unique variant names
```rust
// BAD
enum RuntimePos { Button(ButtonZone), Sensor(SensorArea) }

// GOOD
enum RuntimePos { ButtonZonePos(ButtonZone), SensorAreaPos(SensorArea) }
```

### Issue 2: trait_decl_id Error
**Error:** `Could not find: trait_decl_id: 18`
**Solution:** Remove String usage, use Vec<u8>

### Issue 3: Overflow Panics
**Error:** Runtime panic on overflow
**Solution:** Use checked arithmetic or prove bounds
```rust
// GOOD: Check for overflow
fn checked_add(x: u32, y: u32) -> Option<u32> {
    if y <= u32::MAX - x { Some(x + y) } else { None }
}
```

### Issue 4: Missing Fuel
**Error:** Loop doesn't terminate
**Solution:** Set maxHeartbeats in Lean
```lean
set_option maxHeartbeats 1000000
```

---

## Verification Properties

| Property | Meaning | Example |
|----------|---------|---------|
| No panics | Function never crashes | checked_add handles all inputs |
| Correctness | Output matches spec | sort returns sorted list |
| Roundtrip | decode(encode(x)) = x | serialization is lossless |
| Invariant | Critical property holds | state machine valid |
| Termination | Program finishes | loop exits |
| Fairness | Resources allocated equally | round-robin scheduling |

---

## Project Structure

```
project/
├── src/
│   └── lib.rs           # Rust source (Aeneas-friendly)
├── lean/
│   ├── lakefile.lean    # Lean project config
│   ├── lean-toolchain   # Lean version
│   └── *.lean           # Generated + hand-written proofs
├── Cargo.toml
└── Makefile
```

---

## Example: Complete Verification

### Rust Source (src/lib.rs)
```rust
pub fn max_of(a: i32, b: i32) -> i32 {
    if a >= b { a } else { b }
}
```

### Charon + Aeneas
```bash
charon cargo --preset=aeneas
aeneas -backend lean -dest lean src/lib.llbc
```

### Generated Lean (lean/Lib.lean)
```lean
def max_of (a : I32) (b : I32) : I32 :=
  if a >= b then a else b
```

### Hand-written Proof (lean/Proofs.lean)
```lean
import Lib
open Aeneas

theorem max_of_correct (a b : I32) :
  max_of a b ≥ a ∧ max_of a b ≥ b := by
  simp [max_of]
  split
  · constructor
    · linarith
    · linarith
  · constructor
    · linarith
    · linarith
```

---

## Resources

- [Aeneas GitHub](https://github.com/AeneasVerif/aeneas)
- [Charon GitHub](https://github.com/AeneasVerif/charon)
- [Lean 4 Documentation](https://lean-lang.org/doc/)
- [Aeneas Tutorial](https://github.com/AeneasVerif/aeneas/tree/main/tests/lean/Tutorial)
- [Mathlib Documentation](https://leanprover-community.github.io/mathlib4_docs/)

---

## Quick Start Checklist

- [ ] Install Lean 4 (`elan`)
- [ ] Install Charon (build from source)
- [ ] Install Aeneas (build from source with OCaml 5)
- [ ] Write Aeneas-friendly Rust code
- [ ] Run `charon cargo --preset=aeneas`
- [ ] Run `aeneas -backend lean -dest lean crate.llbc`
- [ ] Open Lean project in VS Code with lean4 extension
- [ ] Write proofs about generated code
- [ ] Run `lake build` to verify
