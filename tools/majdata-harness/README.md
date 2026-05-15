# Majdata Harness

This subproject is a lightweight C# harness for inspecting small gameplay logic fragments inspired by `../reference/MajdataPlay` without needing a Unity environment.

## Scope

This harness is intended for:

- click-consumption experiments
- queue-gating experiments
- order-sensitive note interaction checks
- reduced reproductions of suspicious gameplay semantics

This harness is **not** intended to run real Unity note objects.

## Environment

Use the local Nix shell:

```bash
nix-shell
```

from `tools/majdata-harness/`.

## Run

```bash
dotnet run --project MajdataHarness.csproj
```

## Current scenarios

- tap consumes shared click before hold head
- hold head consumes shared click before tap when order is reversed

These scenarios model the reference-style logic at a small scale and help verify semantic expectations before patching the Lean runtime.
