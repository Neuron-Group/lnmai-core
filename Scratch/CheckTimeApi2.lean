def main : IO Unit := do
  let t1 ← IO.monoMsNow
  let t2 ← IO.monoNanosNow
  IO.println s!"{t1} {t2}"
