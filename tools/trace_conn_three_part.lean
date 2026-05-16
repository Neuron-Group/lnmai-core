import LnmaiCore

open LnmaiCore
open LnmaiCore.ChartLoader

private def chartBuiltSameHeadConnThreePartChain : ChartSpec :=
  simai_lowered_chart! "&first=0\n&inote_1=\n(120)\n1-3[4:1]*>5[4:1]*<7[4:1],\n"

#eval do
  IO.println "== lowered slides =="
  for slide in chartBuiltSameHeadConnThreePartChain.slides do
    IO.println (repr slide)

  let tactic := defaultTacticFromChart chartBuiltSameHeadConnThreePartChain
  IO.println "== tactic events =="
  for evt in tactic.events do
    IO.println s!"{repr evt}"

  let result := simulateChartSpecWithTactic chartBuiltSameHeadConnThreePartChain tactic
  IO.println "== result =="
  IO.println s!"missing={missingJudgedNoteIndices result}"
  IO.println s!"achievesAP={achievesAP result}"
  for evt in result.events do
    IO.println (repr evt)
