import LnmaiCore
open LnmaiCore


def realChartPath : System.FilePath :=
  "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"

private def eventSummary (evt : JudgeEvent) : String :=
  s!"note={evt.noteIndex} kind={repr evt.kind} grade={repr evt.grade} diff={evt.diff.toMicros}"

private def nonPerfectEvents (events : List JudgeEvent) : List JudgeEvent :=
  events.filter (fun evt => evt.grade != JudgeGrade.Perfect)

#eval do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 2 with
  | .error err =>
      IO.println s!"parse error {repr err}"
  | .ok chart =>
      let result := simulateChartSpecWithTactic chart (defaultTacticFromChart chart)
      let nonPerfect := nonPerfectEvents result.events
      IO.println s!"AP={achievesAP result} APPlus={achievesAPPlus result} missing={missingJudgedNoteIndices result} nonPerfectCount={nonPerfect.length}"
      for evt in nonPerfect do
        IO.println (eventSummary evt)
