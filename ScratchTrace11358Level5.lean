import LnmaiCore

open LnmaiCore

def realChartPath : System.FilePath :=
  "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"

private def eventSummary (evt : JudgeEvent) : String :=
  s!"note={evt.noteIndex} kind={repr evt.kind} grade={repr evt.grade} diff={evt.diff.toMicros}"

private def findSkeleton? (entries : List NoteTimingSkeleton) (noteIndex : Nat) : Option NoteTimingSkeleton :=
  entries.find? (fun entry => entry.noteIndex = noteIndex)

#eval do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 5 with
  | .error err =>
      IO.println s!"parse error {repr err}"
  | .ok chart =>
      let skeleton := chartTimingSkeleton chart
      let result := simulateChartSpecWithTactic chart (defaultTacticFromChart chart)
      let nonPerfect := result.events.filter (fun evt : JudgeEvent => evt.grade != JudgeGrade.Perfect)
      IO.println s!"AP={achievesAP result} APPlus={achievesAPPlus result} missing={missingJudgedNoteIndices result} nonPerfectCount={nonPerfect.length}"
      for evt in nonPerfect do
        IO.println (eventSummary evt)
        IO.println s!"  skeleton={repr (findSkeleton? skeleton evt.noteIndex)}"
