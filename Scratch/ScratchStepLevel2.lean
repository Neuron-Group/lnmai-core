import LnmaiCore
open LnmaiCore
open LnmaiCore.ChartLoader
open LnmaiCore.InputModel
open LnmaiCore.Lifecycle


def realChartPath : System.FilePath :=
  "tools/assets/11358_インドア系ならトラックメイカー/maidata.txt"

private def holdStateString (note : HoldNote) : String :=
  match note.state with
  | .HeadWaiting => "HeadWaiting"
  | .HeadJudgeable => "HeadJudgeable"
  | .HeadJudged g => s!"HeadJudged {repr g}"
  | .BodyHeld => "BodyHeld"
  | .BodyReleased => "BodyReleased"
  | .Ended g => s!"Ended {repr g}"

#eval do
  let content ← IO.FS.readFile realChartPath
  match Simai.compileLowered content 2 with
  | .error err => IO.println s!"parse error {repr err}"
  | .ok chart =>
      let tactic := defaultTacticFromChart chart
      let initial := ChartLoader.buildGameState chart
      let batches := expandReplayBatchesFrom initial.currentTime (tacticBatches tactic)
      let interesting := batches.filter (fun b => b.currentTime.toMicros >= 73800000 && b.currentTime.toMicros <= 75200000)
      let rec loop (st : GameState) (rest : List TimedInputBatch) : IO Unit :=
        match rest with
        | [] => pure ()
        | batch :: tail =>
            let (next, evts, _, _) := Scheduler.stepFrameTimed st batch
            let holds := next.activeHolds.filter (fun entry =>
              let idx := entry.2.params.noteIndex
              idx = 57 || idx = 58)
            IO.println s!"time={batch.currentTime.toMicros} batch={repr batch.events} events={repr (evts.map (fun e => (e.noteIndex, e.kind, e.grade)))} holds={repr (holds.map (fun h => (h.1, h.2.params.noteIndex, holdStateString h.2)))} queues={(next.holdQueues.getD .K1 {notes:=[]}).currentIndex},{(next.holdQueues.getD .K8 {notes:=[]}).currentIndex}"
            loop next tail
      loop initial interesting
