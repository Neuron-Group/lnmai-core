import LnmaiCore
open LnmaiCore
open InputModel

private def onlyTargetBad (result : RuntimeSimulationResult) (target : Nat) : Bool :=
  let bad := result.events.filter (fun evt => evt.grade != JudgeGrade.Perfect)
  bad.length = 1 && bad.head?.map JudgeEvent.noteIndex = some target

private def seqForTimes (t1 t2 t3 t4 : Int) : ManualTacticSequence :=
  mkManualTacticSequence
    [ tapAt 81774171 ButtonZone.K1
    , holdSensorAt 82258042 SensorArea.A1 true
    , holdSensorAt t1 SensorArea.A1 false
    , holdSensorAt t1 SensorArea.A8 true
    , holdSensorAt t2 SensorArea.A8 false
    , holdSensorAt t2 SensorArea.A7 true
    , holdSensorAt t3 SensorArea.A7 false
    , holdSensorAt t3 SensorArea.A6 true
    , holdSensorAt t4 SensorArea.A6 false ]

private def keepEvent (evt : TimedInputEvent) : Bool :=
  let t := evt.at.toMicros
  !(t = 81774171 || t = 82258042 || t = 82318525 || t = 82379008 || t = 82439492 || t = 82516643)

#eval do
  let content ← IO.FS.readFile "tools/assets/11264_幽霊東京/maidata.txt"
  match Simai.compileLowered content 5 with
  | .error err => IO.println s!"parse error: {repr err}"
  | .ok chart =>
      let baseTactic := defaultTacticFromChart chart
      let preserved : List TimedInputEvent := baseTactic.events.filter keepEvent
      let t1s : List Int := [82258042, 82274708, 82291375, 82308042, 82324708, 82341375]
      let t2s : List Int := [82358042, 82374675, 82391342, 82408008, 82424675]
      let t3s : List Int := [82418042, 82434708, 82451375, 82468042]
      let t4s : List Int := [82499977, 82516643, 82533310]
      for t1 in t1s do
        for t2 in t2s do
          for t3 in t3s do
            for t4 in t4s do
              let overridden := mkManualTacticSequence (preserved ++ (seqForTimes t1 t2 t3 t4).events)
              let result := simulateChartSpecWithTactic chart overridden
              if onlyTargetBad result 345 then
                IO.println s!"candidate {t1} {t2} {t3} {t4}"
                return
      IO.println "none"
