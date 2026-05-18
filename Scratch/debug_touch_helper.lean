import LnmaiCore.Scheduler
open LnmaiCore
#eval let q : InputModel.SensorQueueVec Lifecycle.TouchNote := SensorVec.ofFn (fun area => if area == .A1 then { notes := [], currentIndex := 0 } else { notes := [] })
  let queue := InputModel.sensorQueueAt q .A1
  let q2 := if queue.currentIndex == 0 then InputModel.setSensorQueueAt q .A1 queue.advance else q
  q2.getD .A1 {notes:=[]}
