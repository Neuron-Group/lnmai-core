/-
  LnMai Core — verified game judgment runtime.

  Modules:
    Types      — domain types (grades, notes, styles, events)
    Constants  — timing windows and frame constants
    Convert    — grade conversion (MAJI, GACHI, GORI) with theorems
    Judge      — pure judgment functions (tap, touch, slide, hold end)
    Score      — score/combo computation
    Lifecycle  — note state machines (tap, hold, touch, slide)
    InputModel — frame input, queues, game state
    ChartLoader — declarative chart-to-runtime loader
    Scheduler  — stepFrame: one function per frame
-/

import LnmaiCore.Types
import LnmaiCore.Areas
import LnmaiCore.Constants
import LnmaiCore.Convert
import LnmaiCore.Judge
import LnmaiCore.Score
import LnmaiCore.Lifecycle
import LnmaiCore.InputModel
import LnmaiCore.ChartLoader
import LnmaiCore.Scheduler
-- import LnmaiCore.Proofs.InnerScreenUnfair
-- import LnmaiCore.Proofs.SlideZoneSkipping
-- import LnmaiCore.Proofs.SlideJudgmentSegments
-- import LnmaiCore.Proofs.SlideDEWifi
