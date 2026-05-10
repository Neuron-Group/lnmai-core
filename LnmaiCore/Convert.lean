/-
  Grade conversion: maps the raw 15-tier judgment to a reduced grade
  depending on the selected difficulty mode (JudgeStyle).

  Fully structural — no LT/LE comparisons needed.
-/

import LnmaiCore.Types

namespace LnmaiCore.Convert

open JudgeGrade

----------------------------------------------------------------------------
-- Conversion Functions (fully structural: enumerate all 15 grades)
----------------------------------------------------------------------------

def convertMaji : JudgeGrade → JudgeGrade
  -- Great* → Good
  | LateGreat      => LateGood
  | LateGreat2nd   => LateGood
  | LateGreat3rd   => LateGood
  | FastGreat      => FastGood
  | FastGreat2nd   => FastGood
  | FastGreat3rd   => FastGood
  -- Perfect3rd → Great
  | LatePerfect3rd => LateGreat
  | FastPerfect3rd => FastGreat
  -- Everything else → Miss (late side) or TooFast (fast side)
  | LatePerfect2nd => Miss
  | LateGood       => Miss
  | FastPerfect2nd => TooFast
  | FastGood       => TooFast
  -- Fixed points
  | Perfect => Perfect
  | Miss    => Miss
  | TooFast => TooFast

def convertGachi : JudgeGrade → JudgeGrade
  -- Perfect3rd → Good
  | LatePerfect3rd => LateGood
  | FastPerfect3rd => FastGood
  -- Perfect2nd → Great
  | LatePerfect2nd => LateGreat
  | FastPerfect2nd => FastGreat
  -- Everything else → Miss / TooFast
  | LateGreat      => Miss
  | LateGreat2nd   => Miss
  | LateGreat3rd   => Miss
  | LateGood       => Miss
  | FastGreat      => TooFast
  | FastGreat2nd   => TooFast
  | FastGreat3rd   => TooFast
  | FastGood       => TooFast
  -- Fixed points
  | Perfect => Perfect
  | Miss    => Miss
  | TooFast => TooFast

def convertGori : JudgeGrade → JudgeGrade
  | Perfect => Perfect
  | Miss    => Miss
  -- All late grades → Miss
  | LateGood       => Miss
  | LateGreat3rd   => Miss
  | LateGreat2nd   => Miss
  | LateGreat      => Miss
  | LatePerfect3rd => Miss
  | LatePerfect2nd => Miss
  -- All fast grades → TooFast
  | FastPerfect2nd => TooFast
  | FastPerfect3rd => TooFast
  | FastGreat      => TooFast
  | FastGreat2nd   => TooFast
  | FastGreat3rd   => TooFast
  | FastGood       => TooFast
  | TooFast        => TooFast

def convertGrade (style : JudgeStyle) (g : JudgeGrade) : JudgeGrade :=
  match style with
  | .Default => g
  | .Maji    => convertMaji g
  | .Gachi   => convertGachi g
  | .Gori    => convertGori g

----------------------------------------------------------------------------
-- Theorems
--   Note: these conversion functions are NOT idempotent for all inputs
--   (e.g. Maji maps LateGreat3rd→LateGood→Miss on second application).
--   They are applied exactly once per note in the game.
--   We prove what we can: fixed points and structural properties.
----------------------------------------------------------------------------

theorem perfect_fixed (style : JudgeStyle) : convertGrade style Perfect = Perfect := by
  cases style <;> rfl

theorem miss_fixed (style : JudgeStyle) : convertGrade style Miss = Miss := by
  cases style <;> rfl

theorem tooFast_fixed_maji_gachi : convertMaji TooFast = TooFast ∧ convertGachi TooFast = TooFast := by
  constructor <;> rfl

theorem perfect_is_upper_bound (style : JudgeStyle) (g : JudgeGrade) : convertGrade style g = Perfect → g = Perfect := by
  cases style <;> cases g <;> simp [convertGrade, convertMaji, convertGachi, convertGori]


end LnmaiCore.Convert
