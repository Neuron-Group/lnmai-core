-- Audit: print the axioms each proved theorem depends on.
--
-- If any of these printed `sorryAx`, the theorem would be unproven. Standard
-- axioms (`propext`, `Classical.choice`, `Quot.sound`) are expected.
import Verification.Areas
import Verification.Types
import Verification.Convert
import Verification.Judge
import Verification.Score

#print axioms Verification.Areas.toLnmSensorArea_ofLnmSensorArea
#print axioms Verification.Areas.buttonZone_of_index_equiv
#print axioms Verification.Areas.outerSlot_to_button_zone_equiv
#print axioms Verification.TypesProof.judgeGrade_dist_from_perfect_equiv
#print axioms Verification.TypesProof.noteType_base_score_equiv
#print axioms Verification.ConvertProof.convert_grade_equiv
#print axioms Verification.JudgeProof.correct_slide_grade_equiv
#print axioms Verification.ScoreProof.base_score_equiv
