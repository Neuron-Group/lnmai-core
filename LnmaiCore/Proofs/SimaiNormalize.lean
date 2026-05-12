import LnmaiCore.Simai

namespace LnmaiCore.Proofs.SimaiNormalize

open LnmaiCore.Simai

def normalizedLineSlide : Except ParseError FrontendChartResult :=
  parseFrontendChartResult "&first=0\n&inote_1=\n(120)\n1-3[4:1],\n" 1

def normalizedSlideTopologyAttachedCheck : Bool :=
  match normalizedLineSlide with
  | .error _ => false
  | .ok chart =>
      match chart.semantic.normalized.slides, chart.semantic.lowered.slides with
      | [slide], [lowered] =>
          !slide.judgeQueues.isEmpty && slide.totalJudgeQueueLen > 0 &&
          !lowered.judgeQueues.isEmpty && lowered.totalJudgeQueueLen = slide.totalJudgeQueueLen
      | _, _ => false

theorem normalized_slide_topology_attached_from_parser :
    normalizedSlideTopologyAttachedCheck = true := by
  native_decide

end LnmaiCore.Proofs.SimaiNormalize
