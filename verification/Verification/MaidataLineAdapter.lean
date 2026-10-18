import Verification.GeneratedMaidataLineAdapter
import Verification.MaidataLineFields

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai
open private startsWithAmp parseKeyValueLine from LnmaiCore.Simai.Source.Maidata

namespace Verification.MaidataLineAdapterProof

open Verification.MaidataLineFieldsProof
open maidata_line_adapter maidata_scan

variable {Text View Level Ops : Type}

/-- Point laws identifying the runtime LinePrimitives dictionary with the
handwritten StringOps facade. The adapter performs no other operation. -/
structure PrimitiveAgrees (S : StringOps Text View Level)
    (P : LinePrimitives Ops Text View Level) : Prop where
  borrow : ∀ op text, P.borrow op text = ok (S.borrow text)
  trim : ∀ op view, P.trim op view = ok (S.trim view)
  is_empty : ∀ op view, P.is_empty op view = ok (S.isEmpty view)
  starts_with_amp : ∀ op view, P.starts_with_amp op view = ok (S.startsWith view "&")
  starts_with_inote : ∀ op view,
    P.starts_with_inote op view = ok (S.startsWith view "&inote_")
  header : ∀ op view, P.header op view = S.header view
  level : ∀ op view, P.level op view = S.level view
  push_newline : ∀ op text, P.push_newline op text = S.pushNewline text
  push_str : ∀ op text view, P.push_str op text view = S.pushStr text view

abbrev lineRead (P : LinePrimitives Ops Text View Level) :
    maidata_scan.LineRead (MaidataLineOps Text Ops) (Header Text) :=
  MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineReadPair P

abbrev lineWrite (P : LinePrimitives Ops Text View Level) :
    maidata_scan.LineWrite (MaidataLineOps Text Ops) (Header Text) (Fields Text)
      (alloc.vec.Vec (MaidataChartBlock Text Level))
      (MaidataChartBlock Text Level) :=
  MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineWritePairVecPairVecMaidataChartBlockMaidataChartBlock P

theorem read_points (E : ValueEncoding Text View Level) (S : StringOps Text View Level)
    (L : ReadStringLaws E S) (P : LinePrimitives Ops Text View Level)
    (A : PrimitiveAgrees S P) (lines : Slice Text) (prims : Ops) :
    Verification.MaidataScanProof.ReadPoints (lineRead P) ⟨lines, prims⟩
      (decodeLines E ⟨lines⟩) (decodeHeader E) := by
  refine { blank := ?_, amp := ?_, header := ?_, chart := ?_ }
  · intro index hi
    have hindex : index.val < lines.val.length := by
      simpa only [decodeLines, List.length_map] using hi
    obtain ⟨raw, hget, hraw⟩ := spec_imp_exists (Slice.index_usize_spec lines index hindex)
    simp only [lineRead, MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineReadPair,
      MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineReadPair.is_blank,
      hget, bind_tc_ok, A.borrow, A.trim, A.is_empty, L.empty, L.trim, L.borrow,
      hraw, decodeLines, List.getElem_map]
  · intro index hi
    have hindex : index.val < lines.val.length := by
      simpa only [decodeLines, List.length_map] using hi
    obtain ⟨raw, hget, hraw⟩ := spec_imp_exists (Slice.index_usize_spec lines index hindex)
    simp only [lineRead, MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineReadPair,
      MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineReadPair.starts_with_amp,
      hget, bind_tc_ok, A.borrow, A.trim, A.starts_with_amp, L.trim, L.borrow,
      L.startsWithView, startsWithAmp, hraw, decodeLines, List.getElem_map]
  · intro index hi
    have hindex : index.val < lines.val.length := by
      simpa only [decodeLines, List.length_map] using hi
    obtain ⟨raw, hget, hraw⟩ := spec_imp_exists (Slice.index_usize_spec lines index hindex)
    obtain ⟨out, hheader, hdecode⟩ := spec_imp_exists (L.header (S.borrow raw))
    apply exists_imp_spec
    refine ⟨out, ?_, ?_⟩
    · simp only [lineRead, MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineReadPair,
        MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineReadPair.header,
        hget, bind_tc_ok, A.borrow, A.header]
      exact hheader
    · simpa only [hraw, decodeLines, List.getElem_map, L.borrow] using hdecode
  · intro header
    rcases header with ⟨key, value⟩
    simp only [lineRead, MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineReadPair,
      MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineReadPair.is_chart_header,
      bind_tc_ok, A.borrow, A.starts_with_inote, L.startsWithView, L.borrow, decodeHeader]
    rfl

theorem append_field_eq (P : LinePrimitives Ops Text View Level) (prims : Ops)
    (lines : Slice Text) (fields : Fields Text) (header : Header Text) :
    (lineWrite P).append_field ⟨lines, prims⟩ fields header =
      alloc.vec.Vec.push fields header := rfl

theorem append_chart_eq (P : LinePrimitives Ops Text View Level) (prims : Ops)
    (lines : Slice Text) (charts : alloc.vec.Vec (MaidataChartBlock Text Level))
    (body : MaidataChartBlock Text Level) :
    (lineWrite P).append_chart ⟨lines, prims⟩ charts body =
      alloc.vec.Vec.push charts body := rfl

/-- The extracted begin_chart reads the header key once and keeps the value
verbatim. The runtime level operation is supplied explicitly. -/
theorem begin_chart_eq (S : StringOps Text View Level)
    (P : LinePrimitives Ops Text View Level) (A : PrimitiveAgrees S P)
    (prims : Ops) (lines : Slice Text) (key value : Text) (level : Level)
    (hlevel : S.level (S.borrow key) = ok level) :
    (lineWrite P).begin_chart ⟨lines, prims⟩ (key, value) =
      ok { level_index := level, raw_body := value } := by
  simp only [lineWrite,
    MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineWritePairVecPairVecMaidataChartBlockMaidataChartBlock,
    MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineWritePairVecPairVecMaidataChartBlockMaidataChartBlock.begin_chart,
    bind_tc_ok, A.borrow, A.level, hlevel]
  simp [hlevel]

/-- Newline is pushed before the source line is indexed, exactly as the runtime
facade does; each primitive result is supplied explicitly. -/
theorem append_body_line_eq (S : StringOps Text View Level)
    (P : LinePrimitives Ops Text View Level) (A : PrimitiveAgrees S P)
    (prims : Ops) (lines : Slice Text) (body : MaidataChartBlock Text Level)
    (index : Usize) (raw nl out : Text)
    (hget : Slice.index_usize lines index = ok raw)
    (hnl : S.pushNewline body.raw_body = ok nl)
    (hout : S.pushStr nl (S.borrow raw) = ok out) :
    (lineWrite P).append_body_line ⟨lines, prims⟩ body index =
      ok { body with raw_body := out } := by
  simp only [lineWrite,
    MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineWritePairVecPairVecMaidataChartBlockMaidataChartBlock,
    MaidataLineOps.Insts.Lnmai_core_verifyMaidata_scanLineWritePairVecPairVecMaidataChartBlockMaidataChartBlock.append_body_line,
    hget, hnl, hout, bind_tc_ok, A.borrow, A.push_newline, A.push_str]

end Verification.MaidataLineAdapterProof
