import Verification.Generated
import Verification.MaidataScanFuel

/-!
UNELABORATED. Actual maidata_scan line-scanning bodies from extraction-3
(SHA256 d037f357eb395803e657d1ca4b445cc73908e6910c59bb5c37af0de3493ab1bf).
ReadPoints and WritePoints are individual trait-call contracts. No body,
loop, whole-file, group, or pass correspondence is assumed.

`ready` and `bodyReady` describe representation/capacity/allocation resources,
not equality of the final scan. Their concrete runtime instances remain open.
In particular a bound on the number of lines is not a UTF-8 byte-capacity bound.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai
open Verification.MaidataScanModel Verification.MaidataScanFuel

set_option maxHeartbeats 1000000

namespace Verification.MaidataScanProof

open private startsWithAmp parseKeyValueLine parseMaidataLines
  from LnmaiCore.Simai.Source.Maidata

def headerBlock (header : String × String) : MaidataChartBlock :=
  { levelIndex := ((header.1.drop 7).toString.toNat?).getD 0
    rawBody := header.2 }

def appendRawLine (body : MaidataChartBlock) (line : String) : MaidataChartBlock :=
  { body with rawBody := body.rawBody ++ "\n" ++ line }

/-- The completed value of a partly written body, over a borrowed index range. -/
def bodyResume (lines : List String) (stop cursor : Nat)
    (body : MaidataChartBlock) : MaidataChartBlock :=
  { levelIndex := body.levelIndex
    rawBody := appendBody body.rawBody ((lines.drop cursor).take (stop - cursor)) }

theorem bodyResume_done (lines : List String) (stop cursor : Nat)
    (body : MaidataChartBlock) (h : stop ≤ cursor) :
    bodyResume lines stop cursor body = body := by
  cases body
  simp [bodyResume, Nat.sub_eq_zero_of_le h, appendBody]

theorem bodyResume_step (lines : List String) (stop cursor : Nat)
    (body : MaidataChartBlock) (hmore : cursor < stop) (hstop : stop ≤ lines.length) :
    bodyResume lines stop cursor body =
      bodyResume lines stop (cursor + 1)
        (appendRawLine body lines[cursor]'(by omega)) := by
  have hi : cursor < lines.length := by omega
  have hsub : stop - cursor = (stop - (cursor + 1)) + 1 := by omega
  simp only [bodyResume, List.drop_eq_getElem_cons hi, hsub, List.take_succ_cons,
    appendBody, appendRawLine]

def chartBody (lines : List String) (cursor : Nat)
    (header : String × String) : MaidataChartBlock :=
  bodyResume lines (scanEnd keepBody lines (cursor + 1)) (cursor + 1)
    (headerBlock header)

theorem chartBody_eq (lines : List String) (cursor : Nat)
    (header : String × String) :
    chartBody lines cursor header =
      { levelIndex := ((header.1.drop 7).toString.toNat?).getD 0
        rawBody := appendBody header.2 (splitPrefix keepBody (lines.drop (cursor + 1))).1 } := by
  simp only [chartBody, bodyResume, headerBlock, scanEnd, Nat.add_sub_cancel_left,
    splitPrefix_take]

structure Encoding (Header Fields Charts Body : Type) where
  header : Header → String × String
  fields : Fields → List (String × String)
  charts : Charts → List MaidataChartBlock
  body : Body → MaidataChartBlock

def decodeOutput {Header Fields Charts Body : Type}
    (enc : Encoding Header Fields Charts Body) (out : Fields × Charts) : MaidataFile :=
  { metadata := { fields := enc.fields out.1 }
    charts := enc.charts out.2 }

/-- Exact observations at one valid index, plus the chart-key observation.
Header parsing is a single line operation (including allocation of key/value). -/
structure ReadPoints {Header Ops : Type} (Read : maidata_scan.LineRead Ops Header)
    (ops : Ops) (lines : List String) (decodeHeader : Header → String × String) : Prop where
  blank : ∀ (index : Usize) (hi : index.val < lines.length),
    Read.is_blank ops index = ok (decide (trim lines[index.val] = ""))
  amp : ∀ (index : Usize) (hi : index.val < lines.length),
    Read.starts_with_amp ops index = ok (startsWithAmp lines[index.val])
  header : ∀ (index : Usize) (hi : index.val < lines.length),
    Read.header ops index ⦃ out => out.map decodeHeader = parseKeyValueLine lines[index.val] ⦄
  chart : ∀ header,
    Read.is_chart_header ops header = ok ((decodeHeader header).1.startsWith "&inote_")

private theorem increment_index (index len : Usize) (hlt : index.val < len.val) :
    ∃ next, index + 1#usize = ok next ∧ next.val = index.val + 1 := by
  have hbound : index.val + (1#usize).val ≤ UScalar.max .Usize := by scalar_tac
  obtain ⟨next, hnext, hv⟩ := spec_imp_exists
    (UScalar.add_spec (x := index) (y := 1#usize) hbound)
  exact ⟨next, hnext, by simpa using hv⟩

/-- Proof of the actual extracted endpoint body: stop before the first amp,
or consume exactly one non-amp line using a checked, bounded increment. -/
theorem chart_body_end_body_spec {Header Ops : Type}
    (Read : maidata_scan.LineRead Ops Header) (ops : Ops) (lines : List String)
    (decodeHeader : Header → String × String) (R : ReadPoints Read ops lines decodeHeader)
    (len cursor : Usize) (hlen : len.val = lines.length) (hcursor : cursor.val ≤ len.val) :
    maidata_scan.chart_body_end_loop.body Read len ops cursor ⦃ out =>
      match out with
      | .done stop => cursor.val ≤ stop.val ∧ stop.val ≤ len.val ∧
          stop.val = scanEnd keepBody lines cursor.val
      | .cont next => next.val = cursor.val + 1 ∧ next.val ≤ len.val ∧
          scanEnd keepBody lines next.val = scanEnd keepBody lines cursor.val ⦄ := by
  by_cases hmore : cursor < len
  · have hi : cursor.val < lines.length := by simpa [hlen] using hmore
    have hstep := scanEnd_step keepBody lines cursor.val hi
    cases ha : startsWithAmp lines[cursor.val] with
    | true =>
        have hstop : cursor.val = scanEnd keepBody lines cursor.val := by
          simpa [keepBody, ha] using hstep.symm
        apply exists_imp_spec
        refine ⟨.done cursor, ?_, Nat.le_refl _, hcursor, hstop⟩
        simp [maidata_scan.chart_body_end_loop.body, hmore, R.amp cursor hi, ha]
    | false =>
        obtain ⟨next, hadd, hnext⟩ := increment_index cursor len (by simpa using hmore)
        have hsame : scanEnd keepBody lines next.val = scanEnd keepBody lines cursor.val := by
          simpa [keepBody, ha, hnext] using hstep.symm
        apply exists_imp_spec
        refine ⟨.cont next, ?_, hnext, ?_, hsame⟩
        · simp [maidata_scan.chart_body_end_loop.body, hmore, R.amp cursor hi, ha, hadd]
        · omega
  · have hge : lines.length ≤ cursor.val := by simpa [hlen] using hmore
    apply exists_imp_spec
    refine ⟨.done cursor, ?_, Nat.le_refl _, hcursor, ?_⟩
    · simp [maidata_scan.chart_body_end_loop.body, hmore]
    · exact (scanEnd_finished keepBody lines cursor.val hge).symm

/-- Endpoint loop termination uses len-cursor; len=Usize.max is allowed. -/
theorem chart_body_end_loop_spec {Header Ops : Type}
    (Read : maidata_scan.LineRead Ops Header) (ops : Ops) (lines : List String)
    (decodeHeader : Header → String × String) (R : ReadPoints Read ops lines decodeHeader)
    (len start : Usize) (hlen : len.val = lines.length) (hstart : start.val ≤ len.val) :
    maidata_scan.chart_body_end_loop Read len ops start ⦃ stop =>
      start.val ≤ stop.val ∧ stop.val ≤ len.val ∧
        stop.val = scanEnd keepBody lines start.val ⦄ := by
  unfold maidata_scan.chart_body_end_loop
  apply loop.spec_decr_nat (fun cursor : Usize => len.val - cursor.val)
    (fun cursor => start.val ≤ cursor.val ∧ cursor.val ≤ len.val ∧
      scanEnd keepBody lines cursor.val = scanEnd keepBody lines start.val)
  · intro cursor hinv
    rcases hinv with ⟨hlower, hupper, heq⟩
    apply spec_mono (chart_body_end_body_spec Read ops lines decodeHeader R
      len cursor hlen hupper)
    intro out hout
    cases out with
    | done stop =>
        rcases hout with ⟨hcs, hstop, hval⟩
        exact ⟨Nat.le_trans hlower hcs, hstop, hval.trans heq⟩
    | cont next =>
        rcases hout with ⟨hnext, hn, hsame⟩
        exact ⟨⟨by omega, hn, hsame.trans heq⟩, by omega⟩
  · exact ⟨Nat.le_refl _, hstart, rfl⟩

theorem chart_body_end_spec {Header Ops : Type}
    (Read : maidata_scan.LineRead Ops Header) (ops : Ops) (lines : List String)
    (decodeHeader : Header → String × String) (R : ReadPoints Read ops lines decodeHeader)
    (len start : Usize) (hlen : len.val = lines.length) (hstart : start.val ≤ len.val) :
    maidata_scan.chart_body_end Read len start ops ⦃ stop =>
      start.val ≤ stop.val ∧ stop.val ≤ len.val ∧
        stop.val = scanEnd keepBody lines start.val ⦄ := by
  exact chart_body_end_loop_spec Read ops lines decodeHeader R len start hlen hstart

def SkipLine (line : String) : Prop :=
  trim line = "" ∨ startsWithAmp line = false ∨ parseKeyValueLine line = none

def HeaderAt {Header : Type} (decodeHeader : Header → String × String)
    (lines : List String) (cursor : Nat) (hi : cursor < lines.length) (header : Header) : Prop :=
  trim lines[cursor] ≠ "" ∧ startsWithAmp lines[cursor] = true ∧
    parseKeyValueLine lines[cursor] = some (decodeHeader header)

/-- A proved plan postcondition, made from the actual per-line observations.
The suffix equality is deliberately not one of the read/write premises. -/
def PlanAt {Header : Type} (decodeHeader : Header → String × String)
    (lines : List String) (cursor : Nat) (hi : cursor < lines.length) :
    maidata_scan.LinePlan Header → Prop
  | .Skip next => next.val = cursor + 1 ∧ SkipLine lines[cursor]
  | .Metadata header next => next.val = cursor + 1 ∧
      HeaderAt decodeHeader lines cursor hi header ∧
      (decodeHeader header).1.startsWith "&inote_" = false
  | .Chart header range => range.start.val = cursor + 1 ∧
      range.«end».val = scanEnd keepBody lines (cursor + 1) ∧
      HeaderAt decodeHeader lines cursor hi header ∧
      (decodeHeader header).1.startsWith "&inote_" = true

/-- All branches of the extracted plan_line, including a None header. -/
theorem plan_line_spec {Header Ops : Type}
    (Read : maidata_scan.LineRead Ops Header) (ops : Ops) (lines : List String)
    (decodeHeader : Header → String × String) (R : ReadPoints Read ops lines decodeHeader)
    (len cursor : Usize) (hlen : len.val = lines.length) (hi : cursor.val < lines.length) :
    maidata_scan.plan_line Read len cursor ops ⦃ plan => PlanAt decodeHeader lines cursor.val hi plan ⦄ := by
  obtain ⟨next, hadd, hnext⟩ := increment_index cursor len (by omega)
  have hnextBound : next.val ≤ len.val := by omega
  by_cases hb : trim lines[cursor.val] = ""
  · apply exists_imp_spec
    refine ⟨.Skip next, ?_, hnext, Or.inl hb⟩
    simp [maidata_scan.plan_line, hadd, R.blank cursor hi, hb]
  · cases ha : startsWithAmp lines[cursor.val] with
    | false =>
        apply exists_imp_spec
        refine ⟨.Skip next, ?_, hnext, Or.inr (Or.inl ha)⟩
        simp [maidata_scan.plan_line, hadd, R.blank cursor hi, hb, R.amp cursor hi, ha]
    | true =>
        obtain ⟨header, hcall, hmap⟩ := spec_imp_exists (R.header cursor hi)
        cases header with
        | none =>
            have hparse : parseKeyValueLine lines[cursor.val] = none := by
              simpa only [Option.map_none] using hmap.symm
            apply exists_imp_spec
            refine ⟨.Skip next, ?_, hnext, Or.inr (Or.inr hparse)⟩
            simp [maidata_scan.plan_line, hadd, R.blank cursor hi, hb,
              R.amp cursor hi, ha, hcall]
        | some header =>
            have hparse : parseKeyValueLine lines[cursor.val] = some (decodeHeader header) := by
              simpa only [Option.map_some] using hmap.symm
            cases hc : (decodeHeader header).1.startsWith "&inote_" with
            | false =>
                apply exists_imp_spec
                refine ⟨.Metadata header next, ?_, hnext, ⟨hb, ha, hparse⟩, hc⟩
                simp [maidata_scan.plan_line, hadd, R.blank cursor hi, hb,
                  R.amp cursor hi, ha, hcall, R.chart header, hc]
            | true =>
                obtain ⟨stop, hstop, _, _, hval⟩ := spec_imp_exists
                  (chart_body_end_spec Read ops lines decodeHeader R len next hlen hnextBound)
                apply exists_imp_spec
                refine ⟨.Chart header { start := next, «end» := stop }, ?_,
                  hnext, ?_, ⟨hb, ha, hparse⟩, hc⟩
                · simp [maidata_scan.plan_line, hadd, R.blank cursor hi, hb,
                    R.amp cursor hi, ha, hcall, R.chart header, hc, hstop]
                · simpa only [hnext] using hval

theorem plan_skip_resume {Header : Type} (decodeHeader : Header → String × String)
    (lines : List String) (cursor : Nat) (hi : cursor < lines.length) (next : Usize)
    (hplan : PlanAt decodeHeader lines cursor hi (.Skip next))
    (fields : List (String × String)) (charts : List MaidataChartBlock) :
    resume lines next.val fields charts = resume lines cursor fields charts := by
  rcases hplan with ⟨hnext, hskip⟩
  rw [resume_step lines cursor fields charts hi]
  rcases hskip with hb | ha | hparse
  · simp [hnext, hb]
  · simp [hnext, ha]
  · simp [hnext, hparse]

theorem plan_metadata_resume {Header : Type} (decodeHeader : Header → String × String)
    (lines : List String) (cursor : Nat) (hi : cursor < lines.length)
    (header : Header) (next : Usize)
    (hplan : PlanAt decodeHeader lines cursor hi (.Metadata header next))
    (fields : List (String × String)) (charts : List MaidataChartBlock) :
    resume lines next.val (fields ++ [decodeHeader header]) charts =
      resume lines cursor fields charts := by
  rcases hplan with ⟨hnext, ⟨hb, ha, hparse⟩, hc⟩
  cases hd : decodeHeader header with
  | mk key value =>
      simp only [hd] at hparse hc ⊢
      rw [resume_step lines cursor fields charts hi]
      simp [hnext, hb, ha, hparse, hc]

theorem plan_chart_resume {Header : Type} (decodeHeader : Header → String × String)
    (lines : List String) (cursor : Nat) (hi : cursor < lines.length)
    (header : Header) (range : maidata_scan.Span)
    (hplan : PlanAt decodeHeader lines cursor hi (.Chart header range))
    (fields : List (String × String)) (charts : List MaidataChartBlock) :
    resume lines range.«end».val fields (charts ++ [chartBody lines cursor (decodeHeader header)]) =
      resume lines cursor fields charts := by
  rcases hplan with ⟨_, hend, ⟨hb, ha, hparse⟩, hc⟩
  cases hd : decodeHeader header with
  | mk key value =>
      simp only [hd] at hparse hc ⊢
      rw [resume_step lines cursor fields charts hi]
      simp [hend, hb, ha, hparse, hc, chartBody_eq]

/-- Resource predicates are deliberately supplied separately from decoding.
Each obligation below describes one trait operation, at a source-valid index.
`advance` only preserves resources for unchanged output as less input remains.
`bodyLine` preserves levelIndex and appends precisely newline + the raw line.
No contract mentions `complete`, `resume`, a plan, or a final scan result. -/
structure WritePoints {Header Fields Charts Body Ops : Type}
    (Write : maidata_scan.LineWrite Ops Header Fields Charts Body) (ops : Ops)
    (lines : List String) (enc : Encoding Header Fields Charts Body)
    (ready : Nat → Fields → Charts → Prop) (bodyReady : Nat → Nat → Body → Prop) : Prop where
  advance : ∀ (cursor next : Nat) (fields : Fields) (charts : Charts),
    cursor ≤ next → next ≤ lines.length → ready cursor fields charts →
      ready next fields charts
  metadata : ∀ (cursor : Usize) (fields : Fields) (charts : Charts) (header : Header)
      (hi : cursor.val < lines.length),
    ready cursor.val fields charts →
    parseKeyValueLine lines[cursor.val] = some (enc.header header) →
    Write.append_field ops fields header ⦃ out =>
      enc.fields out = enc.fields fields ++ [enc.header header] ∧
        ready (cursor.val + 1) out charts ⦄
  beginChart : ∀ (cursor : Usize) (fields : Fields) (charts : Charts) (header : Header)
      (hi : cursor.val < lines.length),
    ready cursor.val fields charts →
    parseKeyValueLine lines[cursor.val] = some (enc.header header) →
    (enc.header header).1.startsWith "&inote_" = true →
    Write.begin_chart ops header ⦃ out =>
      enc.body out = headerBlock (enc.header header) ∧
        bodyReady (scanEnd keepBody lines (cursor.val + 1)) (cursor.val + 1) out ⦄
  bodyLine : ∀ (stop line : Usize) (body : Body)
      (hline : line.val < stop.val) (hstop : stop.val ≤ lines.length),
    bodyReady stop.val line.val body →
    Write.append_body_line ops body line ⦃ out =>
      enc.body out = appendRawLine (enc.body body) lines[line.val]'(by omega) ∧
        bodyReady stop.val (line.val + 1) out ⦄
  chart : ∀ (cursor stop : Usize) (fields : Fields) (charts : Charts) (body : Body),
    cursor.val < stop.val → stop.val ≤ lines.length →
    ready cursor.val fields charts → bodyReady stop.val stop.val body →
    Write.append_chart ops charts body ⦃ out =>
      enc.charts out = enc.charts charts ++ [enc.body body] ∧
        ready stop.val fields out ⦄

/-- The real inner body has just one append and one checked cursor increment. -/
theorem body_lines_body_spec {Header Fields Charts Body Ops : Type}
    (Write : maidata_scan.LineWrite Ops Header Fields Charts Body) (ops : Ops)
    (lines : List String) (enc : Encoding Header Fields Charts Body)
    (ready : Nat → Fields → Charts → Prop) (bodyReady : Nat → Nat → Body → Prop)
    (W : WritePoints Write ops lines enc ready bodyReady)
    (stop line : Usize) (body : Body) (hline : line.val ≤ stop.val)
    (hstop : stop.val ≤ lines.length) (hr : bodyReady stop.val line.val body) :
    maidata_scan.collect_lines_loop0_loop0.body Write ops stop body line ⦃ out =>
      match out with
      | .done value => bodyReady stop.val stop.val value ∧
          enc.body value = bodyResume lines stop.val line.val (enc.body body)
      | .cont (value, next) => next.val = line.val + 1 ∧ next.val ≤ stop.val ∧
          bodyReady stop.val next.val value ∧
          bodyResume lines stop.val next.val (enc.body value) =
            bodyResume lines stop.val line.val (enc.body body) ⦄ := by
  by_cases hmore : line < stop
  · have hlt : line.val < stop.val := by simpa using hmore
    obtain ⟨value, hwrite, hvalue, hready⟩ := spec_imp_exists
      (W.bodyLine stop line body hlt hstop hr)
    obtain ⟨next, hadd, hnext⟩ := increment_index line stop hlt
    apply exists_imp_spec
    refine ⟨.cont (value, next), ?_, hnext, ?_, ?_, ?_⟩
    · simp [maidata_scan.collect_lines_loop0_loop0.body, hmore, hwrite, hadd]
    · omega
    · simpa only [hnext] using hready
    · rw [hnext, hvalue]
      exact (bodyResume_step lines stop.val line.val (enc.body body) hlt hstop).symm
  · have hge : stop.val ≤ line.val := by simpa using hmore
    have heq : line.val = stop.val := by omega
    apply exists_imp_spec
    refine ⟨.done body, ?_, ?_, ?_⟩
    · simp [maidata_scan.collect_lines_loop0_loop0.body, hmore]
    · simpa only [heq] using hr
    · exact (bodyResume_done lines stop.val line.val (enc.body body) hge).symm

/-- Actual range collection, with a preserved completed-body value. -/
theorem body_lines_loop_spec {Header Fields Charts Body Ops : Type}
    (Write : maidata_scan.LineWrite Ops Header Fields Charts Body) (ops : Ops)
    (lines : List String) (enc : Encoding Header Fields Charts Body)
    (ready : Nat → Fields → Charts → Prop) (bodyReady : Nat → Nat → Body → Prop)
    (W : WritePoints Write ops lines enc ready bodyReady)
    (stop start : Usize) (initial : Body) (hstart : start.val ≤ stop.val)
    (hstop : stop.val ≤ lines.length) (hr : bodyReady stop.val start.val initial) :
    maidata_scan.collect_lines_loop0_loop0 Write ops stop initial start ⦃ out =>
      bodyReady stop.val stop.val out ∧
        enc.body out = bodyResume lines stop.val start.val (enc.body initial) ⦄ := by
  unfold maidata_scan.collect_lines_loop0_loop0
  apply loop.spec_decr_nat (fun p : Body × Usize => stop.val - p.2.val)
    (fun p => p.2.val ≤ stop.val ∧ bodyReady stop.val p.2.val p.1 ∧
      bodyResume lines stop.val p.2.val (enc.body p.1) =
        bodyResume lines stop.val start.val (enc.body initial))
  · rintro ⟨body, line⟩ ⟨hline, hready, hinv⟩
    apply spec_mono (body_lines_body_spec Write ops lines enc ready bodyReady W
      stop line body hline hstop hready)
    intro out hout
    cases out with
    | done value =>
        rcases hout with ⟨hready', hval⟩
        exact ⟨hready', hval.trans hinv⟩
    | cont state =>
        rcases state with ⟨value, next⟩
        rcases hout with ⟨hnext, hbound, hready', hval⟩
        exact ⟨⟨hbound, hready', hval.trans hinv⟩, by omega⟩
  · exact ⟨hstart, hr, rfl⟩

/-- The actual outer body. The plan and the inner range-loop are proved above;
the only remaining hypotheses at this layer are individual read/write points. -/
theorem collect_lines_body_spec {Header Fields Charts Body Ops : Type}
    (Read : maidata_scan.LineRead Ops Header)
    (Write : maidata_scan.LineWrite Ops Header Fields Charts Body) (ops : Ops)
    (lines : List String) (enc : Encoding Header Fields Charts Body)
    (ready : Nat → Fields → Charts → Prop) (bodyReady : Nat → Nat → Body → Prop)
    (R : ReadPoints Read ops lines enc.header)
    (W : WritePoints Write ops lines enc ready bodyReady)
    (len cursor : Usize) (fields : Fields) (charts : Charts)
    (hlen : len.val = lines.length) (hcursor : cursor.val ≤ len.val)
    (hr : ready cursor.val fields charts) :
    maidata_scan.collect_lines_loop0.body Read Write len ops fields charts cursor ⦃ out =>
      match out with
      | .done value => ready len.val value.1 value.2 ∧
          decodeOutput enc value = resume lines cursor.val (enc.fields fields) (enc.charts charts)
      | .cont (fields', charts', next) => cursor.val < next.val ∧ next.val ≤ len.val ∧
          ready next.val fields' charts' ∧
          resume lines next.val (enc.fields fields') (enc.charts charts') =
            resume lines cursor.val (enc.fields fields) (enc.charts charts) ⦄ := by
  by_cases hmore : cursor < len
  · have hi : cursor.val < lines.length := by simpa [hlen] using hmore
    obtain ⟨plan, hcall, hplan⟩ := spec_imp_exists
      (plan_line_spec Read ops lines enc.header R len cursor hlen hi)
    cases plan with
    | Skip next =>
        have hnext := hplan.1
        have hnextBound : next.val ≤ lines.length := by omega
        have hready := W.advance cursor.val next.val fields charts (by omega) hnextBound hr
        have hsame := plan_skip_resume enc.header lines cursor.val hi next hplan
          (enc.fields fields) (enc.charts charts)
        apply exists_imp_spec
        refine ⟨.cont (fields, charts, next), ?_, ?_, ?_, hready, hsame⟩
        · simp [maidata_scan.collect_lines_loop0.body, hmore, hcall]
        · omega
        · omega
    | Metadata header next =>
        rcases hplan with ⟨hnext, hheader, hchart⟩
        obtain ⟨fields', hwrite, hfields, hready⟩ := spec_imp_exists
          (W.metadata cursor fields charts header hi hr hheader.2.2)
        have hsame : resume lines next.val (enc.fields fields') (enc.charts charts) =
            resume lines cursor.val (enc.fields fields) (enc.charts charts) := by
          rw [hfields]
          exact plan_metadata_resume enc.header lines cursor.val hi header next
            ⟨hnext, hheader, hchart⟩ (enc.fields fields) (enc.charts charts)
        apply exists_imp_spec
        refine ⟨.cont (fields', charts, next), ?_, ?_, ?_, ?_, hsame⟩
        · simp [maidata_scan.collect_lines_loop0.body, hmore, hcall, hwrite]
        · omega
        · omega
        · simpa only [hnext] using hready
    | Chart header range =>
        rcases hplan with ⟨hstart, hend, hheader, hchart⟩
        have hbounds := scanEnd_bounds keepBody lines (cursor.val + 1) (by omega)
        have hstartBound : range.start.val ≤ range.«end».val := by omega
        have hendBound : range.«end».val ≤ lines.length := by omega
        have hprogress : cursor.val < range.«end».val := by omega
        obtain ⟨body, hbegin, hbody, hreadyBody⟩ := spec_imp_exists
          (W.beginChart cursor fields charts header hi hr hheader.2.2 hchart)
        have hreadyRange : bodyReady range.«end».val range.start.val body := by
          simpa only [hstart, hend] using hreadyBody
        obtain ⟨body', hcollect, hreadyBody', hbody'⟩ := spec_imp_exists
          (body_lines_loop_spec Write ops lines enc ready bodyReady W
            range.«end» range.start body hstartBound hendBound hreadyRange)
        obtain ⟨charts', hwrite, hcharts, hready⟩ := spec_imp_exists
          (W.chart cursor range.«end» fields charts body' hprogress hendBound hr hreadyBody')
        have hsourceBody : enc.body body' = chartBody lines cursor.val (enc.header header) := by
          simpa only [chartBody, hstart, hend, hbody] using hbody'
        have hsame : resume lines range.«end».val (enc.fields fields) (enc.charts charts') =
            resume lines cursor.val (enc.fields fields) (enc.charts charts) := by
          rw [hcharts, hsourceBody]
          exact plan_chart_resume enc.header lines cursor.val hi header range
            ⟨hstart, hend, hheader, hchart⟩ (enc.fields fields) (enc.charts charts)
        apply exists_imp_spec
        refine ⟨.cont (fields, charts', range.«end»), ?_, hprogress, ?_, hready, hsame⟩
        · simp [maidata_scan.collect_lines_loop0.body, hmore, hcall, hbegin, hcollect, hwrite]
        · omega
  · have hge : lines.length ≤ cursor.val := by simpa [hlen] using hmore
    have heq : cursor.val = len.val := by omega
    apply exists_imp_spec
    refine ⟨.done (fields, charts), ?_, ?_, ?_⟩
    · simp [maidata_scan.collect_lines_loop0.body, hmore]
    · simpa only [heq] using hr
    · exact (resume_done lines cursor.val (enc.fields fields) (enc.charts charts) hge).symm

/-- Complete extracted outer loop with an arbitrary initial cursor and prefixes.
Metadata/chart order is retained by the single-record append laws; the chart
delimiter is retained by chart_body_end_spec, even for an empty body. -/
theorem collect_lines_loop_spec {Header Fields Charts Body Ops : Type}
    (Read : maidata_scan.LineRead Ops Header)
    (Write : maidata_scan.LineWrite Ops Header Fields Charts Body) (ops : Ops)
    (lines : List String) (enc : Encoding Header Fields Charts Body)
    (ready : Nat → Fields → Charts → Prop) (bodyReady : Nat → Nat → Body → Prop)
    (R : ReadPoints Read ops lines enc.header)
    (W : WritePoints Write ops lines enc ready bodyReady)
    (len start : Usize) (fields : Fields) (charts : Charts)
    (hlen : len.val = lines.length) (hstart : start.val ≤ len.val)
    (hr : ready start.val fields charts) :
    maidata_scan.collect_lines_loop0 Read Write len fields charts ops start ⦃ out =>
      ready len.val out.1 out.2 ∧
        decodeOutput enc out = resume lines start.val (enc.fields fields) (enc.charts charts) ⦄ := by
  unfold maidata_scan.collect_lines_loop0
  apply loop.spec_decr_nat (fun p : Fields × Charts × Usize => len.val - p.2.2.val)
    (fun p => p.2.2.val ≤ len.val ∧ ready p.2.2.val p.1 p.2.1 ∧
      resume lines p.2.2.val (enc.fields p.1) (enc.charts p.2.1) =
        resume lines start.val (enc.fields fields) (enc.charts charts))
  · rintro ⟨fields', charts', cursor⟩ ⟨hcursor, hready, hinv⟩
    apply spec_mono (collect_lines_body_spec Read Write ops lines enc ready bodyReady R W
      len cursor fields' charts' hlen hcursor hready)
    intro out hout
    cases out with
    | done value =>
        rcases hout with ⟨hready', hvalue⟩
        exact ⟨hready', hvalue.trans hinv⟩
    | cont state =>
        rcases state with ⟨fields'', charts'', next⟩
        rcases hout with ⟨hprogress, hbound, hready', hvalue⟩
        exact ⟨⟨hbound, hready', hvalue.trans hinv⟩, by omega⟩
  · exact ⟨hstart, hr, rfl⟩

theorem collect_lines_spec {Header Fields Charts Body Ops : Type}
    (Read : maidata_scan.LineRead Ops Header)
    (Write : maidata_scan.LineWrite Ops Header Fields Charts Body) (ops : Ops)
    (lines : List String) (enc : Encoding Header Fields Charts Body)
    (ready : Nat → Fields → Charts → Prop) (bodyReady : Nat → Nat → Body → Prop)
    (R : ReadPoints Read ops lines enc.header)
    (W : WritePoints Write ops lines enc ready bodyReady)
    (len : Usize) (fields : Fields) (charts : Charts)
    (hlen : len.val = lines.length) (hr : ready 0 fields charts) :
    maidata_scan.collect_lines Read Write len fields charts ops ⦃ out =>
      ready len.val out.1 out.2 ∧
        decodeOutput enc out = complete lines (enc.fields fields) (enc.charts charts) ⦄ := by
  simpa [maidata_scan.collect_lines, resume_start] using
    collect_lines_loop_spec Read Write ops lines enc ready bodyReady R W
      len 0#usize fields charts hlen (by simp) hr

/-- Decoded result of the actual extracted public kernel, equal to the original
source with REVERSED input accumulators. No source fuel assumption remains. -/
theorem collect_lines_source_equiv {Header Fields Charts Body Ops : Type}
    (Read : maidata_scan.LineRead Ops Header)
    (Write : maidata_scan.LineWrite Ops Header Fields Charts Body) (ops : Ops)
    (lines : List String) (enc : Encoding Header Fields Charts Body)
    (ready : Nat → Fields → Charts → Prop) (bodyReady : Nat → Nat → Body → Prop)
    (R : ReadPoints Read ops lines enc.header)
    (W : WritePoints Write ops lines enc ready bodyReady)
    (len : Usize) (fields : Fields) (charts : Charts)
    (hlen : len.val = lines.length) (hr : ready 0 fields charts) :
    (do let out ← maidata_scan.collect_lines Read Write len fields charts ops
        ok (decodeOutput enc out)) =
      ok (parseMaidataLines lines (enc.fields fields).reverse (enc.charts charts).reverse) := by
  obtain ⟨out, hout, _, hdecoded⟩ := spec_imp_exists
    (collect_lines_spec Read Write ops lines enc ready bodyReady R W len fields charts hlen hr)
  simp only [hout, bind_tc_ok, hdecoded, complete_eq_source]

/-- Public source correspondence, conditional only on the individual point laws,
the borrowed line representation, and empty output resource initialization.
The UTF-8 split/record facade itself is not silently included in the kernel. -/
theorem parse_source_equiv {Header Fields Charts Body Ops : Type}
    (Read : maidata_scan.LineRead Ops Header)
    (Write : maidata_scan.LineWrite Ops Header Fields Charts Body) (ops : Ops)
    (lines : List String) (enc : Encoding Header Fields Charts Body)
    (ready : Nat → Fields → Charts → Prop) (bodyReady : Nat → Nat → Body → Prop)
    (R : ReadPoints Read ops lines enc.header)
    (W : WritePoints Write ops lines enc ready bodyReady)
    (len : Usize) (fields : Fields) (charts : Charts) (content : String)
    (hlen : len.val = lines.length) (hr : ready 0 fields charts)
    (hlines : lines = content.splitOn "\n")
    (hfields : enc.fields fields = []) (hcharts : enc.charts charts = []) :
    (do let out ← maidata_scan.collect_lines Read Write len fields charts ops
        ok (Except.ok (decodeOutput enc out) : Except ParseError MaidataFile)) =
      ok (parseSourceMaidata content) := by
  obtain ⟨out, hout, _, hdecoded⟩ := spec_imp_exists
    (collect_lines_spec Read Write ops lines enc ready bodyReady R W len fields charts hlen hr)
  simp only [hout, bind_tc_ok, hdecoded, hfields, hcharts, hlines, parse_public_complete]

end Verification.MaidataScanProof
