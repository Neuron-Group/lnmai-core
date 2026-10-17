import Verification.MaidataScan
import Verification.MaidataHeaderReference

/-!
UNELABORATED source factoring of runtime simai/maidata.rs::MaidataLineOps.
The input is an actual Aeneas Slice; both output collections are actual Vecs.
The dictionaries below use Slice.index_usize and Vec.push, rather than assuming
the indexed reads or vector updates as trait-level equivalence hypotheses.

Rust String/&str/Natural are represented by Text/View/Level. Only their listed
single-value operations remain conditional. This file is not an extraction of
the runtime impls: that field/borrow/primitive registration is an open boundary.
The generic maidata_scan.collect_lines called at the end IS the real Generated
kernel already handled by MaidataScan. No line-plan, body, or scan law is assumed.

Vec ready bounds count elements. Body readiness separately counts UTF-8 bytes,
including one newline per remaining input line. This conservative budget also
counts lines beyond the next header. byteLimit must be instantiated with a valid
native String object-size limit and successful allocation assumptions; the
existing Aeneas Vec model does not model native allocator failure or byte sizes.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify
open LnmaiCore LnmaiCore.Simai
open Verification.MaidataScanProof

namespace Verification.MaidataLineFieldsProof

open private startsWithAmp parseKeyValueLine parseMaidataLines
  from LnmaiCore.Simai.Source.Maidata

structure ValueEncoding (Text View Level : Type) where
  text : Text → String
  view : View → String
  level : Level → Nat

abbrev Header (Text : Type) := Text × Text
abbrev Fields (Text : Type) := alloc.vec.Vec (Header Text)

/-- Exact two runtime fields; no opaque whole-record update operation. -/
structure ChartBlock (Text Level : Type) where
  level_index : Level
  raw_body : Text

abbrev Charts (Text Level : Type) := alloc.vec.Vec (ChartBlock Text Level)

structure LineOps (Text : Type) where
  lines : Slice Text

/-- Borrowed views are separate from owned String values. header is the one-line
find('=')/trim/substring allocation helper, not maidata_scan.plan_line. level is
strip_prefix("&inote_")/Natural::from_lean_str/default on a single key. -/
structure StringOps (Text View Level : Type) where
  borrow : Text → View
  trim : View → View
  isEmpty : View → Bool
  startsWith : View → String → Bool
  header : View → Result (Option (Header Text))
  level : View → Result Level
  pushNewline : Text → Result Text
  pushStr : Text → View → Result Text

variable {Text View Level : Type}

def decodeHeader (E : ValueEncoding Text View Level) (header : Header Text) :
    String × String :=
  (E.text header.1, E.text header.2)

def decodeBody (E : ValueEncoding Text View Level) (body : ChartBlock Text Level) :
    MaidataChartBlock :=
  { levelIndex := E.level body.level_index
    rawBody := E.text body.raw_body }

def decodeLines (E : ValueEncoding Text View Level) (ops : LineOps Text) : List String :=
  ops.lines.val.map E.text

def encoding (E : ValueEncoding Text View Level) :
    MaidataScanProof.Encoding (Header Text) (Fields Text) (Charts Text Level)
      (ChartBlock Text Level) :=
  { header := decodeHeader E
    fields := fun fields => fields.val.map (decodeHeader E)
    charts := fun charts => charts.val.map (decodeBody E)
    body := decodeBody E }

/-- The four runtime LineRead methods, with their actual indexed access. -/
def readCall (S : StringOps Text View Level) :
    maidata_scan.LineRead (LineOps Text) (Header Text) :=
  { is_blank := fun ops index => do
      let line ← Slice.index_usize ops.lines index
      ok (S.isEmpty (S.trim (S.borrow line)))
    starts_with_amp := fun ops index => do
      let line ← Slice.index_usize ops.lines index
      ok (S.startsWith (S.trim (S.borrow line)) "&")
    header := fun ops index => do
      let line ← Slice.index_usize ops.lines index
      S.header (S.borrow line)
    is_chart_header := fun _ header =>
      ok (S.startsWith (S.borrow header.1) "&inote_") }

/-- Both Vec pushes and both body field assignments are concrete. In particular,
append_body_line pushes '\n' BEFORE indexing the source line, as runtime does. -/
def writeCall (S : StringOps Text View Level) :
    maidata_scan.LineWrite (LineOps Text) (Header Text) (Fields Text)
      (Charts Text Level) (ChartBlock Text Level) :=
  { append_field := fun _ fields header => alloc.vec.Vec.push fields header
    begin_chart := fun _ header => do
      let level ← S.level (S.borrow header.1)
      ok { level_index := level, raw_body := header.2 }
    append_body_line := fun ops body index => do
      let nl ← S.pushNewline body.raw_body
      let line ← Slice.index_usize ops.lines index
      let raw ← S.pushStr nl (S.borrow line)
      ok { body with raw_body := raw }
    append_chart := fun _ charts body => alloc.vec.Vec.push charts body }

/-- No indexing or multi-line operation occurs in these String contracts. -/
structure ReadStringLaws (E : ValueEncoding Text View Level)
    (S : StringOps Text View Level) : Prop where
  borrow : ∀ text, E.view (S.borrow text) = E.text text
  trim : ∀ view, E.view (S.trim view) = LnmaiCore.Simai.trim (E.view view)
  empty : ∀ view, S.isEmpty view = decide (E.view view = "")
  startsWithView : ∀ view text, S.startsWith view text = (E.view view).startsWith text
  header : ∀ view, S.header view ⦃ out =>
    out.map (decodeHeader E) = parseKeyValueLine (E.view view) ⦄

/-- String mutation laws are byte-bounded single operations. The initial
header byte bound is derived separately from the original parser. -/
structure WriteStringLaws (E : ValueEncoding Text View Level)
    (S : StringOps Text View Level) (byteLimit : Nat) : Prop where
  level : ∀ view, (E.view view).startsWith "&inote_" = true →
    S.level view ⦃ out => E.level out = ((E.view view).drop 7).toString.toNat?.getD 0 ⦄
  newline : ∀ text, (E.text text).utf8ByteSize + 1 ≤ byteLimit →
    S.pushNewline text ⦃ out => E.text out = E.text text ++ "\n" ⦄
  append : ∀ text view,
    (E.text text).utf8ByteSize + (E.view view).utf8ByteSize ≤ byteLimit →
    S.pushStr text view ⦃ out => E.text out = E.text text ++ E.view view ⦄

theorem decodeLines_length (E : ValueEncoding Text View Level) (ops : LineOps Text) :
    (decodeLines E ops).length = ops.lines.val.length := by
  simp only [decodeLines, List.length_map]

/-- Bounds and decoding for one real Slice.index_usize, derived from the slice. -/
theorem line_at (E : ValueEncoding Text View Level) (ops : LineOps Text)
    (index : Usize) (hi : index.val < (decodeLines E ops).length) :
    ∃ raw, Slice.index_usize ops.lines index = ok raw ∧
      E.text raw = (decodeLines E ops)[index.val] := by
  have hraw : index.val < ops.lines.val.length := by
    simpa only [decodeLines_length] using hi
  obtain ⟨raw, hget, heq⟩ := spec_imp_exists
    (Slice.index_usize_spec ops.lines index hraw)
  refine ⟨raw, hget, ?_⟩
  simpa only [heq, decodeLines, List.getElem_map]

theorem read_points (E : ValueEncoding Text View Level)
    (S : StringOps Text View Level) (L : ReadStringLaws E S) (ops : LineOps Text) :
    ReadPoints (readCall S) ops (decodeLines E ops) (decodeHeader E) := by
  constructor
  · intro index hi
    obtain ⟨raw, hget, hraw⟩ := line_at E ops index hi
    simp only [readCall, hget, bind_tc_ok, L.empty, L.trim, L.borrow, hraw]
  · intro index hi
    obtain ⟨raw, hget, hraw⟩ := line_at E ops index hi
    simp only [readCall, hget, bind_tc_ok, L.startsWithView, L.trim, L.borrow, hraw,
      startsWithAmp]
  · intro index hi
    obtain ⟨raw, hget, hraw⟩ := line_at E ops index hi
    obtain ⟨out, hheader, hdecode⟩ := spec_imp_exists (L.header (S.borrow raw))
    apply exists_imp_spec
    refine ⟨out, ?_, ?_⟩
    · simpa only [readCall, hget, bind_tc_ok] using hheader
    · simpa only [L.borrow, hraw] using hdecode
  · intro header
    simp only [readCall, L.startsWithView, L.borrow, decodeHeader]

/-- A reusable concrete push lemma. It preserves existing order and duplicates. -/
theorem push_decode {α β : Type} (decode : α → β) (values : alloc.vec.Vec α)
    (value : α) (hroom : values.val.length < Usize.max) :
    alloc.vec.Vec.push values value ⦃ out =>
      out.val.length = values.val.length + 1 ∧
      out.val.map decode = values.val.map decode ++ [decode value] ⦄ := by
  obtain ⟨out, hpush, hvalues⟩ :=
    spec_imp_exists (alloc.vec.Vec.push_spec values value hroom)
  apply exists_imp_spec
  refine ⟨out, hpush, ?_⟩
  rw [hvalues]
  simp [List.length_append, List.length_cons, List.length_nil,
    List.map_append, List.map_cons, List.map_nil]

/-- Minimal write subset: no String law is needed for the actual field Vec push. -/
theorem append_field_spec (E : ValueEncoding Text View Level)
    (S : StringOps Text View Level) (ops : LineOps Text) (fields : Fields Text)
    (header : Header Text) (hroom : fields.val.length < Usize.max) :
    (writeCall S).append_field ops fields header ⦃ out =>
      out.val.length = fields.val.length + 1 ∧
      (encoding E).fields out = (encoding E).fields fields ++ [decodeHeader E header] ⦄ :=
  push_decode (decodeHeader E) fields header hroom

/-- Minimal write subset: the block is moved intact into the chart Vec. -/
theorem append_chart_spec (E : ValueEncoding Text View Level)
    (S : StringOps Text View Level) (ops : LineOps Text) (charts : Charts Text Level)
    (body : ChartBlock Text Level) (hroom : charts.val.length < Usize.max) :
    (writeCall S).append_chart ops charts body ⦃ out =>
      out.val.length = charts.val.length + 1 ∧
      (encoding E).charts out = (encoding E).charts charts ++ [decodeBody E body] ⦄ :=
  push_decode (decodeBody E) charts body hroom

/-- At most one new field OR chart per remaining source line. The two separate
bounds deliberately do not assume how the lines will be classified. -/
def ready (len cursor : Nat) (fields : Fields Text) (charts : Charts Text Level) : Prop :=
  fields.val.length + (len - cursor) ≤ Usize.max ∧
    charts.val.length + (len - cursor) ≤ Usize.max

def byteBudget : List String → Nat
  | [] => 0
  | line :: rest => line.utf8ByteSize + 1 + byteBudget rest

theorem byteBudget_drop_le (lines : List String) (cursor : Nat) :
    byteBudget (lines.drop cursor) ≤ byteBudget lines := by
  induction cursor generalizing lines with
  | zero => simp only [List.drop_zero, Nat.le_refl]
  | succ cursor ih =>
      cases lines with
      | nil => simp only [List.drop_nil, byteBudget, Nat.le_refl]
      | cons line rest =>
          simp only [List.drop_succ_cons]
          have h := ih rest
          change byteBudget (rest.drop cursor) ≤ line.utf8ByteSize + 1 + byteBudget rest
          omega

theorem byteBudget_at (lines : List String) (cursor : Nat) (hi : cursor < lines.length) :
    lines[cursor].utf8ByteSize + 1 + byteBudget (lines.drop (cursor + 1)) ≤ byteBudget lines := by
  have h := byteBudget_drop_le lines cursor
  rw [List.drop_eq_getElem_cons hi] at h
  exact h

/-- A conservative suffix byte budget; independent of where the next '&' lies.
stop is intentionally unused: no endpoint or body equivalence is hidden here. -/
def bodyReady (E : ValueEncoding Text View Level) (byteLimit : Nat)
    (lines : List String) (_stop cursor : Nat) (body : ChartBlock Text Level) : Prop :=
  (E.text body.raw_body).utf8ByteSize + byteBudget (lines.drop cursor) ≤ byteLimit

private theorem newline_bytes : "\n".utf8ByteSize = 1 := rfl

theorem ready_empty (E : ValueEncoding Text View Level) (ops : LineOps Text) :
    ready (decodeLines E ops).length 0 (alloc.vec.Vec.new (Header Text))
      (alloc.vec.Vec.new (ChartBlock Text Level)) := by
  have h := ops.lines.property
  constructor <;> simpa only [decodeLines_length, alloc.vec.Vec.new, List.length_nil,
    Nat.sub_zero, Nat.zero_add] using h

/-- Discharge all five WritePoints fields from concrete Vec pushes, record
updates, the input Slice, and the listed String primitives. No W.metadata,
W.bodyLine, W.chart, bodyResume, line-plan or whole-scan law is an input. -/
theorem write_points (E : ValueEncoding Text View Level)
    (S : StringOps Text View Level) (R : ReadStringLaws E S)
    (byteLimit : Nat) (W : WriteStringLaws E S byteLimit) (ops : LineOps Text)
    (hbytes : byteBudget (decodeLines E ops) ≤ byteLimit) :
    WritePoints (writeCall S) ops (decodeLines E ops) (encoding E)
      (ready (decodeLines E ops).length)
      (bodyReady E byteLimit (decodeLines E ops)) := by
  constructor
  · intro cursor next fields charts hnext hlen hr
    rcases hr with ⟨hf, hc⟩
    constructor <;> omega
  · intro cursor fields charts header hi hr _hparse
    rcases hr with ⟨hf, hc⟩
    have hcap : fields.val.length < Usize.max := by omega
    obtain ⟨out, hpush, hlength, hdecode⟩ := spec_imp_exists
      (append_field_spec E S ops fields header hcap)
    apply exists_imp_spec
    refine ⟨out, hpush, hdecode, ?_⟩
    constructor <;> omega
  · intro cursor fields charts header hi _hr hparse hchart
    have hvalue := MaidataHeaderReference.value_bytes_le (decodeLines E ops)[cursor.val]
      (decodeHeader E header).1 (decodeHeader E header).2 hparse
    have hremaining := byteBudget_at (decodeLines E ops) cursor.val hi
    have hroom : (E.text header.2).utf8ByteSize +
        byteBudget ((decodeLines E ops).drop (cursor.val + 1)) ≤ byteLimit := by
      dsimp only [decodeHeader] at hvalue
      omega
    have hp : (E.view (S.borrow header.1)).startsWith "&inote_" = true := by
      simpa only [R.borrow, encoding, decodeHeader] using hchart
    obtain ⟨level, hlevel, hdecode⟩ := spec_imp_exists (W.level (S.borrow header.1) hp)
    have hn : E.level level = ((E.text header.1).drop 7).toString.toNat?.getD 0 := by
      simpa only [R.borrow] using hdecode
    apply exists_imp_spec
    refine ⟨{ level_index := level, raw_body := header.2 }, ?_, ?_, hroom⟩
    · simp only [writeCall, hlevel, bind_tc_ok]
    · simp only [encoding, decodeBody, headerBlock, decodeHeader, hn]
  · intro stop line body hline hstop hr
    have hi : line.val < (decodeLines E ops).length := by omega
    obtain ⟨raw, hget, hraw⟩ := line_at E ops line hi
    change (E.text body.raw_body).utf8ByteSize +
      byteBudget ((decodeLines E ops).drop line.val) ≤ byteLimit at hr
    rw [List.drop_eq_getElem_cons hi] at hr
    change (E.text body.raw_body).utf8ByteSize +
      ((decodeLines E ops)[line.val].utf8ByteSize + 1 +
        byteBudget ((decodeLines E ops).drop (line.val + 1))) ≤ byteLimit at hr
    have hnroom : (E.text body.raw_body).utf8ByteSize + 1 ≤ byteLimit := by omega
    obtain ⟨nl, hnewline, hprefix⟩ := spec_imp_exists (W.newline body.raw_body hnroom)
    have hpbytes : (E.text nl).utf8ByteSize =
        (E.text body.raw_body).utf8ByteSize + 1 := by
      simpa only [String.utf8ByteSize_append, newline_bytes] using
        congrArg String.utf8ByteSize hprefix
    have hview : E.view (S.borrow raw) = (decodeLines E ops)[line.val] :=
      (R.borrow raw).trans hraw
    have haroom : (E.text nl).utf8ByteSize +
        (E.view (S.borrow raw)).utf8ByteSize ≤ byteLimit := by
      rw [hpbytes, hview]
      omega
    obtain ⟨value, happend, hvalue⟩ := spec_imp_exists
      (W.append nl (S.borrow raw) haroom)
    have htext : E.text value =
        E.text body.raw_body ++ "\n" ++ (decodeLines E ops)[line.val] := by
      simpa only [hprefix, hview] using hvalue
    have hvbytes : (E.text value).utf8ByteSize =
        (E.text body.raw_body).utf8ByteSize + 1 +
          (decodeLines E ops)[line.val].utf8ByteSize := by
      simpa only [String.utf8ByteSize_append, newline_bytes] using
        congrArg String.utf8ByteSize htext
    apply exists_imp_spec
    refine ⟨{ body with raw_body := value }, ?_, ?_, ?_⟩
    · simp only [writeCall, hnewline, bind_tc_ok, hget, happend]
    · simp only [encoding, decodeBody, appendRawLine, htext]
    · change (E.text value).utf8ByteSize +
        byteBudget ((decodeLines E ops).drop (line.val + 1)) ≤ byteLimit
      rw [hvbytes]
      omega
  · intro cursor stop fields charts body hprogress hstop hr _hbody
    rcases hr with ⟨hf, hc⟩
    have hcap : charts.val.length < Usize.max := by omega
    obtain ⟨out, hpush, hlength, hdecode⟩ := spec_imp_exists
      (append_chart_spec E S ops charts body hcap)
    apply exists_imp_spec
    refine ⟨out, hpush, hdecode, ?_⟩
    constructor <;> omega

/-- Plug the concrete dictionaries into MaidataScan's theorem for the real
Generated collect_lines kernel. Remaining contracts are the listed String points.
This module and its integration have not been locally elaborated. -/
theorem collect_source_equiv (E : ValueEncoding Text View Level)
    (S : StringOps Text View Level) (R : ReadStringLaws E S)
    (byteLimit : Nat) (W : WriteStringLaws E S byteLimit) (ops : LineOps Text)
    (hbytes : byteBudget (decodeLines E ops) ≤ byteLimit)
    (fields : Fields Text) (charts : Charts Text Level)
    (hr : ready (decodeLines E ops).length 0 fields charts) :
    (do let out ← maidata_scan.collect_lines (readCall S) (writeCall S)
          (Slice.len ops.lines) fields charts ops
        ok (decodeOutput (encoding E) out)) =
      ok (parseMaidataLines (decodeLines E ops)
        ((encoding E).fields fields).reverse ((encoding E).charts charts).reverse) := by
  exact MaidataScanProof.collect_lines_source_equiv (readCall S) (writeCall S) ops
    (decodeLines E ops) (encoding E) (ready (decodeLines E ops).length)
    (bodyReady E byteLimit (decodeLines E ops)) (read_points E S R ops)
    (write_points E S R byteLimit W ops hbytes) (Slice.len ops.lines) fields charts
    (by simp only [Slice.len_val, decodeLines_length]) hr

/-- Empty outputs need no independent Vec capacity premise: Slice already bounds
the input length. The split-on-newline allocation facade is still a premise. -/
theorem parse_with_fields_equiv (E : ValueEncoding Text View Level)
    (S : StringOps Text View Level) (R : ReadStringLaws E S)
    (byteLimit : Nat) (W : WriteStringLaws E S byteLimit) (ops : LineOps Text)
    (hbytes : byteBudget (decodeLines E ops) ≤ byteLimit) (content : String)
    (hlines : decodeLines E ops = content.splitOn "\n") :
    (do let out ← maidata_scan.collect_lines (readCall S) (writeCall S)
          (Slice.len ops.lines) (alloc.vec.Vec.new (Header Text))
          (alloc.vec.Vec.new (ChartBlock Text Level)) ops
        ok (Except.ok (decodeOutput (encoding E) out) : Except ParseError MaidataFile)) =
      ok (parseSourceMaidata content) := by
  exact MaidataScanProof.parse_source_equiv (readCall S) (writeCall S) ops
    (decodeLines E ops) (encoding E) (ready (decodeLines E ops).length)
    (bodyReady E byteLimit (decodeLines E ops)) (read_points E S R ops)
    (write_points E S R byteLimit W ops hbytes) (Slice.len ops.lines)
    (alloc.vec.Vec.new (Header Text)) (alloc.vec.Vec.new (ChartBlock Text Level)) content
    (by simp only [Slice.len_val, decodeLines_length]) (ready_empty E ops) hlines rfl rfl

end Verification.MaidataLineFieldsProof
