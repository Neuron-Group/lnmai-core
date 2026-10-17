import Verification.Generated
import Verification.PrefixDirectivesRanges

/-!
Proof scripts for the corrected actual prefix_directives extraction supplied in
/tmp/lnmai-prefix-prepend-erase-extraction-3-raw/LnmaiCoreVerify.lean, SHA256
c484a224e074b00976a46021b2daf9225796e037cc39ac7cd21a45ec3b569e97.
Raw backup LnmaiCoreVerify.raw.lean, SHA256
2016ff766c9bfb0647ddaaab0f399f46fb9dd5284e602d40b8535a8c4d8fd28a.

No compiler has been run on this file. Scalar and Vec bounds below are obtained
from Slice.property; this does not model Rust allocator failure or UTF-8 encoding.

The supplied normalized stage3 artifact differs from stage2 at exactly byte
offset 258407: LF (10) is restored to CR (13) in is_prefix_whitespace. All other
bytes are unchanged. These scripts target that exact corrected artifact, not
the stale stage2 Generated. Source-model correspondence is now unrestricted;
the reference model and the real loop-control proofs are unchanged.
-/

open Aeneas Aeneas.Std Result WP lnmai_core_verify

set_option maxHeartbeats 1000000

namespace Verification.PrefixDirectivesExtracted

open PrefixDirectivesModel PrefixDirectivesRanges

/-- One primitive character observation, not an assumed prefix operation. -/
theorem directive_closer_eq (c : Char) :
    prefix_directives.directive_closer c = ok (directiveCloser c) := by
  by_cases hb : c = '{'
  · simp [prefix_directives.directive_closer, directiveCloser, hb]
  · by_cases hp : c = '('
    · simp [prefix_directives.directive_closer, directiveCloser, hb, hp]
    · by_cases ha : c = '<'
      · simp [prefix_directives.directive_closer, directiveCloser, hb, hp, ha]
      · simp [prefix_directives.directive_closer, directiveCloser, hb, hp, ha]

/-- Precisely why scanning stopped: no closer, or the first closer, whose
successor is returned. A closer at the final index is distinguished from absence. -/
def FirstCloserStop (chars : List Char) (begin finish out : Nat)
    (closer : Char) : Prop :=
  (out = finish ∧ closer ∉ rangeChars chars begin finish) ∨
  ∃ hit, begin ≤ hit ∧ hit < finish ∧ out = hit + 1 ∧
    chars[hit]? = some closer ∧ closer ∉ rangeChars chars begin hit

/-- The actual extracted scanner, with bounded point reads and checked
increments. The loop invariant excludes earlier occurrences of the closer. -/
theorem after_first_closer_loop_spec (chars : Slice Char)
    (initialCursor finish : Usize) (closer : Char)
    (hbegin : initialCursor.val ≤ finish.val)
    (hfinish : finish.val ≤ chars.val.length) :
    prefix_directives.after_first_closer_loop chars initialCursor finish closer
      ⦃ out => initialCursor.val ≤ out.val ∧ out.val ≤ finish.val ∧
        rangeChars chars.val out.val finish.val =
          afterFirst closer (rangeChars chars.val initialCursor.val finish.val) ∧
        FirstCloserStop chars.val initialCursor.val finish.val out.val closer ⦄ := by
  unfold prefix_directives.after_first_closer_loop
  apply loop.spec_decr_nat (fun (cursor : Usize) => finish.val - cursor.val)
    (fun cursor => CloserInv chars.val closer initialCursor.val finish.val cursor.val)
  · intro cursor inv
    have hlo := inv.lower
    have hhi := inv.upper
    by_cases hmore : cursor < finish
    · have hlt : cursor.val < finish.val := by simpa using hmore
      have hi : cursor.val < chars.val.length := by omega
      obtain ⟨c, hget, hc⟩ := spec_imp_exists (Slice.index_usize_spec chars cursor hi)
      have hcap := chars.property
      have hinc : cursor.val + (1#usize).val ≤ Usize.max := by
        simpa using (show cursor.val + 1 ≤ Usize.max by omega)
      obtain ⟨next, hnext, hn⟩ := spec_imp_exists
        (UScalar.add_spec (x := cursor) (y := 1#usize) hinc)
      have hnv : next.val = cursor.val + 1 := by simpa using hn
      by_cases hhit : c = closer
      · have hhit' : chars.val[cursor.val]'hi = closer := hc.symm.trans hhit
        have hpost : initialCursor.val ≤ next.val ∧ next.val ≤ finish.val ∧
            rangeChars chars.val next.val finish.val =
              afterFirst closer (rangeChars chars.val initialCursor.val finish.val) ∧
            FirstCloserStop chars.val initialCursor.val finish.val next.val closer := by
          refine ⟨by omega, by omega, ?_, ?_⟩
          · simpa only [hnv] using closer_done_hit inv hlt hhit'
          · refine Or.inr ⟨cursor.val, hlo, hlt, hnv, ?_, inv.nohit⟩
            simp only [List.getElem?_eq_getElem hi, hhit']
        simpa [prefix_directives.after_first_closer_loop.body, hmore, hlt,
          hget, hnext, hhit] using hpost
      · have hmiss : chars.val[cursor.val]'hi ≠ closer := by
          intro heq
          exact hhit (hc.trans heq)
        have hcontinue :
            CloserInv chars.val closer initialCursor.val finish.val next.val ∧
              finish.val - next.val < finish.val - cursor.val := by
          refine ⟨?_, by omega⟩
          simpa only [hnv] using closer_step inv hlt hmiss
        simpa [prefix_directives.after_first_closer_loop.body, hmore, hlt,
          hget, hnext, hhit] using hcontinue
    · have hge : finish.val ≤ cursor.val := by simpa using hmore
      have hnone := closer_done_absent inv (by omega)
      have hpost : initialCursor.val ≤ finish.val ∧ finish.val ≤ finish.val ∧
          rangeChars chars.val finish.val finish.val =
            afterFirst closer (rangeChars chars.val initialCursor.val finish.val) ∧
          FirstCloserStop chars.val initialCursor.val finish.val finish.val closer :=
        ⟨hbegin, Nat.le_refl _, hnone.2, Or.inl ⟨rfl, hnone.1⟩⟩
      simpa [prefix_directives.after_first_closer_loop.body, hmore,
        Nat.not_lt.mpr hge] using hpost
  · exact closer_init chars.val closer hbegin hfinish

theorem after_first_closer_spec (chars : Slice Char)
    (initialCursor finish : Usize) (closer : Char)
    (hbegin : initialCursor.val ≤ finish.val)
    (hfinish : finish.val ≤ chars.val.length) :
    prefix_directives.after_first_closer chars initialCursor finish closer
      ⦃ out => initialCursor.val ≤ out.val ∧ out.val ≤ finish.val ∧
        rangeChars chars.val out.val finish.val =
          afterFirst closer (rangeChars chars.val initialCursor.val finish.val) ∧
        FirstCloserStop chars.val initialCursor.val finish.val out.val closer ⦄ := by
  simpa only [prefix_directives.after_first_closer] using
    after_first_closer_loop_spec chars initialCursor finish closer hbegin hfinish

/-- Actual copying from an empty buffer. Slice.property suffices for every
push: the copied length is cursor - initialCursor, strictly below Usize.max
whenever the actual cursor < finish guard holds. -/
theorem strip_prefix_chars_loop_spec (chars : Slice Char)
    (initialCursor finish : Usize)
    (hbegin : initialCursor.val ≤ finish.val)
    (hfinish : finish.val ≤ chars.val.length) :
    prefix_directives.strip_prefix_chars_loop chars finish
      (alloc.vec.Vec.new Char) initialCursor
      ⦃ out => out.val.length = finish.val - initialCursor.val ∧
        out.val = rangeChars chars.val initialCursor.val finish.val ⦄ := by
  unfold prefix_directives.strip_prefix_chars_loop
  apply loop.spec_decr_nat
    (fun (state : alloc.vec.Vec Char × Usize) => finish.val - state.2.val)
    (fun state => CopyInv chars.val initialCursor.val finish.val state.2.val state.1.val)
  · rintro ⟨out, cursor⟩ inv
    dsimp only at inv ⊢
    have hlo := inv.lower
    have hhi := inv.upper
    by_cases hmore : cursor < finish
    · have hlt : cursor.val < finish.val := by simpa using hmore
      have hi : cursor.val < chars.val.length := by omega
      obtain ⟨c, hget, hc⟩ := spec_imp_exists (Slice.index_usize_spec chars cursor hi)
      have hcap := chars.property
      have hcopied := copy_length inv
      have hpushBound : out.val.length < Usize.max := by omega
      obtain ⟨out', hpush, hp⟩ := spec_imp_exists
        (alloc.vec.Vec.push_spec out c hpushBound)
      have hinc : cursor.val + (1#usize).val ≤ Usize.max := by
        simpa using (show cursor.val + 1 ≤ Usize.max by omega)
      obtain ⟨next, hnext, hn⟩ := spec_imp_exists
        (UScalar.add_spec (x := cursor) (y := 1#usize) hinc)
      have hnv : next.val = cursor.val + 1 := by simpa using hn
      have hcontinue :
          CopyInv chars.val initialCursor.val finish.val next.val out'.val ∧
            finish.val - next.val < finish.val - cursor.val := by
        refine ⟨?_, by omega⟩
        simpa only [hp, hnv, hc] using copy_step inv hlt
      simpa [prefix_directives.strip_prefix_chars_loop.body, hmore, hlt,
        hget, hpush, hnext] using hcontinue
    · have hge : finish.val ≤ cursor.val := by simpa using hmore
      have heq : cursor.val = finish.val := by omega
      have hout := copy_done inv (by omega)
      have hlen : out.val.length = finish.val - initialCursor.val := by
        simpa only [heq] using copy_length inv
      simpa [prefix_directives.strip_prefix_chars_loop.body, hmore,
        Nat.not_lt.mpr hge] using And.intro hlen hout
  · simpa using copy_init chars.val hbegin hfinish

/-- The actual predicate terminates on every character, including CR. -/
theorem is_prefix_whitespace_total (c : Char) :
    prefix_directives.is_prefix_whitespace c ⦃ _ => True ⦄ := by
  by_cases hs : c = ' '
  · simp [prefix_directives.is_prefix_whitespace, hs]
  · by_cases ht : c = '\t'
    · simp [prefix_directives.is_prefix_whitespace, hs, ht]
    · by_cases hcr : c = '\r'
      · simp [prefix_directives.is_prefix_whitespace, hs, ht, hcr]
      · by_cases hn : c = '\n'
        · simp [prefix_directives.is_prefix_whitespace, hs, ht, hcr, hn]
        · simp [prefix_directives.is_prefix_whitespace, hs, ht, hcr, hn]

/-- Concrete point calculations for the restored CR literal. Neither is an
assumption about a complete trimming or prefix operation. -/
theorem is_prefix_whitespace_cr :
    prefix_directives.is_prefix_whitespace '\r' = ok true := by
  simp [prefix_directives.is_prefix_whitespace]

theorem source_whitespace_cr : isPrefixWhitespace '\r' = true := by
  simp [isPrefixWhitespace]

/-- All-input safety for the real forward loop. No character can cause a read
or increment outside the bounded range. -/
theorem trim_range_loop0_bounds_spec (chars : Slice Char)
    (initialCursor finish : Usize)
    (hbegin : initialCursor.val ≤ finish.val)
    (hfinish : finish.val ≤ chars.val.length) :
    prefix_directives.trim_range_loop0 chars initialCursor finish
      ⦃ out => initialCursor.val ≤ out.val ∧ out.val ≤ finish.val ⦄ := by
  unfold prefix_directives.trim_range_loop0
  apply loop.spec_decr_nat (fun (cursor : Usize) => finish.val - cursor.val)
    (fun cursor => initialCursor.val ≤ cursor.val ∧ cursor.val ≤ finish.val)
  · rintro cursor ⟨hlower, hupper⟩
    by_cases hmore : cursor < finish
    · have hlt : cursor.val < finish.val := by simpa using hmore
      have hi : cursor.val < chars.val.length := by omega
      obtain ⟨c, hget, _⟩ := spec_imp_exists (Slice.index_usize_spec chars cursor hi)
      obtain ⟨white, hwhite, _⟩ := spec_imp_exists (is_prefix_whitespace_total c)
      cases white with
      | false =>
          simpa [prefix_directives.trim_range_loop0.body, hmore, hlt,
            hget, hwhite] using And.intro hlower hupper
      | true =>
          have hcap := chars.property
          have hinc : cursor.val + (1#usize).val ≤ Usize.max := by
            simpa using (show cursor.val + 1 ≤ Usize.max by omega)
          obtain ⟨next, hnext, hn⟩ := spec_imp_exists
            (UScalar.add_spec (x := cursor) (y := 1#usize) hinc)
          have hnv : next.val = cursor.val + 1 := by simpa using hn
          have hcontinue :
              (initialCursor.val ≤ next.val ∧ next.val ≤ finish.val) ∧
                finish.val - next.val < finish.val - cursor.val := by
            exact ⟨⟨by omega, by omega⟩, by omega⟩
          simpa [prefix_directives.trim_range_loop0.body, hmore, hlt,
            hget, hwhite, hnext] using hcontinue
    · have hge : finish.val ≤ cursor.val := by simpa using hmore
      simpa [prefix_directives.trim_range_loop0.body, hmore,
        Nat.not_lt.mpr hge] using And.intro hlower hupper
  · exact ⟨Nat.le_refl _, hbegin⟩

/-- All-input safety for the real backward loop. The subtraction is justified
by the actual begin < cursor guard before the indexed read. -/
theorem trim_range_loop1_bounds_spec (chars : Slice Char)
    (begin finish : Usize)
    (hbegin : begin.val ≤ finish.val)
    (hfinish : finish.val ≤ chars.val.length) :
    prefix_directives.trim_range_loop1 chars begin finish
      ⦃ out => begin.val ≤ out.val ∧ out.val ≤ finish.val ⦄ := by
  unfold prefix_directives.trim_range_loop1
  apply loop.spec_decr_nat (fun (cursor : Usize) => cursor.val - begin.val)
    (fun cursor => begin.val ≤ cursor.val ∧ cursor.val ≤ finish.val)
  · rintro cursor ⟨hlower, hupper⟩
    by_cases hmore : begin < cursor
    · have hlt : begin.val < cursor.val := by simpa using hmore
      have hsub : (1#usize).val ≤ cursor.val := by
        simpa using (show 1 ≤ cursor.val by omega)
      obtain ⟨previous, hprevious, hp⟩ := spec_imp_exists
        (UScalar.sub_spec (x := cursor) (y := 1#usize) hsub)
      have hpv : previous.val = cursor.val - 1 := by simpa using hp.1
      have hi : previous.val < chars.val.length := by omega
      obtain ⟨c, hget, _⟩ := spec_imp_exists (Slice.index_usize_spec chars previous hi)
      obtain ⟨white, hwhite, _⟩ := spec_imp_exists (is_prefix_whitespace_total c)
      cases white with
      | false =>
          simpa [prefix_directives.trim_range_loop1.body, hmore, hlt,
            hprevious, hget, hwhite] using And.intro hlower hupper
      | true =>
          have hcontinue :
              (begin.val ≤ previous.val ∧ previous.val ≤ finish.val) ∧
                previous.val - begin.val < cursor.val - begin.val := by
            exact ⟨⟨by omega, by omega⟩, by omega⟩
          simpa [prefix_directives.trim_range_loop1.body, hmore, hlt,
            hprevious, hget, hwhite] using hcontinue
    · have hge : cursor.val ≤ begin.val := by simpa using hmore
      simpa [prefix_directives.trim_range_loop1.body, hmore,
        Nat.not_lt.mpr hge] using And.intro hlower hupper
  · exact ⟨hbegin, Nat.le_refl _⟩

/-- Both actual trim loops return a subrange of their valid input interval,
without any character restriction or source-equivalence premise. -/
theorem trim_range_bounds_spec (chars : Slice Char) (begin finish : Usize)
    (hbegin : begin.val ≤ finish.val)
    (hfinish : finish.val ≤ chars.val.length) :
    prefix_directives.trim_range chars begin finish
      ⦃ out => begin.val ≤ out.«begin».val ∧
        out.«begin».val ≤ out.«end».val ∧ out.«end».val ≤ finish.val ⦄ := by
  apply exists_imp_spec
  obtain ⟨left, hleft, hlower, hupper⟩ := spec_imp_exists
    (trim_range_loop0_bounds_spec chars begin finish hbegin hfinish)
  obtain ⟨right, hright, hrlower, hrupper⟩ := spec_imp_exists
    (trim_range_loop1_bounds_spec chars left finish hupper hfinish)
  refine ⟨⟨left, right⟩, ?_, hlower, hrlower, hrupper⟩
  simp only [prefix_directives.trim_range, hleft, hright, bind_tc_ok]

/-- The real prefix dispatcher supplies the valid interval required by copying.
This handles empty/non-prefix/missing-closer/second-trim paths for ALL inputs. -/
theorem strip_prefix_range_bounds_spec (chars : Slice Char) :
    prefix_directives.strip_prefix_range chars
      ⦃ selected => selected.«begin».val ≤ selected.«end».val ∧
        selected.«end».val ≤ chars.val.length ⦄ := by
  apply exists_imp_spec
  obtain ⟨trimmed, htrim, _, hordered, hend⟩ := spec_imp_exists
    (trim_range_bounds_spec chars 0#usize (Slice.len chars) (by simp) (by simp))
  have hfinish : trimmed.«end».val ≤ chars.val.length := by simpa using hend
  by_cases hempty : trimmed.«begin» = trimmed.«end»
  · refine ⟨trimmed, ?_, hordered, hfinish⟩
    simp [prefix_directives.strip_prefix_range, htrim, hempty]
  · have hne : trimmed.«begin».val ≠ trimmed.«end».val := by
      intro heq
      exact hempty (UScalar.eq_of_val_eq heq)
    have hi : trimmed.«begin».val < chars.val.length := by omega
    obtain ⟨c, hget, _⟩ := spec_imp_exists
      (Slice.index_usize_spec chars trimmed.«begin» hi)
    cases hcloser : directiveCloser c with
    | none =>
        refine ⟨trimmed, ?_, hordered, hfinish⟩
        simp [prefix_directives.strip_prefix_range, htrim, hempty, hget,
          directive_closer_eq, hcloser]
    | some closer =>
        obtain ⟨after, hafter, _, haupper, _, _⟩ := spec_imp_exists
          (after_first_closer_spec chars trimmed.«begin» trimmed.«end» closer
            hordered hfinish)
        obtain ⟨selected, hselected, _, hsordered, hsupper⟩ := spec_imp_exists
          (trim_range_bounds_spec chars after trimmed.«end» haupper hfinish)
        refine ⟨selected, ?_, hsordered, hsupper.trans hfinish⟩
        simp [prefix_directives.strip_prefix_range, htrim, hempty, hget,
          directive_closer_eq, hcloser, hafter, hselected]

/-- Unconditional actual-output characterization. The selected interval is
returned by the REAL range operation, whose equation is a proved postcondition,
not an assumed facade law. Every output scalar is copied in order from it. -/
theorem strip_prefix_chars_range_spec (chars : Slice Char) :
    prefix_directives.strip_prefix_chars chars
      ⦃ out => ∃ selected : prefix_directives.PrefixRange,
        prefix_directives.strip_prefix_range chars = ok selected ∧
        selected.«begin».val ≤ selected.«end».val ∧
        selected.«end».val ≤ chars.val.length ∧
        out.val.length = selected.«end».val - selected.«begin».val ∧
        out.val = rangeChars chars.val selected.«begin».val selected.«end».val ⦄ := by
  apply exists_imp_spec
  obtain ⟨selected, hselected, hbegin, hfinish⟩ := spec_imp_exists
    (strip_prefix_range_bounds_spec chars)
  obtain ⟨out, hout, hlength, hcopy⟩ := spec_imp_exists
    (strip_prefix_chars_loop_spec chars selected.«begin» selected.«end» hbegin hfinish)
  refine ⟨out, ?_, selected, hselected, hbegin, hfinish, hlength, hcopy⟩
  simp only [prefix_directives.strip_prefix_chars, hselected, hout, bind_tc_ok]

/-- Unconditional public safety/order result for the exact supplied artifact.
The source-value specification below adds unrestricted source correspondence. -/
theorem strip_prefix_chars_bounds_spec (chars : Slice Char) :
    prefix_directives.strip_prefix_chars chars
      ⦃ out => out.val.length ≤ chars.val.length ∧ out.val.Sublist chars.val ⦄ := by
  apply exists_imp_spec
  obtain ⟨out, hout, selected, _, hbegin, hfinish, hlength, hcopy⟩ := spec_imp_exists
    (strip_prefix_chars_range_spec chars)
  refine ⟨out, hout, by omega, ?_⟩
  rw [hcopy]
  exact rangeChars_sublist chars.val selected.«begin».val selected.«end».val

/-- Unrestricted single-character correspondence for the corrected artifact.
All four branches are calculated; no complete-operation equality is assumed. -/
theorem is_prefix_whitespace_eq (c : Char) :
    prefix_directives.is_prefix_whitespace c = ok (isPrefixWhitespace c) := by
  by_cases hs : c = ' '
  · simp [prefix_directives.is_prefix_whitespace, isPrefixWhitespace, hs]
  · by_cases ht : c = '\t'
    · simp [prefix_directives.is_prefix_whitespace, isPrefixWhitespace, hs, ht]
    · by_cases hcr : c = '\r'
      · simp [prefix_directives.is_prefix_whitespace, isPrefixWhitespace, hs, ht, hcr]
      · by_cases hn : c = '\n'
        · simp [prefix_directives.is_prefix_whitespace, isPrefixWhitespace, hs, ht, hcr, hn]
        · simp [prefix_directives.is_prefix_whitespace, isPrefixWhitespace, hs, ht, hcr, hn]

/-- The real forward trim loop on all inputs, using the proved point predicate
calculation and no abstract operation agreement premise. -/
theorem trim_range_loop0_spec (chars : Slice Char)
    (initialCursor finish : Usize)
    (hbegin : initialCursor.val ≤ finish.val)
    (hfinish : finish.val ≤ chars.val.length) :
    prefix_directives.trim_range_loop0 chars initialCursor finish
      ⦃ out => initialCursor.val ≤ out.val ∧ out.val ≤ finish.val ∧
        rangeChars chars.val out.val finish.val =
          (rangeChars chars.val initialCursor.val finish.val).dropWhile isPrefixWhitespace ⦄ := by
  unfold prefix_directives.trim_range_loop0
  apply loop.spec_decr_nat (fun (cursor : Usize) => finish.val - cursor.val)
    (fun cursor => FrontInv chars.val initialCursor.val finish.val cursor.val)
  · intro cursor inv
    have hlo := inv.lower
    have hhi := inv.upper
    by_cases hmore : cursor < finish
    · have hlt : cursor.val < finish.val := by simpa using hmore
      have hi : cursor.val < chars.val.length := by omega
      obtain ⟨c, hget, hc⟩ := spec_imp_exists (Slice.index_usize_spec chars cursor hi)
      have hwhite := is_prefix_whitespace_eq c
      cases hw : isPrefixWhitespace c with
      | false =>
          have hw' : isPrefixWhitespace chars.val[cursor.val]'hi = false := by
            simpa only [hc] using hw
          have hpost : initialCursor.val ≤ cursor.val ∧ cursor.val ≤ finish.val ∧
              rangeChars chars.val cursor.val finish.val =
                (rangeChars chars.val initialCursor.val finish.val).dropWhile isPrefixWhitespace :=
            ⟨hlo, hhi, front_done_nonwhitespace inv hlt hw'⟩
          simpa [prefix_directives.trim_range_loop0.body, hmore, hlt,
            hget, hwhite, hw] using hpost
      | true =>
          have hw' : isPrefixWhitespace chars.val[cursor.val]'hi = true := by
            simpa only [hc] using hw
          have hcap := chars.property
          have hinc : cursor.val + (1#usize).val ≤ Usize.max := by
            simpa using (show cursor.val + 1 ≤ Usize.max by omega)
          obtain ⟨next, hnext, hn⟩ := spec_imp_exists
            (UScalar.add_spec (x := cursor) (y := 1#usize) hinc)
          have hnv : next.val = cursor.val + 1 := by simpa using hn
          have hcontinue : FrontInv chars.val initialCursor.val finish.val next.val ∧
              finish.val - next.val < finish.val - cursor.val := by
            refine ⟨?_, by omega⟩
            simpa only [hnv] using front_step inv hlt hw'
          simpa [prefix_directives.trim_range_loop0.body, hmore, hlt,
            hget, hwhite, hw, hnext] using hcontinue
    · have hge : finish.val ≤ cursor.val := by simpa using hmore
      have hpost : initialCursor.val ≤ cursor.val ∧ cursor.val ≤ finish.val ∧
          rangeChars chars.val cursor.val finish.val =
            (rangeChars chars.val initialCursor.val finish.val).dropWhile isPrefixWhitespace :=
        ⟨hlo, hhi, front_done_empty inv (by omega)⟩
      simpa [prefix_directives.trim_range_loop0.body, hmore, Nat.not_lt.mpr hge] using hpost
  · exact front_init chars.val hbegin hfinish

/-- The real backward trim loop, including its guarded subtraction BEFORE
the indexed read. The termination rank decreases on each whitespace character. -/
theorem trim_range_loop1_spec (chars : Slice Char)
    (begin finish : Usize)
    (hbegin : begin.val ≤ finish.val)
    (hfinish : finish.val ≤ chars.val.length) :
    prefix_directives.trim_range_loop1 chars begin finish
      ⦃ out => begin.val ≤ out.val ∧ out.val ≤ finish.val ∧
        rangeChars chars.val begin.val out.val =
          trimEndChars (rangeChars chars.val begin.val finish.val) ⦄ := by
  unfold prefix_directives.trim_range_loop1
  apply loop.spec_decr_nat (fun (cursor : Usize) => cursor.val - begin.val)
    (fun cursor => BackInv chars.val begin.val finish.val cursor.val)
  · intro cursor inv
    have hlo := inv.lower
    have hhi := inv.upper
    by_cases hmore : begin < cursor
    · have hlt : begin.val < cursor.val := by simpa using hmore
      have hsub : (1#usize).val ≤ cursor.val := by
        simpa using (show 1 ≤ cursor.val by omega)
      obtain ⟨previous, hprevious, hp⟩ := spec_imp_exists
        (UScalar.sub_spec (x := cursor) (y := 1#usize) hsub)
      have hpv : previous.val = cursor.val - 1 := by simpa using hp.1
      have hi : previous.val < chars.val.length := by omega
      have hi' : cursor.val - 1 < chars.val.length := by omega
      obtain ⟨c, hget, hc⟩ := spec_imp_exists (Slice.index_usize_spec chars previous hi)
      have hwhite := is_prefix_whitespace_eq c
      cases hw : isPrefixWhitespace c with
      | false =>
          have hw' : isPrefixWhitespace chars.val[cursor.val - 1]'hi' = false := by
            simpa only [hc, hpv] using hw
          have hpost : begin.val ≤ cursor.val ∧ cursor.val ≤ finish.val ∧
              rangeChars chars.val begin.val cursor.val =
                trimEndChars (rangeChars chars.val begin.val finish.val) :=
            ⟨hlo, hhi, back_done_nonwhitespace inv hlt hw'⟩
          simpa [prefix_directives.trim_range_loop1.body, hmore, hlt,
            hprevious, hget, hwhite, hw] using hpost
      | true =>
          have hw' : isPrefixWhitespace chars.val[cursor.val - 1]'hi' = true := by
            simpa only [hc, hpv] using hw
          have hcontinue : BackInv chars.val begin.val finish.val previous.val ∧
              previous.val - begin.val < cursor.val - begin.val := by
            refine ⟨?_, by omega⟩
            simpa only [hpv] using back_step inv hlt hw'
          simpa [prefix_directives.trim_range_loop1.body, hmore, hlt,
            hprevious, hget, hwhite, hw] using hcontinue
    · have hge : cursor.val ≤ begin.val := by simpa using hmore
      have hpost : begin.val ≤ cursor.val ∧ cursor.val ≤ finish.val ∧
          rangeChars chars.val begin.val cursor.val =
            trimEndChars (rangeChars chars.val begin.val finish.val) :=
        ⟨hlo, hhi, back_done_empty inv (by omega)⟩
      simpa [prefix_directives.trim_range_loop1.body, hmore, Nat.not_lt.mpr hge] using hpost
  · exact back_init chars.val hbegin hfinish

/-- Composition of the TWO actual trim loops, in their generated order. -/
theorem trim_range_spec (chars : Slice Char)
    (begin finish : Usize)
    (hbegin : begin.val ≤ finish.val)
    (hfinish : finish.val ≤ chars.val.length) :
    prefix_directives.trim_range chars begin finish
      ⦃ out => begin.val ≤ out.«begin».val ∧
        out.«begin».val ≤ out.«end».val ∧ out.«end».val ≤ finish.val ∧
        rangeChars chars.val out.«begin».val out.«end».val =
          trimChars (rangeChars chars.val begin.val finish.val) ⦄ := by
  apply exists_imp_spec
  obtain ⟨left, hleft, hlower, hupper, hfront⟩ := spec_imp_exists
    (trim_range_loop0_spec chars begin finish hbegin hfinish)
  obtain ⟨right, hright, hrlower, hrupper, hback⟩ := spec_imp_exists
    (trim_range_loop1_spec chars left finish hupper hfinish)
  refine ⟨⟨left, right⟩, ?_, hlower, hrlower, hrupper, ?_⟩
  · simp only [prefix_directives.trim_range, hleft, hright, bind_tc_ok]
  · dsimp only
    rw [hback, hfront]
    rfl

/-- Every actual prefix dispatch branch, including empty, non-prefix, absent
closer, and second-trim cases. It consumes at most ONE directive. -/
theorem strip_prefix_range_spec (chars : Slice Char) :
    prefix_directives.strip_prefix_range chars
      ⦃ selected => selected.«begin».val ≤ selected.«end».val ∧
        selected.«end».val ≤ chars.val.length ∧
        rangeChars chars.val selected.«begin».val selected.«end».val =
          stripChars chars.val ⦄ := by
  apply exists_imp_spec
  obtain ⟨trimmed, htrim, _, hordered, hend, htrimmed⟩ := spec_imp_exists
    (trim_range_spec chars 0#usize (Slice.len chars)
      (by simp) (by simp))
  have hfinish : trimmed.«end».val ≤ chars.val.length := by simpa using hend
  have htrimmed' : rangeChars chars.val trimmed.«begin».val trimmed.«end».val =
      trimChars chars.val := by simpa using htrimmed
  by_cases hempty : trimmed.«begin» = trimmed.«end»
  · have hnil : rangeChars chars.val trimmed.«begin».val trimmed.«end».val = [] := by
      simp only [hempty, rangeChars_self]
    have htrimnil : trimChars chars.val = [] := htrimmed'.symm.trans hnil
    refine ⟨trimmed, ?_, hordered, hfinish, ?_⟩
    · simp [prefix_directives.strip_prefix_range, htrim, hempty]
    · simp only [hnil, stripChars, htrimnil, stripTrimmed]
  · have hne : trimmed.«begin».val ≠ trimmed.«end».val := by
      intro heq
      exact hempty (UScalar.eq_of_val_eq heq)
    have hmore : trimmed.«begin».val < trimmed.«end».val := by omega
    have hi : trimmed.«begin».val < chars.val.length := by omega
    obtain ⟨c, hget, hc⟩ := spec_imp_exists
      (Slice.index_usize_spec chars trimmed.«begin» hi)
    cases hcloser : directiveCloser c with
    | none =>
        have hnone : directiveCloser chars.val[trimmed.«begin».val]'hi = none := by
          simpa only [hc] using hcloser
        have hsource := stripTrimmed_range_none chars.val hmore hfinish hnone
        refine ⟨trimmed, ?_, hordered, hfinish, ?_⟩
        · simp [prefix_directives.strip_prefix_range, htrim, hempty, hget,
            directive_closer_eq, hcloser]
        · change rangeChars chars.val trimmed.«begin».val trimmed.«end».val =
            stripTrimmed (trimChars chars.val)
          rw [← htrimmed']
          exact hsource.symm
    | some closer =>
        have hsome : directiveCloser chars.val[trimmed.«begin».val]'hi = some closer := by
          simpa only [hc] using hcloser
        obtain ⟨after, hafter, halower, haupper, hresidual, _⟩ := spec_imp_exists
          (after_first_closer_spec chars trimmed.«begin» trimmed.«end» closer
            hordered hfinish)
        obtain ⟨selected, hselected, _, hsordered, hsupper, hsval⟩ := spec_imp_exists
          (trim_range_spec chars after trimmed.«end» haupper hfinish)
        refine ⟨selected, ?_, hsordered, hsupper.trans hfinish, ?_⟩
        · simp [prefix_directives.strip_prefix_range, htrim, hempty, hget,
            directive_closer_eq, hcloser, hafter, hselected]
        · calc
            rangeChars chars.val selected.«begin».val selected.«end».val =
                trimChars (rangeChars chars.val after.val trimmed.«end».val) := hsval
            _ = trimChars (afterFirst closer
                (rangeChars chars.val trimmed.«begin».val trimmed.«end».val)) := by
              rw [hresidual]
            _ = stripTrimmed (rangeChars chars.val trimmed.«begin».val trimmed.«end».val) :=
              (stripTrimmed_range_some chars.val closer hmore hfinish hsome).symm
            _ = stripChars chars.val := by rw [htrimmed']; rfl

/-- Unrestricted source-model specification for the exact corrected artifact.
No input exclusion or hsize is needed: machine bounds derive from chars.property. -/
theorem strip_prefix_chars_spec (chars : Slice Char) :
    prefix_directives.strip_prefix_chars chars
      ⦃ out => out.val.length ≤ chars.val.length ∧
        out.val = PrefixDirectivesModel.stripChars chars.val ⦄ := by
  apply exists_imp_spec
  obtain ⟨selected, hselected, hbegin, hfinish, hsource⟩ := spec_imp_exists
    (strip_prefix_range_spec chars)
  obtain ⟨out, hout, hlength, hcopy⟩ := spec_imp_exists
    (strip_prefix_chars_loop_spec chars selected.«begin» selected.«end» hbegin hfinish)
  refine ⟨out, ?_, ?_, hcopy.trans hsource⟩
  · simp only [prefix_directives.strip_prefix_chars, hselected, hout, bind_tc_ok]
  · omega

theorem strip_prefix_chars_equiv (chars : Slice Char) :
    (do let out ← prefix_directives.strip_prefix_chars chars
        ok out.val) = ok (stripChars chars.val) := by
  obtain ⟨out, hout, _, hval⟩ := spec_imp_exists
    (strip_prefix_chars_spec chars)
  simp only [hout, bind_tc_ok, hval]

end Verification.PrefixDirectivesExtracted
