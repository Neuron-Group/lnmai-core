import Verification.PrefixDirectivesModel
import Verification.PrefixAsciiTrim
import Verification.StringSplit
import LnmaiCore.Simai.Tokenize

/-! Direct source connection to stripPrefixDirectives and sanitizeSlideToken.
No whole-prefix, whole-sanitizer, whole-parser, or trim agreement premise.
This file is a source draft; actual Rust loop extraction is a later task. -/

namespace Verification.PrefixDirectivesReference

open Verification.PrefixDirectivesModel
open Verification.PrefixAsciiTrim

def tailText (s : String) (closer : Char) : String :=
  String.intercalate (String.ofList [closer])
    (s.splitOn (String.ofList [closer])).tail

theorem tailText_toList (s : String) (closer : Char) :
    (tailText s closer).toList = afterFirst closer s.toList := by
  rw [afterFirst_splitTail]
  unfold tailText
  rw [StringSplit.splitOn_singleton]
  rw [String.toList_intercalate]
  have hmap (xs : List (List Char)) :
      ((xs.map String.ofList).tail.map String.toList) = xs.tail := by
    cases xs <;> simp [List.map_map, Function.comp_def]
  simp only [hmap, String.toList_ofList, List.splitOn_eq_splitOnP]

private theorem split_singleton_nonempty (s : String) (closer : Char) :
    s.splitOn (String.ofList [closer]) ≠ [] := by
  rw [StringSplit.splitOn_singleton]
  intro hnil
  exact List.splitOnP_ne_nil (fun c => c == closer) s.toList
    (List.map_eq_nil_iff.mp hnil)

/-- The fallback branch is retained, then eliminated by the real splitOn theorem. -/
def sourceAfter (s : String) (closer : Char) : String :=
  match s.splitOn (String.ofList [closer]) with
  | _ :: rest => LnmaiCore.Simai.trim (String.intercalate (String.ofList [closer]) rest)
  | [] => s

theorem sourceAfter_toList (s : String) (closer : Char) :
    (sourceAfter s closer).toList = trimChars (afterFirst closer s.toList) := by
  have hne := split_singleton_nonempty s closer
  unfold sourceAfter
  cases hs : s.splitOn (String.ofList [closer]) with
  | nil => exact False.elim (hne hs)
  | cons first rest =>
      dsimp only
      rw [trim_toList]
      have ht : (String.intercalate (String.ofList [closer]) rest).toList =
          afterFirst closer s.toList := by
        simpa only [tailText, hs, List.tail_cons] using tailText_toList s closer
      rw [ht]

/-- Exact original body after its initial trim. -/
def sourceBody (t : String) : String :=
  if t = "" then t
  else if t.startsWith "{" then sourceAfter t '}'
  else if t.startsWith "(" then sourceAfter t ')'
  else if t.startsWith "<" then sourceAfter t '>'
  else t

theorem original_factor (token : String) :
    LnmaiCore.Simai.stripPrefixDirectives token =
      sourceBody (LnmaiCore.Simai.trim token) := by
  have hbrace : String.ofList ['}'] = "}" := String.ofList_toList (s := "}")
  have hparen : String.ofList [')'] = ")" := String.ofList_toList (s := ")")
  have hangle : String.ofList ['>'] = ">" := String.ofList_toList (s := ">")
  unfold LnmaiCore.Simai.stripPrefixDirectives
  dsimp only
  generalize LnmaiCore.Simai.trim token = t
  simp only [sourceBody, sourceAfter, hbrace, hparen, hangle]
  -- Reduce both catch-all and explicit-nil matchers on constructors.
  cases hbraceSplit : t.splitOn "}" <;>
    cases hparenSplit : t.splitOn ")" <;>
    cases hangleSplit : t.splitOn ">" <;> rfl

private theorem startsWith_singleton_cons (t : String) (c d : Char) (rest : List Char)
    (hchars : t.toList = c :: rest) :
    t.startsWith (String.ofList [d]) = (c == d) := by
  apply Bool.eq_iff_iff.mpr
  simp only [String.startsWith_string_iff, String.toList_ofList,
    hchars, List.singleton_prefix_cons_iff, beq_iff_eq]
  exact eq_comm

theorem sourceBody_toList (t : String) :
    (sourceBody t).toList = stripTrimmed t.toList := by
  cases hchars : t.toList with
  | nil =>
      have ht : t = "" := by
        apply String.toList_inj.mp
        simpa using hchars
      simp [ht, sourceBody, stripTrimmed]
  | cons c rest =>
      have ht : t ≠ "" := by
        intro he
        subst t
        simp at hchars
      have hb : t.startsWith "{" = (c == '{') :=
        startsWith_singleton_cons t c '{' rest hchars
      have hp : t.startsWith "(" = (c == '(') :=
        startsWith_singleton_cons t c '(' rest hchars
      have ha : t.startsWith "<" = (c == '<') :=
        startsWith_singleton_cons t c '<' rest hchars
      by_cases hbrace : c = '{'
      · simp [sourceBody, ht, hb, hp, ha, sourceAfter_toList,
          hchars, stripTrimmed, directiveCloser, hbrace]
      · by_cases hparen : c = '('
        · simp [sourceBody, ht, hb, hp, ha, sourceAfter_toList,
            hchars, stripTrimmed, directiveCloser, hbrace, hparen]
        · by_cases hangle : c = '<'
          · simp [sourceBody, ht, hb, hp, ha, sourceAfter_toList,
              hchars, stripTrimmed, directiveCloser, hbrace, hparen, hangle]
          · simp [sourceBody, ht, hb, hp, ha, sourceAfter_toList,
              hchars, stripTrimmed, directiveCloser, hbrace, hparen, hangle]

theorem stripPrefixDirectives_toList (token : String) :
    (LnmaiCore.Simai.stripPrefixDirectives token).toList = stripChars token.toList := by
  simp only [original_factor, sourceBody_toList, trim_toList, stripChars]

theorem stripChars_original (token : String) :
    String.ofList (stripChars token.toList) = LnmaiCore.Simai.stripPrefixDirectives token := by
  apply String.toList_inj.mp
  simp only [String.toList_ofList, stripPrefixDirectives_toList]

/-- The accepted sanitizer is downstream of both trims and the one prefix pass. -/
theorem sanitizeSlideToken_toList (token : String) :
    (LnmaiCore.Simai.sanitizeSlideToken token).toList =
      (stripChars token.toList).filter (fun c =>
        c ≠ 'b' && c ≠ 'x' && c ≠ 'f' && c ≠ '!' && c ≠ '?' && c ≠ '$') := by
  simp only [LnmaiCore.Simai.sanitizeSlideToken, String.toList_ofList,
    stripPrefixDirectives_toList]

end Verification.PrefixDirectivesReference
