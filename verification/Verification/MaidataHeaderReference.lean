import Verification.PrefixDirectivesReference
import LnmaiCore.Simai.Source.Maidata

/-! The source header's value is a trimmed suffix after the first '='.
This derives its UTF-8 byte bound from the original parser, including Unicode,
missing delimiters, repeated delimiters and CR. No String primitive or complete
header equivalence is assumed. This does not identify the Rust String facade. -/

namespace Verification.MaidataHeaderReference

open LnmaiCore.Simai
open PrefixDirectivesModel PrefixDirectivesReference PrefixAsciiTrim
open private parseKeyValueLine from LnmaiCore.Simai.Source.Maidata

/-- Deleting characters cannot increase the number of UTF-8 bytes. Character
counts alone would not establish this bound for mixed-width Unicode input. -/
theorem ofList_bytes_le_of_sublist {small large : List Char}
    (h : small.Sublist large) :
    (String.ofList small).utf8ByteSize ≤ (String.ofList large).utf8ByteSize := by
  induction h with
  | slnil => exact Nat.le_refl _
  | cons c h ih =>
      simp only [String.ofList_cons, String.utf8ByteSize_append]
      omega
  | cons_cons c h ih =>
      simpa only [String.ofList_cons, String.utf8ByteSize_append] using
        Nat.add_le_add_left ih (String.singleton c).utf8ByteSize

theorem bytes_le_of_sublist (small large : String)
    (h : small.toList.Sublist large.toList) :
    small.utf8ByteSize ≤ large.utf8ByteSize := by
  simpa only [String.ofList_toList] using ofList_bytes_le_of_sublist h

/-- The original split/intercalate implementation keeps every later '='. -/
theorem value_toList (line key value : String)
    (hparse : parseKeyValueLine line = some (key, value)) :
    value.toList = trimChars (afterFirst '=' line.toList) := by
  cases hsplit : line.splitOn "=" with
  | nil =>
      simp only [parseKeyValueLine, hsplit] at hparse
      exact absurd hparse (by simp)
  | cons first rest =>
      have hpair :
          (trim first, trim (String.intercalate "=" rest)) = (key, value) := by
        exact Option.some.inj (by simpa only [parseKeyValueLine, hsplit] using hparse)
      have hvalue := congrArg Prod.snd hpair
      dsimp only at hvalue
      rw [← hvalue, trim_toList]
      have htail : (String.intercalate "=" rest).toList =
          afterFirst '=' line.toList := by
        simpa only [tailText, hsplit, List.tail_cons] using tailText_toList line '='
      rw [htail]

theorem value_sublist (line key value : String)
    (hparse : parseKeyValueLine line = some (key, value)) :
    value.toList.Sublist line.toList := by
  rw [value_toList line key value hparse]
  exact (trimChars_sublist _).trans (afterFirst_sublist '=' line.toList)

/-- Discharges the header byte-capacity fact used by the concrete Vec collector. -/
theorem value_bytes_le (line key value : String)
    (hparse : parseKeyValueLine line = some (key, value)) :
    value.utf8ByteSize ≤ line.utf8ByteSize :=
  bytes_le_of_sublist value line (value_sublist line key value hparse)

end Verification.MaidataHeaderReference
