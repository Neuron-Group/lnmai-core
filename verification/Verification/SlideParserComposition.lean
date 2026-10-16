import Verification.SlideParser
import Verification.ShapeSolver

open Aeneas Aeneas.Std Result lnmai_core_verify

set_option maxHeartbeats 1000000

/-!
Composition of the extracted shape solver with the slide-parser control flow.
Unlike the frontend-parametric lemmas, these results do not assume that an
opaque solver agrees with `solveSlideShape`: they invoke the extracted solver.

The enum/list/string interpretation, runtime shape/error conversion, and
allocation boundaries still require their concrete representation proofs.
`FrontendLaws` and the just-type frontend remain explicit premises.
-/

namespace Verification.SlideParserComposition
open LnmaiCore LnmaiCore.Simai Verification.Areas Verification.ShapeProof
open Verification.SlideParserProof
open private sanitizeSlideText from LnmaiCore.Simai.SlideParser

def ofBodyKind : SlideBodyKind → shape.SlideBodyKind
  | .line => .Line
  | .circleRight => .CircleRight
  | .circleLeft => .CircleLeft
  | .circleUp => .CircleUp
  | .v => .V
  | .pp => .Pp
  | .qq => .Qq
  | .p => .P
  | .q => .Q
  | .s => .S
  | .z => .Z
  | .turn => .Turn
  | .wifi => .Wifi

@[simp] theorem body_kind_roundtrip (kind : SlideBodyKind) :
    toLnmBodyKind (ofBodyKind kind) = kind := by
  cases kind <;> rfl

/-- The runtime solver call and its shape/error interpretation. This retains
the exact raw diagnostic text and both optional endpoints. -/
def solveBody (body : ParsedSlideBody) : Result (Except ParseError SlideShape) := do
  let result ← shape.solve_shape (ofBodyKind body.kind) (ofLnmOuterSlot body.startLane)
    (body.endArea.map ofLnmSensorArea) (body.turnArea.map ofLnmSensorArea)
  ok (toLnmResult toLnmShape body.rawText result)

theorem solve_body_equiv (body : ParsedSlideBody) :
    solveBody body = ok (solveSlideShape body) := by
  have h := solve_shape_equiv (ofBodyKind body.kind) (ofLnmOuterSlot body.startLane)
    (body.endArea.map ofLnmSensorArea) (body.turnArea.map ofLnmSensorArea) body.rawText
  simpa [solveBody, Option.map_map, Function.comp_def] using h

def shapeFromText (f : Frontend) (text : String) :
    Result (Except ParseError SlideShape) := do
  let parsed ← parseBodyRuntime f text
  match parsed with
  | .error err => ok (.error err)
  | .ok body => solveBody body

theorem shape_from_text_equiv (f : Frontend) (text : String)
    (laws : FrontendLaws f text) :
    shapeFromText f text = ok (detectShapeFromText text) := by
  cases hb : parseSlideBodyFromText text <;>
    simp [shapeFromText, parse_body_equiv f text laws, hb, solve_body_equiv,
      detectShapeFromText, Bind.bind, Except.bind]

def parseNote (f : Frontend) (sanitized : String)
    (start : OuterSlot) (endArea : SensorArea) :
    Result (Except ParseError SlideNoteSemantics) := do
  let parsed ← shapeFromText f sanitized
  ok (do
    let shape ← parsed
    let just ← f.just sanitized
    pure
      { rawText := sanitized
        startSlot := start
        endArea := endArea
        shape := shape
        isJustRight := just })

theorem parse_note_equiv (f : Frontend) (raw sanitized : String)
    (start : OuterSlot) (endArea : SensorArea)
    (hsanitized : sanitized = sanitizeSlideText raw)
    (laws : FrontendLaws f sanitized)
    (hjust : f.just sanitized = detectJustType sanitized) :
    parseNote f sanitized start endArea = ok (parseSlideNote raw start endArea) := by
  simp only [parseNote, shape_from_text_equiv f sanitized laws, bind_tc_ok]
  rw [hjust, hsanitized]
  rfl

theorem parse_shape_text_equiv (f : Frontend) (raw sanitized : String)
    (hsanitized : sanitized = sanitizeSlideText raw)
    (laws : FrontendLaws f sanitized) :
    shapeFromText f sanitized = ok (parseSlideShapeText raw) := by
  rw [shape_from_text_equiv f sanitized laws, hsanitized]
  rfl

def parseNoteFromBody (f : Frontend) (raw : String)
    (body : ParsedSlideBody) (endArea : SensorArea) :
    Result (Except ParseError SlideNoteSemantics) := do
  let parsed ← solveBody body
  ok (do
    let shape ← parsed
    let just ← f.just raw
    pure
      { rawText := raw
        startSlot := body.startLane
        endArea := endArea
        shape := shape
        isJustRight := just })

theorem parse_note_from_body_equiv (f : Frontend) (raw : String)
    (body : ParsedSlideBody) (endArea : SensorArea)
    (hjust : f.just raw = detectJustType raw) :
    parseNoteFromBody f raw body endArea = ok (parseSlideNoteFromBody raw body endArea) := by
  simp only [parseNoteFromBody, solve_body_equiv, bind_tc_ok, hjust]
  rfl

end Verification.SlideParserComposition
