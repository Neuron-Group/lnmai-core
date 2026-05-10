# LnMai Core Handoff

- Lean core is the gameplay authority; Rust should stay on IO, rendering, and audio via FFI.
- Today’s work focused on slide parity with `../reference/MajdataPlay`.
- Added explicit slide topology data (`SlideKind`, parent links, track count, judge-at timing) so the core can distinguish single, wifi, and connected-part slides.
- Split host-facing output into `AudioCommand` and `RenderCommand`, while keeping judgment and queue state inside Lean.
- Added timestamped input batching so the core can step by time windows instead of raw frame-only input.
- Tightened slide judgment to match reference behavior for checkability, parent finish / pending-finish gating, queue traversal, too-late cutoff, judged-delay completion, and connected-slide force-finish propagation.
- Matched slide bar hiding more closely by carrying per-area `arrowProgressWhenOn` / `arrowProgressWhenFinished` metadata and emitting hide commands with the right end index semantics.
- Kept full-hide behavior separate from partial bar hiding so final slide cleanup still matches the reference `HideAllBar` path.
- `lake build` currently passes after the slide work.
- Next likely work: apply the same parity-first pass to tap / hold / touch / touch-hold behavior before adding any new abstractions.
