//! Differential tests: Rust output vs the authoritative Lean implementation.
//!
//! The harness is driven by environment variables so it is a no-op on machines
//! without a Lean build:
//!
//! - `LNMAI_LEAN_PARSER_CLI`: path to the Lean `simai-parser-cli` executable
//!   (`lake build simai-parser-cli` in `lnmai-core`).
//!
//! When set, each chart is parsed by the Lean CLI (newline-delimited JSON over
//! stdin) and by this Rust port, and the two `lowered` payloads must be equal.
//! Without it, the tests print a skip notice and pass.

use std::io::{BufRead, BufReader, Write};
use std::process::{Command, Stdio};

use serde_json::Value;

const CHARTS: &[(&str, u32)] = &[
    ("&first=0\n&inote_1=\n(120)\n1,2,3,\n", 1),
    ("&first=0\n&inote_1=\n(120)\n1h[4:1],\n", 1),
    ("&first=0\n&inote_1=\n(120)\n1-3[4:1],\n", 1),
    ("&first=0\n&inote_1=\n(120)\n1v3[4:1],\n", 1),
    ("&first=0\n&inote_1=\n(120)\n1w5[4:1],\n", 1),
    ("&first=0\n&inote_1=\n(120)\n1/2/3/4,\n", 1),
    ("&first=0\n&inote_1=\n(120)\n(240)1,2,\n", 1),
    ("&first=0\n&inote_1=\n(120)\n1>2,3<7,\n", 1),
    // touch / touch-hold / grouped touches
    ("&first=0\n&inote_1=\n(120)\nA1,A2,A3,\n", 1),
    ("&first=0\n&inote_1=\n(120)\nA1h[4:1],\n", 1),
    ("&first=0\n&inote_1=\n(120)\nA1/A2/A1,\n", 1),
    ("&first=0\n&inote_1=\n(120)\nA1h[4:1]/A2h[4:1],\n", 1),
    // holds and multi-note segments
    ("&first=0\n&inote_1=\n(120)\n1h[2:1],2h[2:1],\n", 1),
    ("&first=0\n&inote_1=\n(120)\n1/2,3/4,\n", 1),
    // slides incl. chains and headless
    ("&first=0\n&inote_1=\n(120)\n1-3[4:1],5-7[4:1],\n", 1),
    ("&first=0\n&inote_1=\n(120)\n1-3[4:1]/5-7[4:1],\n", 1),
    ("&first=0\n&inote_1=\n(120)\n1-3[4:1]!,5-7[4:1],\n", 1),
    ("&first=0\n&inote_1=\n(120)\n1-3[8:1][8:1],\n", 1),
];

/// `(name, chart, frame batches)` runtime scenarios.
fn runtime_scenarios() -> Vec<(&'static str, &'static str, Vec<Value>)> {
    let taps = "&first=0\n&inote_1=\n(120)\n1,2,3,\n";
    let touches = "&first=0\n&inote_1=\n(120)\nA1,A2,A3,\n";
    let touch_hold = "&first=0\n&inote_1=\n(120)\nA1h[4:1],\n";
    let holds = "&first=0\n&inote_1=\n(120)\n1h[2:1],\n";
    let slide = "&first=0\n&inote_1=\n(120)\n1-3[4:1],\n";
    let slide_sensor = "&first=0\n&inote_1=\n(120)\n1-3[4:1],\n";

    let c = |tp: i64, zone: &str| serde_json::json!({"buttonClick": {"tp": tp, "zone": zone}});
    let sc = |tp: i64, area: &str| serde_json::json!({"sensorClick": {"tp": tp, "area": area}});
    let sh = |tp: i64, area: &str, down: bool| serde_json::json!({"sensorHold": {"tp": tp, "area": area, "isDown": down}});
    let bh = |tp: i64, zone: &str, down: bool| serde_json::json!({"buttonHold": {"tp": tp, "zone": zone, "isDown": down}});
    let b = |tp: i64, evs: Vec<Value>| serde_json::json!({ "currentTime": tp, "events": evs });

    vec![
        (
            "taps",
            taps,
            vec![
                b(0, vec![c(0, "K1")]),
                b(250000, vec![]),
                b(500000, vec![c(500000, "K2")]),
                b(750000, vec![]),
                b(1000000, vec![c(1000000, "K3")]),
                b(1600000, vec![]),
            ],
        ),
        (
            "touch",
            touches,
            vec![
                b(0, vec![sc(0, "A1")]),
                b(250000, vec![]),
                b(500000, vec![sc(500000, "A2")]),
                b(750000, vec![]),
                b(1000000, vec![sc(1000000, "A3")]),
                b(1600000, vec![]),
            ],
        ),
        (
            "touch-hold",
            touch_hold,
            vec![
                b(0, vec![sc(0, "A1")]),
                b(100000, vec![sh(100000, "A1", true)]),
                b(300000, vec![sh(300000, "A1", true)]),
                b(600000, vec![sh(600000, "A1", true)]),
                b(900000, vec![sh(900000, "A1", false)]),
                b(1300000, vec![]),
            ],
        ),
        (
            "hold-release",
            holds,
            vec![
                b(0, vec![c(0, "K1")]),
                b(100000, vec![bh(100000, "K1", true)]),
                b(300000, vec![bh(300000, "K1", true)]),
                b(700000, vec![bh(700000, "K1", false)]),
                b(1200000, vec![]),
                b(2000000, vec![]),
            ],
        ),
        (
            "slide-noinput",
            slide,
            vec![
                b(0, vec![]),
                b(500000, vec![]),
                b(1000000, vec![]),
                b(1500000, vec![]),
                b(2500000, vec![]),
            ],
        ),
        (
            "slide-sensor",
            slide_sensor,
            vec![
                b(0, vec![sh(0, "A1", true)]),
                b(200000, vec![sh(200000, "A3", true)]),
                b(400000, vec![]),
                b(900000, vec![sh(900000, "A1", false), sh(900000, "A3", false)]),
                b(1400000, vec![]),
                b(2200000, vec![]),
            ],
        ),
    ]
}

fn lean_cli() -> Option<String> {
    std::env::var("LNMAI_LEAN_PARSER_CLI").ok().filter(|s| !s.is_empty())
}

/// Ask the Lean parser CLI for the lowered chart of `(content, level)`.
fn lean_lowered(cli: &str, content: &str, level: u32) -> Option<Value> {
    let mut child = Command::new(cli)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .ok()?;
    let request = serde_json::json!({
        "mode": "lowered",
        "levelIndex": level,
        "content": content,
    });
    {
        let stdin = child.stdin.as_mut()?;
        writeln!(stdin, "{}", request).ok()?;
        stdin.flush().ok()?;
    }
    let stdout = child.stdout.take()?;
    let mut reader = BufReader::new(stdout);
    let mut line = String::new();
    reader.read_line(&mut line).ok()?;
    let _ = child.wait();
    serde_json::from_str(line.trim()).ok()
}

#[test]
fn lowered_chart_matches_lean() {
    let Some(cli) = lean_cli() else {
        eprintln!("skip: set LNMAI_LEAN_PARSER_CLI to run differential tests");
        return;
    };

    for (content, level) in CHARTS {
        let rust_raw = lnmai_core::ffi::parse_lowered_chart_json(content, *level);
        let rust: Value = serde_json::from_str(&rust_raw).unwrap();
        let Some(lean) = lean_lowered(&cli, content, *level) else {
            eprintln!("skip: Lean CLI did not respond for {:?}", content);
            continue;
        };
        // Both sides return `{"ok":true,"result":...}`.
        if lean.get("ok").and_then(Value::as_bool) == Some(false) {
            // If Lean rejects, the Rust port should also reject.
            assert_eq!(rust["ok"], false, "chart {:?}: Rust accepted, Lean rejected", content);
            continue;
        }
        assert_eq!(rust["ok"], true, "chart {:?}: Rust rejected, Lean accepted", content);
        if rust["result"] != lean["result"] {
            eprintln!("=== chart {:?}", content);
            eprintln!("RUST: {}", serde_json::to_string_pretty(&rust["result"]).unwrap());
            eprintln!("LEAN: {}", serde_json::to_string_pretty(&lean["result"]).unwrap());
        }
        assert_eq!(
            rust["result"], lean["result"],
            "chart {:?}: lowered ChartSpec differs from Lean",
            content
        );
    }
}

// ---------------------------------------------------------------------------
// Runtime stepping differential test
// ---------------------------------------------------------------------------

struct LeanRuntime {
    child: std::process::Child,
    stdin: std::process::ChildStdin,
    reader: BufReader<std::process::ChildStdout>,
}

impl LeanRuntime {
    fn spawn(cli: &str) -> Option<LeanRuntime> {
        let mut child = Command::new(cli)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .ok()?;
        let stdin = child.stdin.take()?;
        let stdout = child.stdout.take()?;
        Some(LeanRuntime { child, stdin, reader: BufReader::new(stdout) })
    }

    fn request(&mut self, req: &Value) -> Value {
        writeln!(self.stdin, "{}", req).unwrap();
        self.stdin.flush().unwrap();
        let mut line = String::new();
        self.reader.read_line(&mut line).unwrap();
        serde_json::from_str(line.trim()).unwrap()
    }

    fn load(&mut self, content: &str, level: u32) -> Value {
        self.request(&serde_json::json!({ "op": "load", "content": content, "levelIndex": level }))
    }

    fn step(&mut self, batch: &Value) -> Value {
        self.request(&serde_json::json!({ "op": "step", "batch": batch }))
    }
}

impl Drop for LeanRuntime {
    fn drop(&mut self) {
        let _ = self.request(&serde_json::json!({ "op": "free" }));
        let _ = self.child.kill();
    }
}

fn lean_runtime_cli() -> Option<String> {
    std::env::var("LNMAI_LEAN_RUNTIME_CLI").ok().filter(|s| !s.is_empty())
}

#[test]
fn runtime_step_matches_lean() {
    let Some(cli) = lean_runtime_cli() else {
        eprintln!("skip: set LNMAI_LEAN_RUNTIME_CLI to run runtime differential tests");
        return;
    };
    let Some(mut lean) = LeanRuntime::spawn(&cli) else {
        eprintln!("skip: could not spawn Lean runtime CLI");
        return;
    };

    for (name, content, batches) in runtime_scenarios() {
        compare_runtime(&mut lean, name, content, &batches);
    }
}

/// Compare a full stepping session (load + N frames) between Rust and Lean.
fn compare_runtime(lean: &mut LeanRuntime, name: &str, content: &str, batches: &[Value]) {
    let loaded = lean.load(content, 1);
    assert_eq!(loaded["ok"], true, "scenario {:?}: Lean load failed: {}", name, loaded);

    let created: Value = serde_json::from_str(&lnmai_core::ffi::create_empty_session_handle()).unwrap();
    let handle = created["result"]["handle"].as_u64().unwrap();
    let rust_loaded: Value =
        serde_json::from_str(&lnmai_core::ffi::load_chart_into_session_from_text(handle, content, 1)).unwrap();
    assert_eq!(rust_loaded["ok"], true, "scenario {:?}: Rust load failed", name);

    for (i, batch) in batches.iter().enumerate() {
        let lean_resp = lean.step(batch);
        let rust_resp: Value =
            serde_json::from_str(&lnmai_core::ffi::step_game_state_handle_light(handle, &batch.to_string())).unwrap();
        if rust_resp["result"] != lean_resp["result"] {
            eprintln!("=== scenario {:?} frame {}", name, i);
            eprintln!("RUST: {}", serde_json::to_string_pretty(&rust_resp["result"]).unwrap());
            eprintln!("LEAN: {}", serde_json::to_string_pretty(&lean_resp["result"]).unwrap());
        }
        assert_eq!(
            rust_resp["result"], lean_resp["result"],
            "scenario {:?} frame {}: runtime step differs from Lean",
            name, i
        );
    }

    let _ = lnmai_core::ffi::free_game_state_handle(handle);
}

/// Tiny deterministic PRNG so the input sequence is identical run to run.
struct Lcg(u64);
impl Lcg {
    fn next_u32(&mut self) -> u32 {
        self.0 = self.0.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
        (self.0 >> 33) as u32
    }
    fn pick<'a>(&mut self, xs: &'a [&'a str]) -> &'a str {
        xs[(self.next_u32() as usize) % xs.len()]
    }
}

fn random_batches(frames: usize, seed: u64) -> Vec<Value> {
    let zones = ["K1", "K2", "K3", "K4", "K5", "K6", "K7", "K8"];
    let sensors = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "B1", "B2", "A1"];
    let mut rng = Lcg(seed);
    let mut batches = Vec::new();
    let delta: i64 = 50000;
    for frame in 0..frames {
        let tp = frame as i64 * delta;
        let mut events: Vec<Value> = Vec::new();
        let roll = rng.next_u32() % 100;
        if roll < 25 {
            events.push(serde_json::json!({"buttonClick": {"tp": tp, "zone": rng.pick(&zones)}}));
        } else if roll < 45 {
            events.push(serde_json::json!({"sensorClick": {"tp": tp, "area": rng.pick(&sensors)}}));
        } else if roll < 65 {
            let down = rng.next_u32() % 2 == 0;
            events.push(serde_json::json!({"sensorHold": {"tp": tp, "area": rng.pick(&sensors), "isDown": down}}));
        } else if roll < 80 {
            let down = rng.next_u32() % 2 == 0;
            events.push(serde_json::json!({"buttonHold": {"tp": tp, "zone": rng.pick(&zones), "isDown": down}}));
        }
        batches.push(serde_json::json!({ "currentTime": tp, "events": events }));
    }
    batches
}

#[test]
fn runtime_random_matches_lean() {
    let Some(cli) = lean_runtime_cli() else {
        eprintln!("skip: set LNMAI_LEAN_RUNTIME_CLI to run random differential tests");
        return;
    };
    let Some(mut lean) = LeanRuntime::spawn(&cli) else {
        eprintln!("skip: could not spawn Lean runtime CLI");
        return;
    };

    // A chart mixing every note family, spread over several seconds.
    let content = "&first=0\n&inote_1=\n(120)\n1,2h[4:1],A1,A2h[4:1],1-3[4:1],5-7[4:1],3,4,5,1,2,3,\n";

    for seed in [1u64, 7, 42] {
        let batches = random_batches(80, seed);
        compare_runtime(&mut lean, &format!("random-{}", seed), content, &batches);
    }
}

