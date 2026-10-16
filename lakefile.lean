import Lake
open Lake DSL System

package «lnmai-core» where
  version := v!"0.1.0"

require mathlib from git "https://github.com/leanprover-community/mathlib4" @ "v4.30.0"

-- Use the system C compiler (and SDK), with the Lean toolchain's headers.
-- Passing the C source as a linker argument loses these flags on macOS.
target lnmaiNativeO pkg : FilePath := do
  let src ← inputTextFile (pkg.dir / "native" / "lnmai_native.c")
  let header ← inputTextFile (pkg.dir / "native" / "lnmai_native.h")
  let src ← src.bindM fun path => header.mapM fun _ => pure path
  buildO (pkg.buildDir / "native" / "lnmai_native.o") src
    #["-I", (← getLeanIncludeDir).toString] #["-O2", "-fPIC"] "cc" getLeanTrace

extern_lib lnmaiNative pkg := do
  let obj ← lnmaiNativeO.fetch
  buildStaticLib (pkg.staticLibDir / nameToStaticLib "lnmaiNative") #[obj]

target lnmaiNativeTestO pkg : FilePath := do
  let src ← inputTextFile (pkg.dir / "native" / "lnmai_native_test.c")
  let header ← inputTextFile (pkg.dir / "native" / "lnmai_native.h")
  let src ← src.bindM fun path => header.mapM fun _ => pure path
  buildO (pkg.buildDir / "native" / "lnmai_native_test.o") src
    #["-I", (← getLeanIncludeDir).toString] #["-O2", "-fPIC"] "cc" getLeanTrace

lean_lib LnmaiCore where
  -- Real-chart regressions evaluate the runtime while elaborating proofs.
  -- Load its native modules through Lake, including their dependencies.
  precompileModules := true
lean_lib Proofs

@[default_target]
lean_exe «lnmai-core» where
  root := `Main

lean_exe «real-chart-verification» where
  root := `Apps.RealChartVerification

lean_exe «real-chart-benchmark» where
  root := `Apps.RealChartBenchmark

lean_exe «real-chart-verification-100524» where
  root := `Apps.RealChartVerification100524

lean_exe «simai-parser-cli» where
  root := `Apps.SimaiParserCli

lean_exe «runtime-step-cli» where
  root := `Apps.RuntimeStepCli
  moreLinkObjs := #[lnmaiNativeTestO]

lean_exe «probe-koishi-touch54» where
  root := `Apps.ProbeKoishiTouch54

lean_exe «dump-koishi-non-perfects» where
  root := `Apps.DumpKoishiNonPerfects

lean_exe «probe-koishi-touchhold-cluster» where
  root := `Apps.ProbeKoishiTouchHoldCluster

