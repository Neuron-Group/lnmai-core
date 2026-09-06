{
  description = "lnmai-core";

  nixConfig = {
    extra-substituters = [ "https://lnmai-core.cachix.org" ];
    extra-trusted-public-keys = [ "lnmai-core.cachix.org-1:rYcjvGbYnD1X9NWUExTn2dly2tFFzuamDEj02rJG7F8=" ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    lean4-nix = {
      url = "github:lenianiva/lean4-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, lean4-nix }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" ];
      mkSystem = system: let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (lean4-nix.readToolchainFile ./lean-toolchain) ];
        };
        lib = pkgs.lib;
        coreFiles = lib.fileset.unions [
          ./LnmaiCore
          ./LnmaiCore.lean
          ./lakefile.toml
          ./lake-manifest.json
          ./lean-toolchain
        ];
        coreSource = lib.fileset.toSource {
          root = ./.;
          fileset = coreFiles;
        };
        ffiSource = lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions [
            coreFiles
            ./Main.lean
            ./include
          ];
        };

        lake2nix = pkgs.callPackage lean4-nix.lake {};
        lakeDeps = lake2nix.buildDeps {
          # buildDeps reads only the manifest, so avoid coupling expensive
          # dependency derivations to every project source change.
          src = ./lake-manifest.json;
          manifestFile = ./lake-manifest.json;
          depOverride = {
            proofwidgets = {
              nativeBuildInputs = [ pkgs.nodejs_24 ];
              buildPhase = ''
                runHook preBuild
                lake build proofwidgets
                lake build ProofWidgets:shared
                lake build ProofWidgets:static
                runHook postBuild
              '';
              installPhase = ''
                runHook preInstall
                mkdir -p "$out"
                rsync -a --exclude=".lake" ./ "$out/"
                cp -rP .lake "$out"
                runHook postInstall
              '';
            };
          };
        };
        commonArgs = {
          inherit lakeDeps;
        };
        lakeDepsBundle = pkgs.symlinkJoin {
          name = "lnmai-core-lake-deps";
          paths = builtins.attrValues lakeDeps;
        };

        lnmaiCoreLib = lake2nix.mkPackage (commonArgs // {
          name = "LnmaiCore";
          src = coreSource;
          buildLibrary = true;
          installArtifacts = false;
          postInstall = ''
            mkdir -p "$out/.lake"
            cp -rP .lake/build "$out/.lake/"
          '';
        });

        ffiArtifacts = lake2nix.mkPackage (commonArgs // {
          name = "lnmai-core";
          src = ffiSource;
          lakeArtifacts = lnmaiCoreLib;
          buildPhase = ''
            runHook preBuild
            lake build LnmaiCore.FFI:c.o
            lake build lnmai-core
            runHook postBuild
          '';
          postInstall = ''
            if [ -d .lake/build/bin ]; then
              cp -R .lake/build/bin "$out"
            fi
            mkdir -p "$out/include"
            cp -R include/. "$out/include/"

            for rsp in "$out"/bin/*.rsp "$out"/.lake/build/bin/*.rsp; do
              if [ -f "$rsp" ]; then
                substituteInPlace "$rsp" --replace-fail "$PWD" "$out"
              fi
            done

            raw_rsp="$out/bin/lnmai-core.rsp"
            if [ ! -f "$raw_rsp" ] && [ -f "$out/.lake/build/bin/lnmai-core.rsp" ]; then
              raw_rsp="$out/.lake/build/bin/lnmai-core.rsp"
            fi
            if [ -f "$raw_rsp" ]; then
              mkdir -p "$out/share/lnmai-core"
              awk '
                BEGIN { have_prev = 0; skip_next = 0 }
                $0 == "\"--sysroot\"" { skip_next = 1; next }
                skip_next { skip_next = 0; next }
                $0 == "\"-lc_nonshared\"" || $0 == "\"-l:ld.so\"" || $0 == "\"-lpthread_nonshared\"" { next }
                have_prev && prev == "\"-L\"" && $0 ~ /\/lib\/glibc"$/ { have_prev = 0; prev = ""; next }
                have_prev { print prev }
                { prev = $0; have_prev = 1 }
                END { if (have_prev) print prev }
              ' "$raw_rsp" > "$out/share/lnmai-core/ffi-link.rsp"
            fi
          '';
        });

        buildScript = pkgs.writeShellApplication {
          name = "lnmai-core-build";
          runtimeInputs = with pkgs; [
            coreutils
            git
            lean.lean-all
          ];
          text = ''
            if [ "$#" -eq 0 ]; then
              set -- lnmai-core
            fi
            exec lake build "$@"
          '';
        };
      in {
        packages = {
          default = ffiArtifacts;
          ffi-artifacts = ffiArtifacts;
          lnmai-core-lib = lnmaiCoreLib;
          mathlib = lakeDeps.mathlib;
          lake-deps = lakeDepsBundle;
        };

        apps.default = {
          type = "app";
          program = "${buildScript}/bin/lnmai-core-build";
        };

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            emscripten
            git
            lean.lean-all
            nodejs_24
            tree-sitter
          ];

          shellHook = ''
            echo "Lean repo tooling shell ready."
            echo "Nix build target: nix build .#ffi-artifacts"
            echo "Bootstrap local CodeGraph with: bash tools/bootstrap_codegraph.sh"
          '';
        };
      };
    in {
      packages = forAllSystems (system: (mkSystem system).packages);
      apps = forAllSystems (system: (mkSystem system).apps);
      devShells = forAllSystems (system: (mkSystem system).devShells);
    };
}
