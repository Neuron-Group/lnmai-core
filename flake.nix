{
  description = "lnmai-core";

  inputs = {
    nixpkgs.follows = "lean4-nix/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    lean4-nix.url = "github:lenianiva/lean4-nix";
  };

  outputs = { self, nixpkgs, flake-utils, lean4-nix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        leanManifestBase = import "${lean4-nix}/manifests/v4.30.0.nix";
        # lean4-nix does not ship a v4.30.0-rc2 manifest, so we reuse the
        # nearby v4.30.0 bootstrap/build logic and pin the rc2 binary toolchain.
        leanManifest = leanManifestBase // {
          tag = "v4.30.0-rc2";
          rev = "3dc1a088b6d2d8eafe25a7cd7ec7b58d731bd7cc";
          toolchain = {
            x86_64-linux = {
              url = "https://github.com/leanprover/lean4/releases/download/v4.30.0-rc2/lean-4.30.0-rc2-linux.tar.zst";
              hash = "sha256-W1FiXxVPChOze9iS8dlfeen9W58NCVtBJiFe4ryNvoY=";
            };
          };
        };
        leanOverlay = final: prev: {
          lean = (final.callPackage "${lean4-nix}/lib/toolchain.nix" {}).fetchBinaryLean leanManifest;
        };

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ leanOverlay ];
        };

        lake2nix = pkgs.callPackage lean4-nix.lake {};
        lakeDeps = lake2nix.buildDeps {
          src = ./.;
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
          src = ./.;
        };

        lnmaiCoreLib = lake2nix.mkPackage (commonArgs // {
          name = "LnmaiCore";
          buildLibrary = true;
        });

        ffiArtifacts = lake2nix.mkPackage (commonArgs // {
          name = "lnmai-core";
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
        packages.default = ffiArtifacts;
        packages.ffi-artifacts = ffiArtifacts;
        packages.lnmai-core-lib = lnmaiCoreLib;

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
      });
}
