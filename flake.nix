{
  description = "lnmai-core";

  inputs = {
    self.submodules = true;
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        commonEnv = ''
          export PATH="${pkgs.lib.makeBinPath [ pkgs.coreutils pkgs.elan pkgs.git ]}:$PATH"
        '';
        buildScript = pkgs.writeShellApplication {
          name = "lnmai-core-build";
          runtimeInputs = with pkgs; [
            coreutils
            elan
            git
          ];
          text = ''
            ${commonEnv}
            lake build "$@"
          '';
        };
      in {
        packages.default = buildScript;

        apps.default = {
          type = "app";
          program = "${buildScript}/bin/lnmai-core-build";
        };

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            coreutils
            elan
            git
            nodejs_24
            tree-sitter
            emscripten
          ];

          shellHook = ''
            ${commonEnv}
            echo "Lean repo tooling shell ready."
            echo "Bootstrap local CodeGraph with: bash tools/bootstrap_codegraph.sh"
          '';
        };
      });
}
