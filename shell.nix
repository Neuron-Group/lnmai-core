{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = with pkgs; [
    nodejs_24
    tree-sitter
    emscripten
    git
  ];

  shellHook = ''
    echo "Lean repo tooling shell ready."
    echo "Bootstrap local CodeGraph with: bash tools/bootstrap_codegraph.sh"
  '';
}
