{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }:
  flake-utils.lib.eachDefaultSystem (
    system:
    let
       pkgs = (import nixpkgs { inherit system; });
       nativeBuildInputs = with pkgs; [ pkg-config ];
       buildInputs       = with pkgs; [ fasm ];
       devInputs         = with pkgs; [ tree watchexec hexyl gf ];
    in
    {
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = nativeBuildInputs ++ devInputs;
        buildInputs = buildInputs ++ devInputs;

        shellHook = ''
          export WORKSPACE_DIR=$(pwd)
        '';
      };
    }
  );
}
