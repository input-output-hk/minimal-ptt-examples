{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    nixpkgs-22_11.url = "github:NixOS/nixpkgs/22.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    ## See https://input-output-hk.github.io/cardano-haskell-packages/
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

outputs = { self, flake-utils, ... }@inputs:
    (flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        topLevel = import escrow/escrow.nix {
          inherit system;
          sources = inputs;
        };
      in
      {
        #packages = topLevel.bitte-packages;
        legacyPackages = topLevel;
        iog.dapp = topLevel.escrow;
      })) // {
      iog.dapp = self.legacyPackages.x86_64-linux.escrow;
    };




/*
  outputs = inputs@{ flake-parts, CHaP, haskellNix, ... }:

    flake-parts.lib.mkFlake { inherit inputs; } {

      flake.internal-lib = import ./internal-lib.nix;

      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      imports = [
        escrow/escrow-common.nix
        escrow/escrow.nix
      ];

      perSystem = { pkgs, ... }: { formatter = pkgs.nixfmt; };

    };
*/
}
