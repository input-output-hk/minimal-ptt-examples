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

  outputs = { self, flake-utils, CHaP, haskellNix, nixpkgs, ...}@inputs:
    (flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = import nixpkgs { inherit system; };
        internal-lib = import ./internal-lib.nix;
        escrow = internal-lib.make-haskell-nix-pkg {
          inherit (inputs) haskellNix CHaP;
          inherit pkgs;
          src = "${self}/escrow";
          compiler-nix-name = "ghc8107";
        };
        flake = escrow.flake { };
      in
      {

        packages.escrow = flake.packages."escrow:lib:escrow";
        packages.default = flake.packages."certification:lib:certification";


        devShells.escrow = escrow.shellFor {
        # tools = self.escrow-common.toolsFor escrow.index-state;
          buildInputs = [
          ## For UTF-8 locales
          pkgs.glibcLocales
          ];
          LANG = "C.UTF-8";
        };

        iog.dapp = escrow;
      }));

}


#      let
#          topLevel = import escrow/escrow.nix {
#            sources = inputs;
#          };
#        in
#          {
#              packages.iog.dappss = topLevel.escrow;
#          };
