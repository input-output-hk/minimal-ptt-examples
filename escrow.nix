{ self, inputs, config, ... }: {

  perSystem = { config, pkgs, system, ... }:
    let
      split = self.internal-lib.make-haskell-nix-pkg {
        inherit (inputs) haskellNix CHaP;
        inherit pkgs;
        src = ./.;
        compiler-nix-name = "ghc8107";
      };
      flake = escrow.flake { };
    in {
      packages.escrow = flake.packages."escrow:lib:escrow";

      packages.split-test = flake.packages."escrow:test:spec";

      devShells.escrow = escrow.shellFor {
        withHoogle = true;
        tools = { hpack = { inherit (escrow) index-state; }; };
        buildInputs = [ pkgs.glibcLocales ];
        LANG = "C.UTF-8";
      };

    };
}
