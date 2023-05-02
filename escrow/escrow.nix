{ self, inputs, ... }: {

  perSystem = { self', pkgs, final, inputs', ... }:
    let
      escrow = self.internal-lib.make-haskell-nix-pkg {
        inherit (inputs) haskellNix CHaP;
        inherit pkgs;
        src = "${self}/escrow";
        compiler-nix-name = "ghc8107";
      };
    in {
      devShells.escrow = escrow.shellFor {
        # tools = self.escrow-common.toolsFor escrow.index-state;
        buildInputs = [
          ## For UTF-8 locales
          pkgs.glibcLocales
        ];
        LANG = "C.UTF-8";
      };

      packages.escrow = "escrow";
        # escrow.escrow.components.exes.escrow;
    };
}
