{ self, inputs, ... }: {

  perSystem = { self', pkgs, final, inputs', ... }:
    let
      escrow = self.internal-lib.make-haskell-nix-pkg {
        inherit (inputs) haskellNix CHaP;
        inherit pkgs;
        src = "${self}/escrow";
        compiler-nix-name = "ghc8107";
      };
      flake = escrow.flake { };
    in {

      # packages.escrow = builtins.trace (flake.packages) (flake.packages."escrow");
      #"certification:lib:certification" = <CODE>; "escrow:lib:escrow" = <CODE>; "escrow:test:escrow-test


      #packages = builtins.trace (escrow.certification) flake.packages;
      # packages = flake.packages;



      packages.default = flake.packages."escrow:lib:escrow";

      #defaultPackage = flake.packages."escrow:test:escrow-test";

      devShells.escrow = escrow.shellFor {
        # tools = self.escrow-common.toolsFor escrow.index-state;
        buildInputs = [
          ## For UTF-8 locales
          pkgs.glibcLocales
        ];
        LANG = "C.UTF-8";
      };


        # escrow.escrow.components.exes.escrow;
    };

}
