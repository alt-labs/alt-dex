{
  inputs = {
    flake-utils = {
      type = "github";
      owner = "numtide";
      repo = "flake-utils";
    };

    # The Plutus "flake" isn't really a flake - it doesn't define any
    # outputs and is only used for input pinning according to to
    # the comments
    plutusSrc = {
      type = "github";
      owner = "input-output-hk";
      repo = "plutus";
      rev = "2721c59fd2302b75c4138456c29fd5b509e8340a";
      flake = false;
    };

    haskellNix = {
      type = "github";
      owner = "input-output-hk";
      repo = "haskell.nix";
      # FIXME rev taken from Plutus doesn't work?
      rev = "64cd5f70ce0d619390039a1a3d57c442552b0924";
    };

    nixpkgs.follows = "haskellNix/nixpkgs-unstable";

    flake-compat = {
      type = "github";
      owner = "edolstra";
      repo = "flake-compat";
      rev = "12c64ca55c1014cdc1b16ed5a804aa8576601ff2";
      flake = false;
    };
  };

  outputs = { self, flake-utils, plutusSrc, nixpkgs, haskellNix, ... }:
    let
      inherit (flake-utils.lib) defaultSystems;

      perSystem = nixpkgs.lib.genAttrs defaultSystems;

      nixpkgsFor = system: import nixpkgs {
        overlays = [ haskellNix.overlay ];
        inherit (haskellNix) config;
        inherit system;
      };

      projectFor = system:
        let
          pkgs = nixpkgsFor system;
          plutus = import plutusSrc { inherit system; };
        in
          import ./nix/haskell.nix {
            inherit system pkgs plutus;
          };

    in
      {
        flake = perSystem (system: (projectFor system).flake {});

        defaultPackage = perSystem (
          system:
            self.flake.${system}.packages."alt-dex:lib:alt-dex"
        );

        packages = perSystem (system: self.flake.${system}.packages);

        apps = perSystem (system: self.flake.${system}.apps);

        devShell = perSystem (system: self.flake.${system}.devShell);

        # NOTE `nix flake check` will not work at the moment due to use of
        # IFD in haskell.nix

        # checks = perSystem (
        #   system:
        #     let
        #       flakePkgs = self.flake.${system}.packages;
        #     in
        #       {
        #         tests =
        #           flakePkgs."alt-dex:test:alt-dex-tests";
        #         deploy-app =
        #           flakePkgs."alt-dex:exe:deploy-app";
        #         governance-demo =
        #           flakePkgs."alt-dex:exe:governance-demo";
        #         lendex-demo =
        #           flakePkgs."alt-dex:exe:lendex-demo";
        #         alt-dex =
        #           flakePkgs."alt-dex:exe:alt-dex";
        #         nft-demo = flakePkgs."alt-dex:exe:nft-demo";
        #         nft-marketplace =
        #           flakePkgs."alt-dex:exe:nft-marketplace";
        #       }
        # );

      };
}
