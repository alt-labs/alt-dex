{ pkgs
, plutus
, doCoverage ? false
, deferPluginErrors ? true
, ...
}:

let
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "alt-dex";
    src = ./..;
  };

  plutusPkgs = plutus.pkgs;

  sources = import ./sources.nix {};
in
pkgs.haskell-nix.cabalProject {
  inherit src;

  name = "alt-dex";

  # Plutus uses a patched GHC. And so shall we.
  compiler-nix-name = "ghc810420210212";

  # -- Materialization
  # See https://input-output-hk.github.io/haskell.nix/tutorials/materialization/:
  # Update using:
  #   nix-build default.nix 2>&1 | grep -om1 '/nix/store/.*-updateMaterialized' | bash
  # plan-sha256 = "0000000000000000000000000000000000000000000000000000";
  # materialized = ./materialization/alt-dex.materialized;
  shell = {
    # putting packages here will make them available in the hoogle index generated
    # by the shell
    additional = ps: with ps; [
      plutus-core
      plutus-ledger-api
      plutus-tx
      plutus-tx-plugin
      word-array
      prettyprinter-configurable
      plutus-extra
      tasty-plutus
      plutus-pretty
      plutus-numeric
      playground-common
      plutus-chain-index
      plutus-chain-index-core
      plutus-contract
      plutus-ledger
      plutus-pab
      plutus-playground-server
      plutus-use-cases
      quickcheck-dynamic
      web-ghc
    ];

    withHoogle = true;

    tools.cabal = "latest";

    nativeBuildInputs = with pkgs;
      [
        # Haskell Tools
        entr
        ghcid
        git

        # Use plutus for these packages for now, the versions from haskell.nix
        # nixpkgs are too new and require builds
        plutusPkgs.haskellPackages.fourmolu
        plutusPkgs.niv
        plutusPkgs.stack

        plutus.plutus.haskell-language-server
        plutus.plutus.hlint
        jq
        nixfmt

        # hls doesn't support preprocessors yet so this has to exist in PATH
        haskellPackages.record-dot-preprocessor

        # Graphviz Diagrams for documentation
        graphviz
        pkg-config
        plutusPkgs.libsodium-vrf
      ] ++ (
        lib.optionals (!stdenv.isDarwin) [
          rPackages.plotly
          R
          systemdMinimal
        ]
      );
  };


  modules = [
    {
      packages = {
        eventful-sql-common.doHaddock = false;
        eventful-sql-common.ghcOptions = [
          ''
            -XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances
                    -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses''
        ];

        plutus-use-cases.doHaddock = deferPluginErrors;
        plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

        plutus-contract.doHaddock = deferPluginErrors;
        plutus-contract.flags.defer-plugin-errors = deferPluginErrors;

        plutus-ledger.doHaddock = deferPluginErrors;
        plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

        cardano-crypto-praos.components.library.pkgconfig =
          plutusPkgs.lib.mkForce [ [ plutusPkgs.libsodium-vrf ] ];
        cardano-crypto-class.components.library.pkgconfig =
          plutusPkgs.lib.mkForce [ [ plutusPkgs.libsodium-vrf ] ];

        # This allows us to generate .tix coverage files, which could be useful?
        "${src.name}".components.library = {
          doHoogle = deferPluginErrors;
          doCoverage = doCoverage;
        };
      };
    }
  ];

  # Using this allows us to leave these nix-specific hashes _out_ of cabal.project
  # Normally, they'd be placed under the `source-repository-package` section as a comment like so:
  # `--sha256: ...`
  sha256map = {
    # Enforce we are using the same hash as niv has
    # i.e. this will now fail to nix-build if you bump it but don't bump the `cabal.project`.

    # `plutus`, `plutus-apps`, & `plutus-extra`
    "https://github.com/input-output-hk/plutus.git"."${sources.plutus.rev}" =
      sources.plutus.sha256;
    "https://github.com/input-output-hk/plutus-apps.git"."${sources.plutus-apps.rev}" =
      sources.plutus-apps.sha256;
    "https://github.com/ngua/plutus-extra.git"."${sources.plutus-extra.rev}" =
      sources.plutus-extra.sha256;

    # `cardano-*`
    "https://github.com/input-output-hk/cardano-addresses"."${sources.cardano-addresses.rev}" =
      sources.cardano-addresses.sha256;
    "https://github.com/input-output-hk/cardano-base"."${sources.cardano-base.rev}" =
      sources.cardano-base.sha256;
    "https://github.com/input-output-hk/cardano-crypto.git"."${sources.cardano-crypto.rev}" =
      sources.cardano-crypto.sha256;
    "https://github.com/input-output-hk/cardano-ledger-specs"."${sources.cardano-ledger-specs.rev}" =
      sources.cardano-ledger-specs.sha256;
    "https://github.com/input-output-hk/cardano-node.git"."${sources.cardano-node.rev}" =
      sources.cardano-node.sha256;
    "https://github.com/input-output-hk/cardano-prelude"."${sources.cardano-prelude.rev}" =
      sources.cardano-prelude.sha256;
    "https://github.com/j-mueller/cardano-wallet"."${sources.cardano-wallet.rev}" =
      sources.cardano-wallet.sha256;

    # other git dependencies
    "https://github.com/Quid2/flat.git"."${sources.flat.rev}" =
      sources.flat.sha256;
    "https://github.com/input-output-hk/goblins"."${sources.goblins.rev}" =
      sources.goblins.sha256;
    "https://github.com/input-output-hk/iohk-monitoring-framework"."${sources.iohk-monitoring-framework.rev}" =
      sources.iohk-monitoring-framework.sha256;
    "https://github.com/input-output-hk/ouroboros-network"."${sources.ouroboros-network.rev}" =
      sources.ouroboros-network.sha256;
    "https://github.com/input-output-hk/optparse-applicative"."${sources.optparse-applicative.rev}" =
      sources.optparse-applicative.sha256;
    "https://github.com/input-output-hk/purescript-bridge.git"."${sources.purescript-bridge.rev}" =
      sources.purescript-bridge.sha256;
    "https://github.com/input-output-hk/servant-purescript.git"."${sources.servant-purescript.rev}" =
      sources.servant-purescript.sha256;
    "https://github.com/input-output-hk/Win32-network"."${sources.Win32-network.rev}" =
      sources.Win32-network.sha256;
  };
}
