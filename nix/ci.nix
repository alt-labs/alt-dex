{ sourcesFile ? ./sources.json, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }, deferPluginErrors ? true
, doCoverage ? true }:
let
  project = import ./haskell.nix {
    inherit sourcesFile system sources plutus deferPluginErrors doCoverage;
  };
in rec {
  # These will be built by the CI.
  inherit (project) projectCoverageReport;
  inherit (project.alt-dex.components) library;
  inherit (project.alt-dex.components.exes)
    lendex-demo nft-demo alt-dex;
  inherit (project.alt-dex.components.tests)
    alt-dex-tests;

  # This will run the tests within this build and produce the test logs as output
  check = plutus.pkgs.runCommand "run-tests" { } ''
    ${alt-dex-tests}/bin/alt-dex-tests > $out
  '';

}
