{ ... }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "elm-csv";
  buildInputs = [
    niv.niv
    git

    # elm
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-json
    elmPackages.elm-review
    elmPackages.elm-test
    elmPackages.elm-verify-examples

    # to get remote stuff in CI
    cacert
  ];
}
