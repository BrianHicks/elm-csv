{ ... }:
let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  niv = import sources.niv { };

  # NOTE: this will be in nixpkgs in 21.05. When we're using that release,
  # we can just drop the custom thing here.
  elm-review = nixpkgs.callPackage ./nix/pkgs/elm-review { };
in with nixpkgs;
stdenv.mkDerivation {
  name = "elm-csv";
  buildInputs = [
    niv.niv
    git

    # elm
    elmPackages.elm
    elmPackages.elm-format
    elmPackages.elm-test
    elmPackages.elm-verify-examples
    elm-review.elm-review

    # to get remote stuff in CI
    cacert
  ];
}
