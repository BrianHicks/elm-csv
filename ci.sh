#!/usr/bin/env nix-shell
#!nix-shell --pure -i bash
set -euo pipefail

# tests
elm-verify-examples
elm-test

# docs
elm make --docs=documentation.json

# linting
elm-format --validate src

# elm-review tries to download elm-json, and it fails in CI. We'll try again
# in the 20.05 release of Nix, where it's packaged natively.
# elm-review
