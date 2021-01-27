#!/usr/bin/env nix-shell
#!nix-shell --pure -i bash
set -euo pipefail

group() {
  echo "::group::${1:-}"
  "${@}"
  echo
  echo "::endgroup::"
}

# tests
group elm-verify-examples
group elm-test

# docs
group elm make --docs=documentation.json

# linting
group elm-format --validate src
group elm-review
