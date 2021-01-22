#!/usr/bin/env nix-shell
#!nix-shell --pure -i bash
set -euo pipefail

# tests
elm-verify-examples
elm-test

# docs
elm make --docs=documentation.json

# linting
elm-review
elm-format --validate src
