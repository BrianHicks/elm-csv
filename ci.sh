#!/usr/bin/env nix-shell
#!nix-shell --pure -i bash

# tests
elm-verify-examples
elm-test

# docs
elm make --docs=documentation.json
