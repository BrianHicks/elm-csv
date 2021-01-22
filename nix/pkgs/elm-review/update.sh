#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nodePackages.node2nix
set -euo pipefail

cd "$(dirname "$(realpath "$0")")"
node2nix
