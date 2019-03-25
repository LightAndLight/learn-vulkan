#!/usr/bin/env bash
nix-shell --run "cabal new-repl --extra-lib-dirs=$(nix-build '<nixpkgs>' -A vulkan-loader)/lib"
