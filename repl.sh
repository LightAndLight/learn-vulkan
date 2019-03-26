#!/usr/bin/env bash
nix-shell --run "cabal new-repl learn-vulkan --extra-lib-dirs=$(nix-build '<nixpkgs>' -A vulkan-loader)/lib"
