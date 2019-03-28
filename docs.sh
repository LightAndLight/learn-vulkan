#!/usr/bin/env sh
nix-shell --run "cabal new-haddock vulkan-hs"
cp -R dist-newstyle/build/x86_64-linux/ghc-8.6.3/vulkan-hs-0.1/doc/html/vulkan-hs/* docs/
