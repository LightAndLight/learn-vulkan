{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./learn-vulkan.nix;

  haskellPackages =
    (if compiler == "default"
     then pkgs.haskell.packages.ghc863
     else pkgs.haskell.packages.${compiler}).override {
      overrides = self: super: {
        vulkan-api = pkgs.haskell.lib.dontHaddock super.vulkan-api;
      };
    };

in

  haskellPackages.callPackage f {}

