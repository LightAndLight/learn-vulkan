{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./vulkan-hs.nix;

  haskellPackages =
    if compiler == "default"
    then pkgs.haskell.packages.ghc863
    else pkgs.haskell.packages.${compiler};

in

  haskellPackages.callPackage f {}
