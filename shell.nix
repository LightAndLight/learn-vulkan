let
  compiler = "default";
  overlays = [ (import ./overlay.nix compiler) ];
  compName = if compiler == "default" then "ghc863" else compiler;
in
{ nixpkgs ? import <nixpkgs> { inherit overlays; } }:
  nixpkgs.haskell.packages.${compName}.vulkan-hs.env.overrideAttrs (oldAttrs: {
     shellHook = ''
       export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${nixpkgs.vulkan-validation-layers}/lib
       export XDG_DATA_DIRS=$XDG_DATA_DIRS:${nixpkgs.vulkan-validation-layers}/share
     '';

      nativeBuildInputs = 
        oldAttrs.nativeBuildInputs ++ 
        [ nixpkgs.vulkan-tools nixpkgs.glslang (import <nixpkgs> {}).renderdoc ];
  })
