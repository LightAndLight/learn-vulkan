let
  compiler = "default";
  overlays = [ (import ./overlay.nix compiler) ];
in
{ nixpkgs ? import <nixpkgs> { inherit overlays; } }:
let
  app = import ./app { inherit nixpkgs compiler; };
in
  app.env.overrideAttrs (oldAttrs: {
     shellHook = ''
       export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${nixpkgs.vulkan-validation-layers}/lib
       export XDG_DATA_DIRS=$XDG_DATA_DIRS:${nixpkgs.vulkan-validation-layers}/share
     '';

      nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ nixpkgs.vulkan-tools ];
  })
