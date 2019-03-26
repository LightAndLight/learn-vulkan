{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let

  inherit (nixpkgs) pkgs;
  app = import ./app { inherit nixpkgs compiler; };

in

  (pkgs.stdenv.mkDerivation {
    name = "learn-vulkan";
    src = ./.;
    buildInputs =
      [
        app
        pkgs.vulkan-validation-layers
        pkgs.vulkan-tools
      ];
    installPhase =
''
mkdir -p $out/bin
cat <<EOF >$out/bin/run
#!/usr/bin/env sh
export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:${pkgs.vulkan-validation-layers}/lib
export XDG_DATA_DIRS=\$XDG_DATA_DIRS:${pkgs.vulkan-validation-layers}/share
${app}/bin/learn-vulkan
EOF
chmod +x $out/bin/run
'';
  })
