{ mkDerivation, base, GLFW-b, stdenv, vulkan-api
, vulkan-loader, vulkan-validation-layers
}:
mkDerivation {
  pname = "learn-vulkan";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends =
    [ base GLFW-b vulkan-api ];
  executableSystemDepends =
    [ vulkan-loader vulkan-validation-layers ];
  license = stdenv.lib.licenses.bsd3;
}
