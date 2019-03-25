{ mkDerivation, base, GLFW-b, stdenv, vulkan-api, vulkan-loader }:
mkDerivation {
  pname = "learn-vulkan";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base GLFW-b vulkan-api vulkan-loader ];
  license = stdenv.lib.licenses.bsd3;
}
