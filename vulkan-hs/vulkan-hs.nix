{ mkDerivation, base, GLFW-b, stdenv, vulkan-api
, mmap, managed
, vulkan-loader, vulkan-validation-layers
}:
mkDerivation {
  pname = "vulkan-hs";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  libraryHaskellDepends =
    [ base GLFW-b vulkan-api mmap managed ];
  librarySystemDepends =
    [ vulkan-loader vulkan-validation-layers ];
  license = stdenv.lib.licenses.bsd3;
}
