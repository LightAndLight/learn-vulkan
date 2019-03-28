{ mkDerivation, base, GLFW-b, stdenv, vulkan-hs
, mmap, containers, managed, glslang
, vulkan-loader, vulkan-validation-layers
, Cabal, filepath, directory, process
}:
mkDerivation {
  pname = "learn-vulkan";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  setupHaskellDepends = [ Cabal directory filepath glslang process ];
  executableHaskellDepends =
    [ base GLFW-b vulkan-hs mmap containers managed ];
  executableSystemDepends =
    [ vulkan-loader vulkan-validation-layers ];
  license = stdenv.lib.licenses.bsd3;
}
