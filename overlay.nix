compiler: self: super:
let
  compName = if compiler == "default" then "ghc863" else compiler;
in {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ${compName} = super.haskell.packages.${compName}.override {
        overrides = hpSelf: hpSuper: {
          vulkan-hs = import ./vulkan-hs { nixpkgs = self; inherit compiler; };
          vulkan-api = self.haskell.lib.dontHaddock hpSuper.vulkan-api;
        };
      };
    };
  };
}
