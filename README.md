# learn-vulkan

Learning a little bit of graphics programming with Vulkan.

I'm using [`vulkan-api`](http://hackage.haskell.org/package/vulkan-api), following 
[vulkan-tutorial.com](vulkan-tutorial.com) and building out high-level interface for
the parts that I touch (think: native Haskell datatypes and functions for everything).

You can build it with `nix-build .`, or with `./run.sh` which runs `cabal new-run` in a nix shell.
