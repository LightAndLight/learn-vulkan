import Control.Monad
import Data.Traversable
import Distribution.Simple
import Distribution.Simple.Setup
import System.Directory
import System.FilePath
import System.Process

main =
  defaultMainWithHooks $
  simpleUserHooks
  { preBuild = \args flags -> do
      let shaderDir = "shaders"
      withCurrentDirectory "shaders" $ do
        shaderFiles <- listDirectory "."
        for shaderFiles $ \file ->
          when (takeExtension file `elem` [".vert", ".frag"]) $
          callProcess "glslangValidator" [ "-V", file ]
      preBuild simpleUserHooks args flags
  }
