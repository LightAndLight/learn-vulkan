module Vertex where

import Data.Word (Word32)
import Foreign (Storable(..), castPtr, sizeOf)
import Linear.V2 (V2)
import Linear.V3 (V3)

data Vertex = Vertex { vPos :: V2 Float, vColor :: V3 Float }
  deriving (Eq, Ord, Show)

instance Storable Vertex where
  sizeOf _ =
    sizeOf (undefined :: V2 Float) + sizeOf (undefined :: V3 Float)

  alignment _ =
    alignment (undefined :: Float)

  peek p =
    Vertex <$> peek (castPtr p) <*> peekByteOff p (sizeOf (undefined :: V2 Float))

  poke p (Vertex a b) =
    poke (castPtr p) a *> pokeByteOff p (sizeOf (undefined :: V2 Float)) b

vertexStride :: Word32
vertexStride = fromIntegral $ sizeOf (undefined :: Vertex)

vPosOffset :: Word32
vPosOffset = 0

vColorOffset :: Word32
vColorOffset = fromIntegral $ sizeOf (undefined :: V2 Float)
