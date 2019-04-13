module Vertex where

import Data.Word (Word32)
import Foreign (sizeOf)
import Linear.V2 (V2)
import Linear.V3 (V3)

data Vertex = Vertex { vPos :: V2 Float, vColor :: V3 Float }
  deriving (Eq, Ord, Show)

vertexStride :: Word32
vertexStride =
  fromIntegral $
  sizeOf (undefined :: V2 Float) + sizeOf (undefined :: V3 Float)

vPosOffset :: Word32
vPosOffset = 0

vColorOffset :: Word32
vColorOffset = fromIntegral $ sizeOf (undefined :: V2 Float)
