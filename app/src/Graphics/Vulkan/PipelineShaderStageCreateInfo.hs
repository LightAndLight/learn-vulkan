{-# language BangPatterns #-}
{-# language DataKinds, GADTs, KindSignatures, TypeOperators #-}
{-# language EmptyCase, EmptyDataDeriving #-}
{-# language FlexibleContexts, FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
module Graphics.Vulkan.PipelineShaderStageCreateInfo where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.&.), (.|.))
import Data.Void (Void)
import Data.Word (Word32)

import qualified Foreign
import qualified Foreign.C.String as Foreign
import qualified Foreign.C.Types as Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_mesh_shader as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_ray_tracing as Vk

data VkPipelineShaderStageCreateFlag
  deriving (Eq, Ord, Show)

vkPipelineShaderStageCreateBits ::
  Vk.VkPipelineShaderStageCreateFlags ->
  [VkPipelineShaderStageCreateFlag]
vkPipelineShaderStageCreateBits _ = []

unVkPipelineShaderStageCreateBits ::
  [VkPipelineShaderStageCreateFlag] ->
  Vk.VkPipelineShaderStageCreateFlags
unVkPipelineShaderStageCreateBits [] = 0
unVkPipelineShaderStageCreateBits (x:_) = case x of

data VkShaderStageFlag
  = Vertex
  | TessellationControl
  | TessellationEvaluation
  | Geometry
  | Fragment
  | Compute
  | AllGraphics
  | All
  | RaygenNV
  | AnyHitNV
  | ClosestHitNV
  | MissNV
  | IntersectionNV
  | CallableNV
  | TaskNV
  | MeshNV
  deriving (Eq, Ord, Show)

vkShaderStageBit ::
  Vk.VkShaderStageBitmask a ->
  VkShaderStageFlag
vkShaderStageBit a =
  case a of
    Vk.VK_SHADER_STAGE_VERTEX_BIT -> Vertex
    Vk.VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT -> TessellationControl
    Vk.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT -> TessellationEvaluation
    Vk.VK_SHADER_STAGE_GEOMETRY_BIT -> Geometry
    Vk.VK_SHADER_STAGE_FRAGMENT_BIT -> Fragment
    Vk.VK_SHADER_STAGE_COMPUTE_BIT -> Compute
    Vk.VK_SHADER_STAGE_ALL_GRAPHICS -> AllGraphics
    Vk.VK_SHADER_STAGE_ALL -> All
    -- Vk.VK_SHADER_STAGE_RAYGEN_BIT_NV -> RaygenNV
    -- Vk.VK_SHADER_STAGE_ANY_HIT_BIT_NV -> AnyHitNV
    -- Vk.VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV -> ClosestHitNV
    -- Vk.Vk.VK_SHADER_STAGE_MISS_BIT_NV -> MissNV
    -- Vk.VK_SHADER_STAGE_INTERSECTION_BIT_NV -> IntersectionNV
    -- Vk.VK_SHADER_STAGE_CALLABLE_BIT_NV -> CallableNV
    -- Vk.VK_SHADER_STAGE_TASK_BIT_NV -> TaskNV
    -- Vk.VK_SHADER_STAGE_MESH_BIT_NV -> MeshNV

unVkShaderStageBit ::
  VkShaderStageFlag ->
  Vk.VkShaderStageBitmask a
unVkShaderStageBit a =
  case a of
    Vertex -> Vk.VK_SHADER_STAGE_VERTEX_BIT
    TessellationControl -> Vk.VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT
    TessellationEvaluation -> Vk.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT
    Geometry -> Vk.VK_SHADER_STAGE_GEOMETRY_BIT
    Fragment -> Vk.VK_SHADER_STAGE_FRAGMENT_BIT
    Compute -> Vk.VK_SHADER_STAGE_COMPUTE_BIT
    AllGraphics -> Vk.VK_SHADER_STAGE_ALL_GRAPHICS
    All -> Vk.VK_SHADER_STAGE_ALL
    -- RaygenNV -> Vk.VK_SHADER_STAGE_RAYGEN_BIT_NV
    -- AnyHitNV -> Vk.VK_SHADER_STAGE_ANY_HIT_BIT_NV
    -- ClosestHitNV -> Vk.VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV
    -- MissNV -> Vk.Vk.VK_SHADER_STAGE_MISS_BIT_NV
    -- IntersectionNV -> Vk.VK_SHADER_STAGE_INTERSECTION_BIT_NV
    -- CallableNV -> Vk.VK_SHADER_STAGE_CALLABLE_BIT_NV
    -- TaskNV -> Vk.VK_SHADER_STAGE_TASK_BIT_NV
    -- MeshNV -> Vk.VK_SHADER_STAGE_MESH_BIT_NV

vkShaderStageBits ::
  Vk.VkShaderStageFlags ->
  [VkShaderStageFlag]
vkShaderStageBits bs =
  foldr
    (\(mask, val) b -> if mask .&. bs == mask then val : b else b)
    []
    [ (Vk.VK_SHADER_STAGE_VERTEX_BIT, Vertex)
    , (Vk.VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT, TessellationControl)
    , (Vk.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT, TessellationEvaluation)
    , (Vk.VK_SHADER_STAGE_GEOMETRY_BIT, Geometry)
    , (Vk.VK_SHADER_STAGE_FRAGMENT_BIT, Fragment)
    , (Vk.VK_SHADER_STAGE_COMPUTE_BIT, Compute)
    , (Vk.VK_SHADER_STAGE_ALL_GRAPHICS, AllGraphics)
    , (Vk.VK_SHADER_STAGE_ALL, All)
    -- , (Vk.VK_SHADER_STAGE_RAYGEN_BIT_NV, RaygenNV)
    -- , (Vk.VK_SHADER_STAGE_ANY_HIT_BIT_NV, AnyHitNV)
    -- , (Vk.VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV, ClosestHitNV)
    -- , (Vk.Vk.VK_SHADER_STAGE_MISS_BIT_NV, MissNV)
    -- , (Vk.VK_SHADER_STAGE_INTERSECTION_BIT_NV, IntersectionNV)
    -- , (Vk.VK_SHADER_STAGE_CALLABLE_BIT_NV, CallableNV)
    -- , (Vk.VK_SHADER_STAGE_TASK_BIT_NV, TaskNV)
    -- , (Vk.VK_SHADER_STAGE_MESH_BIT_NV, MeshNV)
    ]

unVkShaderStageBits ::
  [VkShaderStageFlag] ->
  Vk.VkShaderStageFlags
unVkShaderStageBits =
  foldr (\a b -> unVkShaderStageBit a .|. b) 0

data VkSpecializationInfo :: [*] -> * where
  Nil :: VkSpecializationInfo '[]
  Cons ::
    Foreign.Storable t =>
    Word32 ->
    t ->
    VkSpecializationInfo ts ->
    VkSpecializationInfo (t ': ts)
instance Eq (VkSpecializationInfo '[]) where; Nil == Nil = True
instance
  (Eq t, Eq (VkSpecializationInfo ts)) =>
  Eq (VkSpecializationInfo (t ': ts)) where

  Cons a b c == Cons a' b' c' = a == a' && b == b' && c == c'
instance Ord (VkSpecializationInfo '[]) where; compare Nil Nil = EQ
instance
  (Ord t, Ord (VkSpecializationInfo ts)) =>
  Ord (VkSpecializationInfo (t ': ts)) where

  compare (Cons a b c) (Cons a' b' c') =
    case compare a a' of
      EQ ->
        case compare b b' of
          EQ -> compare c c'
          res -> res
      res -> res

instance Show (VkSpecializationInfo '[]) where
  showsPrec _ Nil = showString "Nil"
instance
  (Show t, Show (VkSpecializationInfo ts)) =>
  Show (VkSpecializationInfo (t ': ts)) where

  showsPrec d (Cons a b c) =
    showParen (d > 10) $
    showString "Cons " .
    showsPrec 11 a .
    showString " " .
    showsPrec 11 b .
    showString " " .
    showsPrec 11 c

specializationInfoCount :: VkSpecializationInfo a -> Word32
specializationInfoCount = go 0
  where
    go :: Word32 -> VkSpecializationInfo a -> Word32
    go !n Nil = n
    go !n (Cons _ _ a) = go (n+1) a

specializationInfoSize :: VkSpecializationInfo a -> Foreign.CSize
specializationInfoSize = go 0
  where
    go :: Foreign.CSize -> VkSpecializationInfo a -> Foreign.CSize
    go !n Nil = 0
    go !n (Cons _ a b) = go (n + fromIntegral (Foreign.sizeOf a)) b

withSpecializationInfoEntries ::
  forall a b.
  Word32 ->
  VkSpecializationInfo a ->
  (Foreign.Ptr Vk.VkSpecializationMapEntry -> IO b) ->
  IO b
withSpecializationInfoEntries count si f = do
  (ptr, _) <- Vk.mallocVkDataArray (fromIntegral count)
  go 0 si ptr
  where
    go ::
      forall x.
      Word32 -> -- ^ offset into the array, in bytes
      VkSpecializationInfo x ->
      Foreign.Ptr Vk.VkSpecializationMapEntry ->
      IO b
    go !offset Nil !ptr = f ptr
    go !offset (Cons cid a b) !ptr = do
      let aSize = Foreign.sizeOf a
      Vk.writeField @"constantID" ptr cid
      Vk.writeField @"offset" ptr offset
      Vk.writeField @"size" ptr (fromIntegral aSize)
      go (offset + fromIntegral aSize) b (Foreign.plusPtr ptr aSize)

withSpecializationInfoData ::
  forall a b.
  Foreign.CSize ->
  VkSpecializationInfo a ->
  (Foreign.Ptr Void -> IO b) ->
  IO b
withSpecializationInfoData bufSize si f = do
  Foreign.allocaBytes (fromIntegral bufSize) $ go si
  where
    go ::
      forall x.
      VkSpecializationInfo x ->
      Foreign.Ptr Void ->
      IO b
    go Nil !ptr = f ptr
    go (Cons cid a b) !ptr = do
      let aSize = Foreign.sizeOf a
      Foreign.poke (Foreign.castPtr ptr) a
      go b (Foreign.plusPtr ptr aSize)

unVkSpecializationInfo ::
  MonadIO m =>
  VkSpecializationInfo a ->
  m Vk.VkSpecializationInfo
unVkSpecializationInfo si =
  liftIO $
  withSpecializationInfoEntries entryCount si $ \entriesPtr ->
  withSpecializationInfoData bufSize si $ \dataPtr ->
  Vk.newVkData $ \siPtr -> do
    Vk.writeField @"mapEntryCount" siPtr entryCount
    Vk.writeField @"pMapEntries" siPtr entriesPtr
    Vk.writeField @"dataSize" siPtr bufSize
    Vk.writeField @"pData" siPtr dataPtr
  where
    entryCount = specializationInfoCount si
    bufSize = specializationInfoSize si

data VkPipelineShaderStageCreateInfo ts
  = VkPipelineShaderStageCreateInfo
  { flags :: [VkPipelineShaderStageCreateFlag]
  , stage :: VkShaderStageFlag
  , module_ :: Vk.VkShaderModule
  , pName :: String
  , pSpecializationInfo :: Maybe (VkSpecializationInfo ts)
  }

instance Eq (VkPipelineShaderStageCreateInfo '[]) where
  a == b =
    flags a == flags b &&
    stage a == stage b &&
    module_ a == module_ b &&
    pName a == pName b &&
    pSpecializationInfo a == pSpecializationInfo b
instance
  (Eq t, Eq (VkSpecializationInfo ts)) =>
  Eq (VkPipelineShaderStageCreateInfo (t ': ts)) where
  a == b =
    flags a == flags b &&
    stage a == stage b &&
    module_ a == module_ b &&
    pName a == pName b &&
    pSpecializationInfo a == pSpecializationInfo b
instance Ord (VkPipelineShaderStageCreateInfo '[]) where
  compare a b =
    case compare (flags a) (flags b) of
      EQ ->
        case compare (stage a) (stage b) of
          EQ ->
            case compare (module_ a) (module_ b) of
              EQ ->
                case compare (pName a) (pName b) of
                  EQ -> compare (pSpecializationInfo a) (pSpecializationInfo b)
                  res -> res
              res -> res
          res -> res
      res -> res
instance
  (Ord t, Ord (VkSpecializationInfo ts)) =>
  Ord (VkPipelineShaderStageCreateInfo (t ': ts)) where
  compare a b =
    case compare (flags a) (flags b) of
      EQ ->
        case compare (stage a) (stage b) of
          EQ ->
            case compare (module_ a) (module_ b) of
              EQ ->
                case compare (pName a) (pName b) of
                  EQ -> compare (pSpecializationInfo a) (pSpecializationInfo b)
                  res -> res
              res -> res
          res -> res
      res -> res
instance Show (VkPipelineShaderStageCreateInfo '[]) where
  showsPrec d a =
    showParen (d > 10) $
    showString "{" .
    showString "flags = " . showsPrec 0 (flags a) . showString ", " .
    showString "stage = " . showsPrec 0 (stage a) . showString ", " .
    showString "module_ = " . showsPrec 0 (module_ a) . showString ", " .
    showString "pName_ = " . showsPrec 0 (pName a) . showString ", " .
    showString "pSpecializationInfo_ = " . showsPrec 0 (pSpecializationInfo a) .
    showString "}"
instance
  (Show t, Show (VkSpecializationInfo ts)) =>
  Show (VkPipelineShaderStageCreateInfo (t ': ts)) where
  showsPrec d a =
    showParen (d > 10) $
    showString "{" .
    showString "flags = " . showsPrec 0 (flags a) . showString ", " .
    showString "stage = " . showsPrec 0 (stage a) . showString ", " .
    showString "module_ = " . showsPrec 0 (module_ a) . showString ", " .
    showString "pName_ = " . showsPrec 0 (pName a) . showString ", " .
    showString "pSpecializationInfo_ = " . showsPrec 0 (pSpecializationInfo a) .
    showString "}"

unVkPipelineShaderStageCreateInfo ::
  MonadIO m =>
  VkPipelineShaderStageCreateInfo ts ->
  m Vk.VkPipelineShaderStageCreateInfo
unVkPipelineShaderStageCreateInfo info =
  liftIO $
  Foreign.withCString (pName info) $ \strPtr -> do
  specInfo <-
    maybe
      (pure Vk.VK_NULL_HANDLE)
      (fmap Vk.unsafePtr . unVkSpecializationInfo)
      (pSpecializationInfo info)
  Vk.newVkData $ \ptr -> do
    Vk.writeField @"flags" ptr (unVkPipelineShaderStageCreateBits $ flags info)
    Vk.writeField @"stage" ptr (unVkShaderStageBit $ stage info)
    Vk.writeField @"module" ptr (module_ info)
    Vk.writeField @"pName" ptr strPtr
    Vk.writeField @"pSpecializationInfo" ptr specInfo
