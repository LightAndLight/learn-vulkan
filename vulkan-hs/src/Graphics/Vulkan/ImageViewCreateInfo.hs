{-# language DataKinds, TypeApplications #-}
{-# language EmptyCase, EmptyDataDeriving #-}
{-# language ViewPatterns #-}
module Graphics.Vulkan.ImageViewCreateInfo where

import Control.Exception (bracket)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Managed.Safe (MonadManaged, using, managed)
import Data.Bits ((.&.), (.|.))
import Data.Word (Word32)
import Unsafe.Coerce (unsafeCoerce)

import qualified Foreign.Marshal.Alloc as Foreign
import qualified Foreign.Ptr as Foreign
import qualified Foreign.Storable as Foreign
import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Core_1_1 as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_sampler_ycbcr_conversion as Vk
import qualified Graphics.Vulkan.Marshal as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_fragment_density_map as Vk

import Graphics.Vulkan.Format (VkFormat, vkFormat, unVkFormat)
import Graphics.Vulkan.Result (vkResult)

data VkImageViewCreateFlag
  -- = FragmentDensityMapDynamic
  deriving (Eq, Ord, Show)

{-
vkImageViewCreateBit ::
  Vk.VkImageViewCreateBitmask a ->
  VkImageViewCreateFlag
vkImageViewCreateBit a =
  case a of
    -- Vk.VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT -> FragmentDensityMapDynamic

unVkImageViewCreateBit ::
  VkImageViewCreateFlag ->
  Vk.VkImageViewCreateBitmask a
unVkImageViewCreateBit a =
  case a of
    -- FragmentDensityMapDynamic -> Vk.VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
-}

vkImageViewCreateBits ::
  Vk.VkImageViewCreateFlags ->
  [VkImageViewCreateFlag]
vkImageViewCreateBits bs = []
  {-
  foldr
    (\(mask, val) b -> if mask .&. bs == mask then val : b else b)
    []
    [ (Vk.VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT, FragmentDensityMapDynamic)
    []
-}

unVkImageViewCreateBits ::
  [VkImageViewCreateFlag] ->
  Vk.VkImageViewCreateFlags
unVkImageViewCreateBits _ = 0
  -- foldr (\a b -> unVkImageViewCreateBit a .|. b) 0

data VkImageViewType
  = OneD
  | TwoD
  | ThreeD
  | Cube
  | OneDArray
  | TwoDArray
  | CubeArray
  deriving (Eq, Ord, Show)

vkImageViewType :: Vk.VkImageViewType -> VkImageViewType
vkImageViewType a =
  case a of
    Vk.VK_IMAGE_VIEW_TYPE_1D -> OneD
    Vk.VK_IMAGE_VIEW_TYPE_2D -> TwoD
    Vk.VK_IMAGE_VIEW_TYPE_3D -> ThreeD
    Vk.VK_IMAGE_VIEW_TYPE_CUBE -> Cube
    Vk.VK_IMAGE_VIEW_TYPE_1D_ARRAY -> OneDArray
    Vk.VK_IMAGE_VIEW_TYPE_2D_ARRAY -> TwoDArray
    Vk.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY -> CubeArray

unVkImageViewType :: VkImageViewType -> Vk.VkImageViewType
unVkImageViewType a =
  case a of
    OneD -> Vk.VK_IMAGE_VIEW_TYPE_1D
    TwoD -> Vk.VK_IMAGE_VIEW_TYPE_2D
    ThreeD -> Vk.VK_IMAGE_VIEW_TYPE_3D
    Cube -> Vk.VK_IMAGE_VIEW_TYPE_CUBE
    OneDArray -> Vk.VK_IMAGE_VIEW_TYPE_1D_ARRAY
    TwoDArray -> Vk.VK_IMAGE_VIEW_TYPE_2D_ARRAY
    CubeArray -> Vk.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY

data VkComponentSwizzle
  = SwizzleIdentity
  | SwizzleZero
  | SwizzleOne
  | SwizzleR
  | SwizzleG
  | SwizzleB
  | SwizzleA
  deriving (Eq, Ord, Show)

vkComponentSwizzle :: Vk.VkComponentSwizzle -> VkComponentSwizzle
vkComponentSwizzle a =
  case a of
    Vk.VK_COMPONENT_SWIZZLE_IDENTITY -> SwizzleIdentity
    Vk.VK_COMPONENT_SWIZZLE_ZERO -> SwizzleZero
    Vk.VK_COMPONENT_SWIZZLE_ONE -> SwizzleOne
    Vk.VK_COMPONENT_SWIZZLE_R -> SwizzleR
    Vk.VK_COMPONENT_SWIZZLE_G -> SwizzleG
    Vk.VK_COMPONENT_SWIZZLE_B -> SwizzleB
    Vk.VK_COMPONENT_SWIZZLE_A -> SwizzleA

unVkComponentSwizzle :: VkComponentSwizzle -> Vk.VkComponentSwizzle
unVkComponentSwizzle a =
  case a of
    SwizzleIdentity -> Vk.VK_COMPONENT_SWIZZLE_IDENTITY
    SwizzleZero -> Vk.VK_COMPONENT_SWIZZLE_ZERO
    SwizzleOne -> Vk.VK_COMPONENT_SWIZZLE_ONE
    SwizzleR -> Vk.VK_COMPONENT_SWIZZLE_R
    SwizzleG -> Vk.VK_COMPONENT_SWIZZLE_G
    SwizzleB -> Vk.VK_COMPONENT_SWIZZLE_B
    SwizzleA -> Vk.VK_COMPONENT_SWIZZLE_A

data VkComponentMapping
  = VkComponentMapping
  { r :: VkComponentSwizzle
  , g :: VkComponentSwizzle
  , b :: VkComponentSwizzle
  , a :: VkComponentSwizzle
  } deriving (Eq, Ord, Show)

vkComponentMapping :: Vk.VkComponentMapping -> VkComponentMapping
vkComponentMapping cm =
  VkComponentMapping
  { r = vkComponentSwizzle $ Vk.getField @"r" cm
  , g = vkComponentSwizzle $ Vk.getField @"g" cm
  , b = vkComponentSwizzle $ Vk.getField @"b" cm
  , a = vkComponentSwizzle $ Vk.getField @"a" cm
  }

unVkComponentMapping :: MonadIO m => VkComponentMapping -> m Vk.VkComponentMapping
unVkComponentMapping cm =
  liftIO $
  Vk.newVkData $ \p -> do
    Vk.writeField @"r" p (unVkComponentSwizzle $ r cm)
    Vk.writeField @"g" p (unVkComponentSwizzle $ g cm)
    Vk.writeField @"b" p (unVkComponentSwizzle $ b cm)
    Vk.writeField @"a" p (unVkComponentSwizzle $ a cm)

data VkImageAspectFlag
  = Color
  | Depth
  | Stencil
  | Metadata
  | Plane0
  | Plane1
  | Plane2
  -- | MemoryPlane0EXT
  -- | MemoryPlane1EXT
  -- | MemoryPlane2EXT
  -- | MemoryPlane3EXT
  | Plane0KHR
  | Plane1KHR
  | Plane2KHR
  deriving (Eq, Ord, Show)

vkImageAspectBit :: Vk.VkImageAspectBitmask a -> VkImageAspectFlag
vkImageAspectBit a =
  case a of
    Vk.VK_IMAGE_ASPECT_COLOR_BIT -> Color
    Vk.VK_IMAGE_ASPECT_DEPTH_BIT -> Depth
    Vk.VK_IMAGE_ASPECT_STENCIL_BIT -> Stencil
    Vk.VK_IMAGE_ASPECT_METADATA_BIT -> Metadata
    (unsafeCoerce -> Vk.VK_IMAGE_ASPECT_PLANE_0_BIT) -> Plane0
    (unsafeCoerce -> Vk.VK_IMAGE_ASPECT_PLANE_1_BIT) -> Plane1
    (unsafeCoerce -> Vk.VK_IMAGE_ASPECT_PLANE_2_BIT) -> Plane2
    {-
    Vk.VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT -> MemoryPlane0EXT
    Vk.VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT -> MemoryPlane1EXT
    Vk.VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT -> MemoryPlane2EXT
    Vk.VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT -> MemoryPlane3EXT
-}
    (unsafeCoerce -> Vk.VK_IMAGE_ASPECT_PLANE_0_BIT_KHR) -> Plane0KHR
    (unsafeCoerce -> Vk.VK_IMAGE_ASPECT_PLANE_1_BIT_KHR) -> Plane1KHR
    (unsafeCoerce -> Vk.VK_IMAGE_ASPECT_PLANE_2_BIT_KHR) -> Plane2KHR

unVkImageAspectBit :: VkImageAspectFlag -> Vk.VkImageAspectBitmask a
unVkImageAspectBit a =
  case a of
    Color -> Vk.VK_IMAGE_ASPECT_COLOR_BIT
    Depth -> Vk.VK_IMAGE_ASPECT_DEPTH_BIT
    Stencil -> Vk.VK_IMAGE_ASPECT_STENCIL_BIT
    Metadata -> Vk.VK_IMAGE_ASPECT_METADATA_BIT
    Plane0 -> unsafeCoerce Vk.VK_IMAGE_ASPECT_PLANE_0_BIT
    Plane1 -> unsafeCoerce Vk.VK_IMAGE_ASPECT_PLANE_1_BIT
    Plane2 -> unsafeCoerce Vk.VK_IMAGE_ASPECT_PLANE_2_BIT
    {-
    MemoryPlane0EXT -> Vk.VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT
    MemoryPlane1EXT -> Vk.VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT
    MemoryPlane2EXT -> Vk.VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT
    MemoryPlane3EXT -> Vk.VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT
-}
    Plane0KHR -> unsafeCoerce Vk.VK_IMAGE_ASPECT_PLANE_0_BIT_KHR
    Plane1KHR -> unsafeCoerce Vk.VK_IMAGE_ASPECT_PLANE_1_BIT_KHR
    Plane2KHR -> unsafeCoerce Vk.VK_IMAGE_ASPECT_PLANE_2_BIT_KHR

vkImageAspectBits ::
  Vk.VkImageAspectFlags ->
  [VkImageAspectFlag]
vkImageAspectBits bs =
  foldr
    (\(mask, val) b -> if mask .&. bs == mask then val : b else b)
    []
    [ (Vk.VK_IMAGE_ASPECT_COLOR_BIT, Color)
    , (Vk.VK_IMAGE_ASPECT_DEPTH_BIT, Depth)
    , (Vk.VK_IMAGE_ASPECT_STENCIL_BIT, Stencil)
    , (Vk.VK_IMAGE_ASPECT_METADATA_BIT, Metadata)
    , (unsafeCoerce Vk.VK_IMAGE_ASPECT_PLANE_0_BIT, Plane0)
    , (unsafeCoerce Vk.VK_IMAGE_ASPECT_PLANE_1_BIT, Plane1)
    , (unsafeCoerce Vk.VK_IMAGE_ASPECT_PLANE_2_BIT, Plane2)
    {-
    , (Vk.VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT, MemoryPlane0EXT)
    , (Vk.VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT, MemoryPlane1EXT)
    , (Vk.VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT, MemoryPlane2EXT)
    , (Vk.VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT, MemoryPlane3EXT)
-}
    , (unsafeCoerce Vk.VK_IMAGE_ASPECT_PLANE_0_BIT_KHR, Plane0KHR)
    , (unsafeCoerce Vk.VK_IMAGE_ASPECT_PLANE_1_BIT_KHR, Plane1KHR)
    , (unsafeCoerce Vk.VK_IMAGE_ASPECT_PLANE_2_BIT_KHR, Plane2KHR)
    ]

unVkImageAspectBits ::
  [VkImageAspectFlag] ->
  Vk.VkImageAspectFlags
unVkImageAspectBits = foldr (\a b -> unVkImageAspectBit a .|. b) 0

data VkImageSubresourceRange
  = VkImageSubresourceRange
  { aspectMask :: [VkImageAspectFlag]
  , baseMipLevel :: Word32
  , levelCount :: Word32
  , baseArrayLayer :: Word32
  , layerCount :: Word32
  } deriving (Eq, Ord, Show)

vkImageSubresourceRange :: Vk.VkImageSubresourceRange -> VkImageSubresourceRange
vkImageSubresourceRange p =
  VkImageSubresourceRange
  { aspectMask = vkImageAspectBits $ Vk.getField @"aspectMask" p
  , baseMipLevel = Vk.getField @"baseMipLevel" p
  , levelCount = Vk.getField @"levelCount" p
  , baseArrayLayer = Vk.getField @"baseArrayLayer" p
  , layerCount = Vk.getField @"layerCount" p
  }

unVkImageSubresourceRange :: MonadIO m => VkImageSubresourceRange -> m Vk.VkImageSubresourceRange
unVkImageSubresourceRange p =
  liftIO $
  Vk.newVkData $ \ptr -> do
    Vk.writeField @"aspectMask" ptr (unVkImageAspectBits $ aspectMask p)
    Vk.writeField @"baseMipLevel" ptr (baseMipLevel p)
    Vk.writeField @"levelCount" ptr (levelCount p)
    Vk.writeField @"baseArrayLayer" ptr (baseArrayLayer p)
    Vk.writeField @"layerCount" ptr (layerCount p)

data VkImageViewCreateInfo
  = VkImageViewCreateInfo
  { flags :: [VkImageViewCreateFlag]
  , image :: Vk.VkImage
  , viewType :: VkImageViewType
  , format :: VkFormat
  , components :: VkComponentMapping
  , subresourceRange :: VkImageSubresourceRange
  } deriving (Eq, Ord, Show)

vkImageViewCreateInfo :: Vk.VkImageViewCreateInfo -> VkImageViewCreateInfo
vkImageViewCreateInfo a =
  VkImageViewCreateInfo
  { flags = vkImageViewCreateBits $ Vk.getField @"flags" a
  , image = Vk.getField @"image" a
  , viewType = vkImageViewType $ Vk.getField @"viewType" a
  , format = vkFormat $ Vk.getField @"format" a
  , components = vkComponentMapping $ Vk.getField @"components" a
  , subresourceRange = vkImageSubresourceRange $ Vk.getField @"subresourceRange" a
  }

unVkImageViewCreateInfo :: MonadIO m => VkImageViewCreateInfo -> m Vk.VkImageViewCreateInfo
unVkImageViewCreateInfo a =
  liftIO $
  Vk.newVkData $ \ptr -> do
    Vk.writeField @"sType" ptr Vk.VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
    Vk.writeField @"pNext" ptr Vk.VK_NULL
    Vk.writeField @"flags" ptr (unVkImageViewCreateBits $ flags a)
    Vk.writeField @"image" ptr (image a)
    Vk.writeField @"viewType" ptr (unVkImageViewType $ viewType a)
    Vk.writeField @"format" ptr (unVkFormat $ format a)
    Vk.writeField @"components" ptr =<< unVkComponentMapping (components a)
    Vk.writeField @"subresourceRange" ptr =<< unVkImageSubresourceRange (subresourceRange a)

vkCreateImageView ::
  (MonadManaged m, MonadIO m) =>
  Vk.VkDevice ->
  VkImageViewCreateInfo ->
  Foreign.Ptr Vk.VkAllocationCallbacks ->
  m Vk.VkImageView
vkCreateImageView d info cbs = do
  ivPtr <- using $ managed Foreign.alloca
  info' <- unVkImageViewCreateInfo info
  liftIO $ vkResult =<< Vk.vkCreateImageView d (Vk.unsafePtr info') cbs ivPtr
  using $ managed (bracket (Foreign.peek ivPtr) (\iv -> Vk.vkDestroyImageView d iv cbs))
