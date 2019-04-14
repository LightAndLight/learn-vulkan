module Graphics.Vulkan.IndexType where

import qualified Graphics.Vulkan.Core_1_0 as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_ray_tracing as Vk

data VkIndexType
  = IndexUInt16
  | IndexUInt32
  -- IndexNoneNV
  deriving (Eq, Ord, Show)

unVkIndexType ::
  VkIndexType ->
  Vk.VkIndexType
unVkIndexType a =
  case a of
    IndexUInt16 -> Vk.VK_INDEX_TYPE_UINT16
    IndexUInt32 -> Vk.VK_INDEX_TYPE_UINT32
    -- IndexNoneNV -> Vk.VK_INDEX_TYPE_NONE_NV
