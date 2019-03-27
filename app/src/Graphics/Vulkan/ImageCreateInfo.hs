module Graphics.Vulkan.ImageCreateInfo where

import Data.Bits ((.&.), (.|.))

import qualified Graphics.Vulkan.Core_1_0 as Vk

data VkImageUsageFlag
  = Src
  | Dst
  | Sampled
  | Storage
  | ColorAttachment
  | DepthStencilAttachment
  | TransientAttachment
  | InputAttachment
  -- | ShadingRateImage
  -- | FragmentDensityMap
  deriving (Eq, Ord, Show)

vkImageUsageBit ::
  Vk.VkImageUsageBitmask a ->
  VkImageUsageFlag
vkImageUsageBit a =
  case a of
    Vk.VK_IMAGE_USAGE_TRANSFER_SRC_BIT -> Src
    Vk.VK_IMAGE_USAGE_TRANSFER_DST_BIT -> Dst
    Vk.VK_IMAGE_USAGE_SAMPLED_BIT -> Sampled
    Vk.VK_IMAGE_USAGE_STORAGE_BIT -> Storage
    Vk.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT -> ColorAttachment
    Vk.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT -> DepthStencilAttachment
    Vk.VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT -> TransientAttachment
    Vk.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT -> InputAttachment
    -- Vk.VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV -> ShadingRateImage
    -- Vk.VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT -> FragmentDensityMap

unVkImageUsageBit ::
  VkImageUsageFlag ->
  Vk.VkImageUsageBitmask a
unVkImageUsageBit a =
  case a of
    Src -> Vk.VK_IMAGE_USAGE_TRANSFER_SRC_BIT
    Dst -> Vk.VK_IMAGE_USAGE_TRANSFER_DST_BIT
    Sampled -> Vk.VK_IMAGE_USAGE_SAMPLED_BIT
    Storage -> Vk.VK_IMAGE_USAGE_STORAGE_BIT
    ColorAttachment -> Vk.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
    DepthStencilAttachment -> Vk.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
    TransientAttachment -> Vk.VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
    InputAttachment -> Vk.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
    -- ShadingRateImage -> Vk.VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV
    -- FragmentDensityMap -> Vk.VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT

vkImageUsageBits ::
  Vk.VkImageUsageFlags ->
  [VkImageUsageFlag]
vkImageUsageBits bs =
  foldr
    (\(mask, val) b -> if mask .&. bs == mask then val : b else b)
    []
    [ (Vk.VK_IMAGE_USAGE_TRANSFER_SRC_BIT, Src)
    , (Vk.VK_IMAGE_USAGE_TRANSFER_DST_BIT, Dst)
    , (Vk.VK_IMAGE_USAGE_SAMPLED_BIT, Sampled)
    , (Vk.VK_IMAGE_USAGE_STORAGE_BIT, Storage)
    , (Vk.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT, ColorAttachment)
    , (Vk.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, DepthStencilAttachment)
    , (Vk.VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT, TransientAttachment)
    , (Vk.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT, InputAttachment)
    -- , (Vk.VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV, ShadingRateImage)
    -- , (Vk.VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT, FragmentDensityMap)
    ]

unVkImageUsageBits ::
  [VkImageUsageFlag] ->
  Vk.VkImageUsageFlags
unVkImageUsageBits = foldr (\a b -> unVkImageUsageBit a .|. b) 0
