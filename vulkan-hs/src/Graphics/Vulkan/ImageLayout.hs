module Graphics.Vulkan.ImageLayout where

import qualified Graphics.Vulkan.Core_1_0 as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_fragment_density_map as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_maintenance2 as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_shared_presentable_image as Vk
import qualified Graphics.Vulkan.Ext.VK_KHR_swapchain as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_shading_rate_image as Vk

data VkImageLayout
  = Undefined
  | General
  | ColorAttachmentOptimal
  | DepthStencilAttachmentOptimal
  | DepthStencilReadOnlyOptimal
  | ShaderReadOnlyOptimal
  | TransferSrcOptimal
  | TransferDstOptimal
  | Preinitialized
  -- DepthReadOnlyStencilAttachmentOptimal
  -- DepthAttachmentStencilReadOnlyOptimal
  | PresentSrcKHR
  | SharedPresentKHR
  -- ShadingRateOptimalNV
  -- FragmentDensityMapOptimalEXT
  | DepthReadOnlyStencilAttachmentOptimalKHR
  | DepthAttachmentStencilReadOnlyOptimalKHR
  deriving (Eq, Ord, Show)

unVkImageLayout :: VkImageLayout -> Vk.VkImageLayout
unVkImageLayout a =
  case a of
    Undefined -> Vk.VK_IMAGE_LAYOUT_UNDEFINED
    General -> Vk.VK_IMAGE_LAYOUT_GENERAL
    ColorAttachmentOptimal -> Vk.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
    DepthStencilAttachmentOptimal -> Vk.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
    DepthStencilReadOnlyOptimal -> Vk.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
    ShaderReadOnlyOptimal -> Vk.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    TransferSrcOptimal -> Vk.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
    TransferDstOptimal -> Vk.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
    Preinitialized -> Vk.VK_IMAGE_LAYOUT_PREINITIALIZED
    -- DepthReadOnlyStencilAttachmentOptimal ->
      -- Vk.VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
    -- DepthAttachmentStencilReadOnlyOptimal ->
      -- Vk.VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
    PresentSrcKHR ->
      Vk.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
    SharedPresentKHR ->
      Vk.VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR
    -- ShadingRateOptimalNV ->
      -- Vk.VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV
    -- FragmentDensityMapOptimalEXT ->
      -- Vk.VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
    DepthReadOnlyStencilAttachmentOptimalKHR ->
      Vk.VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL_KHR
    DepthAttachmentStencilReadOnlyOptimalKHR ->
      Vk.VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL_KHR
