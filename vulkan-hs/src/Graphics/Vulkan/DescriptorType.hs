module Graphics.Vulkan.DescriptorType where

import qualified Graphics.Vulkan.Core_1_0 as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_inline_uniform_block as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_ray_tracing

data VkDescriptorType
  = Sampler
  | CombinedImageSampler
  | SampledImage
  | StorageImage
  | UniformTexelBuffer
  | StorageTexelBuffer
  | UniformBuffer
  | StorageBuffer
  | UniformBufferDynamic
  | StorageBufferDynamic
  | InputAttachment
  -- InlineUniformBlockEXT
  -- AccelerationStructureNV
  deriving (Eq, Ord, Show)

unVkDescriptorType ::
  VkDescriptorType ->
  Vk.VkDescriptorType
unVkDescriptorType a =
  case a of
    Sampler ->
      Vk.VK_DESCRIPTOR_TYPE_SAMPLER
    CombinedImageSampler ->
      Vk.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
    SampledImage ->
      Vk.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
    StorageImage ->
      Vk.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
    UniformTexelBuffer ->
      Vk.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
    StorageTexelBuffer ->
      Vk.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
    UniformBuffer ->
      Vk.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
    StorageBuffer ->
      Vk.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
    UniformBufferDynamic ->
      Vk.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
    StorageBufferDynamic ->
      Vk.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
    InputAttachment ->
      Vk.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
    -- InlineUniformBlockEXT ->
      -- Vk.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
    -- AccelerationStructureNV ->
      -- Vk.VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
