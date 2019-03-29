module Graphics.Vulkan.Access where

import Data.Bits ((.|.))
import Unsafe.Coerce (unsafeCoerce)

import qualified Graphics.Vulkan.Core_1_0 as Vk
import qualified Graphics.Vulkan.Ext.VK_EXT_blend_operation_advanced as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_conditional_rendering as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_fragment_density_map as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_transform_feedback as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_ray_tracing as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_shading_rate_image as Vk
import qualified Graphics.Vulkan.Ext.VK_NVX_device_generated_commands as Vk

data VkAccessFlag
  = IndirectCommandRead
  | IndexRead
  | VertexAttributeRead
  | UniformRead
  | InputAttachmentRead
  | ShaderRead
  | ShaderWrite
  | ColorAttachmentRead
  | ColorAttachmentWrite
  | DepthStencilAttachmentRead
  | DepthStencilAttachmentWrite
  | TransferRead
  | TransferWrite
  | HostRead
  | HostWrite
  | MemoryRead
  | MemoryWrite
  -- TransformFeedbackWriteEXT
  -- TransformFeedbackCounterReadEXT
  -- TransformFeedbackCounterWriteEXT
  -- ConditionalRenderingReadEXT
  | CommandProcessReadNVX
  | CommandProcessWriteNVX
  | ColorAttachmentReadNoncoherentEXT
  -- ShadingRateImageReadNV
  -- AccelerationStructureReadNV
  -- AccelerationStructureWriteNV
  -- FragmentDensityMapReadEXT
  deriving (Eq, Ord, Show)

unVkAccessBit :: VkAccessFlag -> Vk.VkAccessBitmask a
unVkAccessBit a =
  case a of
    IndirectCommandRead -> Vk.VK_ACCESS_INDIRECT_COMMAND_READ_BIT
    IndexRead -> Vk.VK_ACCESS_INDEX_READ_BIT
    VertexAttributeRead -> Vk.VK_ACCESS_VERTEX_ATTRIBUTE_READ_BIT
    UniformRead -> Vk.VK_ACCESS_UNIFORM_READ_BIT
    InputAttachmentRead -> Vk.VK_ACCESS_INPUT_ATTACHMENT_READ_BIT
    ShaderRead -> Vk.VK_ACCESS_SHADER_READ_BIT
    ShaderWrite -> Vk.VK_ACCESS_SHADER_WRITE_BIT
    ColorAttachmentRead -> Vk.VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
    ColorAttachmentWrite -> Vk.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
    DepthStencilAttachmentRead -> Vk.VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
    DepthStencilAttachmentWrite -> Vk.VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
    TransferRead -> Vk.VK_ACCESS_TRANSFER_READ_BIT
    TransferWrite -> Vk.VK_ACCESS_TRANSFER_WRITE_BIT
    HostRead -> Vk.VK_ACCESS_HOST_READ_BIT
    HostWrite -> Vk.VK_ACCESS_HOST_WRITE_BIT
    MemoryRead -> Vk.VK_ACCESS_MEMORY_READ_BIT
    MemoryWrite -> Vk.VK_ACCESS_MEMORY_WRITE_BIT
    -- TransformFeedbackWriteEXT -> Vk.VK_ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT
    -- TransformFeedbackCounterReadEXT -> Vk.VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT
    -- TransformFeedbackCounterWriteEXT -> Vk.VK_ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT
    -- ConditionalRenderingReadEXT -> Vk.VK_ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT
    CommandProcessReadNVX -> unsafeCoerce Vk.VK_ACCESS_COMMAND_PROCESS_READ_BIT_NVX
    CommandProcessWriteNVX -> unsafeCoerce Vk.VK_ACCESS_COMMAND_PROCESS_WRITE_BIT_NVX
    ColorAttachmentReadNoncoherentEXT -> unsafeCoerce Vk.VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
    -- ShadingRateImageReadNV -> Vk.VK_ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV
    -- AccelerationStructureReadNV -> Vk.VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV
    -- AccelerationStructureWriteNV -> Vk.VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV
    -- FragmentDensityMapReadEXT -> Vk.VK_ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT

unVkAccessBits :: [VkAccessFlag] -> Vk.VkAccessFlags
unVkAccessBits = foldr (\a b -> unVkAccessBit a .|. b) 0
