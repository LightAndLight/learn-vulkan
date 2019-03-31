module Graphics.Vulkan.Pipeline.BindPoint where

import qualified Graphics.Vulkan.Core_1_0 as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_ray_tracing as Vk

data VkPipelineBindPoint
  = Graphics
  | Compute
  -- RayTracingNV
  deriving (Eq, Ord, Show)

unVkPipelineBindPoint ::
  VkPipelineBindPoint ->
  Vk.VkPipelineBindPoint
unVkPipelineBindPoint a =
  case a of
    Graphics -> Vk.VK_PIPELINE_BIND_POINT_GRAPHICS
    Compute -> Vk.VK_PIPELINE_BIND_POINT_COMPUTE
    -- RayTracingNV -> Vk.VK_PIPELINE_BIND_POINT_RAY_TRACING_NV

