{-# language ViewPatterns #-}
module Graphics.Vulkan.PipelineStage where

import Data.Bits ((.|.))
import Unsafe.Coerce (unsafeCoerce)

-- import qualified Graphics.Vulkan.Ext.VK_EXT_conditional_rendering as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_fragment_density_map as Vk
-- import qualified Graphics.Vulkan.Ext.VK_EXT_transform_feedback as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_mesh_shader as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_ray_tracing as Vk
-- import qualified Graphics.Vulkan.Ext.VK_NV_shading_rate_image as Vk
import qualified Graphics.Vulkan.Ext.VK_NVX_device_generated_commands as Vk

data VkPipelineStageFlag
  = TopOfPipe
  | DrawIndirect
  | VertexInput
  | VertexShader
  | TessellationControlShader
  | TessellationEvaluationShader
  | GeometryShader
  | FragmentShader
  | EarlyFragmentTests
  | LateFragmentTests
  | ColorAttachmentOutput
  | ComputeShader
  | Transfer
  | BottomOfPipe
  | Host
  | AllGraphics
  | AllCommands
  -- TransformFeedbackEXT
  -- ConditionalRenderingEXT
  | CommandProcessNVX
  -- ShadingRateImageNV
  -- RayTracingShaderNV
  -- AccelerationStructureBuildNV
  -- TaskShaderNV
  -- MeshShaderNV
  -- FragmentDensityProcessEXT
  deriving (Eq, Ord, Show)

unVkPipelineStageBit ::
  VkPipelineStageFlag ->
  Vk.VkPipelineStageBitmask a
unVkPipelineStageBit a =
  case a of
    TopOfPipe ->
      Vk.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
    DrawIndirect ->
      Vk.VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT
    VertexInput ->
      Vk.VK_PIPELINE_STAGE_VERTEX_INPUT_BIT
    VertexShader ->
      Vk.VK_PIPELINE_STAGE_VERTEX_SHADER_BIT
    TessellationControlShader ->
      Vk.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT
    TessellationEvaluationShader ->
      Vk.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
    GeometryShader ->
      Vk.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
    FragmentShader ->
      Vk.VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    EarlyFragmentTests ->
      Vk.VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
    LateFragmentTests ->
      Vk.VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
    ColorAttachmentOutput ->
      Vk.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    ComputeShader ->
      Vk.VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT
    Transfer ->
      Vk.VK_PIPELINE_STAGE_TRANSFER_BIT
    BottomOfPipe ->
      Vk.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
    Host ->
      Vk.VK_PIPELINE_STAGE_HOST_BIT
    AllGraphics ->
      Vk.VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT
    AllCommands ->
      Vk.VK_PIPELINE_STAGE_ALL_COMMANDS_BIT
    -- TransformFeedbackEXT ->
      -- Vk.VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT
    -- ConditionalRenderingEXT ->
      -- Vk.VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
    CommandProcessNVX ->
      unsafeCoerce $
      Vk.VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX
    -- ShadingRateImageNV ->
      -- Vk.VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV
    -- RayTracingShaderNV ->
      -- Vk.VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV
    -- AccelerationStructureBuildNV ->
      -- Vk.VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV
    -- TaskShaderNV ->
      -- Vk.VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV
    -- MeshShaderNV ->
      -- Vk.VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV
    -- FragmentDensityProcessEXT ->
      -- Vk.VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT

unVkPipelineStageBits :: [VkPipelineStageFlag] -> Vk.VkPipelineStageFlags
unVkPipelineStageBits = foldr (\a b -> unVkPipelineStageBit a .|. b) 0
